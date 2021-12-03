(defpackage #:cl-fluent-logger/logger/fluent
  (:use #:cl
        #:cl-fluent-logger/logger/base)
  (:import-from #:cl-fluent-logger/event-time
                #:event-time)
  (:import-from #:usocket)
  (:import-from #:messagepack)
  (:import-from #:chanl
                #:unbounded-channel
                #:send
                #:recv
                #:recv-blocks-p)
  (:import-from #:local-time)
  (:import-from #:pack)
  (:import-from #:bordeaux-threads)
  (:export #:fluent-logger
           #:fluent-logger-tag
           #:fluent-logger-host
           #:fluent-logger-port
           #:fluent-logger-timeout))
(in-package #:cl-fluent-logger/logger/fluent)

(defstruct fluent-connection
  (buffer (make-instance 'chanl:unbounded-channel))
  socket
  (socket-lock (bt:make-recursive-lock "fluentd socket lock"))
  write-thread)

(defclass fluent-logger (base-logger)
  ((tag :type (or null string)
        :initarg :tag
        :initform nil
        :accessor fluent-logger-tag)
   (host :type string
         :initarg :host
         :initform "127.0.0.1"
         :accessor fluent-logger-host)
   (port :type integer
         :initarg :port
         :initform 24224
         :accessor fluent-logger-port)
   (timeout :type number
            :initarg :timeout
            :initform 3.0
            :accessor fluent-logger-timeout)
   (nanosecond-precision :type boolean
                         :initarg :nanosecond-precision
                         :initform nil
                         :accessor fluent-logger-nanosecond-precision)

   (connection-registry :initform (make-hash-table :test 'eq))
   (connection-registry-lock :initform (bt:make-lock "connection registry lock"))))

(defmethod initialize-instance :after ((logger fluent-logger) &rest initargs)
  (declare (ignore initargs))
  (with-slots (host port) logger
    (unless host
      (setf host "127.0.0.1"))
    (unless port
      (setf port 24224))))

(defun fluent-logger-connection (fluent-logger)
  (with-slots (connection-registry connection-registry-lock) fluent-logger
    (or (gethash (bt:current-thread) connection-registry)
        (bt:with-lock-held (connection-registry-lock)
          (setf (gethash (bt:current-thread) connection-registry)
                (make-fluent-connection))))))

(define-condition connection-not-established (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Socket connection is not established yet."))))

(defmethod open-logger ((fluent-logger fluent-logger))
  (let ((connection (fluent-logger-connection fluent-logger)))
    (symbol-macrolet ((socket (fluent-connection-socket connection))
                      (socket-lock (fluent-connection-socket-lock connection))
                      (write-thread (fluent-connection-write-thread connection)))
      (bt:with-recursive-lock-held (socket-lock)
        (when socket
          (restart-case
              (error "Socket is already opened.")
            (close-logger ()
              :report "Close the existing socket"
              (close-logger fluent-logger))))
        (with-slots (host port timeout) fluent-logger
          (setf socket
                (usocket:socket-connect host port
                                        :element-type '(unsigned-byte 8)
                                        :timeout timeout))))
      (when write-thread
        (bt:destroy-thread write-thread))
      (setf write-thread
            (bt:make-thread
              (lambda ()
                (loop
                  (handler-case
                      (flush-buffer connection :infinite t)
                    (connection-not-established (e)
                      (warn "~A" e)
                      (warn "Retrying in a second..."))
                    (error (e)
                      (warn "Error (~A) raised while flushing the buffer: ~A" (type-of e) e)))
                  (sleep 1)))
              :name "fluent-logger write-thread")))
    connection))

(defmethod close-logger ((fluent-logger fluent-logger))
  (let ((connection (fluent-logger-connection fluent-logger)))
    (symbol-macrolet ((socket (fluent-connection-socket connection))
                      (socket-lock (fluent-connection-socket-lock connection))
                      (buffer (fluent-connection-buffer connection))
                      (write-thread (fluent-connection-write-thread connection)))
      (when write-thread
        (bt:destroy-thread write-thread)
        (setf write-thread nil))
      (when socket
        (flush-buffer connection :infinite nil)
        (bt:with-recursive-lock-held (socket-lock)
          (ignore-errors
            (usocket:socket-close socket))
          (setf socket nil))))
    connection))

(defgeneric make-packet (fluent-logger tag timestamp data)
  (:method ((fluent-logger fluent-logger) tag timestamp data)
    (let ((tag
            (cond
              ((and (fluent-logger-tag fluent-logger)
                    tag)
               (format nil "~A.~A"
                       (fluent-logger-tag fluent-logger)
                       tag))
              (tag)
              ((fluent-logger-tag fluent-logger)))))
      (messagepack:encode
       (list tag timestamp data)))))

(defgeneric send-packet (fluent-logger bytes)
  (:method ((fluent-logger fluent-logger) bytes)
    (check-type bytes (array (unsigned-byte 8) (*)))
    (chanl:send (fluent-connection-buffer (fluent-logger-connection fluent-logger)) bytes))
  (:method :before ((fluent-logger fluent-logger) bytes)
    (unless (fluent-connection-socket (fluent-logger-connection fluent-logger))
      (open-logger fluent-logger))))

(defun flush-buffer (connection &key infinite)
  (symbol-macrolet ((socket (fluent-connection-socket connection))
                    (socket-lock (fluent-connection-socket-lock connection))
                    (buffer (fluent-connection-buffer connection)))
    (handler-case
        (progn
          (unless socket
            (error 'connection-not-established))
          (let ((stream (usocket:socket-stream socket)))
            (loop
              (when (and (not infinite)
                         (chanl:recv-blocks-p buffer))
                (return))
              (let ((bytes (chanl:recv buffer)))
                (declare ((array (unsigned-byte 8) (*)) bytes))
                (bt:with-recursive-lock-held (socket-lock)
                  (write-sequence bytes stream)
                  (force-output stream))))
            t))
      ((or usocket:socket-error
           #+sbcl sb-int:simple-stream-error) (e)
        (warn "Socket error: ~A" e)
        (bt:with-lock-held (socket-lock)
          (ignore-errors
            (usocket:socket-close socket))
          (setf socket nil))

        nil))))

(defmethod post-with-time ((fluent-logger fluent-logger) tag data time)
  (let ((messagepack:*extended-types* (messagepack:define-extension-types
                                          '(:numeric 0 event-time))))
    (send-packet fluent-logger
                 (make-packet fluent-logger
                              tag
                              (etypecase time
                                (local-time:timestamp
                                 (if (fluent-logger-nanosecond-precision fluent-logger)
                                     (make-instance 'event-time
                                                    :data
                                                    (pack:pack ">II"
                                                               (local-time:timestamp-to-unix time)
                                                               (local-time:nsec-of time)))
                                     (local-time:timestamp-to-unix time)))
                                (integer
                                 time))
                              data))))
