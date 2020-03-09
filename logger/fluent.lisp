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

   (buffer :initform nil
           :accessor fluent-logger-buffer)
   (socket :initform nil
           :accessor fluent-logger-socket)
   (socket-lock :initform (bt:make-recursive-lock))
   (write-thread :initform nil)))

(defmethod initialize-instance :after ((logger fluent-logger) &rest initargs)
  (declare (ignore initargs))
  (with-slots (host port) logger
    (unless host
      (setf host "127.0.0.1"))
    (unless port
      (setf port 24224))))

(defmethod open-logger ((fluent-logger fluent-logger))
  (with-slots (host port timeout socket buffer write-thread) fluent-logger
    (when socket
      (restart-case
          (error "Socket is already opened.")
        (close-logger ()
          :report "Close the existing socket"
          (close-logger fluent-logger))))
    (setf socket
          (usocket:socket-connect host port
                                  :element-type '(unsigned-byte 8)
                                  :timeout timeout))
    (setf buffer (make-instance 'chanl:unbounded-channel))
    (when write-thread
      (error "write-thread is already running"))
    (setf write-thread
          (bt:make-thread
            (lambda ()
              (loop
                (flush-buffer fluent-logger :infinite t)
                (sleep 5)))
            :name "fluent-logger write-thread"))))

(defmethod close-logger ((fluent-logger fluent-logger))
  (with-slots (socket buffer write-thread) fluent-logger
    (when socket
      (when buffer
        (flush-buffer fluent-logger)
        (setf buffer nil))
      (usocket:socket-close socket)
      (setf socket nil))
    (when write-thread
      (bt:destroy-thread write-thread)
      (setf write-thread nil))))

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
    (chanl:send (fluent-logger-buffer fluent-logger) bytes)))

(defgeneric flush-buffer (fluent-logger &key infinite)
  (:method ((fluent-logger fluent-logger) &key infinite)
    (handler-case
        (progn
          (unless (fluent-logger-socket fluent-logger)
            (open-logger fluent-logger))
          (with-slots (socket socket-lock buffer) fluent-logger
            (let ((stream (usocket:socket-stream socket)))
              (bt:with-recursive-lock-held (socket-lock)
                (loop
                  (when (and (not infinite)
                             (chanl:recv-blocks-p buffer))
                    (return))
                  (let ((bytes (chanl:recv buffer)))
                    (declare ((array (unsigned-byte 8) (*)) bytes))
                    (write-sequence bytes stream)))
                (force-output stream)
                t))))
      ((or usocket:socket-error
           #+sbcl sb-int:simple-stream-error) (e)
        (warn "Socket error: ~A" e)
        (with-slots (socket socket-lock) fluent-logger
          (when socket
            (bt:with-lock-held (socket-lock)
              (ignore-errors
                (usocket:socket-close socket))
              (setf socket nil))))

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
