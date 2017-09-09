(defpackage #:cl-fluent-logger/logger/fluent
  (:use #:cl
        #:cl-fluent-logger/logger/base)
  (:import-from #:cl-fluent-logger/event-time
                #:event-time)
  (:import-from #:usocket)
  (:import-from #:messagepack)
  (:import-from #:local-time)
  (:import-from #:pack)
  (:import-from #:bordeaux-threads)
  (:export #:fluent-logger
           #:fluent-logger-tag
           #:fluent-logger-host
           #:fluent-logger-port
           #:fluent-logger-buffer-limit
           #:fluent-logger-timeout))
(in-package #:cl-fluent-logger/logger/fluent)

(defparameter *default-buffer-limit*
  (* 8 1024 1024))

(defun make-pendings-array ()
  (make-array 0 :element-type '(array (unsigned-byte 8) (*))
                :adjustable t :fill-pointer 0))

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
   (buffer-limit :type integer
                 :initarg :buffer-limit
                 :initform *default-buffer-limit*
                 :accessor fluent-logger-buffer-limit)
   (timeout :type number
            :initarg :timeout
            :initform 3.0
            :accessor fluent-logger-timeout)
   (nanosecond-precision :type boolean
                         :initarg :nanosecond-precision
                         :initform nil
                         :accessor fluent-logger-nanosecond-precision)

   (pendings :type array
             :initform (make-pendings-array)
             :accessor fluent-logger-pendings)
   (pendings-lock :initform (bt:make-lock))
   (socket :initform nil
           :accessor fluent-logger-socket)
   (socket-lock :initform (bt:make-recursive-lock))))

(defmethod initialize-instance :after ((logger fluent-logger) &rest initargs)
  (declare (ignore initargs))
  (with-slots (host port) logger
    (unless host
      (setf host "127.0.0.1"))
    (unless port
      (setf port 24224))))

(defmethod open-logger ((fluent-logger fluent-logger))
  (with-slots (host port timeout socket) fluent-logger
    (when socket
      (restart-case
          (error "Socket is already opened.")
        (close-logger ()
          :report "Close the existing socket"
          (close-logger fluent-logger))))
    (setf socket
          (usocket:socket-connect host port
                                  :element-type '(unsigned-byte 8)
                                  :timeout timeout))))

(defmethod close-logger ((fluent-logger fluent-logger))
  (with-slots (socket pendings) fluent-logger
    (when socket
      (when pendings
        (send-packet fluent-logger (make-array 0 :element-type '(unsigned-byte 8))))
      (usocket:socket-close socket)
      (setf socket nil))))

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
    (with-slots (pendings pendings-lock) fluent-logger
      (bt:with-lock-held (pendings-lock)
        (vector-push-extend bytes pendings))
      (handler-case
          (progn
            (unless (fluent-logger-socket fluent-logger)
              (open-logger fluent-logger))
            (with-slots (socket socket-lock) fluent-logger
              (let ((stream (usocket:socket-stream socket)))
                (bt:with-recursive-lock-held (socket-lock)
                  (bt:with-lock-held (pendings-lock)
                    (loop for bytes across pendings
                          do (write-sequence bytes stream))
                    (setf pendings (make-pendings-array)))
                  (force-output stream)
                  t))))
        ((or usocket:socket-error
          #+sbcl sb-int:simple-stream-error) ()
          (with-slots (socket) fluent-logger
            (when socket
              (ignore-errors
               (usocket:socket-close socket))
              (setf socket nil)))
          (bt:with-lock-held (pendings-lock)
            (when (< (fluent-logger-buffer-limit fluent-logger)
                     (loop for bytes across pendings
                           summing (length bytes)))
              (warn "Buffer limit exceeded")
              (setf pendings (make-pendings-array))))

          nil)))))

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
