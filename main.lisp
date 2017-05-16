(defpackage #:cl-fluent-logger
  (:nicknames #:cl-fluent-logger/main
              #:fluent-logger)
  (:use #:cl)
  (:import-from #:usocket)
  (:import-from #:messagepack)
  (:import-from #:local-time)
  (:import-from #:pack)
  (:export #:fluent-logger
           #:fluent-logger-tag
           #:fluent-logger-host
           #:fluent-logger-port
           #:fluent-logger-buffer-limit
           #:fluent-logger-timeout
           #:open-socket
           #:close-socket
           #:send))
(in-package #:cl-fluent-logger)

(defparameter *default-buffer-limit*
  (* 8 1024 1024))

(defun make-pendings-array ()
  (make-array 0 :element-type '(array (unsigned-byte 8) (*))
                :adjustable t :fill-pointer 0))

(defclass fluent-logger ()
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
   (socket :initform nil
           :accessor fluent-logger-socket)
   (socket-lock :initform (bt:make-lock))))

(defmethod initialize-instance :after ((logger fluent-logger) &rest initargs)
  (declare (ignore initargs))
  (with-slots (host port) logger
    (unless host
      (setf host "127.0.0.1"))
    (unless port
      (setf port 24224))))

(defgeneric open-socket (fluent-logger)
  (:method ((fluent-logger fluent-logger))
    (with-slots (host port timeout socket) fluent-logger
      (when socket
        (restart-case
            (error "Socket is already opened.")
          (close-socket ()
            :report "Close the existing socket"
            (close-socket fluent-logger))))
      (setf socket
            (usocket:socket-connect host port
                                    :element-type '(unsigned-byte 8)
                                    :timeout timeout)))))

(defgeneric close-socket (fluent-logger)
  (:method ((fluent-logger fluent-logger))
    (with-slots (socket pendings) fluent-logger
      (when socket
        (when pendings
          (send-packet fluent-logger #()))
        (usocket:socket-close socket)
        (setf socket nil)))))

(defgeneric make-packet (fluent-logger label timestamp data)
  (:method ((fluent-logger fluent-logger) label timestamp data)
    (let ((tag
            (cond
              ((and (fluent-logger-tag fluent-logger)
                    label)
               (format nil "~A.~A"
                       (fluent-logger-tag fluent-logger)
                       label))
              (label)
              ((fluent-logger-tag fluent-logger)))))
      (messagepack:encode
       (list tag timestamp data)))))

(defgeneric send-packet (fluent-logger bytes)
  (:method ((fluent-logger fluent-logger) bytes)
    (check-type bytes (array (unsigned-byte 8) (*)))
    (with-slots (pendings) fluent-logger
      (vector-push-extend bytes pendings)
      (handler-case
          (progn
            (unless (fluent-logger-socket fluent-logger)
              (open-socket fluent-logger))
            (with-slots (socket socket-lock) fluent-logger
              (let ((stream (usocket:socket-stream socket)))
                (bt:with-lock-held (socket-lock)
                  (loop for bytes across pendings
                        do (write-sequence bytes stream))
                  (force-output stream)
                  (setf pendings (make-pendings-array))
                  t))))
        (usocket:socket-error ()
          (close-socket fluent-logger)
          (when (< (fluent-logger-buffer-limit fluent-logger)
                   (loop for bytes across pendings
                         summing (length bytes)))
            (warn "Buffer limit exceeded")
            (setf pendings (make-pendings-array)))

          nil)))))

(defclass event-time (messagepack:extension-type)
  ((data :initarg :data)))

(defmethod initialize-instance :before ((object event-time) &rest initargs)
  (declare (ignore initargs))
  (setf (messagepack::extension-type-id object) 0))

(defgeneric send (fluent-logger label data &optional now)
  (:method ((fluent-logger fluent-logger) label data &optional now)
    (let ((now (or now (local-time:now)))
          (messagepack:*extended-types* (messagepack:define-extension-types
                                            '(:numeric 0 event-time))))
      (send-packet fluent-logger
                   (make-packet fluent-logger
                                label
                                (etypecase now
                                  (local-time:timestamp
                                   (if (fluent-logger-nanosecond-precision fluent-logger)
                                       (make-instance 'event-time
                                                      :data
                                                      (pack:pack ">II"
                                                                 (local-time:timestamp-to-unix now)
                                                                 (local-time:nsec-of now)))
                                       (local-time:timestamp-to-unix now)))
                                  (integer
                                   now))
                                data)))))
