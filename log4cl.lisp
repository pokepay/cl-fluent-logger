(defpackage #:cl-fluent-logger/log4cl
  (:nicknames :fluent-logger/log4cl)
  (:use #:cl)
  (:import-from #:cl-fluent-logger
                #:fluent-logger
                #:send)
  (:import-from #:log4cl)
  (:export #:fluent-appender
           #:create-fluent-logger))
(in-package #:cl-fluent-logger/log4cl)

(defclass fluent-appender (log4cl:appender)
  ((logger :accessor fluent-appender-logger
           :initarg :logger
           :initform nil)))

(defmethod initialize-instance :after ((appender fluent-appender) &rest initargs)
  (unless (fluent-appender-logger appender)
    (setf (fluent-appender-logger appender)
          (apply #'make-instance 'fluent-logger :allow-other-keys t initargs))))

(defmethod log4cl:appender-do-append ((appender fluent-appender) logger level log-func)
  (declare (ignore logger))
  (send (fluent-appender-logger appender)
        (string-downcase (log4cl:log-level-to-string level))
        (with-output-to-string (s) (funcall log-func s))))

(defun create-fluent-logger (logger &rest initargs
                             &key tag host port buffer-limit timeout nanosecond-precision
                               level)
  (declare (ignore tag host port buffer-limit timeout nanosecond-precision))
  (log:config logger level)
  (let* ((fluent-logger (apply #'make-instance 'fluent-logger
                               :allow-other-keys t
                               initargs))
         (appender (make-instance 'fluent-appender :logger fluent-logger)))
    (log4cl:add-appender logger appender)
    logger))
