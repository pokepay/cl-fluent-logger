(defpackage #:cl-fluent-logger/logger/broadcast
  (:use #:cl
        #:cl-fluent-logger/logger/base)
  (:export #:broadcast-logger
           #:make-broadcast-logger))
(in-package #:cl-fluent-logger/logger/broadcast)

(defclass broadcast-logger (base-logger)
  ((children :initarg :children
             :initform nil
             :accessor broadcast-logger-children)))

(defun make-broadcast-logger (&rest loggers)
  (flet ((loggerp (obj)
           (typep obj 'base-logger)))
    (assert (every #'loggerp loggers)))
  (make-instance 'broadcast-logger :children loggers))

(defmethod open-logger ((logger broadcast-logger))
  (mapc #'open-logger (broadcast-logger-children logger))
  (values))

(defmethod close-logger ((logger broadcast-logger))
  (mapc #'close-logger (broadcast-logger-children logger))
  (values))

(defmethod post-with-time ((logger broadcast-logger) tag data time)
  (dolist (logger (broadcast-logger-children logger))
    (post-with-time logger tag data time)))
