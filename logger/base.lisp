(defpackage #:cl-fluent-logger/logger/base
  (:use #:cl)
  (:import-from #:local-time
                #:now)
  (:export #:base-logger
           #:open-logger
           #:close-logger
           #:post
           #:post-with-time))
(in-package #:cl-fluent-logger/logger/base)

(defclass base-logger () ())

(defgeneric open-logger (logger)
  (:method (logger)))

(defgeneric close-logger (logger)
  (:method (logger)))

(defgeneric post-with-time (logger tag data time)
  (:method :around ((logger base-logger) tag data time)
    (check-type tag (or string keyword))
    (check-type time (or local-time:timestamp
                         integer))
    (call-next-method logger
                      (let ((*print-case* :downcase))
                        (princ-to-string tag))
                      data
                      time)))

#+sbcl
(declaim (sb-ext:maybe-inline post))
(defun post (logger tag data)
  (post-with-time logger tag data (local-time:now)))
