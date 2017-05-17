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

(defgeneric post (logger tag data)
  (:method ((logger base-logger) tag data)
    (post-with-time logger tag data (local-time:now))))

(defgeneric post-with-time (logger tag data time)
  (:method :before ((logger base-logger) tag data time)
    (check-type tag string)
    (check-type data cons)
    (check-type time (or local-time:timestamp
                         integer))))
