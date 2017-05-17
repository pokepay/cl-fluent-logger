(defpackage #:cl-fluent-logger/logger/null
  (:use #:cl
        #:cl-fluent-logger/logger/base)
  (:export #:null-logger))
(in-package #:cl-fluent-logger/logger/null)

(defclass null-logger (base-logger) ())

(defmethod post-with-time ((logger null-logger) tag data time)
  (declare (ignore tag data time))
  t)
