(defpackage #:cl-fluent-logger/logger/level
  (:use #:cl
        #:cl-fluent-logger/logger/base)
  (:export #:level-logger))
(in-package #:cl-fluent-logger/logger/level)

;; Proper log levels:
;; :debug, :info, :warn, :error and :fatal

(defconstant +log-level-off+ 0)
(defconstant +log-level-trace+ 1)
(defconstant +log-level-debug+ 2)
(defconstant +log-level-info+ 3)
(defconstant +log-level-warn+ 4)
(defconstant +log-level-error+ 5)
(defconstant +log-level-fatal+ 6)
(defconstant +log-level-unknown+ 99)

(defvar *log-level*
  +log-level-warn+)

(defun canonicalize-log-level (log-level)
  (etypecase log-level
    (integer log-level)
    (null +log-level-off+)
    ((eql t) +log-level-unknown+)
    (keyword
     (ecase log-level
       (:off   +log-level-off+)
       (:trace +log-level-trace+)
       (:debug +log-level-debug+)
       (:info  +log-level-info+)
       (:warn  +log-level-warn+)
       (:error +log-level-error+)
       (:fatal +log-level-fatal+)))))

(defclass level-logger (base-logger)
  ((level :initarg :level
          :initform +log-level-warn+
          :accessor level-logger-level)
   (logger :initarg :logger
           :initform (error ":logger is required")
           :accessor level-logger-logger)))

(defmethod initialize-instance ((object level-logger) &key logger level)
  (call-next-method object
                    :logger logger
                    :level (if level
                               (canonicalize-log-level level)
                               +log-level-warn+)))

(defmethod post-with-time ((logger level-logger) tag data time)
  (when (<= (level-logger-level logger) *log-level*)
    (post-with-time (level-logger-logger logger)
                    tag data time)))

(defmacro with-log-level (level &body body)
  `(let ((*log-level* ,(if (constantp level)
                           (canonicalize-log-level level)
                           `(canonicalize-log-level ,level))))
     ,@body))
