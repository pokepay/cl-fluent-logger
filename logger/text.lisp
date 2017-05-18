(defpackage #:cl-fluent-logger/logger/text
  (:use #:cl
        #:cl-fluent-logger/logger/base)
  (:import-from #:local-time)
  (:import-from #:bordeaux-threads)
  (:import-from #:jonathan)
  (:export #:text-logger))
(in-package #:cl-fluent-logger/logger/text)

(defclass text-logger (base-logger)
  ((stream :initarg :stream
           :initform *standard-output*)
   (lock :initform (bt:make-lock))))

(defmethod post-with-time ((logger text-logger) tag data time)
  (let ((time
          (etypecase time
            (integer (local-time:universal-to-timestamp time))
            (local-time:timestamp time)))
        (stream (slot-value logger 'stream))
        (*print-case* :downcase))
    (bt:with-lock-held ((slot-value logger 'lock))
      (format stream "~A ~A:" time tag)
      (typecase data
        (cons
         (loop for (k . v) in data
               do (format stream " ~A=~A"
                          k
                          (uiop:symbol-call :jojo :to-json v))))
        (hash-table
         (maphash (lambda (k v)
                    (format stream " ~A=~A"
                            k
                            (uiop:symbol-call :jojo :to-json v)))
                  data))
        (otherwise
         (prin1 data stream)))
      (fresh-line)
      t)))
