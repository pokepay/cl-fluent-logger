(defpackage #:cl-fluent-logger
  (:nicknames #:cl-fluent-logger/main
              #:fluent)
  (:use #:cl)
  (:import-from #:cl-fluent-logger/logger/base
                #:open-logger
                #:close-logger
                #:post
                #:post-with-time)
  (:import-from #:cl-fluent-logger/logger/fluent
                #:fluent-logger)
  (:import-from #:cl-fluent-logger/logger/text
                #:text-logger)
  (:import-from #:cl-fluent-logger/logger/null
                #:null-logger)
  (:export #:fluent-logger
           #:text-logger
           #:null-logger
           #:open-logger
           #:close-logger
           #:post
           #:post-with-time))
(in-package #:cl-fluent-logger)
