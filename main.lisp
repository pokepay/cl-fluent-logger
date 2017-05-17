(defpackage #:cl-fluent-logger
  (:nicknames #:cl-fluent-logger/main
              #:fluent)
  (:use #:cl)
  (:import-from #:cl-fluent-logger/logger
                #:fluent-logger
                #:open-socket
                #:close-socket
                #:send)
  (:export #:fluent-logger
           #:open-socket
           #:close-socket
           #:send))
(in-package #:cl-fluent-logger)
