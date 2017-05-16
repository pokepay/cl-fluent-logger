# cl-fluent-logger

A Common Lisp structured logger for Fluentd.

## Usage

```common-lisp
(ql:quickload :cl-fluent-logger)

(defvar *logger*
  (make-instance 'fluent-logger:fluent-logger))

(defvar *logger*
  (make-instance 'fluent-logger:fluent-logger
                 :tag "myapp"
                 :host "127.0.0.1"
                 :port 24224))
                 
(fluent-logger:send *logger*
                    "follow"
                    '(("from" . "userA") ("to" . "userB")))
```

### Log4CL Integration

```common-lisp
(ql:quickload :cl-fluent-logger/log4cl)

(defvar *logger* (log:category '(cl-user)))
(fluent-logger/log4cl:create-fluent-logger *logger* :tag "debug" :level :info)

(log:info "hi")
;-> <INFO> [17:03:33]  (cl-user) - hi
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2017 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 3-Clause License.
