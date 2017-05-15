# cl-fluent-logger

A Common Lisp structured logger for Fluentd.

## Usage

```common-lisp
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

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2017 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 3-Clause License.
