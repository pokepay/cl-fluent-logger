# cl-fluent-logger

A Common Lisp structured logger for [Fluentd](https://www.fluentd.org/).

## Usage

```common-lisp
(ql:quickload :cl-fluent-logger)

(defvar *logger*
  (make-instance 'fluent:fluent-logger
                 :tag "myapp"
                 :host "127.0.0.1"
                 :port 24224))

(fluent:post *logger*
             "follow"
             '(("from" . "userA") ("to" . "userB")))

;; Use the logger globally.
(setf fluent:*logger* *logger*)

(fluent:log "follow" '(("from" . "userA") ("to" . "userB")))
```

### Text logger

```common-lisp
(defvar *logger*
  (make-instance 'fluent:text-logger))
  
(fluent:post *logger*
             "follow"
             '(("from" . "userA") ("to" . "userB")))
;-> 2017-05-17T12:45:51.774499+09:00 follow: from="userA" to="userB"
;=> NIL
```

### Broadcasting

```common-lisp
(defvar *logger*
  (fluent:make-broadcast-logger
     (make-instance 'fluent:fluent-logger)
     (make-instance 'fluent:text-logger)))

;; Posts a log to fluentd and also outputs to the standard output.
(fluent:post *logger*
             "follow"
             '(("from" . "userA") ("to" . "userB")))
;-> 2017-05-17T12:45:51.774499+09:00 follow: from="userA" to="userB"
;=> NIL
```

### Level logger

```common-lisp
;; Log levels are one of :trace, :debug, :info, :warn, :error and :fatal.
(defvar *logger*
  (make-instance 'fluent:level-logger
                 :level :debug
                 :logger (make-instance 'fluent:text-logger)))

(fluent:with-logger *logger*
  (fluent:info "follow"
               '(("from" . "userA") ("to" . "userB"))))
;-> 2017-05-17T12:45:51.774499+09:00 follow: from="userA" to="userB"
;=> NIL

(fluent:with-logger *logger*
  (fluent:trace "follow"
                '(("from" . "userA") ("to" . "userB"))))
;=> NIL
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
