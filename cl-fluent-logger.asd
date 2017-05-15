(defsystem "cl-fluent-logger"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :description "A structured logger for Fluentd"
  :depends-on ("cl-fluent-logger/main"))

(asdf:register-system-packages "cl-messagepack" '(#:messagepack))
