(defpackage #:cl-fluent-logger/event-time
  (:use #:cl)
  (:import-from #:messagepack
                #:extension-type
                #:extension-type-id)
  (:export #:event-time))
(in-package #:cl-fluent-logger/event-time)

(defclass event-time (messagepack:extension-type)
  ((data :initarg :data)))

(defmethod initialize-instance :before ((object event-time) &rest initargs)
  (declare (ignore initargs))
  (setf (messagepack::extension-type-id object) 0))
