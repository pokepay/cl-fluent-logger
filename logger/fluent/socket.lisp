(defpackage #:cl-fluent-logger/logger/fluent/socket
  (:use #:cl)
  (:import-from #:usocket)
  #+sbcl
  (:import-from #:sb-bsd-sockets)
  (:export #:socket
           #:tcp-socket
           #:unix-socket
           #:make-tcp-socket
           #:make-unix-socket
           #:socket-connected-p
           #:socket-connect
           #:socket-close
           #:socket-stream))
(in-package #:cl-fluent-logger/logger/fluent/socket)

(defstruct socket
  handle
  (timeout 3.0))

(defstruct (tcp-socket (:include socket)
                       (:conc-name socket-))
  (host "127.0.0.1")
  (port 24224))

(defstruct (unix-socket (:include socket)
                        (:conc-name socket-))
  path)

(defun socket-connected-p (socket)
  (not (null (socket-handle socket))))

(defgeneric socket-connect (socket)
  (:method ((socket tcp-socket))
    (setf (socket-handle socket)
          (usocket:socket-connect (socket-host socket)
                                  (socket-port socket)
                                  :element-type '(unsigned-byte 8)
                                  :timeout (socket-timeout socket)))
    socket)
  (:method ((socket unix-socket))
    #-sbcl (error "Not supported Lisp to use UNIX domain socket")
    #+sbcl
    (let ((local-socket (make-instance 'sb-bsd-sockets:local-socket
                                       :type :stream)))
      (sb-bsd-sockets:socket-connect local-socket (socket-path socket))
      (setf (socket-handle socket) local-socket))
    socket))

(defgeneric socket-close (socket)
  (:method ((socket tcp-socket))
    (usocket:socket-close (socket-handle socket))
    (setf (socket-handle socket) nil)
    socket)
  (:method ((socket unix-socket))
    #-sbcl (error "Not supported Lisp to use UNIX domain socket")
    #+sbcl
    (sb-bsd-sockets:socket-close (socket-handle socket))
    (setf (socket-handle socket) nil)
    socket))

(defgeneric socket-stream (socket)
  (:method ((socket tcp-socket))
    (usocket:socket-stream (socket-handle socket)))
  (:method ((socket unix-socket))
    #-sbcl (error "Not supported Lisp to use UNIX domain socket")
    #+sbcl
    (sb-bsd-sockets:socket-make-stream (socket-handle socket)
                                       :output t
                                       :element-type '(unsigned-byte 8)
                                       :timeout (socket-timeout socket)
                                       :buffering :none)))
