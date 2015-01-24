;;; -*- Lisp -*-

(defsystem #:hysh
  :description "Huang Ying's shell in common lisp"
  :version "0.1"
  :depends-on (:asdf :uiop :alexandria :iterate
	       :iolib.syscalls :iolib.streams :iolib.os
	       :bordeaux-threads :fare-utils :hy-stream
	       :trivial-gray-streams)
  :components ((:file "package")
	       (:file "common" :depends-on ("package"))
	       (:file "syscall-iolib" :depends-on ("package"))
	       (:file "environment" :depends-on ("common" "syscall-iolib"))
	       (:file "stream" :depends-on ("common" "syscall-iolib"))
	       (:file "redirection" :depends-on ("stream"))
	       (:file "hysh" :depends-on ("redirection" "environment"))
	       (:file "glue" :depends-on ("hysh"))
	       (:file "utilities" :depends-on ("hysh")))
  :in-order-to ((test-op (load-op "hysh-test")))
  :perform (test-op :after (o c) (symbol-call :hysh-test :main)))
