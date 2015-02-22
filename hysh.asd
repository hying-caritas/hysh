;;; -*- Lisp -*-

(defsystem #:hysh
  :description "Huang Ying's shell in common lisp"
  :version "0.1"
  :depends-on (#:asdf #:uiop #:alexandria #:iterate #:split-sequence #:hy-utils
	       #:iolib.syscalls #:iolib.streams #:iolib.os #:bordeaux-threads
	       #:hy-stream)
  :components ((:file "package")
	       (:file "common" :depends-on ("package"))
	       (:file "syscall-iolib" :depends-on ("package"))
	       (:file "environment" :depends-on ("common" "syscall-iolib"))
	       (:file "pathname")
	       (:file "stream" :depends-on ("common" "syscall-iolib"))
	       (:file "redirection" :depends-on ("stream"))
	       (:file "task" :depends-on ("redirection" "environment" "pathname"))
	       (:file "glue" :depends-on ("task"))
	       (:file "misc" :depends-on ("task"))
	       (:file "interactive" :depends-on ("task")))
  :in-order-to ((test-op (load-op "hysh-test")))
  :perform (test-op :after (o c) (symbol-call :hysh-test :main)))
