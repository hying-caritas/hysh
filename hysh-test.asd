;;; -*- Lisp -*-

(defsystem :hysh-test
  :depends-on (:hysh :fiveam)
  :serial t
  :components ((:file "test")))
