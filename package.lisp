(in-package #:cl-user)

(defpackage #:hysh
  (:use #:cl #:iterate #:uiop #:trivial-gray-streams
	#:bordeaux-threads #:hy-stream)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:fare-utils #:join-strings)
  (:export
   ;; task
   #:task-alive-p #:wait-task #:task-return-success-p #:close-task
   #:task-return-value #:task-stdin #:task-stdout #:task-stderr
   ;; command
   #:command-error #:command-error-command #:command-error-process
   #:ignore-command-error #:warn-on-command-error #:stop-on-command-error
   #:run* #:run
   ;; io redirect
   #:stop-on-stdio-error #:with-redirect-to-fd-stream
   #:with-redirect-to-file #:with-redirect-to-fd
   #:with-redirect-to-fds #:with-redirect-stdin-to-file
   #:with-redirect-stdout-to-file #:with-redirect-stderr-to-file
   #:with-redirect-stderr-to-stdout #:with-redirect-stdio-to-fds
   #:out/s #:out/ss #:out/lines #:out/err/s #:out/err/ss #:in/s
   #:in/lines #:io/s #:io/ss #:io/ss/run
   ;; glue processes and common lisp functions
   #:prog-or #:prog-and #:background
   #:pipe #:create-task #:with-task
   ;; pipeline filter helpers
   #:filter-char #:filter-line
   ;; glob
   #:zglob))