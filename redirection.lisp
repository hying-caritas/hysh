(in-package :hysh)

;; list of redirection objects, the redirection object is considered
;; read-only
(defvar *redirections* nil)

(defstruct (redirection
	     (:constructor make-redirection (symbol to-fd stream)))
  (symbol nil :type (or null symbol))
  (to-fd -1 :type integer)
  (stream nil :type (or null stream)))

;;; IO redirection

(defun stop-on-stdio-error* (thunk)
  "Transfer control to the end of stop-on-stdio-error* if stream-error
is signaled for *standard-input*, *standard-output* and *error-output*
for calling the thunk.  Return the return values of the thunk.  This
is mainly for SIGPIPE processing in pipeline."
  (declare (type thunk thunk))
  (restart-case
      (handler-bind
	  ((stream-error
	    (lambda (e)
	      (when (member (stream-error-stream e)
			    (list *standard-input* *standard-output* *error-output*))
		(invoke-restart 'stop-on-stdio-error)))))
	(funcall thunk))
    (stop-on-stdio-error ())))

(defmacro stop-on-stdio-error (&body body)
  "Transfer control to the end of stop-on-stdio-error* if stream-error
is signaled for *standard-input*, *standard-output* and *error-output*
for evaluating the body in an implicit PROGN.  Return the values of
the last form of the body.  This is mainly for SIGPIPE processing in
pipeline."
  `(stop-on-stdio-error* (lambda () ,@body)))

(defun call-with-redirect-to-fd-stream (stream-sym redirect-to-fd stream func)
  (progv
      `(*redirections* ,stream-sym)
      (list (cons (make-redirection stream-sym redirect-to-fd stream)
		  *redirections*)
	    stream)
    (funcall func)))

(defmacro with-redirect-to-fd-stream (stream-var redirect-to-fd stream &body body)
  "Redirect the stream-var (for a lisp stream variable, such as
*standard-input*) and redirect-to-fd (for a UNIX file descriptor, such
as 0 (stdin) to the fd-stream, then evalute the body in an implicit
PROGN, return the values of the last form of the body.  Finally cancel
the redirection."
  `(call-with-redirect-to-fd-stream ',stream-var ,redirect-to-fd ,stream
				    (lambda () ,@body)))

(defun call-with-redirect-to-fd-streams (stream-syms redirect-to-fds streams func)
  (let ((redirections (iter (for stream-sym :in stream-syms)
			    (for redirect-to-fd :in redirect-to-fds)
			    (for stream :in streams)
			    (collect (make-redirection stream-sym redirect-to-fd
						       stream)))))
    (progv
	(list* '*redirections* stream-syms)
	(list* (append redirections *redirections*)
	       streams)
     (funcall func))))

(defun call-with-redirect-to-file (stream-sym redirect-to-fd pathname flags func)
  (call-with-file-fd-stream
   pathname flags
   (lambda (stream)
     (call-with-redirect-to-fd-stream stream-sym redirect-to-fd
				      stream func))))

(defmacro with-redirect-to-file (stream-var redirect-to-fd pathname flags &body body)
  "Open the pathname with the flags and redirect the stream-var (for a
lisp stream variable, such as *standard-input*) and the
redirect-to-fd (for a UNIX file descriptor, such as 0 (stdin)) to the
opened file, then evaluate the body in an implicit PROGN, return the
values of the last form of the body.  Finally canceled the redirection
and close the file."
  `(call-with-redirect-to-file ',stream-var ,redirect-to-fd ,pathname ,flags
			       (lambda () ,@body)))

(defun call-with-redirect-to-fd (stream-sym redirect-to-fd fd func)
  (call-with-fd-stream
   fd nil
   (lambda (stream)
     (call-with-redirect-to-fd-stream stream-sym redirect-to-fd
				      stream func))))

(defmacro with-redirect-to-fd (stream-var redirect-to-fd fd &body body)
  "Redirect the stream-var (for a lisp stream variable, such as
*standard-input*) and the redirect-to-fd (for a UNIX file descriptor,
such as 0 (stdin)) to the fd, then evaluate the body in an implicit
PROGN, return the values of the last form of the body.  Finally
canceled the redirection and close the fd."
  `(call-with-redirect-to-fd ',stream-var ,redirect-to-fd ,fd
			     (lambda () ,@body)))

(defun call-with-redirect-to-fds (stream-syms redirect-to-fds fds func)
  (call-with-fd-streams
   (mapcar #'list fds)
   (lambda (&rest streams)
     (call-with-redirect-to-fd-streams stream-syms redirect-to-fds
				       streams func))))

(defmacro with-redirect-to-fds (stream-var-redirect-to-fd-fd-list &body body)
  "Redirect a list of stream-vars (for a lisp stream variable,
such as *standard-input*) and redirect-to-fds (for a UNIX file
descriptor, such as 0 (stdin)) to a list of fds, they are spcified via
the list of (stream-var redirect-to-fd fd), then evaluate the body in
an implicit PROGN, return the values of the last form of the body.
Finally canceled the redirections and close the fds."
  (let (stream-syms redirect-to-fds fds)
    (iter (for (stream-sym redirect-to-fd fd)
	       :in stream-var-redirect-to-fd-fd-list)
	  (push stream-sym stream-syms)
	  (push redirect-to-fd redirect-to-fds)
	  (push fd fds))
    `(call-with-redirect-to-fds ',stream-syms ,(cons 'list redirect-to-fds)
				,(cons 'list fds)
				(lambda () ,@body))))

(defun call-with-redirect-stdio-to-fds (in-fd out-fd err-fd thunk)
  (let (stream-syms redirect-to-fds fds)
    (when in-fd
      (push '*standard-input* stream-syms)
      (push +STDIN-FD+ redirect-to-fds)
      (push in-fd fds))
    (when out-fd
      (push '*standard-output* stream-syms)
      (push +STDOUT-FD+ redirect-to-fds)
      (push out-fd fds))
    (when err-fd
      (push '*error-output* stream-syms)
      (push +STDERR-FD+ redirect-to-fds)
      (push err-fd fds))
    (if stream-syms
	(call-with-redirect-to-fds stream-syms redirect-to-fds
				   fds thunk)
	(funcall thunk))))

(defmacro with-redirect-stdio-to-fds ((in-fd out-fd &optional (err-fd nil))
				      &body body)
  "Redirect the stdin (*standard-input* and 0),
stdout (*standard-output* and 1) and stderr (*error-output* and 2) to
the in-fd, the out-fd and the err-fd respectively, then evaluate the
body in an implicit PROGN, return the values of the last form of the
body, finally cancel the redirections and close the fds."
  `(call-with-redirect-stdio-to-fds ,in-fd ,out-fd ,err-fd
				    (lambda () ,@body)))

(defmacro with-redirect-stdin-to-file (pathname &body body)
  "Redirect stdin to the pathname for evaluating the body in an
implicit PROGN, finally restore the original stdin.  Return the values
of the last form of the body."
  `(with-redirect-to-file
       *stanard-input* +STDIN-FD+ ,pathname
       '(:direction :input :if-does-not-exist :error)
     ,@body))

(defmacro with-redirect-stdout-to-file (pathname &body body)
  "Redirect stdout to the pathname for evaluating the body in an
implicit PROGN, finally restore the original stdout.  Return the
values of the last form of the body."
  `(with-redirect-to-file
       *standard-output* +STDOUT-FD+ ,pathname
       '(:direction :output :if-exists :supersede :if-does-not-exist :create)
     ,@body))

(defmacro with-redirect-stderr-to-file (pathname &body body)
  "Redirect stderr to the pathname for evaluating the body in an
implicit PROGN, finally restore the original stderr.  Return the
values of the last form of the body."
  `(with-redirect-to-file
       *error-output* +STDERR-FD+ ,pathname
       '(:direction :output :if-exists :supersede :if-does-not-exist :create)
       ,@body))

(defun call-with-redirect-stderr-to-stdout (thunk)
  (call-with-redirect-to-fd-stream '*error-output* +STDERR-FD+
				   *standard-output* thunk))

(defmacro with-redirect-stderr-to-stdout (&body body)
  "Redirect stderr to the stdout for evaluating the body in an
implicit PROGN, finally restore the original stderr.  Return the
values of the last form of the body."
  `(with-redirect-to-fd-stream
       *error-output* +STDERR-FD+ *standard-output*
     ,@body))

(defun tmpfd (&optional (template "/tmp/hysh-"))
  (multiple-value-bind (fd file-name) (mkstemp template)
    (delete-file (parse-unix-namestring file-name))
    fd))

(defun %out/stream (thunk reader)
  (declare (type thunk thunk)
	   (type (function (stream)) reader))
  (let ((tmp-fd (tmpfd)))
    (with-redirect-to-fd
	*standard-output* +STDOUT-FD+ tmp-fd
      (let ((ret-vals (multiple-value-list (funcall thunk))))
	(finish-output)
	;; FIXME: interaction between seek and stream
	(lseek tmp-fd 0 seek-set)
	(values-list (nconc (multiple-value-list (funcall reader *standard-output*))
			    ret-vals))))))

(defun out/s* (thunk)
  "Collect the output of calling the thunk via redirecting its stdout.
Finally restore the original stdout.  Return the collected string and
return values of the thunk."
  (declare (type thunk thunk))
  (%out/stream thunk
	       (lambda (stream)
		 (fd-stream-ref stream)
		 (fd-stream-ref stream)
		 (slurp-stream-string stream))))

(defmacro out/s (&body body)
  "Collect the output of evaluating the body in an implicit PROGN via
redirecting its stdout.  Finally restore the original stdout.  Return
the collected string and the values of the last form of the body."
  `(out/s* (lambda () ,@body)))

(defun out/ss* (thunk)
  "Almost same as out/s* except the newline at end of output string is
stripped."
  (declare (type thunk thunk))
  (%out/stream thunk
	       (lambda (stream)
		 (fd-stream-ref stream)
		 (fd-stream-ref stream)
		 (values (stripln (slurp-stream-string stream))))))

(defmacro out/ss (&body body)
  "Almost same as out/s except the newline at end of output string is
stripped."
  `(out/ss* (lambda () ,@body)))

(defun out/lines* (thunk)
  "Collect the output of calling the thunk into lines via redirecting
its stdout.  Finally restore the original stdout.  Return the collected
lines as list and the return values of the thunk."
  (declare (type thunk thunk))
  (%out/stream thunk
	       (lambda (stream)
		 (fd-stream-ref stream)
		 (slurp-stream-lines stream))))

(defmacro out/lines (&body body)
  "Collect the output of evaluating the body in an implicit PROGN into
lines via redirecting its stdout.  Finally restore the orignal stdout.
Return the collected lines as list and the values of the last form of
the body."
  `(out/lines* (lambda () ,@body)))

(defun %out/err/stream (thunk out-reader err-reader)
  (declare (type thunk thunk)
	   (type (function (stream)) out-reader)
	   (type (function (stream)) err-reader))
  (let ((out-fd (tmpfd))
	(err-fd (tmpfd)))
    (with-redirect-to-fds
	((*standard-output* +STDOUT-FD+ out-fd)
	 (*error-output* +STDERR-FD+ err-fd))
      (let ((ret-vals (multiple-value-list (funcall thunk))))
	(lseek out-fd 0 seek-set)
	(lseek err-fd 0 seek-set)
	(values-list (nconc (multiple-value-list (funcall out-reader *standard-output*))
			    (multiple-value-list (funcall err-reader *error-output*))
			    ret-vals))))))

(defun out/err/s* (thunk)
  "Collect the normal and error output as strings of calling the thunk
via redirecting its stdout and stderr.  Finally restore the original
stdout/stderr.  Return the collected strings and the values of the
last form of the body."
  (declare (type thunk thunk))
  (flet ((ref-slurp-stream-string (stream)
	   (fd-stream-ref stream)
	   (fd-stream-ref stream)
	   (slurp-stream-string stream)))
    (%out/err/stream thunk
		     #'ref-slurp-stream-string
		     #'ref-slurp-stream-string)))

(defmacro out/err/s (&body body)
  "Collect the normal and error output as strings of evaluating the
body in an implicit PROGN via redirecting its stdout and stderr.
Finally restore the original stdout/stderr.  Return the collected
strings and the values of the last form of the body."
  `(out/err/s* (lambda () ,@body)))

(defun out/err/ss* (thunk)
  "Collect the normal and error output as strings of calling the thunk
via redirecting its stdout and stderr.  Finally restore the original
stdout/stderr.  Return the collected strings and the values of the
last form of the body."
  (declare (type thunk thunk))
  (flet ((ref-strip-slurp-stream-string (stream)
	   (fd-stream-ref stream)
	   (fd-stream-ref stream)
	   (values (stripln (slurp-stream-string stream)))))
    (%out/err/stream thunk
		     #'ref-strip-slurp-stream-string
		     #'ref-strip-slurp-stream-string)))

(defmacro out/err/ss (&body body)
  "Collect the normal and error output as strings of evaluating the
body in an implicit PROGN via redirecting its stdout and stderr.
Finally restore the original stdout/stderr.  Return the collected
strings and the values of the last form of the body."
  `(out/err/ss* (lambda () ,@body)))

(defun %in/stream (thunk writer)
  "Create a temporary file and corresponding stream, then call the
writer on the stream, redirect stdin to the temporary file, call the
thunk, finally restore the original stdin and delete the file.  Return
the return values of the thunk."
  (declare (type (function (stream)) writer)
	   (type thunk thunk))
  (let ((tmp-fd (tmpfd)))
    (with-redirect-to-fd
	*standard-input* +STDIN-FD+ tmp-fd
      (funcall writer *standard-input*)
      (finish-output *standard-input*)
      (lseek tmp-fd 0 seek-set)
      (funcall thunk))))

(defun in/s* (str thunk)
  "Use the string as the stdin when calling the thunk.  Finally
restore the original stdin.  Return the return values of the thunk."
  (declare (type thunk thunk))
  (%in/stream thunk (lambda (stream) (princ str stream))))

(defmacro in/s (str &body body)
  "Use the string as the stdin when evaluating the body in an implicit
PROGN.  Finally restore the orignal stdin.  Return the values of the
forms."
  `(in/s* ,str (lambda () ,@body)))

(defun in/lines* (lines thunk)
  "Use the lines (joined with end-of-line) as stdin when calling the
thunk.  Finally restore the original stdin.  Return the return values of the thunk."
  (declare (type list lines)
	   (type thunk thunk))
  (%in/stream thunk
	      (lambda (stream)
		(dolist (line lines)
		  (write-line line stream)))))

(defmacro in/lines (lines &body body)
  "Use the lines (joined with end-of-line) as stdin when evaluating
the body in an implicit PROGN.  Finally restore the original stdin.
Return the values of the forms."
  `(in/lines* ,lines (lambda () ,@body)))

(defmacro io/s (str &body body)
  "Use the string as the stdin and collect the stdout and stderr as
strings of evaluating the body in an implicit PROGN via redirecting
its stdin/stdout/stderr.  Finally restore the original
stdin/stdout/stderr.  Return the collected strings and the values of
the last form of the body."
  `(in/s ,str (out/err/s ,@body)))

(defmacro io/ss (str &body body)
  "Almost same as io/s except the newline at end of stdout/stderr
strings are stripped."
  `(in/s ,str (out/err/ss ,@body)))

(defmacro io/ss/run (str &rest cmdline)
  "Redirect stdin/stdout/stderr and run the command line.  See also
io/ss and run."
  `(in/s ,str (out/err/ss (run ,@cmdline))))
