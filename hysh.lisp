(in-package :hysh)

;;; Redirection

;; list of redirection objects, the redirection object is considered
;; read-only
(defvar *redirections* nil)

(defstruct (redirection
	     (:constructor make-redirection (symbol to-fd stream)))
  (symbol nil :type (or null symbol))
  (to-fd -1 :type integer)
  (stream nil :type (or null stream)))

;;; Task

(defgeneric task-alive-p (task)
  (:documentation "Return whether task is alive, will not wait for
  task to exit."))
(defgeneric wait-task (task)
  (:documentation "Wait for task to exit, return the task."))
(defgeneric task-return-value (task)
  (:documentation "Get the return value of the task, if the the task
  has not exited, wait for it.  Return the return value (thread) or
  the exit status and state (process)."))
(defgeneric task-return-success-p (task)
  (:documentation "Return whether the task exited with success status,
  may wait for task to exit."))

(defclass task ()
  ((stdin :initform nil :type stream :reader task-stdin)
   (stdout :initform nil :type stream :reader task-stdout)
   (stderr :initform nil :type stream :reader task-stderr)))

;;; lock order
;;;   *active-tasks-lock* -> process.wait-lock -> process.lock

(defclass process (task)
  ((pid    :initarg :pid :type integer :reader process-pid)
   (lock :initform (make-lock "process lock"))
   (state :initform :running :type (member (:running :exited :terminated :unknown)))
   (status)
   (wait-lock :initform (make-lock "process wait lock")
	      :reader %process-wait-lock))
  (:documentation "Represent a UNIX process."))

(defclass thread-task (task)
  ((thread :initarg :thread))
  (:documentation "Represent a Common Lisp thread."))

(defvar *active-tasks* nil)
(defvar *active-tasks-lock* (make-lock))
(defvar *in-reap-active-tasks* nil)

(defun close-task (task)
  "Release the resources for the task."
  (with-slots ((stdin stdin) (stdout stdout) (stderr stderr))
      task
    (when stdin (close stdin))
    (when stdout (close stdout))
    (when stderr (close stderr)))
  (wait-task task)
  (values))

(defmethod task-return-value ((thread thread-task))
  (join-thread (slot-value thread 'thread)))

(defmethod task-alive-p ((thread thread-task))
  (thread-alive-p (slot-value thread 'thread)))

(defmethod wait-task ((thread thread-task))
  (join-thread (slot-value thread 'thread))
  thread)

(defmethod task-return-success-p ((thread thread-task))
  (task-return-value thread))

(defmacro with-process-locked (process &body body)
  `(with-lock-held ((slot-value ,process 'lock))
     ,@body))

(defun wait-process (process &optional (wait-p t))
  "Wait for the child process to exit, if wait-p is true, will block
until the child process exited, otherwise will return immediately.
Return the process object."
  (declare (type process process))
  (with-slots ((pid pid) (state state)
	       (status status))
      process
    (when (eq state :running)
      (unless (acquire-lock (%process-wait-lock process) wait-p)
	(return-from wait-process process))
      ;;; another thread may change the state
      (when (eq state :running)
	(unwind-protect
	     (multiple-value-bind (ret-pid ret-status)
		 (waitpid (process-pid process)
			  (if wait-p 0 wnohang))
	       (when (not (eql ret-pid 0))
		 (with-process-locked process
		   (cond
		     ((wifexited ret-status)
		      (setf state :exited)
		      (setf status (wexitstatus ret-status)))
		     ((wifsignaled ret-status)
		      (setf state :terminated)
		      (setf status (wtermsig ret-status)))
		     (t
		      (setf state :unknown)
		      (setf status ret-status))))))
	  (release-lock (%process-wait-lock process)))))
    (when (and (not (eq state :running))
	       (not *in-reap-active-tasks*))
      (delete-active-task process)))
  process)

(defmethod task-alive-p ((process process))
  (with-slots ((state state))
      process
    (when (eq state :running)
      (wait-process process nil))
    (eq state :running)))

(defmethod wait-task ((process process))
  (wait-process process))

(defmethod task-return-value ((process process))
  (wait-process process)
  (with-slots ((state state) (status status))
      process
    (with-process-locked process
      (values status state))))

(defmethod task-return-success-p ((process process))
  (wait-process process)
  (with-slots ((state state) (status status))
      process
    (with-process-locked process
      (and (eq state :exited) (eql status 0)))))

(defun add-active-task (task)
  (with-lock-held (*active-tasks-lock*)
    (setf *active-tasks*
	  (cons task *active-tasks*)))
  task)

(defun delete-active-task (task)
  (with-lock-held (*active-tasks-lock*)
    (setf *active-tasks*
	  (delete task *active-tasks*))))

(defun reap-active-tasks ()
  (with-lock-held (*active-tasks-lock*)
    (let ((*in-reap-active-tasks* t))
      (setf *active-tasks*
	    (delete-if (lambda (task)
			 (unless (task-alive-p task)
			   (wait-task task)))
		       *active-tasks*)))))

(deftype thunk (&optional value-type) `(function () ,value-type))

;;; Run command

(defvar *on-command-error* nil)

(define-condition command-error ()
  ((command :reader command-error-command :initarg :command)
   (process :reader command-error-process :initarg :process))
  (:documentation "Signaled when command exit status is failure"))

(defmethod print-object ((cmd-err command-error) stream)
  (with-slots ((state state) (status status))
      (command-error-process cmd-err)
    (format stream "#<command error: ~a, ~a: ~a>"
	    (command-error-command cmd-err)
	    (ecase state
	      (:exited "exited with")
	      (:terminated "terminated by")
	      (:unknown "unknown status"))
	    (if (eq state :terminated)
		(signal-keyword status)
		status))))

(defun ignore-command-error* (thunk)
  "Silently ignore command-error condition for calling the thunk.
Return the return values of the thunk."
  (declare (type thunk thunk))
  (handler-bind
      ((command-error (lambda (cmd-err)
			(declare (ignore cmd-err))
			(continue))))
    (let ((*on-command-error* :ignore))
      (funcall thunk))))

(defmacro ignore-command-error (&body body)
  "Silently ignore command-error condition for evaluating the body in
an implicit PROGN.  Return the values of the last form of the body."
  `(handler-bind
       ((command-error (lambda (cmd-err)
			 (declare (ignore cmd-err))
			 (continue))))
     ,@body))

(defun warn-on-command-error* (thunk)
  "Print warning message for command-error for call the thunk.  Return
the values of the thunk."
  (handler-bind
      ((command-error (lambda (cmd-err)
			(format *error-output* "~a: ~a~%" +PROGRAM+ cmd-err)
			(continue))))
    (let ((*on-command-error* :warn))
      (funcall thunk))))

(defmacro warn-on-command-error (&body body)
  "Print warning message for command-error for evaluating the body in
an implicit PROGN.  Return the values of the last form of the body."
  `(handler-bind
       ((command-error (lambda (cmd-err)
			 (format *error-output* "~a: ~a~%" +PROGRAM+ cmd-err)
			 (continue))))
     (let ((*on-command-error* :warn))
       (progn ,@body))))

(defun stop-on-command-error* (thunk)
  "Print warning message and transfer control to the end of
stop-on-command-error directly and return NIL if command-error
signaled for calling the thunk.  Otherwise call the thunk and return
the return values of the thunk."
  (handler-case
      (let ((*on-command-error* :stop))
	(funcall thunk))
    (command-error (cmd-err)
      (format *error-output* "~a: stop on ~a~%" +PROGRAM+ cmd-err))))

(defmacro stop-on-command-error (&body body)
  "Print warning message and transfer control to the end of
stop-on-command-error directly and return NIL if command-error
signaled for evaluating the body in an implicit PROGN.  Otherwise
evaluate the body in an implicit PROGN and return the values of the
last form of the body."
  `(handler-case
       (let ((*on-command-error* :stop))
	 (progn ,@body))
     (command-error (cmd-err)
       (format *error-output* "~a: stop on ~a~%" +PROGRAM+ cmd-err))))

(defun alloc-argv (args)
  (let* ((argc (length args))
         (argv (cffi:foreign-alloc :pointer :count (1+ argc))))
    (iter (for i :below argc)
	  (for arg :in args)
	  (setf (cffi:mem-aref argv :pointer i)
		(cffi:foreign-string-alloc arg)))
    (setf (cffi:mem-aref argv :pointer argc) (cffi:null-pointer))
    argv))

(defun free-argv (argv)
  (iter (for i :upfrom 0)
	(for ptr := (cffi:mem-aref argv :pointer i))
	(until (cffi:null-pointer-p ptr))
	(cffi:foreign-free ptr)))

(defmacro with-argv ((arg0 argv args) &body body)
  `(let ((,argv (alloc-argv ,args)))
     (unwind-protect
	  (let ((,arg0 (cffi:mem-ref ,argv :pointer)))
	    ,@body)
       (free-argv ,argv))))

(defun call-with-lfp-file-actions (thunk)
  (cffi:with-foreign-object (file-actions 'iolib.os::lfp-spawn-file-actions-t)
    (unwind-protect
	 (progn
	   (iolib.os::lfp-spawn-file-actions-init file-actions)
	   (funcall thunk file-actions))
      (iolib.os::lfp-spawn-file-actions-destroy file-actions))))

(defmacro with-lfp-file-actions (file-actions &body body)
  `(call-with-lfp-file-actions
    (lambda (,file-actions) ,@body)))

(defvar *redirection-saved-fds*)

(defun redirect-stream (stream redirect-to-fd file-actions)
  (let ((fd (fd-of stream)))
    (when (and fd redirect-to-fd)
      (when (= fd redirect-to-fd)
	(setf fd (dup fd))
	(push fd *redirection-saved-fds*))
      (iolib.os::lfp-spawn-file-actions-adddup2 file-actions fd redirect-to-fd))))

(defun redirect (redirection file-actions)
  (redirect-stream (redirection-stream redirection)
		   (redirection-to-fd redirection)
		   file-actions))

(defun call-with-all-redirections (file-actions thunk)
  (let ((*redirection-saved-fds* nil))
    (iter (for redir :in
	       (delete-duplicates (reverse *redirections*)
				  :key #'redirection-to-fd))
	  (redirect redir file-actions))
    (unwind-protect
	 (funcall thunk)
      (mapc #'sys-close *redirection-saved-fds*))))

(defmacro with-all-redirections (file-actions &body body)
  `(call-with-all-redirections ,file-actions (lambda () ,@body)))

(defun any->string (x)
  (typecase x
    (null "")
    (character (format nil "-~A" x))
    (keyword (format nil "--~(~A~)" x))
    (symbol (string-downcase x))
    (string x)
    (pathname (unix-namestring (translate-logical-pathname x)))
    (t (princ-to-string x))))

(defun any->string-list (x)
  (typecase x
    (string (list x))
    (sequence (map 'list #'any->string x))
    (t (list (any->string x)))))

(defun create-simple-process* (cmdline-in)
  (let ((cmdline (mapcan #'any->string-list cmdline-in)))
    (with-argv (arg0 argv cmdline)
      (with-lfp-file-actions file-actions
	(with-all-redirections file-actions
	  (cffi:with-foreign-object (pid 'isys:pid-t)
	    (with-fd-lock-held
	      (iolib.os::lfp-spawnp pid arg0 argv *environ* file-actions (cffi:null-pointer)))
	    (make-instance 'process :pid (cffi:mem-ref pid 'isys:pid-t))))))))

(defun run* (cmdline)
  "Run the command line in a newly created process and wait for it to
exit, if exit status is failure, will signal command-error.  Return
the exit success status of the process."
  (let* ((process (create-simple-process* cmdline))
	 (ret (task-return-success-p (wait-task process))))
    (if (not ret)
	(restart-case
	    (signal (make-condition 'command-error :command cmdline
				    :process process))
	  (continue ())))
    ret))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun quote-non-list (form)
    (if (symbolp form)
	`(quote ,form)
	form)))

(defmacro run (cmd &rest args)
  "Run the command line in a new process and wait for it exit, if exit
status is failure, will signal command-error.  Return whether the
process sucess exit status.  Symbol arguments in command line will be
quoted automatically, other arguments keep as it is.  If you want the
value of a symbol, try something like (or symbol)."
  (let ((cmdline (mapcar #'quote-non-list (cons cmd args))))
    `(run* `(,,@cmdline))))

;;; Run in thread

(defun create-simple-thread* (thunk)
  (let* ((redirections
	  (remove-duplicates *redirections*
			     :test #'equal
			     :key (lambda (redir)
				    (cons (redirection-symbol redir)
					  (redirection-to-fd redir)))
			     :from-end t))
	 (symbol-redirections
	  (remove-duplicates redirections
			     :key #'redirection-symbol
			     :from-end t))
	 (on-command-error *on-command-error*))
    (mapc (lambda (redir)
	    (fd-stream-ref (redirection-stream redir)))
	  redirections)
    (let ((sys-thread
	   (make-thread
	    (lambda ()
	      (progv
		  (mapcar #'redirection-symbol symbol-redirections)
		  (mapcar #'redirection-stream symbol-redirections)
		(let ((*redirections* redirections))
		  (unwind-protect
		       (ecase on-command-error
			 ((nil)   (funcall thunk))
			 (:ignore (ignore-command-error* thunk))
			 (:warn   (warn-on-command-error* thunk))
			 (:stop   (stop-on-command-error* thunk)))
		    (mapc (lambda (redir)
			    (close (redirection-stream redir)))
			  redirections))))))))
      (make-instance 'thread-task :thread sys-thread))))

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

;;; Glue processes and common lisp functions

(defun prog-or* (&rest thunks)
  "Call the thunks one by one, if the exit status of any thunk is
success, return the return values of the thunk immediately without
calling the remaining thunks.  Otherwise return the return values of
the last thunk.  Return NIL if thunks are NIL."
  (declare (type list thunks))
  (iter (for thunk :in thunks)
	(for remaining :on (cdr thunks))
	(for ret-vals := (multiple-value-list (ignore-command-error (funcall thunk))))
	(when (car (last ret-vals))
	  (return-from prog-or* (values-list ret-vals))))
  (let ((last-thunk (car (last thunks))))
    (when last-thunk
      (funcall last-thunk))))

(defmacro prog-or (&rest forms)
  "Evaluate the forms one by one, if the exit status of any form is
success, return the values of the form immediately without evaluating
the remaining forms.  Otherwise return the values of the last form.
Return NIL if forms are NIL."
  (let ((thunks (mapcar (lambda (form) `(lambda () ,form)) forms)))
    `(prog-or* ,@thunks)))

(defun prog-and* (&rest thunks)
  "Call the thunks one by one, if the exit status of any thunk is
failure, return the return values of the thunk immediately without
calling the remaining thunks.  Otherwise return the values of the last
thunk.  Return T if thunks are NIL."
  (declare (type list thunks))
  (iter (for thunk :in thunks)
	(for remaining :on (cdr thunks))
	(for ret-vals := (multiple-value-list (ignore-command-error (funcall thunk))))
	(unless (car (last ret-vals))
	  (return-from prog-and* (values-list ret-vals))))
  (let ((last-thunk (car (last thunks))))
    (if last-thunk
	(funcall last-thunk)
	t)))

(defmacro prog-and (&rest forms)
  "Evaluate the forms one by one, if the exit status of any form is
failure, return the values of the form immediately without evaluating
the remaining forms.  Otherwise return the values of the last form.
Return T if forms are NIL."
  (let ((thunks (mapcar (lambda (form) `(lambda () ,form)) forms)))
    `(prog-and* ,@thunks)))

(defun background* (thunk-or-cmdline)
  "Call the thunk in a newly created task, return the task object."
  (declare (type (or thunk list) thunk-or-cmdline))
  (reap-active-tasks)
  (add-active-task (if (consp thunk-or-cmdline)
		       (create-simple-process* thunk-or-cmdline)
		       (create-simple-thread* thunk-or-cmdline))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun process-task-form (form)
    (case (car form)
      (run*      `(list ,@(cdr form)))
      (run       `(list ,@(mapcar #'quote-non-list (cdr form))))
      (otherwise `(lambda () ,form))))
  (defun process-task-body (body)
    (if (eql 1 (length body))
	(process-task-form (car body))
	`(lambda () ,@body))))

(defmacro background (&body body)
  "Evaluate the body in an implicit PROGN in a newly created task,
return the task object."
  `(background* ,(process-task-body body)))

(defun create-pipe-thread (in-fd out-fd thunk)
  (create-simple-thread*
   (lambda ()
     (stop-on-stdio-error
       (call-with-redirect-stdio-to-fds in-fd out-fd nil thunk)))))

(defun create-pipe-process (in-fd out-fd cmdline)
  (with-redirect-stdio-to-fds
      (in-fd out-fd)
    (create-simple-process* cmdline)))

;;; FIXME call thunk from end to begin ???
(defun pipe* (&rest thunk-or-cmdline-list)
  (declare (type list thunk-or-cmdline-list))
  (let ((tasks
	 (iter (for thunk-or-cmdline :in thunk-or-cmdline-list)
	       (for remaining :first (cdr thunk-or-cmdline-list)
		    :then (cdr remaining))
	       (for ipipes :first nil :then opipes)
	       (for opipes := (if remaining (sys-pipe)))
	       (for in-fd := (if ipipes (pipe-read ipipes)))
	       (for out-fd := (if opipes (pipe-write opipes)))
	       (collect
		   (if (consp thunk-or-cmdline)
		       (create-pipe-process in-fd out-fd thunk-or-cmdline)
		       (create-pipe-thread in-fd out-fd thunk-or-cmdline))))))
    (iter (for task :in tasks)
	  (wait-task task))
    (task-return-success-p (car (last tasks)))))

(defmacro pipe (&rest forms)
  "Create one task for each form in the forms, connect the stdout of
the previous task with the stdin of the following task, evaluate the
form in each task, finally wait for all tasks to exit.  Return the
exit success status of the last task."
  (let ((thunks (mapcar #'process-task-form forms)))
    `(pipe* ,@thunks)))

;;; create-task

(defun create-task* (thunk-or-cmdline &key stdin stdout stderr)
  (declare (type (or thunk list) thunk-or-cmdline))
  (reap-active-tasks)
  (labels ((open-stdio (stdio direction)
	     (etypecase
		 stdio
	       (null
		(cons nil nil))
	       (keyword
		(ecase stdio
		  (:null
		   (cons (sys-open "/dev/null" o-rdwr) nil))
		  (:pipe
		   (let ((pipes (sys-pipe)))
		     (if (eq direction :output)
			 (cons (pipe-write pipes) (pipe-read pipes))
			 (cons (pipe-read pipes) (pipe-write pipes)))))))
	       (integer
		(cons stdio nil))
	       ((or string pathname)
		(let ((file-name
		       (unix-namestring stdio))
		      (flags
		       (if (eq direction :output)
			   (logior o-wronly o-trunc o-creat)
			   o-rdonly)))
		  (cons (sys-open file-name flags) nil)))))
	   (open-stderr (stderr)
	     (if (eq :stdout stderr)
		 (cons nil nil)
		 (open-stdio stderr :output)))
	   (make-stream (fds direction)
	     (when (cdr fds)
	       (prog1
		   (if (eq :output direction)
		       (make-instance 'fd-stream :fd (cdr fds)
				      :output-buffer-size 0)
		       (make-instance 'fd-stream :fd (cdr fds)
				      :input-buffer-size 0))
		 (setf (cdr fds) nil))))
	   (popcar (cons)
	     (prog1
		 (car cons)
	       (setf (car cons) nil)))
	   (close-fds (fds)
	     (when fds
	       (when (car fds)
		 (sys-close (car fds)))
	       (when (cdr fds)
		 (sys-close (cdr fds)))))
	   (create-simple-task ()
	     (if (consp thunk-or-cmdline)
		 (create-simple-process* thunk-or-cmdline)
		 (create-simple-thread* thunk-or-cmdline))))
    (let (stdin-fds stdout-fds stderr-fds
	  stdin-stream stdout-stream stderr-stream
	  task)
      (unwind-protect
	   (progn
	     (setf stdin-fds  (open-stdio stdin :input)
		   stdout-fds (open-stdio stdout :output)
		   stderr-fds (open-stderr stderr))
	     (setf stdin-stream (make-stream stdin-fds :input))
	     (setf stdout-stream (make-stream stdout-fds :output))
	     (setf stderr-stream (make-stream stderr-fds :output))
	     (setf task (with-redirect-stdio-to-fds
			    ((popcar stdin-fds)
			     (popcar stdout-fds)
			     (popcar stderr-fds))
			  (if (eq stderr :stdout)
			      (call-with-redirect-stderr-to-stdout
			       #'create-simple-task)
			      (create-simple-task))))
	     (with-slots ((stdin stdin) (stdout stdout) (stderr stderr))
		 task
	       (setf stdin stdin-stream)
	       (setf stdout stdout-stream)
	       (setf stderr stderr-stream))
	     (setf stdin-stream nil
		   stdout-stream nil
		   stderr-stream nil)
	     (add-active-task task))
	(close-fds stdin-fds)
	(close-fds stdout-fds)
	(close-fds stderr-fds)
	(when stdin-stream
	  (close stdin-stream))
	(when stdout-stream
	  (close stdout-stream))
	(when stderr-stream
	  (close stderr-stream))))))

(defmacro create-task ((&rest keys &key stdin stdout stderr) &body body)
  "Create a task to evaluate the body in an implicit PROGN, return the
task object, with stdin/stdout/stderr redirected as follow: NIL, the
stdin/stdout/stderr of current task will be inherited; :pipe, a pipe
will be created for the stdin/stdout/stderr, corresponding stream can
be gotten via (task-stdin/stdout/stderr task); :null, the
stdin/stdout/stderr will be redirected to /dev/null; a integer
designating a file descriptor, the stdin/stdout/stderr will be
redirected to the file descriptor."
  (declare (ignore stdin stdout stderr))
  `(create-task* ,(process-task-body body) ,@keys))

(defmacro with-task ((task-var (&rest keys &key stdin stdout stderr) &body body-in-task)
		     &body body-on-task)
  "Create a task to evaluate the body-in-task in an implicit PROGN,
with the stdin/stdout/stderr redirected as create-task, then evaluate
the body-on-task with task-var bound to the task.  Return the values
of the last form of the body-on-task.  Finally close the task."
  (declare (ignore stdin stdout stderr))
  `(let ((,task-var (create-task (,@keys) ,@body-in-task)))
    (unwind-protect
	 (progn ,@body-on-task)
      (close-task ,task-var))))
