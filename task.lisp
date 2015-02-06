(in-package :hysh)

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

(defmacro with-argv ((arg0 argv args) &body body)
  `(let ((,argv (alloc-argv ,args)))
     (unwind-protect
	  (let ((,arg0 (cffi:mem-ref ,argv :pointer)))
	    ,@body)
       (free-cstr-vec ,argv))))

;;; Copied from iolib.os
(defun call-with-lfp-spawn-arguments (thunk)
  (cffi:with-foreign-objects ((attributes 'iolib.os::lfp-spawnattr-t)
			      (file-actions 'iolib.os::lfp-spawn-file-actions-t))
    (let ((spawnattr-initialized-p nil)
          (file-actions-initialized-p nil))
      (unwind-protect
           (progn
             (setf spawnattr-initialized-p
                   (iolib.os::lfp-spawnattr-init attributes))
             (setf file-actions-initialized-p
                   (iolib.os::lfp-spawn-file-actions-init file-actions))
             (funcall thunk attributes file-actions))
        (when spawnattr-initialized-p
          (iolib.os::lfp-spawnattr-destroy attributes))
        (when file-actions-initialized-p
          (iolib.os::lfp-spawn-file-actions-destroy file-actions))))))

(defmacro with-lfp-spawn-arguments ((attributes file-actions) &body body)
  `(call-with-lfp-spawn-arguments
    (lambda (,attributes ,file-actions) ,@body)))

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
    (pathname (uiop:unix-namestring (translate-logical-pathname x)))
    (t (princ-to-string x))))

(defun any->string-list (x)
  (typecase x
    (string (list x))
    (sequence (map 'list #'any->string x))
    (t (list (any->string x)))))

(defun create-simple-process* (cmdline-in)
  (let ((cmdline (mapcan #'any->string-list cmdline-in)))
    (with-argv (arg0 argv cmdline)
      (with-lfp-spawn-arguments (attributes file-actions)
	(iolib.os::lfp-spawnattr-setcwd attributes (uiop:unix-namestring *current-directory*))
	(with-all-redirections file-actions
	  (cffi:with-foreign-object (pid 'isys:pid-t)
	    (with-fd-lock-held
	      (iolib.os::lfp-spawnp pid arg0 argv (get-cenv) file-actions attributes))
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
	 (on-command-error *on-command-error*)
	 (environment *environment*)
	 (current-directory *current-directory*))
    (mapc (lambda (redir)
	    (fd-stream-ref (redirection-stream redir)))
	  redirections)
    (environment-ref environment)
    (let ((sys-thread
	   (make-thread
	    (lambda ()
	      (progv
		  (mapcar #'redirection-symbol symbol-redirections)
		  (mapcar #'redirection-stream symbol-redirections)
		(let ((*redirections* redirections)
		      (*environment* environment)
		      (*current-directory* current-directory))
		  (unwind-protect
		       (ecase on-command-error
			 ((nil)   (funcall thunk))
			 (:ignore (ignore-command-error* thunk))
			 (:warn   (warn-on-command-error* thunk))
			 (:stop   (stop-on-command-error* thunk)))
		    (mapc (lambda (redir)
			    (close (redirection-stream redir)))
			  redirections)
		    (close-environment environment))))))))
      (make-instance 'thread-task :thread sys-thread))))

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
		       (uiop:unix-namestring stdio))
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
