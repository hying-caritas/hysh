(in-package :hysh)

;;; Glue processes and common lisp functions

(defmacro run-progn (&rest forms)
  "Evaluate the forms in a implicit PROGN, return the values of the
last form."
  `(progn ,@(convert-to-run-body forms)))

(defun run-or* (&rest thunks)
  "Call the thunks one by one, if the exit status of any thunk is
success, return the return values of the thunk immediately without
calling the remaining thunks.  Otherwise return the return values of
the last thunk.  Return NIL if thunks are NIL."
  (declare (type list thunks))
  (iter (for thunk :in thunks)
	(for remaining :on (cdr thunks))
	(for ret-vals := (multiple-value-list (ignore-command-error (funcall thunk))))
	(when (lastcar ret-vals)
	  (return-from run-or* (values-list ret-vals))))
  (let ((last-thunk (lastcar thunks)))
    (when last-thunk
      (funcall last-thunk))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun convert-to-thunks (forms)
    (mapcar (lambda (form) `(lambda () ,form))
	    (convert-to-run-body forms))))

(defmacro run-or (&rest forms)
  "Evaluate the forms one by one, if the exit status of any form is
success, return the values of the form immediately without evaluating
the remaining forms.  Otherwise return the values of the last form.
Return NIL if forms are NIL."
  `(run-or* ,@(convert-to-thunks forms)))

(defun run-and* (&rest thunks)
  "Call the thunks one by one, if the exit status of any thunk is
failure, return the return values of the thunk immediately without
calling the remaining thunks.  Otherwise return the values of the last
thunk.  Return T if thunks are NIL."
  (declare (type list thunks))
  (iter (for thunk :in thunks)
	(for remaining :on (cdr thunks))
	(for ret-vals := (multiple-value-list (ignore-command-error (funcall thunk))))
	(unless (lastcar ret-vals)
	  (return-from run-and* (values-list ret-vals))))
  (let ((last-thunk (lastcar thunks)))
    (if last-thunk
	(funcall last-thunk)
	t)))

(defmacro run-and (&rest forms)
  "Evaluate the forms one by one, if the exit status of any form is
failure, return the values of the form immediately without evaluating
the remaining forms.  Otherwise return the values of the last form.
Return T if forms are NIL."
  `(run-and* ,@(convert-to-thunks forms)))

(defun background* (thunk-or-cmdline)
  "Call the thunk in a newly created task, return the task object."
  (declare (type (or thunk list) thunk-or-cmdline))
  (reap-active-tasks)
  (add-active-task (if (consp thunk-or-cmdline)
		       (create-simple-process* thunk-or-cmdline)
		       (create-simple-thread* thunk-or-cmdline))))

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
    (task-return-value (lastcar tasks))))

(defmacro pipe (&rest forms)
  "Create one task for each form in the forms, connect the stdout of
the previous task with the stdin of the following task, evaluate the
form in each task, finally wait for all tasks to exit.  Return the
return values of the last task."
  (let ((thunks (mapcar #'process-task-form forms)))
    `(pipe* ,@thunks)))
