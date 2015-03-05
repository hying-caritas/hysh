(in-package :hysh)

;;; variables = null, cenv = null: system default environment, cenv is in effect (os-environ)
;;; variables = empty hash: null environment, cenv must be null or consistent with variables
;;; variables = hash: cenv must be null or consistent with variables
;;; variables = null, cenv = not null: invalid
;;;
;;; cenv is just a converted cache of variables for using same environment for different command
(defclass environment ()
  ((lock :initform (make-lock))
   (refcount :initform 1 :type integer)
   (variables :initform nil :initarg :variables :type (or null hash-table))
   (cenv :initform (cffi:null-pointer) :initarg :cenv :type cffi:foreign-pointer)))

(defvar *environment* (make-instance 'environment))

(defmacro with-environment-locked (environment &body body)
  `(with-lock-held ((slot-value ,environment 'lock)) ,@body))

(defun environment-ref (environment)
  (with-environment-locked environment
    (with-slots (refcount)
	environment
      (assert (/= 0 refcount))
      (incf refcount))))

(defun environment-unref (environment)
  (with-environment-locked environment
    (with-slots (refcount)
	environment
      (assert (/= 0 refcount))
      (decf refcount))))

(defun copy-environment (&optional (environment *environment*))
  (declare (type environment environment))
  (with-environment-locked environment
    (with-slots (variables cenv)
	environment
      (make-instance 'environment
		     :cenv (cffi:null-pointer)
		     :variables (when variables
				  (copy-hash-table variables))))))

(defun %split-var=val (str)
  (multiple-value-bind (var pos)
      (split-sequence #\= str :count 1)
    (nconc var (list (subseq str pos)))))

(defun cenv->hash (cenv)
  (when (cffi:null-pointer-p cenv)
    (return-from cenv->hash nil))
  (let ((hash (make-hash-table :test #'equal)))
    (iter (for ptr :first cenv :then (cffi:mem-aptr ptr :pointer 1))
	  (for var=val := (cffi:mem-aref ptr :pointer))
	  (until (cffi:null-pointer-p var=val))
	  (for (var val) := (%split-var=val (cffi:foreign-string-to-lisp var=val)))
	  (setf (gethash var hash) val))
    hash))

(defun hash->cenv (hash)
  (let* ((cenv (cffi:foreign-alloc :pointer
				   :count (1+ (hash-table-count hash))))
	 (ptr cenv))
    (flet ((add-env (var val)
	     (setf (cffi:mem-aref ptr :pointer)
		   (cffi:foreign-string-alloc (format nil "~a=~a" var val))
		   ptr (cffi:mem-aptr ptr :pointer 1))))
      (maphash #'add-env hash)
      (setf (cffi:mem-aref ptr :pointer) (cffi:null-pointer))
      cenv)))

(defun %free-cenv (cenv)
  (unless (cffi:null-pointer-p cenv)
    (free-cstr-vec cenv)))

(defun close-environment (environment)
  (declare (type environment environment))
  (when (= 0 (environment-unref environment))
    (with-slots (variables cenv)
	environment
      (setf variables nil)
      (%free-cenv cenv)
      (setf cenv (cffi:null-pointer)))))

(defun get-cenv (&optional (environment *environment*))
  (declare (type environment environment))
  (with-environment-locked environment
    (with-slots (variables cenv)
	environment
      (if (cffi:null-pointer-p cenv)
	  (if (null variables)
	      (os-environ)
	      (setf cenv (hash->cenv variables)))
	  cenv))))

(defun environment-variable (var &optional (environment *environment*))
  "Get the value of the var in the environment (default to current
environment)"
  (declare (type environment environment)
	   (type string var))
  (with-environment-locked environment
    (with-slots (variables cenv)
	environment
      (unless variables
	(setf variables (cenv->hash (os-environ))))
      (gethash var variables))))

(defun %prepare-change-env (environment)
  (declare (type environment environment))
  (with-slots (variables cenv)
      environment
    (unless variables
      (setf variables (cenv->hash (os-environ))))
    (unless (cffi:null-pointer-p cenv)
      (%free-cenv cenv)
      (setf cenv (cffi:null-pointer)))))

(defun (setf environment-variable) (val var &optional (environment *environment*))
  "Set the var in the environment (default to current environment) to
the value"
  (declare (type environment environment)
	   (type string var val))
  (with-environment-locked environment
    (%prepare-change-env environment)
    (setf (gethash var (slot-value environment 'variables))
	  val)))

(defun mkunbound-environment-variable (var &optional (environment *environment*))
  "Remove the var in the environment (default to current environment)"
  (declare (type environment environment)
	   (type string var))
  (with-environment-locked environment
    (%prepare-change-env environment)
    (remhash var (slot-value environment 'variables))))

(defun call-with-change-environment (sets unsets thunk)
  (let ((*environment* (copy-environment)))
    (unwind-protect
	 (progn
	   (iter (for (var val) :in sets)
		 (setf (environment-variable var) val))
	   (iter (for var :in unsets)
		 (mkunbound-environment-variable var))
	   (funcall thunk))
      (close-environment *environment*))))

(defmacro with-change-environment ((&rest sets) (&rest unsets) &body body)
  "Make a copy of current environment and make it the current
environment, do specified set/unset operations on the environment,
then evaluate the body in an implicit PROGN, return the values of the
last form of the body.  Finally restore the original environment.  The
format to specify set operations is ((var val) ...), the format to
specify unset operations is (var ...)"
  `(call-with-change-environment ,(cons 'list
					(mapcar (lambda (var-val) (cons 'list var-val))
						sets))
				 ,unsets (lambda () ,@body)))
