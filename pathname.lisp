(in-package :hysh)

(defvar *current-directory* *default-pathname-defaults*)

(defun current-directory ()
  *current-directory*)

(defun (setf current-directory) (pathname)
  (setf *current-directory*
	(uiop:parse-unix-namestring pathname :ensure-directory t)))

(defmacro with-current-directory (pathname &body body)
  `(let ((*current-directory* (uiop:parse-unix-namestring ,pathname
							 :ensure-directory t)))
     (progn ,@body)))
