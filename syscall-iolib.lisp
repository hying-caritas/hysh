(in-package :hysh)

(defvar *fd-lock* (make-lock))

(import '(isys:syscall-error isys::syscall-of isys:code-of isys:strerror
	  isys:waitpid isys:wnohang isys:wifexited isys:wexitstatus
	  isys:wifsignaled isys:wtermsig
	  isys:o-rdonly isys:o-wronly isys:o-rdwr isys:o-excl isys:o-trunc
	  isys:o-append isys:o-creat
	  isys:dup isys:fd-cloexec-p isys:mkstemp isys:lseek
	  isys:seek-set isys:getcwd isys:chdir isys:os-environ))

(setf (symbol-function 'sys-close) #'isys:close)

(defun signal-keyword (signum)
  (cffi:foreign-enum-keyword 'signal signum))

(defmacro with-fd-lock-held (&body body)
  `(with-lock-held (*fd-lock*)
     ,@body))

(defun sys-open (path flags &optional (mode #o666))
  (with-fd-lock-held
    (let ((fd (isys:open path flags mode)))
      (setf (isys:fd-cloexec-p fd) t)
      fd)))

(defmacro pipe-read (ps)
  `(car ,ps))

(defmacro pipe-write (ps)
  `(cadr ,ps))

(defun sys-pipe ()
  (with-fd-lock-held
    (let ((pipes (multiple-value-list (isys:pipe))))
      (setf (isys:fd-cloexec-p (pipe-read pipes)) t)
      (setf (isys:fd-cloexec-p (pipe-write pipes)) t)
      pipes)))

(defun sys-dup (fd)
  (with-fd-lock-held
    (let ((new-fd (isys:dup fd)))
      (setf (isys:fd-cloexec-p new-fd) t)
      new-fd)))
