(in-package :hysh)

(defun do-translate-flags (&key (direction :input) (if-exists :overwrite)
			     (if-does-not-exist :error))
  (logior (ecase direction
	    (:input o-rdonly)
	    (:output o-wronly)
	    (:io o-rdwr))
	  (ecase if-exists
	    (:error o-excl)
	    (:supersede o-trunc)
	    (:append o-append)
	    (:overwrite 0))
	  (ecase if-does-not-exist
	    (:error 0)
	    (:create o-creat))))

(defun translate-flags (flags)
  (apply #'do-translate-flags flags))

(defun open-file-fd-stream (pathname flags)
  (let ((fd (sys-open (unix-namestring pathname)
		      (translate-flags flags)))
	stream)
    (unwind-protect
	 ;; TODO: set input/output-buffer-size according to direction
	 (setf stream (make-instance 'fd-stream :fd fd))
      (unless stream
	(sys-close fd)))
    stream))

(defun call-with-open-fd (pathname flags func)
  (let ((fd (sys-open (unix-namestring pathname)
		      (translate-flags flags))))
    (unwind-protect
	 (funcall func fd)
      (sys-close fd))))

(defmacro with-open-fd ((fd-var pathname flags) &body body)
  `(call-with-open-fd ,pathname ,flags (lambda (,fd-var) ,@body)))

(defun call-with-fd-stream (fd options func)
  (let ((tmp-stream nil))
    (unwind-protect
	 (progn
	   (setf tmp-stream (apply #'make-instance 'fd-stream :fd fd options))
	   (funcall func tmp-stream))
      (if tmp-stream
	  (close tmp-stream)
	  (sys-close fd)))))

(defmacro with-fd-stream ((stream-var fd &rest options) &body body)
  `(call-with-fd-stream ,fd ,options (lambda (,stream-var) ,@body)))

(defun call-with-fd-streams (fd-options-list func)
  (let ((streams (make-list (length fd-options-list))))
    (unwind-protect
	 (progn
	   (iter (for (fd . options) :in fd-options-list)
		 (for stream-list :on streams)
		 (setf (car stream-list)
		       (apply #'make-instance 'fd-stream
			      :fd fd options)))
	   (apply func streams))
      (iter (for stream :in streams)
	    (for (fd . nil) :in fd-options-list)
	    (if stream
		(close stream)
		(sys-close fd))))))

(defmacro with-fd-streams (stream-var-fd-options-list &body body)
  (let ((stream-vars (mapcar #'car stream-var-fd-options-list))
	(fd-options-list (mapcar #'cdr stream-var-fd-options-list)))
    `(call-with-fd-streams (list ,@fd-options-list)
			   (lambda (,@stream-vars) ,@body))))

(defun call-with-file-fd-stream (pathname flags func)
  (let ((fd (sys-open (unix-namestring pathname)
		      (translate-flags flags))))
    (call-with-fd-stream fd nil func)))

(defmacro with-file-fd-stream ((stream-var pathname flags) &body body)
  `(call-with-file-fd-stream ,pathname ,flags
			     (lambda (,stream-var) ,@body)))
