(in-package :hysh)

(deftype thunk (&optional value-type) `(function () ,value-type))

(defconstant +PROGRAM+ '|hysh|)

(defconstant +STDIN-FD+ 0)
(defconstant +STDOUT-FD+ 1)
(defconstant +STDERR-FD+ 2)

(defun free-cstr-vec (cstr-vec)
  (iter (for i :upfrom 0)
	(for ptr := (cffi:mem-aref cstr-vec :pointer i))
	(until (cffi:null-pointer-p ptr))
	(cffi:foreign-free ptr))
  (cffi:foreign-free cstr-vec))

(defun string-join (sep strings)
  (if (null strings)
      nil
      (let* ((sep-len (length sep))
	     (result (make-string (+ (apply #'+ (mapcar #'length strings))
				     (* sep-len (1- (length strings)))))))
	(replace result (car strings))
	(iter (for pos :first (length (car strings))
		   :then (+ pos sep-len (length str)))
	      (for str :in (cdr strings))
	      (replace result sep :start1 pos)
	      (replace result str :start1 (+ pos sep-len)))
	result)))
