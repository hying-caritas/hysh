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

(defun split-string-with-char (str char &key (max -1) from-end)
  (labels ((split (str max acc)
	     (if (= max 1)
		 (cons str acc)
		 (let ((pos (position char str :from-end from-end)))
		   (if pos
		       (split (subseq str (1+ pos)) (1- max)
			      (cons (subseq str 0 pos) acc))
		       (cons str acc))))))
    (nreverse (split str max nil))))
