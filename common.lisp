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
