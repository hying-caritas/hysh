(in-package :hysh)

;; pipeline filter helpers

;; char-func should be char -> char function
(defun filter-char (char-func)
  "Call the char-func for the each character read form
*standard-input*, and write the return value of the char-func to
*standard-output* if it is not NIL."
  (iter (for c := (read-char *standard-input* nil))
	(while c)
	(let ((out-char (funcall char-func c)))
	  (if out-char
	      (write-char out-char))))
  t)

;; line-func should be string -> string (line -> line) function
(defun filter-line (line-func)
  "Call the line-func for the each line read form *standard-input*,
and write the return value of the line-func to *standard-output* if
it is not NIL."
  (iter (for line := (read-line *standard-input* nil))
	(while line)
	(let ((out-line (funcall line-func line)))
	  (if out-line
	      (write-line out-line))))
  t)
