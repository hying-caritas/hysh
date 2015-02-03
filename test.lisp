(in-package #:cl)

(defpackage #:hysh-test
  (:use #:cl #:hysh #:fiveam)
  (:shadowing-import-from #:hysh #:run))

(in-package #:hysh-test)

(def-suite hysh :description "The HYSH testing suite")

(in-suite hysh)

(test out/ss
      "Test the run/ss function"
      (is (equal (out/ss (run echo "1" "2" "3")) "1 2 3"))
      (is (equal (out/ss (pipe (run echo "hello," world)
			       (run tr "hw" "HW")
			       (run sed -e "s/$/!/")))
		 "Hello, World!")))

(test filter
      "Test the common lisp filter"
      (is (equal (out/ss (pipe (run echo "123")
			       (filter-line #'identity)))
		 "123"))
      (is (equal (let ((*standard-output* (make-string-output-stream)))
		   (out/ss (pipe (run echo "1 2 3")
				 (filter-line #'identity))))
		 "1 2 3")))

(test environment
      "Test the environment"
      (is (equal (out/ss (with-change-env (("asdf" "jkl;")) ()
			   (run bash -c "echo $asdf")))
		 "jkl;")))

(defun main ()
  (run! 'hysh))
