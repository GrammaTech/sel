;;;; python-project.lisp --- Python project.
(defpackage :software-evolution-library/test/python-project
  (:nicknames :sel/test/python-project)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/python
   :software-evolution-library/software/project
   :software-evolution-library/software/python-project)
  (:export :test-python-project))
(in-package :software-evolution-library/test/python-project)
(in-readtable :curry-compose-reader-macros)
(defsuite test-python-project "Python representation." (python3.8-available-p))

(defixture fib-project-python
  (:setup
   (setf *soft*
         (from-file (make-instance 'python-project)
                    (python-dir #P"fib-project/"))))
  (:teardown
   (setf *soft* nil)))

(deftest (can-parse-a-python-project :long-running) ()
  (with-fixture fib-project-python
    (is (equal 3 (length (evolve-files *soft*))))
    (is (= 30 (size *soft*)))))
