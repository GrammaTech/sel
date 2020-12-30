;;;; python-project.lisp --- Python project.
(defpackage :software-evolution-library/test/python-project
  (:nicknames :sel/test/python-project)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/project
   :software-evolution-library/software/python-project
   :software-evolution-library/software/tree-sitter)
  (:export :test-python-project))
(in-package :software-evolution-library/test/python-project)
(in-readtable :curry-compose-reader-macros)
(defsuite test-python-project "Python representation."
    (python-tree-sitter-available-p))

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
    (is (= 45 (size *soft*)))))
