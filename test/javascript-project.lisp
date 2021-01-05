;;;; javascript-project.lisp --- Javascript project.
(defpackage :software-evolution-library/test/javascript-project
  (:nicknames :sel/test/javascript-project)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/project
   :software-evolution-library/software/javascript-project
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/components/test-suite)
  (:export :test-javascript-project))
(in-package :software-evolution-library/test/javascript-project)
(in-readtable :curry-compose-reader-macros)
(defsuite test-javascript-project "Javascript representation." (acorn-available-p))

(defixture fib-project-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript-project
                      :component-class 'javascript)
                    (javascript-dir #P"fib-project/"))))
  (:teardown
   (setf *soft* nil)))

(deftest (can-parse-a-javascript-project :long-running) ()
  (with-fixture fib-project-javascript
    (is (equal 2 (length (evolve-files *soft*))))
    (is (not (zerop (size *soft*))))))
