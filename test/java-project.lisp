;;;; java-project.lisp --- Java project.
(defpackage :software-evolution-library/test/java-project
  (:nicknames :sel/test/java-project)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/project
   :software-evolution-library/software/java-project
   :software-evolution-library/components/test-suite)
  (:export :test-java-project))
(in-package :software-evolution-library/test/java-project)
(in-readtable :curry-compose-reader-macros)
(defsuite test-java-project "Java representation."
    (java-tree-sitter-available-p))

(defixture project-java
  (:setup
   (setf *soft*
         (from-file (make-instance 'java-project :component-class 'java)
                    (java-dir #P"project/"))))
  (:teardown
   (setf *soft* nil)))

(deftest (java-can-parse-a-project :long-running) ()
  (with-fixture project-java
    (is (equal 2 (length (evolve-files *soft*))))
    (is (not (zerop (size *soft*))))))
