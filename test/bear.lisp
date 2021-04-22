;;;; bear.lisp --- Clang representation.
(defpackage :software-evolution-library/test/bear
  (:nicknames :sel/test/bear)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/project
   :software-evolution-library/software/clang-project)
  (:export :test-bear))
(in-package :software-evolution-library/test/bear)
(in-readtable :curry-compose-reader-macros)
(defsuite test-bear "Clang representation."
  (lambda () (zerop (nth-value 2 (shell "which bear")))))


;;#-windows
#+TODO
(deftest (able-to-create-a-bear-project :long-running) ()
  (with-fixture grep-bear-project
    (is (equal "make grep" (build-command *project*)))
    (is (equalp '("grep") (artifacts *project*)))
    (is (not (zerop (length (genome-string *project*)))))))

;;#-windows
#+TODO
(deftest (able-to-copy-a-bear-project :long-running) ()
  (with-fixture grep-bear-project
    (is (copy *project*)
        "Able to copy a project built with bear.")))
