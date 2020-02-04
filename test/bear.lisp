;;;; bear.lisp --- Clang representation.
(defpackage :software-evolution-library/test/bear
  (:nicknames :sel/test/bear)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/project
   :software-evolution-library/software/clang-project)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-bear))
(in-package :software-evolution-library/test/bear)
(in-readtable :curry-compose-reader-macros)
(defsuite test-bear "Clang representation."
  (lambda () (zerop (nth-value 2 (shell "which bear")))))

(defvar *project* nil "Software used in project fixtures.")

#-windows
(deftest (able-to-create-a-bear-project :long-running) ()
  (with-fixture grep-bear-project
    (is (equal "make grep" (build-command *project*)))
    (is (equalp '("grep") (artifacts *project*)))
    (is (not (zerop (length (genome *project*)))))))

#-windows
(deftest (able-to-copy-a-bear-project :long-running) ()
  (with-fixture grep-bear-project
    (is (copy *project*)
        "Able to copy a project built with bear.")))
