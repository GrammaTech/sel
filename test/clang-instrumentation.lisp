;;;; clang-instrumentation.lisp --- Tests for Clang instrumentation.
(defpackage :software-evolution-library/test/clang-instrumentation
  (:nicknames :sel/test/clang-instrumentation)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/constants
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :clang-instrumentation))
(in-package :software-evolution-library/test/clang-instrumentation)
(in-readtable :curry-compose-reader-macros)
(defsuite clang-instrumentation)

(defvar *project* nil "Software used in project fixtures.")

(deftest (multi-threaded-clang-instrument-test :long-running) ()
  (with-fixture clang-project
    (do-multi-threaded-instrument-clang-test *project*))
  (with-fixture grep-bear-project
    (do-multi-threaded-instrument-clang-test *project*)))
