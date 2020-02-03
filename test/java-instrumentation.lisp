;;;; java-instrumentation.lisp --- Tests for Java instrumentation.
(defpackage :software-evolution-library/test/java-instrumentation
  (:nicknames :sel/test/java-instrumentation)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
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
   :defmethod :defgeneric))
(in-package :software-evolution-library/test/java-instrumentation)
(in-readtable :curry-compose-reader-macros)

(deftest (multi-threaded-java-instrument-test :long-running) ()
  (with-fixture java-project
    (let ((st-instrumented
           (instrument (copy *soft*) :num-threads 1))
          (mt-instrumented
           (instrument (copy *soft*) :num-threads 4)))
      (is (equalp (genome st-instrumented)
                  (genome mt-instrumented))
          "`instrument` should yield the same genome regardless of the ~
           number of threads utilized.")

      (uninstrument st-instrumented :num-threads 1)
      (uninstrument mt-instrumented :num-threads 4)
      (is (equalp (genome st-instrumented)
                  (genome mt-instrumented))
          "`uninstrument` should yield the same genome regardless of the ~
           number of threads utilized."))))
