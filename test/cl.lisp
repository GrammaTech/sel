;;;; cl.lisp --- Command Line tool tests.
(defpackage :software-evolution-library/test/cl
  (:nicknames :sel/test/cl)
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
  (:export :cl))
(in-package :software-evolution-library/test/cl)
(in-readtable :curry-compose-reader-macros)
(defsuite cl)


(define-command fact-cl-entry
    (n &spec +common-command-line-options+)
  "Test that canonical REST endpoints work. Computes factorial."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose language))
  (if help
      (show-help-for-fact-cl-entry)
      (factorial n)))

(deftest run-factorial-cl-func ()
  (let ((*standard-output* (make-broadcast-stream)))
    (is (eql (fact-cl-entry 5 :verbose 3) 120))
    (is (eql (fact-cl-entry 52235215 :help T) nil))))
