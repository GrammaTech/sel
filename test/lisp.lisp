(defpackage :software-evolution-library/test/lisp
  (:nicknames :sel/test/lisp)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/software/lisp)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-lisp))
(in-package :software-evolution-library/test/lisp)
(in-readtable :curry-compose-reader-macros)

(defsuite test-lisp "Lisp representation")
