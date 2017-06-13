;; Copyright (C) 2013  Eric Schulte
(defpackage :software-evolution-test
  (:nicknames :se-test)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :metabang-bind
   :curry-compose-reader-macros
   :cl-arrows
   :iterate
   :split-sequence
   :stefil
   :cl-ppcre
   :software-evolution
   :software-evolution-utility
   :testbot)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test :batch-test :testbot-test))
(in-package :software-evolution-test)
(enable-curry-compose-reader-macros :include-utf8)
#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
