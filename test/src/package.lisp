;; Copyright (C) 2013  Eric Schulte
(defpackage #:software-evolution-test
  (:nicknames :se-test)
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :cl-arrows
   :iterate
   :stefil
   :cl-ppcre
   :software-evolution
   :software-evolution-utility)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:export :test :batch-test))
