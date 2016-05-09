;; Copyright (C) 2013  Eric Schulte
(defpackage #:software-evolution-test
  (:nicknames :se-test)
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :stefil
   :cl-ppcre
   :software-evolution
   :software-evolution-utility)
  (:export :test :batch-test))
