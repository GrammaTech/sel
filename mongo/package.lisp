(defpackage :software-evolution-mongo
  (:nicknames :se-mongo)
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :cl-arrows
   :split-sequence
   :cl-ppcre
   :cl-mongo
   :software-evolution
   :software-evolution-utility)
  (:export :mongo-database))
