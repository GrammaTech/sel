(defpackage :software-evolution-library-mongo
  (:nicknames :sel-mongo)
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :cl-arrows
   :split-sequence
   :cl-ppcre
   :cl-mongo
   :software-evolution-library
   :software-evolution-library-utility)
  (:export :mongo-database))
