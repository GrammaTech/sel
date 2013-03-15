;; Copyright (C) 2011  Eric Schulte
(defpackage #:software-evolution
  (:use
   :common-lisp
   :alexandria
   :split-sequence
   :metabang-bind
   :cl-store
   :trivial-shell
   :trivial-timeout
   :curry-compose-reader-macros
   :cl-ppcre
   ;; :cl-quickcheck
   :eager-future2
   :memoize
   :elf
   )
  (:shadow :type :magic-number))
