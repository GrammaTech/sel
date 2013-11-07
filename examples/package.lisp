(defpackage :repair
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :split-sequence
        :cl-store
        :cl-ppcre
        :cl-launch
        :bordeaux-threads
        :diff
        :delta-debug
        :software-evolution
        :software-evolution-utility)
  (:shadow :type :magic-number :repair)
  (:export :repair))
