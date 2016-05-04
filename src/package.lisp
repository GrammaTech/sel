(defpackage :clang-instrument
  (:nicknames :ci)
  (:documentation "Instrument C-language source files")
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :cl-arrows
        :iterate
        :split-sequence
        :software-evolution
        :software-evolution-utility)
  (:export :main))
