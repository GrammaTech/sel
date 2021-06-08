(defpackage :software-evolution-library/software/go
  (:nicknames :sel/software/golang :sel/software/go :sel/sw/go :sel/sw/golang)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "go")
;;;===================================================
