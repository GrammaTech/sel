(defpackage :software-evolution-library/software/agda
  (:nicknames :sel/software/agda :sel/sw/agda)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter-base
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language-cache "agda")
;;;===================================================
