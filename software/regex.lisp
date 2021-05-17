(defpackage :software-evolution-library/software/regex
  (:nicknames :sel/software/regex :sel/sw/regex)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "regex")
;;;===================================================
