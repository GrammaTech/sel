(defpackage :software-evolution-library/software/css
  (:nicknames :sel/software/css :sel/sw/css)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "css")
;;;===================================================

(define-language-alias-mappings css ("css"))


#+:TREE-SITTER-CSS
(progn

) ; #+:TREE-SITTER-CSS
