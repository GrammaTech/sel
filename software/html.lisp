(defpackage :software-evolution-library/software/html
  (:nicknames :sel/software/html :sel/sw/html)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter-base
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language-cache "html")
;;;===================================================

(define-language-alias-mappings html ("html"))


#+:TREE-SITTER-HTML
(progn

) ; #+:TREE-SITTER-HTML
