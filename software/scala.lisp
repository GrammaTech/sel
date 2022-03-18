(defpackage :software-evolution-library/software/scala
  (:nicknames :sel/software/scala :sel/sw/scala)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "scala")
;;;===================================================

(define-language-alias-mappings scala ("scala"))


#+:TREE-SITTER-SCALA
(progn

) ; #+:TREE-SITTER-SCALA
