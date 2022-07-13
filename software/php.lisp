(defpackage :software-evolution-library/software/php
  (:nicknames :sel/software/php :sel/sw/php)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter-base
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language-cache "php")
;;;===================================================

(define-language-alias-mappings php ("php"))


#+:TREE-SITTER-PHP
(progn

) ; #+:TREE-SITTER-PHP
