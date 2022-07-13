(defpackage :software-evolution-library/software/c-sharp
  (:nicknames :sel/software/c-sharp :sel/sw/c-sharp)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter-base
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language-cache "c-sharp")
;;;===================================================

(define-language-alias-mappings c-sharp ("c-sharp" "cs"))


#+:TREE-SITTER-C-SHARP
(progn

) ; #+:TREE-SITTER-C-SHARP
