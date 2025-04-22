(defpackage :software-evolution-library/software/ada
  (:nicknames :sel/software/ada :sel/sw/ada)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter-base
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language-cache "ada")
;;;===================================================

(define-language-alias-mappings ada ("adb" "ads"))


#+:TREE-SITTER-ADA
(progn

) ; #+:TREE-SITTER-ADA
