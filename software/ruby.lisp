(defpackage :software-evolution-library/software/ruby
  (:nicknames :sel/software/ruby :sel/sw/ruby)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "ruby")
;;;===================================================

(define-language-alias-mappings ruby ("rb" "ruby"))


#+:TREE-SITTER-RUBY
(progn

) ; #+:TREE-SITTER-RUBY
