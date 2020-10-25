;;;; java-tree-sitter.lisp --- Java software representation with tree-sitter
;;;;                           backend.
(uiop:define-package :software-evolution-library/software/java-tree-sitter
  (:nicknames :sel/software/java-tree-sitter :sel/sw/java-tree-sitter
              :sel/software/java-ts :sel/sw/java-ts)
  (:use :gt/full
        :babel
        :cl-json
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/parseable
        :software-evolution-library/software/non-homologous-parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting))
(in-package :software-evolution-library/software/java-tree-sitter)
(in-readtable :curry-compose-reader-macros)

(define-software java-tree-sitter (tree-sitter) ()
  (:documentation "Java tree-sitter software representation."))


;;; Shared object set-up
(register-tree-sitter-language "tree-sitter-java" :java 'java-tree-sitter-ast)


;;; C tree-sitter classes
(eval-always
  ;; TODO: maybe figure out a way to roll this into the automatically
  ;;       defined classes?
  (defclass java-tree-sitter-ast (tree-sitter-ast)
    ()
    (:documentation "AST for Java from input via tree-sitter.")))

;;; TODO: work on ease-of-use macros for tree-sitter.
(define-tree-sitter-classes ()
  ;; TODO: throw these in a variable that actually looks
  ;;       at the project path.
  "~/quicklisp/local-projects/sel/software/tree-sitter/java/node-types.json"
  "~/quicklisp/local-projects/sel/software/tree-sitter/java/grammar.json"
  :java
  java-tree-sitter-ast)
