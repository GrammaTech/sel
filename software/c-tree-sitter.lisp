;;;; c-tree-sitter.lisp --- C software representation with tree-sitter backend.
(uiop:define-package :software-evolution-library/software/c-tree-sitter
  (:nicknames :sel/software/c-tree-sitter :sel/sw/c-tree-sitter
              :sel/software/c-ts :sel/sw/c-ts)
  (:use :gt/full
        :babel
        :cl-json
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/parseable
        :software-evolution-library/software/non-homologous-parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :c-tree-sitter
           :c-tree-sitter-ast))
(in-package :software-evolution-library/software/c-tree-sitter)
(in-readtable :curry-compose-reader-macros)

(define-software c-tree-sitter (tree-sitter) ()
  (:documentation "C tree-sitter software representation."))


;;; Shared object set-up
(register-tree-sitter-language "tree-sitter-c" :c 'c-tree-sitter-ast)


;;; C tree-sitter classes
(eval-always
  (defmethod inconsistent-production-p
      ((language (eql :c)) (production-name (eql :update-expression)))
    t))

;;; TODO: work on ease-of-use macros for tree-sitter.
(define-tree-sitter-classes ()
  ;; TODO: throw these in a variable that actually looks
  ;;       at the project path.
  "~/quicklisp/local-projects/sel/software/tree-sitter/c/node-types.json"
  "~/quicklisp/local-projects/sel/software/tree-sitter/c/grammar.json"
  :c
  c-tree-sitter-ast)

(defmethod statement-ast-p ((language (eql :c)) (ast c-tree-sitter-ast))
  ;; TODO: this definitely doesn't cover everything.
  (or (typep ast '(or c--statement c-function-definition))
      (equal ";" (lastcar (interleaved-text ast)))))

(defmethod parse-asts ((obj c-tree-sitter)
                       &optional (source (genome-string obj)))
  (convert 'c-tree-sitter-ast source))
