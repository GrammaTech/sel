(defpackage :software-evolution-library/software/ecma
  (:nicknames :sel/software/ecma :sel/sw/ecma)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/tree-sitter-base
        :software-evolution-library/software/template)
  (:documentation "Code common to all ECMAScript variants:
JavaScript, TypeScript, and TSX."))

(in-package :software-evolution-library/software/tree-sitter)


;;; Whitespace rules

(defclass ecma-whitespace (c-style-indentation)
  ())

(defmethod default-whitespace-style ((self ecma-ast))
  (make 'ecma-whitespace))

(define-empty-whitespace-methods (:style ecma-whitespace)
  identifier-ast parameters-ast
  ecma-ast decrement-operator-ast
  decrement-operator-ast ecma-ast
  increment-operator-ast ecma-ast
  ecma-ast increment-operator-ast
  ecma-ast semicolon-ast
  ;; No whitespace before the arguments of an IIFE.
  ecma-parenthesized-expression ecma-arguments)
