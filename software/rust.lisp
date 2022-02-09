(defpackage :software-evolution-library/software/rust
  (:nicknames :sel/software/rust :sel/sw/rust)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "rust")
;;;===================================================

#+:TREE-SITTER-RUST
(progn

(defmethod parse-language ((superclass (eql 'rust-ast)) string &key)
  (labels ((name-generator (string)
             (make-keyword (convert-name :rust string))))
    (parse-string (get-language-from-superclass superclass) string
                  :produce-cst t
                  :name-generator #'name-generator)))


;;; Parse Tree Transforms
(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-function-modifiers)) parse-tree &key)
  "Transform PARSE-TREE such that all modifiers are stored in the :modifiers
field."
  (with-modify-parse-tree (parse-tree)
    ((:error :line-comment :block-comment) (ignore-types))
    (t (label-as :modifiers))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-unary-expression)) parse-tree &key)
  "Store the operators of unary expressions in their own field."
  (with-modify-parse-tree (parse-tree)
    ((:- :* :!) (label-as :operator))))


;;; Whitespace.

(defmethod whitespace-between (style (x (eql :|.|)) (y rust-ast))
  "")

(defmethod whitespace-between (style (x rust-ast) (y (eql :|.|)))
  "")

(defmethod whitespace-between (s (x rust-ast) (y rust-arguments))
  "")

(defmethod whitespace-between (style (x rust-identifier) (y (eql :|:|)))
  "")

(defmethod whitespace-between (style (x (eql :|:|)) (y rust-primitive-type))
  "")

(defmethod whitespace-between (style (x rust-ast) (y (eql :|;|)))
  "")

(defmethod whitespace-between/parent ((parent rust-ast)
                                      style
                                      (x string)
                                      (y t))
  (whitespace-between/parent parent style (make-keyword x) y))

(defmethod whitespace-between/parent ((parent rust-ast)
                                      style
                                      (x (eql :|let|))
                                      (y t))
  " ")

) ; #+:TREE-SITTER-RUST
