(defpackage :software-evolution-library/software/rust
  (:nicknames :sel/software/rust :sel/sw/rust)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter-base
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language-cache "rust")
;;;===================================================

(define-language-alias-mappings rust ("rs" "rust"))


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
    ((language (eql ':rust)) (class (eql 'rust-let-declaration)) parse-tree &key)
  "Store the mutable specifier in its own field."
  (with-modify-parse-tree (parse-tree)
    (:mutable-specifier (label-as :mutable-specifier))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-reference-expression))
     parse-tree &key)
  "Store the mutable specifier in its own field."
  (with-modify-parse-tree (parse-tree)
    (:mutable-specifier (label-as :mutable-specifier))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-reference-type))
     parse-tree &key)
  "Store the mutable specifier in its own field."
  (with-modify-parse-tree (parse-tree)
    (:mutable-specifier (label-as :mutable-specifier))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-parameter))
     parse-tree &key)
  "Store the mutable specifier in its own field."
  (with-modify-parse-tree (parse-tree)
    (:mutable-specifier (label-as :mutable-specifier))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-unary-expression)) parse-tree &key)
  "Store the operators of unary expressions in their own field."
  (with-modify-parse-tree (parse-tree)
    ((:- :* :!) (label-as :operator))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-block)) parse-tree &key
     &aux (children (parse-tree-children parse-tree)))
  "Change the class of rust-block such that it reflects whether it implicitly
returns something or not."
  (labels ((has-return-expression-p ()
             "Return T if a semicolon does not occur after the last expression."
             (let* ((children
                      (remove-if {member _ (append1 (extra-asts :rust) :|}|)}
                                 children
                                 :key #'car))
                    (last-child (lastcar children))
                    (type (parse-tree-type last-child)))
               (when (subtypep (format-symbol 'sel/sw/ts "~a-~a" 'rust type)
                               'rust--expression)
                 last-child)))
           (change-target-child (current-child target-child)
             (if (eq current-child target-child)
                 `(:implicit-return-expression
                   ,(cadr target-child)
                   (,current-child))
                 current-child)))
    (if-let ((target-child (has-return-expression-p)))
      `(,(parse-tree-type parse-tree)
        ,(parse-tree-range parse-tree)
        ,(mapcar (op (change-target-child _ target-child)) children))
      (call-next-method))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-self-parameter)) parse-tree &key)
  "Store the operators of unary expressions in their own field."
  (with-modify-parse-tree (parse-tree)
    ((:&) (label-as :borrow))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-closure-expression)) parse-tree &key)
  "Store the operators of unary expressions in their own field."
  (with-modify-parse-tree (parse-tree)
    ((:move) (label-as :move))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-generic-type)) parse-tree &key)
  "Store the :: of the turbofish operator in its own field. This is to get
around generic-type-with-turbofish being aliased to generic-type."
  (with-modify-parse-tree (parse-tree)
    ((:|::|) (label-as :turbofish-operator))))

(defmethod transform-parse-tree
    ((language (eql :rust)) (class (eql 'rust-range-expression)) parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    ((:|..| :|...| :|..=|) (label-as :operator))))


;;; Methods for tree-sitter generics

(defmethod convert ((to (eql 'rust-identifier))
                    (id identifier-ast)
                    &key)
  (convert-terminal to id))

(defmethod convert ((to (eql 'rust-field-identifier))
                    (id identifier-ast)
                    &key)
  (convert-terminal to id))

(defmethod convert ((to (eql 'rust-primitive-type))
                    (id identifier-ast)
                    &key)
  (convert-terminal to id))

(defmethod convert ((to (eql 'rust-type-identifier))
                    (id identifier-ast)
                    &key)
  (convert-terminal to id))


;;; Whitespace.

(defclass rustfmt-style (c-style-indentation)
  ())

(defmethod default-whitespace-style ((ast rust-ast))
  (make 'rustfmt-style))

(define-empty-whitespace-methods (:style rustfmt-style)
  :|.| rust-ast
  rust-ast :|.|
  rust-ast rust-arguments
  rust-ast rust-type-arguments
  rust-ast :|;|
  rust-ast :|;|
  rust-ast :|:|
  rust-ast :|::|
  :|:| rust-ast
  :|::| rust-ast
  :< rust-type-identifier
  rust-type-identifier :>
  :< rust-primitive-type
  :< rust-lifetime
  rust-lifetime :>
  :|'| rust-identifier
  rust-primitive-type :>
  :& rust-ast
  rust-& rust-self
  rust-type-identifier rust-type-arguments
  rust-type-identifier rust-type-parameters
  rust-identifier rust-parameters
  rust-ast :!
  :! rust-ast
  :|impl| rust-type-parameters
  :|*| rust-mutable-specifier)

(defmethod whitespace-between ((s rustfmt-style) (x (eql :=)) (y rust-ast))
  " ")

(defmethod whitespace-between ((s rustfmt-style) (x rust-ast) (y (eql :|{|)))
  " ")

(defmethod whitespace-between ((s rustfmt-style) (x (eql :|{|)) (y rust-ast))
  " ")

(defmethod whitespace-between ((s rustfmt-style) (x rust-ast) (y (eql :|}|)))
  " ")

(defmethod whitespace-between ((s rustfmt-style) (x (eql :|}|)) (y rust-ast))
  " ")

(defmethod whitespace-between ((s rustfmt-style) (x rust-function-item) (y rust-function-item))
  #.(fmt "~2%"))

(defmethod whitespace-between ((s rustfmt-style) (x rust-attribute-item) (y rust-struct-item))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent rust-ast)
                                      (s rustfmt-style)
                                      (x (eql :|let|))
                                      (y t))
  " ")

(defmethod whitespace-between/parent ((parent rust-parameter)
                                      (s rustfmt-style)
                                      (x (eql :|:|))
                                      (y t))
  "Leave a space after a colon, but only in a parameter."
  " ")

(defmethod whitespace-between/parent ((parent rust-field-declaration)
                                      (s rustfmt-style)
                                      (x (eql :|:|))
                                      (y t))
  "Leave a space after a colon in a field declaration."
  " ")

(defmethod whitespace-between/parent ((parent rust-field-initializer)
                                      (s rustfmt-style)
                                      (x (eql :|:|))
                                      (y t))
  "Leave a space after a colon in a field initializer."
  " ")

(defmethod whitespace-between/parent ((parent rust-generic-type)
                                      (s rustfmt-style)
                                      (y t)
                                      (x |RUST-::|))
  "")

(defmethod whitespace-between/parent ((parent rust-generic-type)
                                      (s rustfmt-style)
                                      (x |RUST-::|)
                                      (y t))
  "")

(defmethod whitespace-between/parent ((parent rust-index-expression)
                                      (s rustfmt-style)
                                      (x rust-ast)
                                      (y (eql :|[|)))
  "")

(defmethod whitespace-between/parent ((parent rust-range-expression)
                                      (s rustfmt-style)
                                      (x rust-ast)
                                      (y |RUST-..|))
  "")

(defmethod whitespace-between/parent ((parent rust-range-expression)
                                      (s rustfmt-style)
                                      (y |RUST-..|)
                                      (x rust-ast))
  "")

(defmethod whitespace-between/parent ((parent rust-range-expression)
                                      (s rustfmt-style)
                                      (y |RUST-..=|)
                                      (x rust-ast))
  "")

(defmethod whitespace-between/parent ((parent rust-range-expression)
                                      (s rustfmt-style)
                                      (x rust-ast)
                                      (y |RUST-..=|))
  "")

) ; #+:TREE-SITTER-RUST
