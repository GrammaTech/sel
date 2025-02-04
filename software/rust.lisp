(defpackage :software-evolution-library/software/rust
  (:nicknames :sel/software/rust :sel/sw/rust)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter-base
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

(eval-always
 (uiop:add-package-local-nickname
  :rust :software-evolution-library/software/rust))

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
  "Store the operator and argument of a unary expression in their own fields."
  (with-modify-parse-tree (parse-tree)
    ((:- :* :!) (label-as :operator))
    (t (label-as :argument))))

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
  (with-modify-parse-tree (parse-tree)
    ((:&) (label-as :borrow))))

(defmethod transform-parse-tree
    ((language (eql ':rust)) (class (eql 'rust-closure-expression)) parse-tree &key)
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

(defmethod transform-parse-tree
    ((language (eql :rust)) (class (eql 'rust-token-tree)) parse-tree &key)
  "Record the delimiters of a Rust macro invocation.
Rust macro invocations can use (), [], and {} equivalently."
  (with-modify-parse-tree (parse-tree)
    ((:|(| :|[| :|{|) (label-as :left-delimiter))
    ((:|)| :|]| :|}|) (label-as :right-delimiter))))


;;; Methods for tree-sitter generics

(defmethod output-transformation :around ((ast rust-arguments) &key &allow-other-keys)
  ;; This is a workaround for children-parser not backtracking: the
  ;; rule is a sequence of `(attribute_item? expression)*` ASTs. The
  ;; problem is the alternative-ast, which is a wildcard, matches the
  ;; attribute_item rule, so there is nothing left to be an expression
  ;; and the match fails.
  (match (direct-children ast)
    ((list (and real-child (alternative-ast)))
     (let* ((id (make 'rust-identifier :text "temp"))
            (temp (copy ast :children (list id))))
       (substitute real-child id (output-transformation temp))))
    (otherwise (call-next-method))))

(defmethod output-transformation :around ((ast rust-tuple-expression) &key &allow-other-keys)
  ;; This is a workaround for children-parser not backtracking: the
  ;; rule is `attribute_item?* expression*` ASTs. Leading alternative
  ;; ASTs all match as attribute items. If there are no other ASTs
  ;; after them (where the expressions should be), the output
  ;; transformation is invalid.
  (let ((children (direct-children ast)))
    ;; If the child list starts with alternative ASTs, check if the
    ;; suffix starts with an attribute item. If it does, we're OK. If
    ;; it doesn't, add a fake expression in place of the last
    ;; alternative-ast, compute an output transformation, then
    ;; substitute the last alternative-ast back in.
    (match children
      ((list* (alternative-ast) _)
       (let ((suffix (drop-while (of-type 'alternative-ast) children)))
         (match suffix
           ((list* (rust-attribute-item) _)
            (call-next-method))
           (otherwise
            (let ((prefix (ldiff children suffix)))
              (let* ((temp-id (make 'rust-identifier :text "temp"))
                     (temp-copy
                       (copy ast
                             :children
                             (append (butlast prefix)
                                     (list temp-id)
                                     suffix))))
                (substitute (lastcar prefix)
                            temp-id
                            (output-transformation temp-copy))))))))
      (otherwise (call-next-method)))))

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

(defmethod convert ((to (eql 'integer))
                    (ast rust-integer-literal)
                    &key)
  (parse-integer (text ast) :junk-allowed t))

(defmethod convert ((to (eql 'float))
                    (ast rust-float-literal) &key)
  (match (text ast)
    ((ppcre "(.*)f32$" text)
     (parse-float text :type 'single-float))
    ((or (ppcre "(.*)f64$" text) text)
     (parse-float text :type 'double-float))))

(defmethod convert ((to (eql 'rust-ast))
                    (value single-float) &key)
  (make 'rust-float-literal :text (fmt "~df32" value)))

(defmethod convert ((to (eql 'rust-ast))
                    (value double-float) &key)
  (make 'rust-float-literal :text (fmt "~df64" value)))

(defmethod convert ((to (eql 'rust-ast))
                    (value integer) &key)
  (make 'rust-integer-literal :text (fmt "~a" value)))

(defmethod constant-fold ((ast rust-block))
  (match ast
    ((rust-block
      (children
       (list
        (rust-implicit-return-expression
         (children
          (list expr))))))
     (constant-fold expr))))

(defmethod exit-control-flow ((ast rust-implicit-return-expression))
  (find-enclosing 'compound-ast (attrs-root*) ast))

(defmethod exit-control-flow ((ast rust-try-expression))
  (cons (find-enclosing 'returnable-ast (attrs-root*) ast)
        (ensure-list (call-next-method))))

(defmethod entry-control-flow ((ast rust-else-clause))
  (children ast))

(defmethod exit-control-flow ((ast rust-macro-invocation))
  (if (source-text= (rust-macro ast) "panic")
      nil
      (call-next-method)))


;;; Symbol table

;;; NB Rust has five namespaces: types, values (variables), macros,
;;; labels and lifetimes. There is no separate namespace for
;;; functions. We re-route requests for function definitions as
;;; variables.

(defmethod get-declaration-ids ((ns (eql :function)) (ast rust-ast))
  "Merge function and variable namespaces for Rust."
  (get-declaration-ids :variable ast))

(defgeneric rust::extract-pattern (ast)
  (:method ((ast rust-identifier)) ast)
  (:method ((ast rust-ast))
    (if (slot-exists-p ast 'rust-pattern)
        (rust-pattern ast)
        (call-next-method))))

(defmethod definition-name-ast ((ast rust--declaration-statement))
  (if (typep ast 'declaration-ast)
      (rust-name ast)
      (call-next-method)))

(defmethod definition-name-ast ((ast rust-let-declaration))
  (rust-pattern ast))

(defmethod declaration-type ((ast rust-parameter))
  (rust-type ast))

(defmethod declaration-type ((ast rust-let-declaration))
  (rust-type ast))

(defun rust::pattern-variables (pattern)
  (declare (rust--pattern pattern))
  (ematch pattern
    ((identifier-ast) (list pattern))
    ((or (rust-reference-pattern)
         (rust-tuple-pattern))
     (mappend #'rust::pattern-variables
              (children-of-type pattern 'rust--pattern)))
    ((rust-tuple-struct-pattern)
     (mappend #'rust::pattern-variables (direct-children pattern)))))

(defmethod outer-declarations ((pat rust--pattern))
  (let ((vars (rust::pattern-variables pat)))
    (values vars
            (mapcar (constantly :variable) vars))))

(defmethod parameter-names ((param rust-parameter))
  (outer-declarations (rust-pattern param)))

(defmethod outer-declarations ((decl rust-let-declaration))
  (outer-declarations (rust-pattern decl)))

(defmethod outer-declarations ((decl rust-macro-invocation))
  "Work around rust-macro-invocation being a subclass of rust--pattern."
  nil)

(defmethod inner-declarations ((decl rust-closure-expression))
  (let ((vars
          (mappend #'rust::pattern-variables
                   (mapcar #'rust::extract-pattern
                           (children-of-type (rust-parameters decl)
                                             'rust-parameter)))))
    (values vars
            (mapcar (constantly :variable) vars))))

(defmethod inner-declarations ((fn rust-function-item))
  (flet ((type-params (fn)
           ;; There is no `rust-type-parameter' class. Type parameter
           ;; names are direct children of the `rust-type-parameters'
           ;; instance.
           (when-let (params (rust-type-parameters fn))
             (children params))))
    (let ((params (call-next-method))
          (name (definition-name-ast fn))
          (type-params (type-params fn)))
      (values (list* name (append type-params params))
              (cons :variable
                    (append
                     (mapcar (constantly :type) type-params)
                     (mapcar (constantly :variable) params)))))))

(defmethod outer-declarations ((fn rust-function-item))
  (let ((name (definition-name-ast fn)))
    (values (list name) '(:variable))))

(defmethod outer-declarations ((ast rust-declaration-list))
  (outer-declarations-merge (children ast)))

(defmethod inner-declarations ((ast rust-declaration-list))
  (outer-declarations ast))

(defmethod outer-declarations ((ast rust-impl-item))
  (outer-declarations (rust-body ast)))

(defmethod outer-declarations ((ast rust-foreign-mod-item))
  (outer-declarations-merge (children ast)))

(defmethod inner-declarations ((ast rust-foreign-mod-item))
  (outer-declarations ast))

(defmethod inner-declarations ((ast rust-for-expression))
  (outer-declarations (rust-pattern ast)))

(defmethod outer-declarations ((ast rust-struct-item))
  (values (list (definition-name-ast ast))
          '(:type)))

(defmethod outer-declarations ((ast rust-enum-item))
  (values (list (definition-name-ast ast))
          '(:type)))

(defmethod outer-declarations ((ast rust-union-item))
  (values (list (definition-name-ast ast))
          '(:type)))

(defmethod outer-declarations ((ast rust-type-item))
  (values (list (definition-name-ast ast))
          '(:type)))

(defmethod outer-declarations ((ast rust-const-item))
  (values (list (definition-name-ast ast))
          '(:variable)))

(defmethod outer-declarations ((ast rust-static-item))
  (values (list (definition-name-ast ast))
          '(:variable)))

(defmethod outer-declarations ((ast rust-trait-item))
  (values (list (definition-name-ast ast))
          '(:type)))

(defmethod exception-set :around ((ast rust-function-item))
  (match ast
    ((rust-function-item
      (rust-name (source-text= "main")))
     ;; TODO Main can return Result. Does this count?
     +exception-bottom-type+)
    ((rust-function-item
      (rust-return-type
       (rust-generic-type
        (rust-type (source-text= "Result"))
        (rust-type-arguments
         (rust-type-arguments
          (children children))))))
     (let ((return-type-exception-set
             (or (second children) +exception-top-type+)))
       (exception-set-union
        (list 'or return-type-exception-set)
        (call-next-method))))
    (otherwise (call-next-method))))

(defmethod exception-set ((ast rust-call-expression))
  (match ast
    ((rust* "$_.unwrap()")
     +exception-bottom-type+)
    (otherwise
     (call-next-method))))

(defmethod exception-set ((ast rust-macro-invocation))
  (string-case (source-text (call-function ast))
    ("panic" +exception-bottom-type+)
    ;; Roof error handling.
    ("throw" +exception-top-type+)
    ;; TODO Filter by error type.
    ("try" +exception-bottom-type+)))


;;; Whitespace.

(defclass rustfmt-style (c-style-indentation)
  ())

(defmethod default-whitespace-style ((ast rust-ast))
  (make 'rustfmt-style))

(define-empty-whitespace-methods (:style rustfmt-style)
  rust-ast :|;|
  :|.| rust-ast
  rust-ast :|.|
  rust-ast rust-arguments
  rust-ast rust-type-arguments
  rust-ast :|;|
  rust-ast :|:|
  rust-ast :|::|
  :|:| rust-ast
  :|::| rust-ast
  :< rust-lifetime
  rust-lifetime :>
  :|'| rust-identifier
  :& rust-ast
  rust-& rust-self
  rust-type-identifier rust-type-arguments
  rust-type-identifier rust-type-parameters
  rust-identifier rust-parameters
  rust-ast :!
  :! rust-ast
  :|impl| rust-type-parameters
  :|*| rust-mutable-specifier
  rust-call-expression :?)

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

(defmethod whitespace-between ((s rustfmt-style)
                               (x rust-function-item)
                               (y rust-function-item))
  #.(fmt "~2%"))

(defmethod whitespace-between ((s rustfmt-style)
                               (x rust-attribute-item)
                               (y rust-struct-item))
  #.(fmt "~%"))

(defmethod whitespace-between ((s rustfmt-style)
                               (x rust-mod-item)
                               (y rust-use-declaration))
  #.(fmt "~%"))

(defmethod whitespace-between ((s rustfmt-style)
                               (x statement-ast)
                               (y rust-implicit-return-expression))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent rust-attribute)
                                      (s rustfmt-style)
                                      (x rust-identifier)
                                      (y rust-token-tree))
  "")

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

;;; Override default method.
(defmethod whitespace-between/parent :around ((parent rust-ast)
                                              (s rustfmt-style)
                                              (x rust-expression-statement)
                                              (y rust-empty-statement))
  "")

(defmethod whitespace-between/parent :around ((parent rust-block)
                                              (s rustfmt-style)
                                              (x rust-implicit-return-expression)
                                              (y (eql :|}|)))
  (match (children parent)
    ((list (eql x))
     " ")
    (otherwise
     #.(string #\Newline))))

(defmethod whitespace-between/parent ((parent rust-type-arguments)
                                      (s rustfmt-style)
                                      (x rust-ast)
                                      (y (eql :>)))
  "")

(defmethod whitespace-between/parent ((parent rust-type-parameters)
                                      (s rustfmt-style)
                                      (x rust-ast)
                                      (y (eql :>)))
  "")

(defmethod whitespace-between/parent ((parent rust-type-arguments)
                                      (s rustfmt-style)
                                      (x (eql :<))
                                      (y rust-ast))
  "")

(defmethod whitespace-between/parent ((parent rust-type-parameters)
                                      (s rustfmt-style)
                                      (x (eql :<))
                                      (y rust-ast))
  "")

(defmethod whitespace-between/parent ((parent rust-const-item)
                                      (s rustfmt-style)
                                      (x (eql :|:|))
                                      (y rust-ast))
  "Force whitespace after a colon in a const item."
  " ")

) ; #+:TREE-SITTER-RUST
