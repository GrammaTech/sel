;;;
;;; Methods shared by both tree-sitter c and cpp languages.
;;; This is not a complete language: you should explicitly
;;; use or :require :c and/or :cpp, and this will get indirectly
;;; loaded as a dependency.
;;;

(defpackage :software-evolution-library/software/c-cpp
  (:nicknames :sel/software/c-cpp :sel/sw/c-cpp)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "c")
(create-tree-sitter-language "cpp")
;;;===================================================

#+(or :tree-sitter-c :tree-sitter-cpp)
(progn


;;; Contextualization
(defun get-context-for (ast context-table)
  ;; TODO: remove this.
  ;; NOTE: this is a temporary function until a symbol table is ready.
  (etypecase ast
    ((or c/cpp-identifier c/cpp-type-identifier)
     (gethash (text ast) context-table))))

(defun binary-expression->cast-expression (ast-type ast)
  "Converts BINARY-EXPRESSION into its corresponding cast expression."
  ;; NOTE: unary -> + and -
  ;;       pointer-expression -> * and &
  (let ((identifier (car (direct-children (c/cpp-left ast))))
        (operator (c/cpp-operator ast)))
    (convert
     ast-type
     `((:class . :cast-expression)
       (:type
        (:class . :type-descriptor)
        (:type
         (:class . :type-identifier)
         (:text . ,(text identifier)))
        ,@(preserve-properties identifier))
       (:value
        (:class . ,(if (typep operator '(or c/cpp-+ c/cpp--))
                       :unary-expression
                       :pointer-expression))
        (:argument . ,(c/cpp-right ast))
        (:operator . ,operator))
       ,@(preserve-properties ast)))))

(defmethod contextualize-ast ((software c/cpp)
                              (ast c/cpp-binary-expression)
                              (context hash-table)
                              &key ast-type
                                (parents (get-parent-asts* software ast))
                              &allow-other-keys)
  ;; TODO: this works around some issues with sizeof for the time being.
  ;;       https://github.com/tree-sitter/tree-sitter-c/issues/51
  (unless (typep (car parents) 'c/cpp-sizeof-expression)
    (match ast
      ((c/cpp-binary-expression
        :c/cpp-left
        (c/cpp-parenthesized-expression
         :children (list (and identifier (c/cpp-identifier)))))
       (when (eql (get-context-for identifier context) :type)
         (binary-expression->cast-expression ast-type ast))))))

(defmethod contextualize-ast ((software c/cpp)
                              (ast c/cpp-binary-expression)
                              context
                              &key ast-type
                                (parents (get-parent-asts* software ast))
                              &allow-other-keys)
  ;; TODO: this works around some issues with sizeof for the time being.
  ;;       https://github.com/tree-sitter/tree-sitter-c/issues/51
  (unless (typep (car parents) 'c/cpp-sizeof-expression)
    ;; TODO: this can likely be addressed with #'scopes to some extent, though
    ;;       it won't find any external global variables.
    (match ast
      ((c/cpp-binary-expression
        :c/cpp-left
        (c/cpp-parenthesized-expression
         :children (list (c/cpp-identifier)))
        :c/cpp-operator
        (or (c/cpp-*) (c/cpp--) (c/cpp-+) (c/cpp-&)))
       ;; TODO: improve this. Currently assumes that we will want it to be a
       ;;       cast expression regardless.
       (binary-expression->cast-expression ast-type ast)))))


;;; Symbol Table

(defmethod symbol-table-union ((root c/cpp-ast) table-1 table-2 &key)
  (multi-map-symbol-table-union
   table-1 table-2
   :allow-multiple (multi-declaration-keys root)))

(defmethod symbol-table-union ((root c/cpp) table-1 table-2 &key)
  (symbol-table-union (genome root) table-1 table-2))

(defmethod symbol-table ((node c/cpp-preproc-if) &optional in)
  (propagate-declarations-down node in))

(defmethod symbol-table ((node c/cpp-preproc-ifdef) &optional in)
  (propagate-declarations-down node in))

(defmethod symbol-table ((node c/cpp-preproc-elif) &optional in)
  (propagate-declarations-down node in))

(defmethod symbol-table ((node c/cpp-preproc-else) &optional in)
  (propagate-declarations-down node in))

;;; TODO: move this into tree-sitter.lisp. Probably need something constructed
;;;       at compile-time to have a full list of extra-ast-types.
;;; TODO: actually, just use a mixin instead if this isn't a work around a bug.
(defmethod attr-missing ((fn-name (eql 'symbol-table)) node)
  (labels ((extra-ast-types (language)
             (mapcar (op (format-symbol 'sel/sw/ts "~a-~a" language _))
                     (extra-asts language))))
    (if (member node (append (extra-ast-types :c) (extra-ast-types :cpp))
                :test #'typep)
        (symbol-table node (empty-map))
        (symbol-table (attrs-root *attrs*) (empty-map)))))

(-> group-by-namespace
    ((soft-list-of ast)
     (soft-list-of symbol-table-namespace))
    (values (soft-alist-of symbol-table-namespace
                           (soft-list-of ast))
            &optional))
(defun group-by-namespace (declarations namespaces)
  (let ((table (make-hash-table)))
    (mapc (op (push _ (gethash _ table))) declarations namespaces)
    (hash-table-alist table)))

(-> convert-grouped-namespaces
    ((soft-alist-of symbol-table-namespace
                    (soft-list-of ast))
     &key (:source-text-fun function))
    (values (soft-alist-of symbol-table-namespace fset:map)
            &optional))
(defun convert-grouped-namespaces (grouped-namespaces
                                   &key (source-text-fun
                                         (lambda (ast)
                                           (or (declarator-name ast)
                                               (source-text ast)))))
  "Convert grouped-namespaces into a grouping of
(namespace . source-text/declaration-map)."
  (fbindrec (source-text-fun
             (create-source-text-map
              (lambda (type declarations)
                (reduce (lambda (map ast)
                          (let ((key (source-text-fun ast)))
                            (with map
                                  key
                                  ;; Preserve overloads within a
                                  ;; single AST.
                                  (if (member type (multi-declaration-keys ast))
                                      (cons ast (@ map key))
                                      (list ast)))))
                        declarations
                        :initial-value (empty-map)))))
    (mapcar
     (lambda (grouping &aux (type (car grouping)))
       (cons type
             (create-source-text-map type (cdr grouping))))
     grouped-namespaces)))

(defmethod outer-defs ((node c/cpp-ast))
  (mvlet ((declarations namespaces (outer-declarations node)))
    (convert 'fset:map
             (convert-grouped-namespaces
              (group-by-namespace declarations namespaces)))))

(defmethod inner-defs ((node c/cpp-ast))
  (mvlet ((declarations namespaces (inner-declarations node)))
    (convert 'fset:map
             (convert-grouped-namespaces
              (group-by-namespace declarations namespaces)))))


;;; Generics and Transformations
(defmethod function-name ((ast c/cpp-function-definition))
  (nest (source-text)
        (find-if «or (of-type 'identifier-ast)
                     (of-type 'cpp-qualified-identifier)»)
        (c/cpp-declarator)
        (c/cpp-declarator ast)))

(defmethod function-parameters ((ast c/cpp-function-definition))
  ;; NOTE: the rule currently allows for any declarator. When
  ;;       macros are involved, this can leave a definition without
  ;;       a function declarator.
  (when-let ((function-declarator
              ;; NOTE: pointer declarators can be wrapped around the function
              ;;       declarator.
              (find-if (of-type 'c/cpp-function-declarator)
                       (c/cpp-declarator ast))))
    (function-parameters function-declarator)))

(defmethod function-parameters ((ast c/cpp-function-declarator))
  (direct-children (c/cpp-parameters ast)))

(defmethod call-arguments ((node c/cpp-call-expression))
  (direct-children (c/cpp-arguments node)))

(defmethod function-body ((ast c/cpp-function-definition)) (c-body ast))

(defmethod no-fallthrough ((ast c/cpp-continue-statement)) t)
(defmethod no-fallthrough ((ast c/cpp-break-statement)) t)

(defmethod inner-declarations ((ast c/cpp-function-definition))
  ;; Annotate the defaults as variables.
  (let ((decls (call-next-method)))
    (values decls
            (mapcar (constantly :variable) decls))))

(defmethod inner-declarations ((ast c/cpp-function-declarator))
  (let* ((declarations
          (remove-if-not {typep _ 'c/cpp-parameter-declaration}
                         (convert 'list (c/cpp-parameters ast))))
         (names (mappend #'parameter-names declarations)))
    (values names
            (mapcar (constantly :variable) names))))

(defmethod inner-declarations ((ast c/cpp-for-statement))
  (when-let (init (c/cpp-initializer ast))
    (outer-declarations init)))

(defmethod outer-declarations ((ast c/cpp-declaration))
  (labels ((get-declarations (d)
             (match d
               ((type (or c/cpp-identifier cpp-qualified-identifier))
                (values (list d) '(:variable)))
               ((type (or c/cpp-array-declarator c/cpp-pointer-declarator
                          c/cpp-function-declarator))
                (outer-declarations d))
               ((c/cpp-init-declarator
                 (c/cpp-declarator
                  (cpp-reference-declarator
                   (direct-children (list r)))))
                (values (list r) '(:variable)))
               ((c/cpp-init-declarator
                 (c/cpp-declarator
                  (c/cpp-pointer-declarator
                   (c/cpp-declarator d))))
                (values (list d) '(:variable)))
               ((c/cpp-init-declarator
                 (c/cpp-declarator
                  (c/cpp-array-declarator
                   (c/cpp-declarator d))))
                (values (list d) '(:variable)))
               ;; Special handling for uninitialized variables.
               (otherwise
                (values (list (c/cpp-declarator d)) '(:variable))))))
    (iter
      (for d in (c/cpp-declarator ast))
      (mvlet ((declarations namespaces (get-declarations d)))
        (appending declarations into declarations-accumulator)
        (appending namespaces into namespaces-accumulator))
      (finally (return (values declarations-accumulator
                               namespaces-accumulator))))))

(defun outer-declarations-merge (merged ast)
  (mvlet ((declarations namespaces (outer-declarations ast)))
    (list (append declarations (car merged))
          (append namespaces (cadr merged)))))

(defmethod inner-declarations ((ast c/cpp-compound-statement))
  (let ((declarations-values-list
          (reduce #'outer-declarations-merge (children ast)
                  :initial-value nil)))
    (values (car declarations-values-list)
            (cadr declarations-values-list))))

(defmethod inner-declarations ((ast cpp-declaration-list))
  (let ((declarations-values-list
          (reduce #'outer-declarations-merge (children ast)
                  :initial-value nil)))
    (values (car declarations-values-list)
            (cadr declarations-values-list))))

(defun get-nested-declaration (ast)
  "Get the declaration nested in AST. This is useful for array and
pointer declarations which are nested on themselves."
  (let ((declarator (c/cpp-declarator ast)))
    (if (typep declarator 'c/cpp-identifier)
        (list declarator)
        (outer-declarations declarator))))

(defmethod outer-declarations ((ast c/cpp-array-declarator))
  (values (get-nested-declaration ast)
          '(:variable)))

(defmethod outer-declarations ((ast c/cpp-pointer-declarator))
  (values (get-nested-declaration ast)
          '(:variable)))

(defmethod outer-declarations ((ast c/cpp-classoid-specifier))
  (if-let (name (c/cpp-name ast))
    (values (list name) '(:tag))
    (values nil nil)))

(defmethod inner-declarations ((ast c/cpp-classoid-specifier))
  ;; Make the type visible inside the type.
  (mvlet* ((table (field-table ast))
           (variables (@ table :variable))
           (members
            (when variables
              (nreverse
               (sort (reduce #'append (range variables))
                     (op (path-later-p ast _ _))))))
           (outer-decls outer-decl-types
            (outer-declarations ast)))
    (values (append outer-decls members)
            (append outer-decl-types
                    (mapcar (constantly :variable) members)))))

(defmethod outer-declarations ((ast c/cpp-enum-specifier))
  (match ast
    ((c/cpp-enum-specifier
      (c/cpp-name name)
      (c/cpp-body
       (c/cpp-enumerator-list
        (direct-children enumerators))))
     (let* ((enumerator-names (mapcar #'c/cpp-name enumerators))
            (length (length enumerator-names)))
       (if name
           (values (cons name enumerator-names)
                   (cons :tag #1=(repeat-sequence '(:variable) length)))
           (values enumerator-names
                   #1#))))))

(defmethod resolve-declaration-type ((decl c/cpp-enumerator) ast
                                     &aux (root (attrs-root*)))
  (let ((enum (find-enclosing 'c/cpp-enum-specifier root decl)))
    ;; If the enum is typedef'd, we probably want the typedef name.
    (or (when-let (typedef (find-enclosing 'c/cpp-type-definition root enum))
          (definition-name-ast typedef))
        (definition-name-ast enum))))

(defmethod outer-declarations ((ast c/cpp-function-declarator))
  ;; TODO: in regards to function overloading, may need to add a keyword argument
  ;;       to the multi map union to support it.
  (values (list (c/cpp-declarator ast))
          '(:function)))

(defmethod outer-declarations ((ast c/cpp-function-definition))
  (when-let ((function-declarator
              ;; NOTE: the rule currently allows for any declarator. When
              ;;       macros are involved, this can leave a definition without
              ;;       a function declarator.
              (find-if (of-type 'c/cpp-function-declarator)
                       (c/cpp-declarator ast))))
    (values (outer-declarations function-declarator)
            '(:function))))

(defmethod outer-declarations ((ast c/cpp-macro-forward-declaration))
  (values (list (car (direct-children ast)))
          '(:macro)))

(defmethod outer-declarations ((ast c/cpp-type-forward-declaration))
  (values (list (car (direct-children ast)))
          '(:type)))

(defmethod outer-declarations ((ast c/cpp-type-definition))
  (mvlet ((type
           (assure ast
             (find-if (of-type 'c/cpp-type-identifier)
                      (car (c/cpp-declarator ast)))))
          (type-decls type-namespaces
           (outer-declarations (c/cpp-type ast))))
    (values (cons type type-decls)
            (cons :type type-namespaces))))

(defmethod enclosing-definition ((sw c/cpp) (ast t))
  (find-enclosing '(or definition-ast c/cpp-primitive-type)
                  sw ast))

(defmethod definition-name-ast ((ast c/cpp-function-definition))
  (declarator-name-ast (c/cpp-declarator ast)))
(defmethod definition-name-ast ((ast c/cpp-struct-specifier))
  (c/cpp-name ast))
(defmethod definition-name-ast ((ast c/cpp-union-specifier))
  (c/cpp-name ast))
(defmethod definition-name-ast ((ast c/cpp-type-definition))
  (declarator-name-ast (c/cpp-declarator ast)))
(defmethod definition-name-ast ((ast c/cpp-preproc-def))
  (c/cpp-name ast))
(defmethod definition-name-ast ((ast c/cpp-preproc-function-def))
  (c/cpp-name ast))
(defmethod definition-name-ast ((ast c/cpp-enum-specifier))
  (c/cpp-name ast))

(defmethod declarator-name-ast ((ast c/cpp-identifier))
  ast)
(defmethod declarator-name-ast ((ast c/cpp-field-identifier))
  ast)
(defmethod declarator-name-ast ((ast c/cpp-type-identifier))
  ast)
(defmethod declarator-name-ast ((ast c/cpp-init-declarator))
  (declarator-name-ast (c/cpp-declarator ast)))
(defmethod declarator-name-ast ((ast c/cpp-parenthesized-declarator))
  (car (children ast)))
(defmethod declarator-name-ast ((ast c/cpp-pointer-declarator))
  (declarator-name-ast (c/cpp-declarator ast)))
(defmethod declarator-name-ast ((ast c/cpp-array-declarator))
  (declarator-name-ast (c/cpp-declarator ast)))
(defmethod declarator-name-ast ((ast c/cpp-function-declarator))
  (declarator-name-ast (c/cpp-declarator ast)))
(defmethod declarator-name-ast ((ast c/cpp-parameter-declaration))
  (declarator-name-ast (c/cpp-declarator ast)))

(defmethod declaration-type ((ast c/cpp-function-definition))
  (c/cpp-type ast))
(defmethod declaration-type ((ast c/cpp-field-declaration))
  (c/cpp-type ast))
(defmethod declaration-type ((ast c/cpp-parameter-declaration))
  (c/cpp-type ast))
(defmethod declaration-type ((ast c/cpp-declaration))
  (c/cpp-type ast))

(defmethod field-name-asts ((ast c/cpp-field-declaration))
  (collect-if (of-type 'c/cpp-field-identifier) ast))
(defmethod field-name-asts ((ast c/cpp-enumerator))
  (list (c/cpp-name ast)))
(defmethod field-name-asts ((ast c/cpp-function-definition))
  (list (definition-name-ast ast)))

(defmethod get-declaration-ids :around (type (ast c/cpp-pointer-expression))
  (get-declaration-ids type (c/cpp-argument ast)))

(defmethod get-declaration-ids :around (type (ast c/cpp-type-descriptor))
  (get-declaration-ids type (c/cpp-type ast)))

(defmethod get-initialization-ast ((ast c/cpp-pointer-expression))
  (get-initialization-ast (c/cpp-argument ast)))

(defmethod get-unbound-vals ((obj c/cpp) (ast ast) &key)
  (get-unbound-vals (genome obj) ast))
(defmethod get-unbound-vals ((obj c) (ast ast) &key)
  (get-unbound-vals (genome obj) ast))
(defmethod get-unbound-vals ((obj cpp) (ast ast) &key)
  (get-unbound-vals (genome obj) ast))

;;; TODO This doesn't apply just to C/C++, but to any language that
;;; supports declarations.
(defmethod get-unbound-vals ((root c/cpp-ast) (ast ast) &key)
  (filter (lambda (id)
            (let ((decl (get-declaration-ast :variable id)))
              (or (no decl)
                  (not (ancestor-of-p root decl ast)))))
          (keep 'variable-declaration-ast
                (identifiers ast)
                :key #'relevant-declaration-type)))

(defmethod find-enclosing-declaration :around (type
                                               root
                                               (id c/cpp-field-identifier))
  (find-enclosing 'declaration-ast root id))

(defmethod find-enclosing-declaration
    ((type (eql 'function-declaration-ast))
     root
     (id c/cpp-identifier))
  "Handling finding the enclosing function declaration for a prototype."
  (or (call-next-method)
      (when-let ((declarator (find-enclosing 'c/cpp-function-declarator root id)))
        (find-enclosing 'c/cpp-declaration root declarator))))

(defmethod deref-type ((ast c/cpp-type-descriptor))
  (match ast
    ((c/cpp-type-descriptor
      (c/cpp-declarator
       (or (c/cpp-abstract-pointer-declarator)
           (c/cpp-abstract-array-declarator)))
      (c/cpp-type type))
     type)
    (otherwise (call-next-method))))

(defmethod infer-type ((ast c/cpp-field-expression)
                       &aux (obj (attrs-root*)))
  (flet ((function-position? ()
           (when-let ((call (find-enclosing 'call-ast obj ast)))
             (eql (call-function call) ast))))
    (if (function-position?)
        (when-let (fn (get-declaration-ast :function ast))
          (resolve-declaration-type fn ast))
        (when-let* ((var (get-declaration-ast :variable ast)))
          (resolve-declaration-type var ast)))))

(defmethod infer-type ((ast c/cpp-pointer-expression))
  "Get the type for a pointer dereference."
  (let ((type (infer-type (c/cpp-argument ast)))) ; (call-next-method)
    (if (source-text= "*" (c/cpp-operator ast))
        (deref-type type)
        type)))

(defmethod infer-type ((ast c/cpp-subscript-expression))
  (deref-type (call-next-method)))

(defmethod infer-type ((ast c/cpp-return-statement))
  (infer-type (only-elt (children ast))))

(defmethod infer-type ((ast c/cpp-parenthesized-expression))
  (infer-type (only-elt (children ast))))

(-> add-field-as (fset:map symbol-table-namespace ast) fset:map)
(defun add-field-as (map ns id)
  "Add ID to MAP in NS, a namespace such as `:type' or `:variable'."
  (let ((ns-map (or (lookup map ns) (empty-map))))
    (values
     (with map ns
           (with ns-map
                 (source-text id)
                 ;; Overloads!
                 (cons id (@ ns-map (source-text id))))))))

(defgeneric field-adjoin (field map)
  (:documentation "Adjoin FIELD to MAP according to the type of FIELD.")
  (:method ((field t) map)
    map)
  (:method ((field function-declaration-ast) map)
    (add-field-as map :function (definition-name-ast field)))
  (:method ((field c/cpp-field-declaration) map)
    (reduce (flip #'field-adjoin)
            (ensure-list (c/cpp-declarator field))
            :initial-value map))
  (:method ((field c/cpp-function-declarator) map)
    (add-field-as map :function (c/cpp-declarator field)))
  (:method ((field c/cpp-field-identifier) map)
    (add-field-as map :variable field))
  (:method ((field c/cpp-type-definition) map)
    (add-field-as map :type (only-elt (c/cpp-declarator field))))
  (:method ((field c/cpp-pointer-declarator) map)
    (reduce (flip #'field-adjoin)
            (ensure-list (c/cpp-declarator field))
            :initial-value map))
  (:method ((field c/cpp-array-declarator) map)
    (reduce (flip #'field-adjoin)
            (ensure-list (c/cpp-declarator field))
            :initial-value map)))

(def-attr-fun field-table ()
  "Build an FSet map from field names to identifiers."
  (:method ((ast t)) (empty-map))
  (:method ((ast c/cpp-struct-specifier))
    (ematch ast
      ((c/cpp-struct-specifier
        (c/cpp-body
         (and (type c/cpp-field-declaration-list)
              (access #'direct-children fields))))
       (assure fset:map
         (reduce (flip #'field-adjoin)
                 fields
                 :initial-value (empty-map))))
      ((c/cpp-struct-specifier
        (c/cpp-body nil))
       (empty-map))))
  (:method ((ast c/cpp-union-specifier))
    (ematch ast
      ((c/cpp-union-specifier
        (c/cpp-body
         (and (type c/cpp-field-declaration-list)
              (access #'direct-children fields))))
       (assure fset:map
         (reduce (flip #'field-adjoin)
                 fields
                 :initial-value (empty-map))))
      ((c/cpp-union-specifier
        (c/cpp-body nil))
       (empty-map))))
  (:method ((ast c/cpp-type-definition))
    (match ast
      ((c/cpp-type-definition
        (c/cpp-type
         (and struct (c/cpp-classoid-specifier))))
       (field-table struct)))))

(defun lookup-in-field-table (class ns key)
  (@ (or (@ (field-table class) ns) (empty-map))
     (source-text key)))

(defun get-field-class (field)
  "Find the declaration of the type of the argument of FIELD."
  (ematch field
    ((c/cpp-field-expression
      ;; TODO Just ->?
      (c/cpp-argument arg))
     ;; Find the type of the argument.
     (when-let* ((type (infer-type arg)))
       ;; Get the declaration of the type of the argument.
       (let ((new-type
              (if (typep (c/cpp-operator field) 'c/cpp-->)
                  (deref-type type)
                  type)))
         ;; (when (not (eql type new-type))
         ;;   (setf (attr-proxy new-type) type))
         (get-declaration-ast
          (if (typep new-type 'c-tag-specifier) ;A mixin class.
              :tag :type)
          new-type))))))

(defmethod get-declaration-ids :around (type (ast c/cpp-field-expression))
  (when-let (class (get-field-class ast))
    (lookup-in-field-table class type (c/cpp-field ast))))

(defmethod get-initialization-ast ((ast cpp-ast) &aux (obj (attrs-root*)))
  "Find the assignment for an unitialized variable."
  (or (call-next-method)
      (when-let* ((id (get-declaration-id :variable ast))
                  (decl
                   (find-enclosing 'variable-declaration-ast obj id)))
        (let ((id-text (source-text id)))
          (find-following
           (lambda (ast)
             (when-let (assignment
                        (find-if (of-type 'c/cpp-assignment-expression)
                                 ast))
               (and (typep (lhs assignment) 'identifier-ast)
                    (equal (source-text (lhs assignment)) id-text)
                    (return-from get-initialization-ast
                      assignment))))
           obj
           decl)))))

(defmethod relevant-declaration-type ((ast c/cpp-primitive-type))
  nil)

(defmethod relevant-declaration-type ((ast c/cpp-field-expression))
  'variable-declaration-ast)

(defmethod relevant-declaration-type ((ast c/cpp-field-declaration))
  nil)

(defun infer-type-as-c/cpp-expression (obj ast)
  "Fall back to inferring the expression type from the surrounding declaration.

That is, if the type of an expression cannot be extracted, then if it
occurs as the RHS of a init declarator in a declaration, take the type
of the declaration.

E.g. given

    int x = y

Then if we cannot infer the type of y per se we infer its type to be int.

Other strategies may also be used. E.g. the type of an expression that
appears as a return statement is assumed to be the type of the function."
  (let ((parents (get-parent-asts* obj ast)))
    (or (match (take 2 parents)
          ((list (type c/cpp-init-declarator)
                 (and decl (type c/cpp-declaration)))
           (resolve-declaration-type decl ast)))
        (and-let* (((typep (first parents) 'c/cpp-return-statement))
                   ((equal (list ast) (children (first parents))))
                   (fn (find-if (of-type 'function-declaration-ast)
                                parents)))
          (declaration-type fn)))))

(defmethod infer-expression-type :around ((ast expression-ast))
  (or (call-next-method)
      (infer-type-as-c/cpp-expression (attrs-root*) ast)))

(defmethod expression-type ((ast c/cpp-declaration))
  (cpp-type ast))

(defmethod resolve-declaration-type ((decl c/cpp-function-declarator)
                                     (ast t)
                                     &aux (obj (attrs-root*)))
  (if-let (fn (find-enclosing 'c/cpp-function-definition obj decl))
    (resolve-declaration-type fn ast)
    (call-next-method)))

(defmethod resolve-declaration-type ((decl c/cpp-ast)
                                     (ast c/cpp-ast)
                                     &aux (obj (attrs-root*)))
  (or
   ;; Look for a surrounding variable declaration.
   (when-let* ((declaration
                (find-if (of-type '(and variable-declaration-ast
                                    (not c/cpp-init-declarator)
                                    (not c/cpp-assignment-expression)))
                         ;; Exclusive of AST.
                         (get-parent-asts* obj decl))))
     (resolve-declaration-type declaration decl))
   ;; If the declaration is for a function, return that
   ;; function's type.
   (and-let* ((function (find-enclosing 'function-ast obj decl))
              ((eql decl (c/cpp-declarator function))))
     (resolve-declaration-type decl function))
   (call-next-method)))

(defgeneric wrap-type-descriptor (declarator type)
  (:documentation "If DECLARATOR declares an indirect type, such as a
  pointer, array, or reference, then wrap TYPE in a type descriptor
  with an appropriate abstract descriptor.")
  (:method (declarator type)
    type)
  (:method ((d c/cpp-init-declarator) type)
    (wrap-type-descriptor (c/cpp-declarator d) type)))

(defmethod resolve-declaration-type ((decl c/cpp-ast) (ast c/cpp-ast)
                                     &aux (root (attrs-root*)))
  "Make sure that a declaration for an indirect type (pointer, array,
  reference) returns an appropriate type descriptor and not just a
  primitive type or type identifier."
  (flet ((relevant-declarator (declarators ast decl)
           (let* ((type
                   (if (typep decl 'function-ast) :function :variable))
                  (id (or (get-declaration-id type ast)
                          decl)))
             (find-if (op (ancestor-of-p root id _))
                      declarators))))
    (match decl
      (nil (call-next-method))
      ((declaration-ast)
       (multiple-value-bind (type declarators)
           (ignore-some-conditions (no-applicable-method-error)
             (values (c/cpp-type decl)
                     (c/cpp-declarator decl)))
         (cond ((not (and type declarators))
                (fail))
               ((when-let* ((declarator (relevant-declarator declarators ast decl))
                            (final-type (wrap-type-descriptor declarator type)))
                  (unless (eql final-type type)
                    (setf (attr-proxy final-type) type)
                    final-type)))
               (t (fail)))))
      (otherwise
       (call-next-method)))))

(defun transform-c-declaration-specifiers
    (parse-tree &aux (position-slot :pre-specifiers))
  "Transform PARSE-TREE such that any specifiers are placed in relevants slots."
  (append
   (butlast parse-tree)
   (list
    (mapcar
     (lambda (child-tree &aux (car (car child-tree)))
       (cond
         ((and (consp car)
               (eql (car car) :type))
          (setf position-slot :post-specifiers)
          child-tree)
         ((member car '(:storage-class-specifier :type-qualifier
                        :attribute-specifier :attribute-declaration
                        :ms-declspec-modifier :virtual-function-specifier
                        :explicit-function-specifier))
          (cons (list position-slot (car child-tree))
                (cdr child-tree)))
         (t child-tree)))
     (lastcar parse-tree)))))

(defun transform-c-type-qualifiers
    (parse-tree &aux (position-slot :pre-type-qualifiers))
  "Transform PARSE-TREE such that any specifiers are placed in relevants slots."
  (append
   (butlast parse-tree)
   (list
    (mapcar
     (lambda (child-tree &aux (car (car child-tree)))
       (cond
         ((and (consp car)
               (eql (car car) :type))
          (setf position-slot :post-type-qualifiers)
          child-tree)
         ((member car '(:type-qualifier))
          (cons (list position-slot (car child-tree))
                (cdr child-tree)))
         (t child-tree)))
     (lastcar parse-tree)))))

(defun transform-case-statement (parse-tree)
  "Transform the case statement's PARSE-TREE to allow the statements
following the case to be stored in the 'statements' AST slot."
  (with-modify-parse-tree (parse-tree)
    ((:case :default :\: :comment :text-fragment) (ignore-types))
    (t (label-as :statements))))

(defun transform-labeled-statement (parse-tree)
  "Transform the labeled statement's PARSE-TREE to allow the last child to be
stored in the 'statement' AST slot."
  (label-last-child :statement parse-tree))

(defun transform-for-statement (parse-tree)
  "Transform the for statement's PARSE-TREE to allow the last child to be
stored in the 'body' AST slot."
  (label-last-child :body parse-tree))

(defun transform-empty-statements (parse-tree)
  "Transform the empty statements in PARSE-TREE such that they appear as empty
statements. This is to get around unnamed semicolons being inserted at the in
ASTs that can have multiple statements in their body."
  (with-modify-parse-tree (parse-tree)
    ((:|;|) (wrap-with :empty-statement))))

(defun label-last-child (label parse-tree)
  "Transform PARSE-TREE such that the last child has the given label, allowing
it to be placed in the corresponding AST slot."
  (destructuring-bind (node-type range children) parse-tree
    (list node-type range
          (append (butlast children)
                  (list (label-parse-tree label (lastcar children)))))))

(defun label-parse-tree (label parse-tree)
  "Transform PARSE-TREE to add the given label to the tree's node type, allowing
it to be placed in the corresponding AST slot."
  (destructuring-bind (node-type range children) parse-tree
    `((,label ,node-type) ,range ,children)))

(defgeneric child-variable-use-p (obj child parent &key &allow-other-keys)
  (:documentation "Return T if CHILD occurs in OBJ as a variable. This is
determined by looking at PARENT.")
  (:method (obj child parent &key &allow-other-keys) nil))

;;; TODO: variable-use-p isn't fleshed out completely for C/C++.
(defmethod variable-use-p ((obj c/cpp) identifier &key &allow-other-keys)
  nil)

(defmethod variable-use-p ((obj c/cpp) (identifier c/cpp-identifier)
                           &key &allow-other-keys)
  (child-variable-use-p obj identifier (get-parent-ast obj identifier)))

(defmethod child-variable-use-p
    ((obj c/cpp) (child identifier-ast) (parent c/cpp-array-declarator)
     &key &allow-other-keys)
  (eq (c/cpp-size parent) child))

(defmethod child-variable-use-p
    ((obj c/cpp) (child identifier-ast) (parent c/cpp-return-statement)
     &key &allow-other-keys)
  (eq (car (direct-children parent)) child))

(defmacro define-identical-child-variable-use-p
    ((&rest types) &body body)
  `(progn
     ,@(iter
         (for type in types)
         (collect
             `(defmethod child-variable-use-p
                  ((obj c/cpp) (child identifier-ast) (parent ,type)
                   &key &allow-other-keys)
                ,@body)))))

;;; TODO: have a common mixin for these instead? What would it be named?
(define-identical-child-variable-use-p
    (c/cpp-init-declarator c/cpp-initializer-pair)
  (eq (c/cpp-value parent) child))

;;; TODO: have a common mixin for these instead? What would it be named?
(define-identical-child-variable-use-p
    (c/cpp-parenthesized-expression c/cpp-binary-expression c/cpp-argument-list
     c/cpp-update-expression c/cpp-pointer-expression c/cpp-subscript-expression
     c/cpp-unary-expression c/cpp-expression-statement
     c/cpp-assignment-expression)
  t)

(defgeneric initializer-aliasee (sw lhs rhs)
  (:documentation "Resolve the aliasee of an initializer.
Should return `:failure' in the base case.")
  (:method (sw lhs rhs) :failure)
  ;; Methods for pointers apply to C and C++; methods for references
  ;; are defined in cpp.lisp.
  (:method ((sw t)
            (lhs c/cpp-pointer-declarator)
            (rhs c/cpp-pointer-expression))
    (if (typep (c/cpp-operator rhs) 'cpp-&)
        (with-attr-table sw
          (aliasee (c/cpp-argument rhs)))
        (call-next-method)))
  (:method ((sw t)
            (lhs c/cpp-pointer-declarator)
            (rhs identifier-ast))
    ;; Assigning a pointer variable to a pointer variable.
    (with-attr-table sw
      (let ((aliasee (aliasee rhs)))
        (if (not (eql aliasee (get-declaration-id :variable rhs)))
            aliasee
            (call-next-method))))))

(defmethod aliasee ((ast cpp-ast) &aux (sw (attrs-root*)))
  (match ast
    ((and id (type identifier-ast))
     (ematch (get-initialization-ast id)
       ((c/cpp-init-declarator (lhs lhs) (rhs rhs))
        (let ((result (initializer-aliasee sw lhs rhs)))
          (if (eql result :failure)
              (get-declaration-id :variable id)
              result)))
       ((c/cpp-assignment-expression (rhs rhs))
        (aliasee rhs))
       (otherwise nil)))
    (t (call-next-method))))

(defmethod alias-set ((plain-var c/cpp-ast))
  (let ((sw (attrs-root*)))
    (with-attr-table sw
      (when-let (id (get-declaration-id :variable plain-var))
        (iter (for ast in-tree (genome sw))
              (when (and (typep ast 'identifier-ast)
                         (eql (aliasee ast) id))
                (set-collect (get-declaration-id :variable ast) into set))
              (finally (return (convert 'list (less set plain-var)))))))))

(defmethod parameter-names ((ast c/cpp-parameter-declaration))
  (let ((ids (identifiers ast)))
    (if-let (type (c/cpp-type ast))
      ;; We don't want identifiers from type declarations.
      (remove-if (op (shares-path-of-p ast _ type)) ids)
      ids)))

(defmethod parameter-names ((ast c/cpp-pointer-declarator))
  (parameter-names (c/cpp-declarator ast)))

(defmethod parameter-names ((ast c/cpp-identifier))
  (list ast))

(defmethod collect-arg-uses (sw (target c/cpp-ast)
                             &optional alias)
  (unless (typep target 'identifier-ast)
    (return-from collect-arg-uses
      (call-next-method)))
  (labels ((get-decl (var)
             (get-declaration-id :variable
                                 (or (and alias (aliasee var))
                                     var)))
           (occurs-as-object? (ast target)
             (match ast
               ((call-ast
                 (call-function
                  (cpp-field-expression
                   (cpp-argument arg))))
                (eql (get-decl arg) target))))
           (occurs-as-arg? (ast target)
             (match ast
               ((call-ast (call-arguments (and args (type list))))
                (member target
                        (filter (of-type 'identifier-ast) args)
                        :key (op (get-decl _)))))))
    (let ((target (get-decl target)))
      (iter (for ast in-tree (genome sw))
            ;; The outer loop will recurse, so we don't
            ;; need to recurse here.
            (when (or (occurs-as-object? ast target)
                      (occurs-as-arg? ast target))
              (collect ast))))))


;;;; Whitespace
(defmethod whitespace-between/parent ((parent c/cpp-do-statement)
                                      (style c-style-indentation)
                                      (ast1 ast)
                                      ast2)
  (if (or (typep ast1 'compound-ast)
          (not (eq ast1 (body parent))))
      (call-next-method)
      #.(fmt "~%")))

(defmethod whitespace-between/parent (parent
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 (eql ':|#endif|)))
  #.(fmt "~%"))

(defmethod whitespace-between/parent (parent
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 c/cpp-preproc-elif))
  #.(fmt "~%"))

(defmethod whitespace-between/parent (parent
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 c/cpp-preproc-else))
  #.(fmt "~%"))

(defmethod whitespace-between/parent (parent
                                      (style c-style-indentation)
                                      (ast1 (eql ':|#else|))
                                      ast2)
  #.(fmt "~%"))

;;; TODO: this is a workaround for an upstream bug that causes
;;;       newline terminals to have a range which spans multiple
;;;       newlines. Remove this and have string be considered
;;;       a terminal once it is fixed. To check if it has been
;;;       fixed, try #'cl-tree-sitter:parse-string with a
;;;       c-preproc-include and check if the newline spans
;;;       multiple newlines.
(defmethod whitespace-between/parent ((parent root-ast)
                                      (style c-style-indentation)
                                      (ast1 c/cpp-preproc-include)
                                      ast2)
  "")

;;; TODO: remove this when whitespace is fixed in tree-sitter.
(defmethod whitespace-between/parent ((parent root-ast)
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 c/cpp-preproc-include))
  "")


;;; System headers
(-> system-header-names (c/cpp) (values list &optional))
(defun system-header-names (sw)
  "Return a list of the system headers in SW."
  (iter (for ast in-tree (genome sw))
        (match ast
          ((c/cpp-preproc-include (c/cpp-path path))
           (match (source-text path)
             ((ppcre "<(.*)>" s)
              (collect s)))))))

(defun parse-header-synopsis (path-string &key (class-ast 'c-ast))
  "Parse the system header synopsis at PATH-STRING into an AST."
  (labels ((get-synopsis-string (path-string)
             "Get the synopsis string from the header represented
              by PATH-STRING."
             (handler-bind
                 ((#+sbcl sb-ext:file-does-not-exist #-sbcl file-error
                   (lambda (condition)
                     (declare (ignorable condition))
                     (return-from get-synopsis-string))))
               (extract-header-synopsis path-string)))
           (get-section (lines)
             "Returns as values the lines in the section that starts at the
              first items in LINES and the lines that are remaining."
             (assert (scan ".*:$" (first lines)))
             (iter
               (iter:with section)
               (iter:with lines = (drop 2 lines))
               (while (not (equal (car lines) "")))
               ;; NOTE: add semicolon to avoid parsing errors.
               ;;       The newline is to avoid putting the semicolon in
               ;;       comments.
               (push (format nil "~a~%;" (pop lines)) section)
               (finally (return (values section lines)))))
           (markup-section (comment lines)
             "Return as values a section retrieved from LINES that inserts
              comments"
             (mvlet ((section remaining-lines (get-section lines)))
               ;; NOTE: section is reversed here. The comments will be in the
               ;;       before-asts slot and can be used to further markup
               ;;       the AST.
               (values (append1 (intersperse comment section) comment)
                       remaining-lines)))
           (markup-synopsis (synopsis-string)
             "Mark up SYNOPSIS-STRING such that it can be parsed and transformed
              into something that can be referenced as an AST."
             (iter
               (iter:with markup-lines)
               (iter:with lines = (lines synopsis-string))
               (while lines)
               (for section-type =
                    (first (nth-value 1 (scan-to-strings "(Macros|Types):$"
                                                         (car lines)))))
               (if section-type
                   (mvlet* ((section-comment
                             (string-ecase section-type
                               ("Macros" "/*macro*/")
                               ("Types" "/*type*/")))
                            (section-lines
                             remaining-lines
                             (markup-section section-comment lines)))
                     (push section-lines markup-lines)
                     (setf lines remaining-lines))
                   (push (pop lines) markup-lines))
               (finally
                (return
                  (apply #'string+
                          (intersperse #.(format nil "~%")
                                       (reverse (flatten markup-lines))))))))
           (transform-into-forward-declaration (ast comment declaration-type)
             "Transforms AST into a forward declaration of type DECLARATION-TYPE
              if it has a preceding comment which is identical to COMMENT."
             (match ast
               ((c/cpp-expression-statement
                 (before-text before-text)
                 (after-text after-text)
                 (direct-children (list symbol-ast))
                 (before-asts
                  (list (c/cpp-comment
                         (text (equal comment))))))
                (convert class-ast `((:class . ,declaration-type)
                                     (:children ,symbol-ast)
                                     (:before-text . ,before-text)
                                     (:after-text . ,after-text))))
               ((c/cpp-primitive-type
                 (before-asts
                  (list (c/cpp-comment
                         (text (guard text (equal comment text))))))
                 (before-text before-text)
                 (after-text after-text))
                (convert class-ast `((:class . ,declaration-type)
                                     (:children
                                      ((:class . identifier)
                                       (:text . ,(text ast))))
                                     (:before-text . ,before-text)
                                     (:after-text . ,after-text))))))
           (transform-macro-ast (ast)
             (transform-into-forward-declaration
              ast "/*macro*/" 'macro-forward-declaration))
           (transform-type-ast (ast)
             (transform-into-forward-declaration
              ast "/*type*/" 'type-forward-declaration))
           (patch-system-header-ast (ast)
             "Patch all ASTs with a preceding comment to be forward
              declarations."
             ;; NOTE: primitive types are followed by an empty-statement. These
             ;;       need to be removed since the primitive types are being
             ;;       transformed and the empty-statements no long match
             ;;       correctly
             (remove-if (of-type 'c/cpp-empty-statement)
                        (mapcar
                         (op (or (transform-macro-ast _1)
                                 (transform-type-ast _1)
                                 _1))
                         ast))))
    (when-let* ((synopsis-string (get-synopsis-string path-string))
                (markup-synopsis (markup-synopsis synopsis-string))
                (root-ast (convert class-ast markup-synopsis)))
      (patch-system-header-ast root-ast))))


;;; Type Canonicalization
(defgeneric canonicalize-declarator (declarator)
  (:documentation "Get a canonicalized form of DECLARATOR. This should be a
list which is modeled after the representation presented at the following link:
https://blog.robertelder.org/building-a-c-compiler-type-system-the-formidable-declarator/
The canonical form consists of three fundamental  parts: function, pointer, and
array. A fourth part is also used here--parens--but isn't of use aside from
reproducing a, more or less, exact source text representation.
The list will contain items of the form `(:key values)' where `:key' can be any
of the four parts and `values' is the relevant information attached to the key.")
  (:method (ast) nil))

(defmethod canonicalize-declarator ((declarator c/cpp-array-declarator))
  (append (canonicalize-declarator (c/cpp-declarator declarator))
          `((:array ,(c/cpp-size declarator)))))

(defmethod canonicalize-declarator ((declarator c/cpp-abstract-array-declarator))
  (append (canonicalize-declarator (c/cpp-declarator declarator))
          `((:array ,(c/cpp-size declarator)))))

(defmethod canonicalize-declarator ((declarator c/cpp-parenthesized-declarator))
  (append (canonicalize-declarator (car (direct-children declarator)))
          `((:paren))))

(defmethod canonicalize-declarator ((declarator c/cpp-pointer-declarator))
  (append (canonicalize-declarator (c/cpp-declarator declarator))
          ;; NOTE: direct children should contain the type qualifiers.
          `((:pointer ,@(direct-children declarator)))))

(defmethod canonicalize-declarator
    ((declarator c/cpp-abstract-pointer-declarator))
  (append (canonicalize-declarator (c/cpp-declarator declarator))
          ;; NOTE: direct children should contain the type qualifiers.
          `((:pointer ,@(direct-children declarator)))))

(defmethod canonicalize-declarator ((declarator c/cpp-function-declarator))
  (append (canonicalize-declarator (c/cpp-declarator declarator))
          `((:function ,(c/cpp-parameters declarator)))))

(defmethod canonicalize-declarator
    ((declarator c/cpp-abstract-function-declarator))
  (append (canonicalize-declarator (c/cpp-declarator declarator))
          `((:function ,(c/cpp-parameters declarator)))))

;;; The canonical type representation for c/cpp is based on the following link:
;;; https://blog.robertelder.org/building-a-c-compiler-type-system-a-canonical-type-representation/
;;;
;;; The structure contains three parts: a specifier list, a canonical declarator,
;;; and a bitfield part.
;;; The specifier list contains all the information that isn't part of the
;;; declarator. This includes the type qualifiers and the type.
;;; The bitfield part contains the bitfield for field declarations.
;;; The canonical declarator form is described above.
(defun get-specifier-list (ast-type declaration-ast &aux implicit-int-p)
  (labels ((unwind-c/cpp-type (ast)
             "Unwind certain ASTs such that there is a flat specifier list."
             (typecase ast
               (c/cpp-sized-type-specifier
                (if-let ((type (c/cpp-type ast)))
                  (append1 (c/cpp-modifiers ast) type)
                  ;; NOTE: if type isn't present, an implicit int
                  ;;       will be added during normalization.
                  (and (setf implicit-int-p t)
                       (c/cpp-modifiers ast))))
               ((or c/cpp-declaration c/cpp-field-declaration
                    c/cpp-parameter-declaration c/cpp-function-definition)
                (append (c/cpp-pre-specifiers ast)
                        (c/cpp-post-specifiers ast)
                        (unwind-c/cpp-type (c/cpp-type ast))))
               (t (list ast))))
           (ensure-type-specifier (specifier-list)
             "Ensure that SPECIFIER-LIST has a type by adding an 'int' type
              specifier if one isn't present."
             (if implicit-int-p
                 (cons (convert ast-type `((:class . :primitive-type)
                                           (:text . "int")
                                           (:before-text . " ")))
                       specifier-list)
                 specifier-list))
           (qualifier= (qualifier1 qualifier2
                        &aux (target-types
                              '(c/cpp-type-qualifier
                                c/cpp-storage-class-specifier)))
             "Return T if QUALIFIER1 and QUALIFIER2 are equal."
             (and (type= (type-of qualifier1) (type-of qualifier2))
                  (member qualifier1 target-types :test #'typep)
                  (equal (text qualifier1) (text qualifier2))))
           (remove-duplicate-specifiers (specifier-list)
             "Remove duplicate qualifiers from SPECIFIER-LIST. This is only
              done for types where duplicates are redundant."
             (remove-duplicates specifier-list :test #'qualifier=))
           (remove-extraneous-specifiers (specifier-list)
             "Remove specifiers which are unnecessary for analysis."
             (remove-if (of-type '(or c/cpp-signed null)) specifier-list)))
    (remove-duplicate-specifiers
     (remove-extraneous-specifiers
      (ensure-type-specifier
       (unwind-c/cpp-type declaration-ast))))))

(defclass c/cpp-canonical-type (canonical-type)
  ((specifier
    :accessor specifier
    :initarg :specifier
    :initform nil
    :documentation "The specifier part of the canonical type. This generally
includes the type and type qualifiers.")
   (declarator
    :accessor declarator
    :initarg :declarator
    :initform nil
    :documentation "The declarator part of the canonical type. This includes
array, function parameter, parens, and pointer information.")
   (bitfield
    :accessor bitfield
    :initarg :bitfield
    :initform nil
    :documentation "The bitfield part of the canonical type."))
  (:documentation "C/C++ representation of canonical types."))

(defmethod print-object ((self c/cpp-canonical-type) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (specifier declarator bitfield) self
      (format stream "~a ~a ~a"
              specifier
              declarator
              bitfield)))
  self)

(defmethod canonicalize-type
    ((declaration c/cpp-declaration)
     &key ast-type canonical-type
       (declarator (car (c/cpp-declarator declaration))))
  (make-instance
   canonical-type
   :specifier (get-specifier-list ast-type declaration)
   :declarator (canonicalize-declarator declarator)))

(defmethod canonicalize-type
    ((declaration c/cpp-init-declarator)
     &key ast-type canonical-type software)
  (if-let (decl (find-enclosing 'c/cpp-declaration software declaration))
    (canonicalize-type decl :ast-type ast-type :canonical-type canonical-type)
    (call-next-method)))

(defmethod canonicalize-type
    ((declaration c/cpp-field-declaration)
     &key ast-type canonical-type
       (declarator (car (c/cpp-declarator declaration))))
  (make-instance
   canonical-type
   :specifier (get-specifier-list ast-type declaration)
   :declarator (canonicalize-declarator declarator)
   :bitfield (when-let (declaration-children (direct-children declaration))
               (car (direct-children (car declaration-children))))))

(defmethod canonicalize-type
    ((declaration c/cpp-function-definition)
     &key ast-type canonical-type)
  (make-instance
   canonical-type
   :specifier (get-specifier-list ast-type declaration)
   :declarator (canonicalize-declarator (c/cpp-declarator declaration))))

(defmethod canonicalize-type
    ((declaration c/cpp-parameter-declaration)
     &key ast-type canonical-type)
  (make-instance
   canonical-type
   :specifier (get-specifier-list ast-type declaration)
   :declarator (canonicalize-declarator (c/cpp-declarator declaration))))

(defmethod canonical-type= ((canonical-type-1 c/cpp-canonical-type)
                            (canonical-type-2 c/cpp-canonical-type)
                            &key symbol-table)
  (declare (ignore symbol-table))
  (labels ((specifier= (specifier1 specifier2)
             "Return T if SPECIFIER1 and SPECIFIER2 contain the same items in
              any order."
             (set-equal specifier1 specifier2
                        :test (op (equal (source-text _)
                                         (source-text _)))))
           (bitfield= (bitfield1 bitfield2)
             "Return T if BITFIELD1 and BITFIELD2 contain the same text."
             ;; NOTE: this can potentially be incorrect in the presence of
             ;;       macros.
             (cond
               ((not (or bitfield1 bitfield2)) t)
               ((and bitfield1 bitfield2)
                ;; TODO: bitfields can be specified in decimal, hex, etc.
                ;;       There should be a numerical equivalence function that
                ;;       already exists somewhere.
                (equal (source-text bitfield1)
                       (source-text bitfield2)))))
           (declarator-function= (function-part1 function-part2)
             "Return T if the declarator function parts are equal."
             (let ((parameters1 (direct-children (car function-part1)))
                   (parameters2 (direct-children (car function-part2))))
               (when (length= parameters1 parameters2)
                 (iter
                   (for parameter1 in parameters1)
                   (for parameter2 in parameters2)
                   (always
                    (canonical-type= (canonicalize-type parameter1)
                                     (canonicalize-type parameter2)))))))
           (declarator-pointer= (pointer-part1 pointer-part2)
             "Return T if the declarator pointer parts are equal."
             (specifier= pointer-part1 pointer-part2))
           (declarator-array= (array-part1 array-part2)
             "Return T if the CDRs in the declarator array parts are equal."
             ;; NOTE: this may need to account for identifiers at some point
             ;;       when a symbol table is available. Just reuse bitfield= for
             ;;       now.
             (bitfield= (car array-part1) (car array-part2)))
           (declarator=* (declarator1 declarator2)
             "Return T if the declarator array parts are equal."
             (iter
               (for part1 in declarator1)
               (for part2 in declarator2)
               (for key = (car part1))
               (unless (eql key (car part2))
                 (return))
               (for value1 = (cdr part1))
               (for value2 = (cdr part2))
               (always
                (case key
                  (:function (declarator-function= value1 value2))
                  (:pointer (declarator-pointer= value1 value2))
                  (:array (declarator-array= value1 value2))))))
           (declarator= (declarator1 declarator2)
             "Return T if the declarator array parts have the same lengths and
              are equal."
             (when (length= declarator1 declarator2)
               (declarator=* declarator1 declarator2))))
    (and (specifier= (specifier canonical-type-1)
                     (specifier canonical-type-2))
         (declarator= (declarator canonical-type-1)
                      (declarator canonical-type-2))
         (bitfield= (bitfield canonical-type-1)
                    (bitfield canonical-type-2)))))

) ; #+(or :tree-sitter-c :tree-sitter-cpp)
