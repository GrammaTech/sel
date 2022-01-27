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


;;; Generics and Transformations
(defmethod function-name ((ast c/cpp-function-definition))
  (nest (source-text)
        (find-if «or (of-type 'identifier-ast)
                     (of-type 'cpp-qualified-identifier)»)
        (c/cpp-declarator)
        (c/cpp-declarator ast)))

(defmethod function-parameters ((ast c/cpp-function-definition))
  (direct-children (c/cpp-parameters (c/cpp-declarator ast))))

(defmethod call-arguments ((node c/cpp-call-expression))
  (direct-children (c/cpp-arguments node)))

(defmethod function-body ((ast c/cpp-function-definition)) (c-body ast))

(defmethod no-fallthrough ((ast c/cpp-continue-statement)) t)
(defmethod no-fallthrough ((ast c/cpp-break-statement)) t)

(defmethod inner-declarations ((ast c/cpp-function-declarator))
  (remove-if-not {typep _ 'c/cpp-parameter-declaration}
                 (convert 'list (c/cpp-parameters ast))))

(defmethod outer-declarations ((ast c/cpp-declaration))
  (flatten
   (iter (for d in (c/cpp-declarator ast))
     (collect
      (match d
        ((type c/cpp-identifier) d)
        ((type (or c/cpp-array-declarator c/cpp-pointer-declarator))
         (outer-declarations d))
        ((c/cpp-init-declarator
          (c/cpp-declarator
           (cpp-reference-declarator
            (direct-children (list r)))))
         r)
        ((c/cpp-init-declarator
          (c/cpp-declarator
           (c/cpp-pointer-declarator
            (c/cpp-declarator d))))
         d)
           ;; Special handling for uninitialized variables.
        (otherwise (c/cpp-declarator d)))))))

(defmethod inner-declarations ((ast c/cpp-compound-statement))
  (mappend #'outer-declarations (children ast)))

(defmethod inner-declarations ((ast cpp-declaration-list))
  (mappend #'outer-declarations (children ast)))

(defun get-nested-declaration (ast)
  "Get the declaration nested in AST. This is useful for array and
pointer declarations which are nested on themselves."
  (let ((declarator (c/cpp-declarator ast)))
    (if (typep declarator 'c/cpp-identifier)
        (list declarator)
        (outer-declarations declarator))))

(defmethod outer-declarations ((ast c/cpp-array-declarator))
  (get-nested-declaration ast))

(defmethod outer-declarations ((ast c/cpp-pointer-declarator))
  (get-nested-declaration ast))

(defmethod outer-declarations ((ast c/cpp-struct-specifier))
  (list (c/cpp-name ast)))

(defmethod outer-declarations ((ast c/cpp-enum-specifier))
  (match ast
    ((c/cpp-enum-specifier
      (c/cpp-name name)
      (c/cpp-body
       (c/cpp-enumerator-list
        (direct-children enumerators))))
     (cons name (mapcar #'c/cpp-name enumerators)))))

(defmethod outer-declarations ((ast c/cpp-function-declarator))
  (list (c/cpp-declarator ast)))

(defmethod outer-declarations ((ast c/cpp-function-definition))
  (list (c/cpp-declarator (c/cpp-declarator ast))))

(defmethod enclosing-definition ((sw c/cpp) (ast t))
  (find-enclosing '(or definition-ast cpp-class-specifier
                    c/cpp-primitive-type)
                  sw ast))

(defmethod definition-name ((ast c/cpp-function-definition))
  (declarator-name (c/cpp-declarator ast)))
(defmethod definition-name ((ast c/cpp-struct-specifier))
  (source-text (c/cpp-name ast)))
(defmethod definition-name ((ast c/cpp-union-specifier))
  (source-text (c/cpp-name ast)))
(defmethod definition-name ((ast c/cpp-type-definition))
  (declarator-name (c/cpp-declarator ast)))
(defmethod definition-name ((ast c/cpp-preproc-def))
  (source-text (c/cpp-name ast)))
(defmethod definition-name ((ast c/cpp-preproc-function-def))
  (source-text (c/cpp-name ast)))

(defmethod declarator-name ((ast c/cpp-identifier))
  (source-text ast))
(defmethod declarator-name ((ast c/cpp-type-identifier))
  (source-text ast))
(defmethod declarator-name ((ast c/cpp-init-declarator))
  (declarator-name (c/cpp-declarator ast)))
(defmethod declarator-name ((ast c/cpp-parenthesized-declarator))
  (source-text (car (children ast))))
(defmethod declarator-name ((ast c/cpp-pointer-declarator))
  (declarator-name (c/cpp-declarator ast)))
(defmethod declarator-name ((ast c/cpp-array-declarator))
  (declarator-name (c/cpp-declarator ast)))
(defmethod declarator-name ((ast c/cpp-function-declarator))
  (declarator-name (c/cpp-declarator ast)))

(defmethod field-names ((ast c/cpp-field-declaration))
  (collect-if (of-type 'c/cpp-field-identifier) ast))
(defmethod field-names ((ast c/cpp-enumerator))
  (list (c/cpp-name ast)))

(defmethod get-declaration-id ((obj c/cpp) (ast c/cpp-pointer-expression))
  (get-declaration-id obj (c/cpp-argument ast)))

(defmethod get-initialization-ast ((obj c/cpp) (ast c/cpp-pointer-expression))
  (get-initialization-ast obj (c/cpp-argument ast)))

(defmethod get-declaration-ast ((obj c/cpp) (ast c/cpp-pointer-expression))
  (get-declaration-ast obj (c/cpp-argument ast)))

(defmethod get-declaration-ast ((obj c/cpp) (ast c/cpp-type-descriptor))
  (get-declaration-ast obj (c/cpp-type ast)))

(defmethod infer-type ((obj software) (ast c/cpp-field-expression))
  (when-let ((id (get-declaration-id obj ast)))
    ;; Get the type from the declaration of the field argument.
    (infer-type obj id)))

(defmethod get-declaration-ast ((obj software) (ast c/cpp-field-expression))
  (when-let* ((type
               ;; Get the ID from the declaration of the field
               ;; argument.

               ;; If we end up back here something has gone wrong.
               (without-recursion ()
                 (infer-type obj ast)))
              ;; Get the declaration of the type of the argument.
              (type-decl (get-declaration-ast obj type))
              ;; The name of the field we're looking for.
              (target-field-name (source-text (c/cpp-field ast))))
    (match type-decl
      ((c/cpp-struct-specifier
        (c/cpp-body field-list))
       (iter (for field in (direct-children field-list))
             (for field-names = (field-names field))
             (finding field such-that
                      (member target-field-name
                              field-names
                              :test #'source-text=)))))))

(defmethod get-declaration-id ((obj c/cpp) (field c/cpp-field-expression))
  (get-declaration-id obj (c/cpp-argument field)))

(defmethod get-declaration-id ((obj c/cpp) (id identifier-ast))
  (when-let (declaration (get-declaration-ast obj id))
    (let ((id-text (source-text id)))
      (iter (for ast1 in-tree declaration)
            (for subtree =
                 (typecase ast1
                   (c/cpp-init-declarator (lhs ast1))
                   (otherwise ast1)))
            (thereis
             (iter (for ast2 in-tree subtree)
                   (finding ast2 such-that
                            (and (typep ast2 'identifier-ast)
                                 (equal (source-text ast2) id-text)))))))))

(defmethod get-initialization-ast ((obj cpp) (ast ast))
  "Find the assignment for an unitialized variable."
  (or (call-next-method)
      (when-let* ((id (get-declaration-id obj ast))
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

(defmethod infer-expression-type :around ((obj c/cpp) (ast expression-ast))
  "Fall back to inferring the expression type from the surrounding declaration.

That is, if the type of an expression cannot be extracted, then if it
occurs as the RHS of a init declarator in a declaration, take the type
of a declaration.

E.g. given

    int x = y

Then if we cannot infer the type of y per se we infer its type to be int."
  (or (call-next-method)
      (match (take 2 (get-parent-asts* obj ast))
        ((list (type c/cpp-init-declarator)
               (and decl (type c/cpp-declaration)))
         (c/cpp-type decl)))))

(defmethod infer-expression-type ((obj c/cpp) (ast call-ast))
  "Infer the type of a call from its declaration."
  (or (when-let (decl (get-declaration-ast obj (call-function ast)))
        (resolve-declaration-type obj decl ast))
      (call-next-method)))

(defmethod extract-declaration-type ((obj c/cpp) (ast c/cpp-function-declarator))
  (when-let (fn (find-enclosing 'c/cpp-function-definition obj ast))
    (extract-declaration-type obj fn)))

(defmethod extract-declaration-type ((obj c/cpp) (ast c/cpp-function-definition))
  (c/cpp-type ast))

(defmethod extract-declaration-type ((obj c/cpp) (ast c/cpp-field-declaration))
  (c/cpp-type ast))

(defmethod extract-declaration-type ((obj c/cpp) (decl ast))
  (or
   ;; Look for a surrounding variable declaration.
   (when-let ((declaration
               (find-if (of-type '(and variable-declaration-ast
                                   (not c/cpp-init-declarator)))
                        ;; Inclusive of AST.
                        (get-parent-asts obj decl))))
     (c/cpp-type declaration))
   ;; If the declaration is for a function, return that
   ;; function's type.
   (and-let* ((function (find-enclosing 'function-ast obj decl))
              ((eql decl (c/cpp-declarator function))))
     (c/cpp-type function))))

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

;;; TODO: variable-use-p isn't fleshed out completely for C++.
(defmethod variable-use-p ((obj c/cpp) identifier &key &allow-other-keys)
  nil)

(defmethod variable-use-p ((obj c/cpp) (identifier c/cpp-identifier)
                           &key &allow-other-keys)
  (child-variable-use-p obj identifier (car (get-parent-asts* obj identifier))))

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
  (:method ((sw c/cpp)
            (lhs c/cpp-pointer-declarator)
            (rhs c/cpp-pointer-expression))
    (if (typep (c/cpp-operator rhs) 'cpp-&)
        (aliasee sw (c/cpp-argument rhs))
        (call-next-method)))
  (:method ((sw c/cpp)
            (lhs c/cpp-pointer-declarator)
            (rhs identifier-ast))
    ;; Assigning a pointer variable to a pointer variable.
    (let ((aliasee (aliasee sw rhs)))
      (if (not (eql aliasee (get-declaration-id sw rhs)))
          aliasee
          (call-next-method)))))

(defmethod aliasee ((sw c/cpp) (id identifier-ast))
  (ematch (get-initialization-ast sw id)
    ((c/cpp-init-declarator (lhs lhs) (rhs rhs))
     (let ((result (initializer-aliasee sw lhs rhs)))
       (if (eql result :failure)
           (get-declaration-id sw id)
           result)))
    ((c/cpp-assignment-expression (rhs rhs))
     (aliasee sw rhs))
    (otherwise nil)))

(defmethod alias-set ((sw c/cpp) (plain-var c/cpp-ast))
  (when-let (id (get-declaration-id sw plain-var))
    (iter (for ast in-tree (genome sw))
          (when (and (typep ast 'identifier-ast)
                     (eql (aliasee sw ast) id))
            (set-collect (get-declaration-id sw ast) into set))
          (finally (return (convert 'list (less set plain-var)))))))


(defmethod collect-arg-uses ((sw c/cpp) (target identifier-ast)
                             &optional alias)
  (fbindrec ((get-decl
              (if alias
                  (lambda (obj var)
                    (get-declaration-id obj (or (aliasee obj var) var)))
                  #'get-declaration-id))
             (occurs-as-object?
              (lambda (ast target)
                (match ast
                  ((call-ast
                    (call-function
                     (cpp-field-expression
                      (cpp-argument arg))))
                   (eql (get-decl sw arg) target)))))
             (occurs-as-arg?
              (lambda (ast target)
                (match ast
                  ((call-ast (call-arguments args))
                   (member target
                           (filter (of-type 'identifier-ast)
                                   (assure list args))
                           :key (op (get-decl sw _))))))))
    (let ((target (get-decl sw target)))
      (iter (for ast in-tree (genome sw))
            ;; The outer loop will recurse, so we don't
            ;; need to recurse here.
            (when (or (occurs-as-object? ast target)
                      (occurs-as-arg? ast target))
              (set-collect ast into calls))
            (finally (return (convert 'list calls)))))))


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
   :declarator (canonicalize-declarator (cpp-declarator declaration))))

(defmethod canonicalize-type
    ((declaration c/cpp-parameter-declaration)
     &key ast-type canonical-type)
  (make-instance
   canonical-type
   :specifier (get-specifier-list ast-type declaration)
   :declarator (canonicalize-declarator (cpp-declarator declaration))))

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
