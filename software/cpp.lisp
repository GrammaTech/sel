(defpackage :software-evolution-library/software/cpp
  (:nicknames :sel/software/cpp :sel/sw/cpp)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template
        :software-evolution-library/software/string-clauses
        :software-evolution-library/software/c-cpp))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
;;; !! Language generated in c-cpp !!
;;;===================================================

(defconst +cpp-operator-names+
  '("co_await"
    "+" "-"
    "*" "/"
    "%" "^"
    "&" "|"
    "~" "!"
    "=" "<"
    ">" "+="
    "-=" "*="
    "/=" "%="
    "^=" "&="
    "|=" "<<"
    ">>" ">>="
    "<<=" "=="
    "!=" "<="
    ">=" "&&"
    "||" "++"
    "--" ","
    "->*" "->"
    "()" "[]")
  "Names of operators that can occur in operator_name ASTs.")

(defconst +cpp-implicitly-converting-arithmetic-operators+
  '("+" "-" "*" "/" "%"
    "<" ">" "<=" ">=" "==" "!="
    "&" "^" "|"))

(define-language-alias-mappings
    cpp ("c plus plus" "c++" "c-plus-plus" "cc" "cp" "cpp" "cxx" "hpp"))


#+:TREE-SITTER-CPP
(progn

(defmethod initialize-instance :after ((cpp cpp)
                                       &key &allow-other-keys)
  "If no compiler was specified, default to cc."
  (unless (compiler cpp)
    (setf (compiler cpp) "c++")))

;;; The following information is gathered from
;;;   Meta-Compilation for C++ by Edward D. Willink.
;;;
;;; Typed Ambiguities:
;;; 5.5.3.1 Declaration/Declaration
;;; ---Parenthesized variable declaration
;;;     vs
;;;    single-argument type constructor
;;;
;;;    int (x);
;;;
;;;    where 'x' may be a previously defined variable or a new one.
;;;
;;;    NOTE:
;;;    It seems reasonable to exclude this from an acceptable set of C++,
;;;    at least initially. It will likely require some static analysis
;;;    otherwise, but it may be unlikely that a single-argument
;;;    type constructor would be given its own line as it would need
;;;    side-effects to do anything.
;;;
;;; ---Constructed Object Declaration
;;;     vs
;;;    Function Declaration
;;;
;;;    TypeName a(x);
;;;
;;;    where 'x' could be a variable or a type.
;;;
;;;    This can be addressed with a symbol table.
;;;
;;; 5.5.3.2 Declaration/Expression
;;; ---The Most Vexing Parse
;;;    TypeName ()--Constructor or Function Declaration?
;;;
;;;    NOTE: not much that can be done here. The function declaration will
;;;          be taken as what's intended even if it is not.
;;;
;;; 5.5.3.4 Type-id/Expression-list
;;; ---parenthesised-call
;;;     vs
;;;    cast-parenthesis
;;;
;;;    (a) (x);
;;;
;;;    where 'a' could be a type or a function name.
;;;
;;;    NOTE: with the current representation not distinguishing between
;;;          call expressions and functional casts, it probably doesn't
;;;          make much sense to handle this right now either since they're
;;;          semantically identical, more or less.
;;;
;;; ---parenthesised-binary
;;;     vs
;;;    cast-unary
;;;
;;;    (x) - y;
;;;
;;;    where 'x' could be a variable or a typename.
;;;
;;; 5.5.3.5 Call/Functional-cast
;;; fun(x)      // function call
;;; TypeName(x) // functional-cast equivalent to (TypeName) x
;;;
;;; NOTE: for now, don't support this transformation since it would
;;;       require adding a functional-cast AST.

;;; Type-less Ambiguities:
;;;
;;; 5.7.1.1: Declaration/Expression
;;; ---assignment expression
;;;     vs
;;;    parameter declaration
;;;
;;;    int f (x = 7);
;;;
;;;    where 'x' could be a type name or a variable name.
;;;
;;; ---5.7.1.3 type-id/expression-list
;;;
;;;    (x)+5
;;;
;;;    where 'x' can be type name or a variable.

(defmethod contextualize-ast :around (software (ast cpp-ast) context &rest rest
                                      &key ast-type &allow-other-keys)
  (if ast-type
      (call-next-method)
      (apply #'call-next-method software ast context :ast-type 'cpp-ast rest)))

(defun function-declarator->init-declarator (function-declarator)
  "Convert FUNCTION-DECLARATOR into an init-declarator."
  (labels ((abstract-function-parameter-p (parameter-ast)
             "Return T if PARAMETER-AST is an abstract-function parameter."
             (match parameter-ast
               ((cpp-parameter-declaration
                 :cpp-type (or (cpp-type-identifier) (cpp-template-type))
                 :cpp-declarator (cpp-abstract-function-declarator))
                t)))
           (general-identifier->identifier (general-identifier)
             "Convert GENERAL-IDENTIFIER into an identifier AST."
             (convert
              'cpp-ast
              `((:class . :identifier)
                (:text . ,(text general-identifier))
                ,@(preserve-properties general-identifier))))
           (abstract-function-parameter->call-expression (parameter)
             "Convert PARAMETER to a call-expression."
             (let ((parameters (cpp-parameters (cpp-declarator parameter)))
                   (name (cpp-type parameter)))
               (convert
                'cpp-ast
                `((:class . :call-expression)
                  (:function
                   . ,(if (typep name 'cpp-template-type)
                          `((:class . :template-function)
                            (:cpp-arguments . ,(cpp-arguments name))
                            (:cpp-name . ,(general-identifier->identifier
                                           (cpp-name name)))
                            ,@(preserve-properties name))
                          (general-identifier->identifier name)))
                  (:arguments
                   (:class . :argument-list)
                   (:internal-asts-0 ,@(cpp-internal-asts-3 parameters))
                   (:children
                    ,@(mapcar #'convert-for-argument-list
                              (direct-children parameters)))
                   ,@(preserve-properties parameters))
                  ,@(preserve-properties parameter)))))
           (optional-parameter-declaration->assignment-expression (parameter)
             "Convert PARAMETER into an assignment expression."
             (let ((lhs (cpp-type parameter))
                   (rhs (cpp-default-value parameter)))
               (convert
                'cpp-ast
                `((:class . :assignment-expression)
                  (:left
                   (:class . :identifier)
                   (:text . ,(text lhs))
                   ,@(preserve-properties lhs))
                  (:operator (:class . :=))
                  (:right
                   (:class . :identifier)
                   (:text . ,(text rhs))
                   ,@(preserve-properties rhs))
                  ,@(preserve-properties parameter)))))
           (parameter-declaration->identifier (parameter)
             "Convert PARAMETER into an identifier."
             (let* ((type-identifier (cpp-type parameter))
                    (type-properties-grouping
                      (preserve-properties type-identifier :group-by-position t))
                    (parameter-properties-grouping
                      (preserve-properties parameter :group-by-position t)))
               (convert
                'cpp-ast
                `((:class . :identifier)
                  (:text . ,(text type-identifier))
                  ,@(merge-preserved-properties
                     (aget :before parameter-properties-grouping)
                     (aget :before type-properties-grouping))
                  ,@(merge-preserved-properties
                     (aget :after type-properties-grouping)
                     (aget :after parameter-properties-grouping))))))
           (convert-for-argument-list (target-ast)
             "Convert TARGET-AST to a type that is suited for an argument list."
             ;; TODO: this probably doesn't cover every case.
             (econd
              ((abstract-function-parameter-p target-ast)
               (abstract-function-parameter->call-expression target-ast))
              ((typep target-ast 'cpp-optional-parameter-declaration)
               (optional-parameter-declaration->assignment-expression
                target-ast))
              ((typep target-ast 'cpp-parameter-declaration)
               (parameter-declaration->identifier target-ast)))))
    (let ((parameters (cpp-parameters function-declarator)))
      (convert
       'cpp-ast
       `((:class . :init-declarator)
         (:declarator . ,(general-identifier->identifier
                          (cpp-declarator function-declarator)))
         (:value
          (:class . :argument-list)
          (:children
           ,@(mapcar #'convert-for-argument-list
                     (direct-children parameters)))
          ,@(preserve-properties parameters))
         ,@(preserve-properties function-declarator))))))

(defun definitely-a-parameter-p (parameter)
  "Return T if AST is definitely a parameter AST."
  (match parameter
    ((cpp-parameter-declaration
      :cpp-type (identifier-ast)
      :cpp-declarator (identifier-ast))
     t)
    ((cpp-parameter-declaration
      :cpp-pre-specifiers pre-specifiers
      :cpp-post-specifiers post-specifiers)
     (or pre-specifiers post-specifiers))
    ((cpp-optional-parameter-declaration
      :cpp-type (identifier-ast)
      :cpp-declarator declarator)
     declarator)))

(defmethod contextualize-ast ((software cpp)
                              (ast cpp-function-declarator)
                              (context hash-table)
                              &key &allow-other-keys)
  ;; TODO: this can be further improved.
  ;;       Currently, it only checks if the parameters are valid types.
  ;;       Can probably check if parent is a function definition; on the other
  ;;       hand, this may be redundant.
  (labels ((definitely-a-type-p (parameter-ast)
             "Return T if PARAMETER-AST definitely represents a type in
              context."
             (match parameter-ast
               ((cpp-parameter-declaration
                 :cpp-type (and identifier (identifier-ast))
                 ;; Currently assumes that abstract function declarators won't
                 ;; be used unless another valid type is found.
                 :cpp-declarator (not (cpp-abstract-function-declarator)))
                (eql :type (get-context-for identifier context)))
               ((cpp-optional-parameter-declaration
                 :cpp-type (and identifier (identifier-ast)))
                (eql :type (get-context-for identifier context)))))
           (definitely-parameters-p (parameters)
             "Return T if PARAMETERS definitely contains a parameter."
             (find-if «or #'definitely-a-parameter-p #'definitely-a-type-p»
                      (direct-children parameters))))
    (match ast
      ((cpp-function-declarator
        :cpp-declarator identifier
        :cpp-parameters parameters)
       (when (or (equal :function (get-context-for identifier context))
                 (find-if #'definitely-parameters-p parameters))
         (function-declarator->init-declarator ast))))))

(defmethod contextualize-ast ((software cpp)
                              (ast cpp-function-declarator)
                              context
                              &key (parents (get-parent-asts* software ast))
                              &allow-other-keys)
  (labels ((top-level-p (parents)
             "Return T if AST is likely a top-level form in SOFTWARE."
             (every (of-type '(or cpp-translation-unit
                               cpp-preproc-if cpp-preproc-ifdef
                               cpp-class-specifier cpp-namespace-definition
                               cpp-declaration-list cpp-field-declaration-list
                               cpp-struct-specifier cpp-field-declaration
                               cpp-function-declarator cpp-declaration
                               cpp-reference-declarator cpp-pointer-declarator
                               cpp-template-declaration))
                    parents))
           (part-of-definition-p (software ast parents)
             "Return T if AST is part of the declaration of a function
              definition."
             (when-let ((definition (find-if (of-type 'cpp-function-definition)
                                             parents)))
               (shares-path-of-p software ast (cpp-declarator definition))))
           (definitely-parameters-p (ast)
             "Return T if PARAMETERS definitely contains a parameter."
             (match ast
               ((cpp-function-declarator
                 :cpp-parameters parameters)
                (find-if #'definitely-a-parameter-p (direct-children parameters)))))
           (trailing-specifiers-p (ast)
             "Return non-NIL if AST has any trailing specifiers."
             (match ast
               ((cpp-function-declarator
                 :cpp-children children)
                children))))
    ;; NOTE: assume that function declarators are the intention in header files.
    (unless (or (top-level-p parents)
                (part-of-definition-p software ast parents)
                (definitely-parameters-p ast)
                (trailing-specifiers-p ast))
      ;; NOTE: perform blanket transformation for now.
      (function-declarator->init-declarator ast))))

(defclass cpp-variadic-declaration
    (cpp-parameter-declaration cpp-identifier)
  ((text :accessor text
         :initform "..."
         :initarg :text
         :allocation :class)
   (choice-subclasses
    :initform nil
    :reader choice-subclasses
    :allocation :class)))

(defmethod computed-text-node-p ((ast cpp-variadic-declaration)) t)

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-assignment-expression)) parse-tree
     &key)
  "Transform PARSE-TREE such that the operator is stored in the :operator field."
  (add-operator-to-binary-operation parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-field-expression)) parse-tree &key)
  "Transform PARSE-TREE such that the operator is stored in the :operator field."
  (add-operator-to-binary-operation parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-preproc-params)) parse-tree &key)
  "Transform PARSE-TREE such that the operator is stored in the :operator field."
  (transform-c-style-variadic-parameter parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-function-definition)) parse-tree
     &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-operator-cast)) parse-tree
     &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-declaration)) parse-tree &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-field-declaration)) parse-tree &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-parameter-declaration)) parse-tree
     &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-optional-parameter-declaration)) parse-tree
     &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-type-descriptor)) parse-tree &key)
  (transform-c-type-qualifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-case-statement)) parse-tree &key)
  (transform-case-statement parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-labeled-statement)) parse-tree &key)
  (transform-labeled-statement parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-for-statement)) parse-tree &key)
  (transform-for-statement parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-for-range-loop)) parse-tree &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql :cpp)) (class (eql 'cpp-operator-name)) parse-tree &key
     &aux (children (parse-tree-children parse-tree)))
  (labels ((transform-quotes-operator ()
             (with-modify-parse-tree (parse-tree)
               ((:|""|)
                (label-as :name))
               ((:identifier)
                (label-as :suffix-identifier))))
           (transform-new/delete-operator ()
             (with-modify-parse-tree (parse-tree)
               ((:new :delete)
                (label-as :name))
               ((:|[]|)
                (label-as :array))))
           (transform-operator ()
             (with-modify-parse-tree (parse-tree)
               (#.(mapcar #'make-keyword +cpp-operator-names+)
                  (label-as :name)))))
    (cond
      ((find-if (op (eql :|""| (car _))) children)
       (transform-quotes-operator))
      ((find-if (op (member (car _) '(:new :delete))) children)
       (transform-new/delete-operator))
      (t (transform-operator)))))

(defmethod transform-parse-tree
    ((language (eql :cpp)) (class (eql 'cpp-sized-type-specifier))
     parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    ((:error :comment) (ignore-types))
    (t (label-as :modifiers))))

(defmethod transform-parse-tree
    ((language (eql :cpp)) (class (eql 'cpp-access-specifier))
     parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    ((:public :private :protected) (label-as :keyword))))

(defmethod transform-parse-tree
    ((language (eql :cpp)) (class (eql 'cpp-type-parameter-declaration))
     parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    ((:typename :class) (label-as :keyword))))

(defmethod transform-parse-tree
    ((language (eql :cpp)) (class (eql 'cpp-reference-declarator))
     parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    ((:& :&&) (label-as :valueness))))

(defmethod transform-parse-tree
    ((language (eql :cpp)) (class (eql 'cpp-variadic-reference-declarator))
     parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    ((:& :&&) (label-as :valueness))))

(defmethod transform-parse-tree
    ((language (eql :cpp)) (class (eql 'cpp-compound-statement))
     parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-translation-unit)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-preproc-if)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-preproc-ifdef)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-preproc-else)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-preproc-elif)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-declaration-list)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':cpp)) (class (eql 'cpp-template-declaration)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod ext :around ((obj cpp)) (or (call-next-method) "cpp"))

(defmethod function-body ((ast cpp-function-definition)) (cpp-body ast))

(defmethod cpp-declarator ((ast cpp-reference-declarator))
  (let ((children (children ast)))
    (if (single children)
        (cpp-declarator (first children))
        (if-let ((first-non-terminal
                  (find-if-not (of-type 'terminal-symbol)
                               children)))
          (cpp-declarator first-non-terminal)
          (call-next-method)))))

(defmethod c/cpp-declarator ((ast cpp-reference-declarator))
  (cpp-declarator ast))

(defmethod definition-name-ast ((ast cpp-class-specifier))
  (cpp-name ast))

(defmethod definition-name-ast ((ast cpp-namespace-definition))
  (cpp-name ast))

(defmethod canonicalize-declarator ((declarator cpp-reference-declarator))
  ;; NOTE: more or less an alias. Maybe adding it to the type information
  ;;       isn't really needed or maybe it can just be ignored?
  (append (canonicalize-declarator (car (direct-children declarator)))
          `((:reference))))

(defclass cpp-canonical-type (c/cpp-canonical-type)
  ()
  (:documentation "C++ representation of canonical types."))

(defmethod canonicalize-type :around ((declaration cpp-ast) &rest rest
                                      &key &allow-other-keys)
  (multiple-value-call #'call-next-method
    declaration
    (values-list rest)
    :ast-type 'cpp-ast
    :canonical-type 'cpp-canonical-type))

;;; TODO Generalize include handling to C as well as C++. (This will
;;; involve pulling the declarations from the std namespace, and only
;;; for the C compatibility headers.)

(defun find-std-header (name &key (language 'cpp))
  "Find the standard library header named NAME."
  (from-string language (extract-header-synopsis name)))


(defun system-header-names (sw)
  "Return a list of the system headers in SW."
  (iter (for ast in-tree (genome sw))
        (match ast
          ((c/cpp-preproc-include (c/cpp-path path))
           (match (source-text path)
             ((ppcre "<(.*)>" s)
              (collect s)))))))

(defun lookup-in-std-header (header namespaces field)
  "Look up a declaration in a standard library header synopsis.
- HEADER is the name of the header.
- NAMESPACES is a list of namespace (or class) names.
- FIELD is the name of the function or field we want."
  (labels ((find-field (field ast)
             (find-if (lambda (ast)
                        (and-let*  (((typep ast '(or cpp-field-declaration cpp-function-definition)))
                                    (decl
                                     (find-if (of-type 'cpp-function-declarator)
                                              ast)))
                          (source-text= field (cpp-declarator decl))))
                      ast))
           (find-namespace-or-class (ast name)
             (find-if (lambda (ast)
                        (and (typep ast '(or cpp-namespace-definition
                                          type-declaration-ast))
                             (source-text= name
                                           (definition-name-ast ast))))
                      ast)))
    (when-let ((header (find-std-header (source-text header))))
      (find-field field
                  (reduce #'find-namespace-or-class
                          (ensure-list namespaces)
                          :initial-value (genome header))))))



;;; Methods common to all software objects


;;;; Methods for tree-sitter generics

(defmethod call-name ((ast cpp-call-expression))
  "If the call function is a template function, extract just the name of the template function without its arguments."
  (source-text
   (let ((function (call-function ast)))
     (if (typep function 'cpp-template-function)
         (cpp-name function)
         function))))

(defmethod scope-ast-p ((ast cpp-namespace-definition)) t)
(defmethod scope-ast-p ((ast cpp-declaration-list)) t)

(def +unnamed-namespace-ast+
  (make 'cpp-ast)
  "Dummy AST for an unnamed namespace.")

(defmethod parameter-names ((ast cpp-parameter-declaration))
  ;; Note that currently (2021) C++ allows destructuring ("structured
  ;; bindings") in blocks but not in parameter declarations.
  (let ((ids
         ;; If parameters have explicit namespaces we don't want those.
         (remove-if (of-type 'cpp-namespace-identifier)
                    (identifiers ast))))
    (if-let (type (cpp-type ast))
      ;; We don't want identifiers from type declarations.
      (remove-if (op (shares-path-of-p ast _ type)) ids)
      ids)))

(defmethod outer-declarations ((ast cpp-namespace-definition))
  (match ast
    ((cpp-namespace-definition
      (cpp-body
       (cpp-declaration-list (direct-children children))))
     (let ((declarations-values-list
             (reduce #'outer-declarations-merge children
                     :initial-value nil)))
       (values (car declarations-values-list)
            (cadr declarations-values-list))))))

(defun requalify (qualifiers initial)
  "Given a list of names (or template types) to use as qualifiers, and
an identifier to qualify, build a `cpp-qualified-identifier' AST."
  (reduce (lambda (scope name)
            (make 'cpp-qualified-identifier
                  :cpp-scope scope
                  :cpp-name name))
          qualifiers
          :initial-value initial
          :from-end t))

(def-attr-fun decl-qualifiers (qualifiers)
  (:method ((ast t) &optional qualifiers)
    qualifiers))

(defmethod attr-missing ((name (eql 'decl-qualifiers)) node)
  (decl-qualifiers node nil))

(defmethod resolve-declaration-type :around ((obj cpp)
                                             (decl cpp-ast)
                                             (ast cpp-ast))
  "Possibly qualify the type of AST according to AST's namespace qualification."
  (let ((result (call-next-method)))
    (requalify
     (decl-qualifiers ast)
     result)))

(defmethod resolve-declaration-type ((obj cpp)
                                     (decl cpp-ast)
                                     (ast cpp-ast))
  (when-let (first-try (call-next-method))
    (or
     ;; If the first try is not auto, just return it.
     (unless (typep first-try 'cpp-placeholder-type-specifier)
       first-try)
     ;; If there is a surrounding init declarator, infer the type from
     ;; its RHS.
     (when-let (init
                (find-if (of-type 'c/cpp-init-declarator)
                         (get-parent-asts obj decl)))
       (when (or (eql decl init)
                 (ancestor-of-p obj decl (lhs init)))
         (infer-type obj (rhs init))))
     (match decl
       ((cpp-declaration
         (cpp-declarator (and decls (type list))))
        (dolist (decl decls)
          (when (source-text= (lhs decl) ast)
            (return (infer-type obj (rhs decl)))))))
     ;; Go with the original result.
     first-try)))

(defmethod resolve-declaration-type ((obj cpp)
                                     (decl cpp-field-declaration)
                                     (ast call-ast))
  "If AST is a call AST, and the declaration is a field declaration,
then the return type of the call is the return type of the field."
  (match ast
    ((call-ast
      (call-function
       (and field (cpp-field-expression))))
     (resolve-declaration-type obj decl field))
    (otherwise (call-next-method))))

(defmethod resolve-deref-type ((obj cpp) (ast cpp-ast)
                               (type cpp-qualified-identifier))
  (or (and (string$= "::iterator" (source-text type))
           (resolve-container-element-type type))
      (call-next-method)))

(defgeneric resolve-container-element-type (type)
  (:documentation "Assuming TYPE is a container type, try to get the
  type of its elements.")
  (:method ((type ast)) nil)
  (:method ((type cpp-qualified-identifier))
    ;; Recurse on the name first, not the scope: we want the last
    ;; template type if there is more than one.
    (or (resolve-container-element-type (cpp-name type))
        (resolve-container-element-type (cpp-scope type))))
  (:method ((type cpp-template-type))
    (resolve-container-element-type (cpp-arguments type)))
  (:method ((type cpp-template-argument-list))
    (let ((children (direct-children type)))
      (and (single children)
           (first children)))))

(defmethod expression-type ((ast cpp-compound-literal-expression))
  (cpp-type ast))

(defmethod expression-type ((ast cpp-call-expression))
  (match ast
    ;; Extract the type from a casting operator.
    ((cpp-call-expression
      :cpp-function
      (cpp-template-function
       :cpp-name
       (cpp-identifier
        :text (or "const_cast"
                  "static_cast"
                  "dynamic_cast"
                  "reinterpret_cast"))
       :cpp-arguments
       (cpp-template-argument-list
        :children (list type-descriptor))))
     type-descriptor)
    (otherwise
     (call-next-method))))

(defmethod infer-expression-type ((obj cpp) (ast cpp-call-expression))
  (match ast
    ;; Special case: the type of `std::next' should be the same as
    ;; its argument.
    ((cpp-call-expression
      :cpp-function
      (and name
           (cpp-qualified-identifier
            :cpp-name (cpp-identifier :text "next")))
      :cpp-arguments
      (cpp-argument-list
       :children (list arg)))
     (unless (equal (mapcar #'source-text (namespace-qualifiers obj name))
                    '("std"))
       (trivia.fail:fail))
     (infer-type obj arg))
    (otherwise
     (call-next-method))))

(defmethod expression-type ((ast cpp-number-literal))
  ;; NB There are no negative integer literals in C++; they are
  ;; handed through implicit conversion with the unary minus
  ;; operator (TODO).
  (flet ((integer-type (int)
           (econd
            ;; TODO Allow configuring the thresholds? Extract them from
            ;; the environment?
            ((< int (expt 2 16))
             (make 'cpp-primitive-type :text "int"))
            ((< int (expt 2 32))
             (cpp-type (convert 'cpp-ast "long long a;" :deepest t)))
            ((< int (expt 2 64))
             (cpp-type (convert 'cpp-ast "long long int a;" :deepest t))))))
    (match
        ;; C++ does not care about case (in hex numbers) and allows ' as
        ;; a separator.
        (remove #\' (string-downcase (text ast)))
      ;; TODO Unfinished. See
      ;; https://en.cppreference.com/w/cpp/language/integer_literal and
      ;; https://en.cppreference.com/w/cpp/language/floating_literal.
      ((and string (ppcre "^[0-9]+$"))
       (integer-type (parse-integer string)))
      ((ppcre "^[0-9]+\\.[0-9]*$")
       (make 'cpp-primitive-type :text "double"))
      ((ppcre "^[0-9]+\\.[0-9]*f$")
       (make 'cpp-primitive-type :text "float")))))

(defmethod expression-type ((ast cpp-new-expression))
  (cpp-type ast))

(defmethod placeholder-type-p ((ast cpp-auto))
  t)

(defmethod placeholder-type-p ((ast cpp-placeholder-type-specifier))
  t)

(defmethod infer-type :around ((obj software) (ast cpp-field-expression))
  "Handle the special case of inferring the type of a field expression whose argument is a standard library iterator.

Since a field expression is an implicit dereference, if the type is a
iterator we want the type of the container's elements."
  (let ((type-ast (call-next-method)))
    (or (flet ((namespace-qualifiers* (ast)
                 (namespace-qualifiers obj ast)))
          (match type-ast
            ((and
              (type cpp-qualified-identifier)
              (access #'unqualified-name
                      (and (type identifier-ast)
                           (source-text= "iterator")))
              (access #'namespace-qualifiers*
                      (and (list* (source-text= "std") _)
                           (cl:last
                            (list
                             (cpp-template-type
                              (cpp-arguments
                               (access #'direct-children
                                       (list element-type)))))))))
             element-type)))
        type-ast)))

(defmethod infer-expression-type ((obj cpp) (ast cpp-parenthesized-expression))
  (infer-expression-type obj (only-elt (direct-children ast))))

(defmethod infer-expression-type ((obj cpp) (ast cpp-binary-expression))
  (string-case (source-text (cpp-operator ast))
    (#.+cpp-implicitly-converting-arithmetic-operators+
     (let* ((left-type (infer-type obj (cpp-left ast)))
            (right-type (infer-type obj (cpp-right ast)))
            (left-type-descriptor (type-descriptor left-type))
            (right-type-descriptor (type-descriptor right-type))
            (conversion (usual-arithmetic-conversions
                         left-type-descriptor
                         right-type-descriptor)))
       (econd
        ((equal? conversion left-type-descriptor)
         left-type)
        ((equal? conversion right-type-descriptor)
         right-type)
        ((null conversion) nil))))))

(defmethod infer-expression-type ((obj cpp) (ast cpp-this))
  (when-let (type-ast (find-enclosing 'type-declaration-ast obj ast))
    (definition-name-ast type-ast)))

(defun usual-arithmetic-conversions (type1 type2)
  ;; TODO Sized integer types. Complex and imaginary types? Note that
  ;; one thing we would want from type descriptors is to be able to
  ;; have, e.g. an integer type descriptor that matches all integer
  ;; types, so that individual rules don't have to be written for
  ;; signed, signed, long, short ints to express the fact that they
  ;; all get coerce to floats.
  (match* ((string type1) (string type2))
    ;; There's a long double.
    (("long double" _) type1)
    ((_ "long double") type2)
    ;; There's a double.
    (("double" "float") type1)
    (("float" "double") type2)
    (("double" "int") type1)
    (("int" "double") type1)
    ;; There's a float.
    (("float" "int") type1)
    (("int" "float") type2)
    ((x y) (and (equal x y) type1))))

(defgeneric explicit-namespace-qualifiers (ast)
  (:documentation "Explicit namespace qualifiers (e.g. A::x).")
  (:method ((ast cpp-ast)) nil)
  (:method ((ast cpp-qualified-identifier))
    (let ((scope (cpp-scope ast)))
      (if (null scope) (list :global)
          (append (list scope)
                  (explicit-namespace-qualifiers (cpp-name ast))))))
  (:method ((ast cpp-init-declarator))
    (explicit-namespace-qualifiers (cpp-declarator ast))))

(defgeneric implicit-namespace-qualifiers (obj ast)
  (:documentation "Namespace qualifiers derived from surrounding namespaces.")
  (:method ((obj cpp) ast)
    (with-attr-table obj
      (if (ast-path obj ast)
          (split "::" (namespace ast))
          ;; XXX
          nil))))

(defun combine-namespace-qualifiers (explicit implicit)
  "Combine explicit namespace qualifiers (on the AST) and implicit
namespace qualifiers (inherited from the surrounding namespace).

This is not quite as simple as appending them, since \(1) explicit
namespace qualiifiers can refer to the global namespace and \(2)
references may need to be resolved contextually. Consider this
example:

    int x = 2;
    namespace A {
      namespace B {
        int x = 1;
        namespace A {
         namespace B {
           int x = 2;
           return ::x + A::B::x;
         }
        }
      }
    }

This returns 4 (not 3) because `::x` resolves to the `x` in the global
namespace and `A::B::x` resolves to `A::B::A::B::x`, not `A::B::x`."
  (if-let ((tail (member :global explicit)))
    (rest tail)
    (if explicit
        (let ((index (search explicit implicit
                             :key #'source-text
                             :test #'equal
                             :from-end t)))
          (append (take (or index 0) implicit)
                  explicit))
        implicit)))

(defgeneric namespace-qualifiers (obj ast)
  (:documentation "Final namespace qualifiers, derived by resolving
  explicit (relative) namespace qualifiers relative to
  implicit (absolute) ones.")
  (:method ((obj cpp) ast)
    (combine-namespace-qualifiers
     (explicit-namespace-qualifiers ast)
     (implicit-namespace-qualifiers obj ast))))

(defgeneric unqualified-name (name)
  (:documentation "Remove namespace qualifications from NAME.")
  (:method ((ast cpp-identifier))
    ast)
  (:method ((ast cpp-field-identifier))
    ast)
  (:method ((ast cpp-namespace-identifier))
    ast)
  (:method ((ast cpp-type-identifier))
    ast)
  (:method ((ast cpp-template-type))
    ast)
  (:method ((ast cpp-qualified-identifier))
    (declare (optimize (debug 0)))
    (unqualified-name (cpp-name ast)))
  (:method ((ast cpp-namespace-definition-name))
    (lastcar (children ast))))

(defmethod get-declaration-ast ((type t) (obj cpp) (ast ast))
  (let ((explicits (explicit-namespace-qualifiers ast)))
    (if (null explicits)
        (call-next-method)
        (let* ((full-qualifiers
                (mapcar #'source-text (namespace-qualifiers obj ast)))
               (source-text (source-text (unqualified-name ast)))
               (scope
                (occurs-if
                 (lambda (scope)
                   (match scope
                     ((alist (:name . (and name (type string)))
                             (:decl . (and decl (type ast))))
                      (and (equal source-text name)
                           (typep decl type)
                           (equal
                            full-qualifiers
                            (mapcar #'source-text
                                    (namespace-qualifiers obj decl)))))))
                 (scope-tree obj))))
          (aget :decl scope)))))

(defmethod get-declaration-ast :context ((type (eql 'function-declaration-ast))
                                         (obj cpp) (ast ast))
  "When we can't find declaration in the file, look in standard library headers."
  (labels ((lookup-in-std-headers (quals fn-name)
             ;; Memoize as a header-decl property.
             (iter (for header in (system-header-names obj))
                   (thereis (lookup-in-std-header header quals fn-name))))
           (lookup-qualified-name (ast)
             (lookup-in-std-headers
              (namespace-qualifiers obj ast)
              (unqualified-name ast)))
           (lookup-qualified-declaration (qname method)
             (let ((quals (namespace-qualifiers obj qname))
                   (name (unqualified-name qname)))
               (when-let* ((name-as-qualifier
                            (match name
                              ((identifier-ast) name)
                              ((cpp-template-type (cpp-name name))
                               name)))
                           (decl
                            (lookup-in-std-headers
                             (append1 quals name-as-qualifier)
                             method)))
                 ;; Store the qualifiers as a property of the AST.
                 (decl-qualifiers ast (append1 quals name))
                 decl)))
           (lookup-field-method-declaration (field)
             (let-match (((cpp-field-expression
                           (cpp-argument var)
                           (cpp-field method))
                          field))
               (let ((var-decl (get-declaration-ast 'variable obj var)))
                 (match (infer-type obj var-decl)
                   ((and qname (type cpp-qualified-identifier))
                    (lookup-qualified-declaration qname method))
                   ((and type (identifier-ast))
                    (lookup-in-std-headers nil type)))))))
    (let ((decl (call-next-method)))
      (cond ((typep decl
                    ;; TODO No distinguished class for a field
                    ;; declaration with a function declarator.
                    '(or function-declaration-ast
                      cpp-field-declaration))
             decl)
            ((not (eql 'function-declaration-ast
                       (relevant-declaration-type obj ast)))
             decl)
            ((typep ast 'identifier-ast)
             (lookup-qualified-name ast))
            ((typep ast 'cpp-field-expression)
             (lookup-field-method-declaration ast))))))

(defmethod initializer-aliasee ((sw cpp)
                                (lhs cpp-reference-declarator)
                                (rhs cpp-pointer-expression))
  (with-attr-table sw
    (if (typep (cpp-operator rhs) 'cpp-*)
        (aliasee (cpp-argument rhs))
        (call-next-method))))

(defmethod initializer-aliasee ((sw cpp) (lhs cpp-reference-declarator) rhs)
  (with-attr-table sw
    (aliasee rhs)))



;;; Whitespace rules


;;; Namespace Attr

(defmethod namespace ((ast cpp-namespace-definition)
                      &optional in
                      &aux (name (cpp-name ast)))
  (labels ((get-namespace (ast)
             (declare (ignore ast))
             ;; NOTE: tree-sitter-cpp doesn't currently handle
             ;;       inline namespaces
             ;; TODO: look at implicit namespaces and incorporate or factor
             ;;       out what is needed from there.
             (cond
               ;; Anonyous namespace
               ((not name) in)
               ((emptyp in) #1=(text name))
               (t (string+ in "::" #1#)))))
    (mapcar {namespace _ (get-namespace ast)}
            (children ast))
    in))

(defmethod namespace ((ast cpp-qualified-identifier) &optional in)
  "No scope (e.g. `::x`) means the global scope."
  (declare (ignore in))
  (if (null (cpp-scope ast)) ""
      (call-next-method)))


;;; Symbol Table

(define-constant +cpp-multi-declaration-keys+ '(:function)
  :test #'equal
  :documentation
  "A set of keys which indicate that several definitions for a symbol may be
available to use at any point in a C++ AST.")

(defmethod multi-declaration-keys ((root cpp-ast)) +cpp-multi-declaration-keys+)

(defmethod symbol-table ((node cpp-namespace-definition) &optional in)
  (propagate-declarations-down node in))

(defmethod qualify-declared-ast-name ((declared-ast cpp-ast))
  (let* ((source-text (source-text declared-ast)))
    (if (string^= "::" source-text)
        ;; Global namespace.
        (drop-prefix "::" source-text)
        (let* ((namespace (namespace declared-ast))
               (implicit (split "::" namespace))
               (parts (split "::" source-text))
               (explicit
                (append
                 (and (string^= "::" source-text)
                      (list :global))
                 (butlast parts)))
               (combined
                (combine-namespace-qualifiers explicit implicit)))
          (string-join (append1 combined (lastcar parts)) ;
                       "::")))))

(defmethod outer-defs ((node cpp-ast))
  (mvlet ((declarations namespaces (outer-declarations node)))
    (convert 'fset:map
             (convert-grouped-namespaces
              (group-by-namespace declarations namespaces)
              :source-text-fun #'qualify-declared-ast-name))))

(defmethod inner-defs ((node cpp-ast))
  (mvlet ((declarations namespaces (inner-declarations node)))
    (convert 'fset:map
             (convert-grouped-namespaces
              (group-by-namespace declarations namespaces)
              :source-text-fun #'qualify-declared-ast-name))))

) ; #+:TREE-SITTER-CPP
