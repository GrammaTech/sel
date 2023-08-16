(defpackage :software-evolution-library/software/cpp
  (:nicknames :sel/software/cpp :sel/sw/cpp)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/tree-sitter-base
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

(define-language-alias-mappings
    cpp ("c plus plus" "c++" "c-plus-plus" "cc" "cp" "cpp" "cxx" "hpp"
         ;; Module unit extensions. Visual Studio uses .ixx, Clang
         ;; uses the extensions ending with -m.
         "ixx" "cppm" "ccm" "cxxm" "c++m"))

(defgeneric strip-template-arguments (template)
  (:documentation "Strip template arguments (in angle brackets) from STRING.")
  (:method ((string string))
    (with-string-dispatch () string
      (if (not (position #\< string)) string
          (nlet rec ((pos 0)
                     (bracket-count 0)
                     (acc nil))
            (declare (array-index pos)
                     ;; bracket-count is not an index since it can go
                     ;; below 0 in pathalogical cases.
                     (fixnum bracket-count))
            (if (length>= pos string)
                (if (> bracket-count 0)
                    ;; Not actually delimiters. E.g. operator<.
                    string
                    (coerce (nreverse acc) 'string))
                (case-let (char (vref string pos))
                  (#\< (rec (1+ pos) (1+ bracket-count) acc))
                  (#\> (rec (1+ pos) (1- bracket-count) acc))
                  (t (if (> bracket-count 0)
                         (rec (1+ pos) bracket-count acc)
                         (rec (1+ pos) bracket-count (cons char acc)))))))))))

(defun qualified-name-variants (qname)
  "List variants under which to look up QNAME.
For example, for a qualified name like `x::y::z', we might also want
to look it up as `x::z' or just `z'."
  (let ((parts (split "::" qname)))
    (if (single parts) parts
        (multiple-value-bind (namespaces name)
            (halves parts -1)
          (let ((name (car name))
                (namespaces (apply #'vect namespaces))
                (variants (queue)))
            (iter (until (emptyp namespaces))
                  (enq
                   (string+ (string-join namespaces "::" :end t)
                            name)
                   variants)
                  (vector-pop namespaces)
                  (finally (enq name variants)))
            (qlist variants))))))

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

(defmethod contextualize-ast :around (software (ast cpp-ast) &rest rest
                                      &key ast-type &allow-other-keys)
  (if ast-type
      (call-next-method)
      (apply #'call-next-method software ast :ast-type 'cpp-ast rest)))

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
                   (:internal-asts-0 ,@(cpp-internal-asts-0 parameters))
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
                              &rest kwargs
                              &key (parents (get-parent-asts* software ast))
                              &allow-other-keys)
  (apply #'contextualize-ast (genome software) ast
         :parents parents
         kwargs))

(defmethod contextualize-ast ((root cpp-ast)
                              (ast cpp-function-declarator)
                              &key (parents (get-parent-asts* root ast))
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
           (part-of-definition-p (root ast parents)
             "Return T if AST is part of the declaration of a function
              definition."
             (when-let ((definition (find-if (of-type 'cpp-function-definition)
                                             parents)))
               (shares-path-of-p root ast (cpp-declarator definition))))
           (definitely-a-type-p (parameter-ast)
             "Return T if PARAMETER-AST definitely represents a type in
              context."
             (match parameter-ast
               ((cpp-parameter-declaration
                 :cpp-type (and identifier (identifier-ast))
                 ;; Currently assumes that abstract function declarators won't
                 ;; be used unless another valid type is found.
                 :cpp-declarator (not (cpp-abstract-function-declarator)))
                (eql :type (get-context-for identifier)))
               ((cpp-optional-parameter-declaration
                 :cpp-type (and identifier (identifier-ast)))
                (eql :type (get-context-for identifier)))))
           (definitely-parameters-p (ast)
             "Return T if PARAMETERS definitely contains a parameter."
             (match ast
               ((cpp-function-declarator
                 :cpp-parameters parameters)
                (find-if «or #'definitely-a-parameter-p #'definitely-a-type-p»
                         (direct-children parameters)))))
           (trailing-specifiers-p (ast)
             "Return non-NIL if AST has any trailing specifiers."
             (match ast
               ((cpp-function-declarator
                 :cpp-children children)
                children))))
    ;; NOTE: assume that function declarators are the intention in header files.
    (unless (or (top-level-p parents)
                (part-of-definition-p root ast parents)
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
    ((language (eql ':cpp)) (class (eql 'cpp-parameter-list)) parse-tree
     &key)
  (transform-c-style-variadic-parameter parse-tree))

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
  (transform-c-style-variadic-parameter
   (transform-c-declaration-specifiers parse-tree)))

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
    ((language (eql :cpp)) (class (eql 'cpp-enum-specifier))
     parse-tree &key)
  "Label the class/struct terminal of an enum class."
  (with-modify-parse-tree (parse-tree)
    ((:class :struct) (label-as :scope))))

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
  "Extract the declarator from a reference declarator AST."
  (flet ((cpp-declarator* (child)
           (or (slot-value-safe child 'cpp-declarator)
               child)))
    (let ((children (children ast)))
      (if (single children)
          (cpp-declarator* (first children))
          (if-let ((first-non-terminal
                    (find-if-not (of-type 'terminal-symbol)
                                 children)))
            (cpp-declarator* first-non-terminal)
            (call-next-method))))))

(defmethod c/cpp-declarator ((ast cpp-reference-declarator))
  (cpp-declarator ast))

(defmethod declarator-name-ast ((ast cpp-reference-declarator))
  (cpp-declarator ast))

(defmethod declarator-name-ast ((ast cpp-qualified-identifier))
  ast)

(defmethod declarator-name-ast ((ast cpp-operator-name))
  ast)

(defmethod declarator-name-ast ((ast cpp-destructor-name))
  ast)

(defmethod declarator-name-ast ((ast cpp-operator-cast))
  (cpp-type ast))

(defmethod declarator-name-ast ((ast cpp-template-function))
  ast)

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

(defmethod canonicalize-type ((declaration cpp-optional-parameter-declaration)
                              &key ast-type canonical-type)
  (make canonical-type
        :specifier (get-specifier-list ast-type declaration)
        :declarator (canonicalize-declarator (c/cpp-declarator declaration))))

;;; TODO Generalize include handling to C as well as C++. (This will
;;; involve pulling the declarations from the std namespace, and only
;;; for the C compatibility headers.)

(defun find-std-header (name &key (language 'cpp))
  "Find the standard library header named NAME."
  (from-string language (extract-header-synopsis name)))

(defmethod strip-template-arguments ((template cpp-template-function))
  (match template
    ((cpp-template-function
      (cpp-arguments args))
     (handler-case
         (source-text
          (copy template
                :cpp-arguments
                (copy args :children nil)))
       (error ()
         (fail))))
    (otherwise (call-next-method))))

(defmethod strip-template-arguments ((template cpp-template-type))
  (match template
    ((cpp-template-type
      (cpp-arguments args))
     (handler-case
         (source-text
          (copy template
                :cpp-arguments
                (copy args :children nil)))
       (error ()
         (fail))))
    (otherwise (call-next-method))))

(defmethod strip-template-arguments ((ast cpp-ast))
  (strip-template-arguments (source-text ast)))


;;; Methods common to all software objects


;;;; Methods for tree-sitter generics

(defmethod get-declaration-ids ((type (eql :variable))
                                (ast cpp-field-identifier))
  "When asked to resolve `this->AST', resolve it from a field."
  (match (get-parent-ast (attrs-root*) ast)
    ;; Resolve this->x to the field.
    ((and parent
          (cpp-field-expression
           (cpp-argument (cpp-this))
           (cpp-operator (cpp-->))
           (cpp-field (eql ast))))
     (get-declaration-ids type parent))
    (otherwise
     (call-next-method))))

(defmethod get-declaration-ids ((type (eql :function))
                                (fn cpp-function-definition))
  "Get the field declaration of an external function definition."
  (let ((name (definition-name-ast fn)))
    (if (typep name 'cpp-qualified-identifier)
        (if-let (class (friend-function-class fn))
          (lookup-in-field-table class :function (cpp-name name))
          (call-next-method))
        (call-next-method))))

(defmethod get-declaration-ids ((type (eql :type)) (ast cpp-template-type))
  (get-declaration-ids type (cpp-name ast)))

(defmethod relevant-declaration-type ((ast cpp-operator-name))
  'function-declaration-ast)

(defmethod relevant-declaration-type ((ast cpp-preproc-function-def))
  'macro-declaration-ast)

(defmethod call-name ((ast cpp-call-expression))
  "If the call function is a template function, extract just the name of the template function without its arguments."
  (source-text
   (let ((function (call-function ast)))
     (if (typep function 'cpp-template-function)
         (cpp-name function)
         function))))
(defmethod call-name ((ast cpp-call-expression))
  "If the call function is a template function, extract just the name of the template function without its arguments."
  (source-text
   (let ((function (call-function ast)))
     (if (typep function 'cpp-template-function)
         (cpp-name function)
         function))))

(defmethod function-parameters ((field cpp-field-declaration))
  (match field
    ((cpp-field-declaration
      (cpp-declarator
       (list (and declarator (cpp-function-declarator)))))
     (function-parameters declarator))
    (otherwise (call-next-method))))

(defmethod function-parameters ((fn cpp-lambda-expression))
  (when-let (d (cpp-declarator fn))
    (cpp-parameters d)))

(defmethod definition-name-ast ((field cpp-field-declaration))
  (match field
    ((cpp-field-declaration
      (cpp-declarator
       (list (and declarator (cpp-function-declarator)))))
     (declarator-name-ast declarator))
    (otherwise (call-next-method))))

(defmethod function-name ((field cpp-field-declaration))
  (when-let (ast (definition-name-ast field))
    (source-text ast)))

(defmethod relevant-declaration-type ((field cpp-field-declaration))
  (match field
    ((cpp-field-declaration
      (cpp-declarator
       (list (cpp-function-declarator))))
     'function-declaration-ast)
    (otherwise (call-next-method))))

(defmethod outer-declarations ((field cpp-field-declaration))
  (match field
    ;; Functions.
    ((cpp-field-declaration
      (cpp-declarator
       (list (and declarator (cpp-function-declarator)))))
     (outer-declarations declarator))
    ((access #'extract-nested-class
             (and type (type ast)))
     (outer-declarations type))
    (otherwise (call-next-method))))

(defmethod outer-declarations ((spec cpp-enum-specifier))
  (match spec
    ((cpp-enum-specifier
      (cpp-scope (or (cpp-struct) (cpp-class)))
      (cpp-name name)
      (cpp-body nil))
     (values (list name) '(:type)))
    (otherwise
     (call-next-method))))

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

(defmethod parameter-name ((ast cpp-variadic-type-parameter-declaration))
  (if (parameter-names ast)
      (call-next-method)
      "..."))

(defmethod parameter-name ((ast cpp-variadic-declaration))
  (if (parameter-names ast)
      (call-next-method)
      "..."))

(defmethod get-declaration-ids ((ns (eql :tag)) (ast cpp-ast))
  "Merge the tag and type namespaces for C++."
  (get-declaration-ids :type ast))

(defmethod outer-declarations :context ((ast cpp-ast))
  "Merge the tag and type namespaces for C++."
  (multiple-value-bind (decls types)
      (call-next-method)
    (values decls (substitute :type :tag types))))

(defmethod inner-declarations :context ((ast cpp-ast))
  "Merge the tag and type namespaces for C++."
  (multiple-value-bind (decls types)
      (call-next-method)
    (values decls (substitute :type :tag types))))

(defgeneric extract-nested-class (ast)
  (:method ((ast t)) nil)
  (:method ((decl cpp-field-declaration))
    (when (typep decl 'cpp-field-declaration)
      (let ((type (cpp-type decl)))
        (when (typep type 'type-declaration-ast)
          type)))))

(defun nested-classes (ast)
  "Extract nested class definitions in AST."
  (when-let (body (cpp-body ast))
    (filter #'extract-nested-class
            (children body))))

(defun add-nested-class-declarations (ast decls types)
  "Add nested class definitions in AST to DECLS and TYPES."
  (multiple-value-bind (nested-decls nested-types)
      (outer-declarations-merge (nested-classes ast))
    (values (append nested-decls decls)
            (append nested-types types))))

(defmethod inner-declarations :around ((ast cpp-class-specifier))
  (multiple-value-bind (decls types) (call-next-method)
    (add-nested-class-declarations ast decls types)))

(defmethod outer-declarations :around ((ast cpp-class-specifier))
  (multiple-value-bind (decls types) (call-next-method)
    (add-nested-class-declarations ast decls types)))

(defmethod inner-declarations :around ((ast cpp-struct-specifier))
  (multiple-value-bind (decls types) (call-next-method)
    (add-nested-class-declarations ast decls types)))

(defmethod outer-declarations :around ((ast cpp-struct-specifier))
  (multiple-value-bind (decls types) (call-next-method)
    (add-nested-class-declarations ast decls types)))

(defmethod outer-declarations ((ast cpp-alias-declaration))
  (values (list (cpp-name ast)) '(:type)))

(defun template-parameter-types (template)
  (let ((params (cpp-parameters template)))
    (mappend (named-lambda param-name (param)
               (typecase param
                 (cpp-type-parameter-declaration
                  (when-let (name (first (direct-children param)) )
                    (list name)))
                 (cpp-optional-type-parameter-declaration
                  (when-let (name (cpp-name param))
                    (list name)))
                 (cpp-variadic-type-parameter-declaration
                  (when-let (name (first (direct-children param)))
                    (list name)))
                 (cpp-template-template-parameter-declaration
                  (mappend #'param-name (children param)))))
             (children params))))

(defmethod inner-declarations ((ast cpp-template-declaration))
  (let* ((types (template-parameter-types ast)))
    (values types
            (mapcar (constantly :type) types))))

(deftype member-access ()
  '(member :public :private :protected))

(defgeneric static-member-access (ast)
  (:documentation "If AST changes the member access, return the new member access.")
  (:method-combination standard/context)
  (:method :context ((ast cpp-ast))
    (assure (or null member-access)
      (call-next-method)))
  (:method ((ast cpp-namespace-definition))
    (if (cpp-name ast)
        :public
        :private))
  (:method ((ast cpp-access-specifier))
    (etypecase (cpp-keyword ast)
      (cpp-public :public)
      (cpp-private :private)
      (cpp-protected :protected)))
  (:method ((ast cpp-struct-specifier))
    :public)
  (:method ((ast cpp-class-specifier))
    :private)
  (:method ((ast cpp-union-specifier))
    :public)
  (:method ((ast t))
    nil))

(def-attr-fun member-access (in)
  (:documentation "Compute the member access of an AST.
Returns one of `:public', `:private', `:protected'.

Member access can be inherited from a parent (`class' vs. `struct') or
from a prior sibling \(`public:', `private:', `protected:').")
  (:method :context ((ast cpp-ast) &optional in)
    (declare (ignore in))
    (assure member-access
      (call-next-method)))
  (:method ((software software) &optional in)
    (member-access (genome software) in)
    in)
  (:method ((ast ast) &optional in)
    (dolist (child (children ast))
      (member-access child in))
    in)
  (:method ((node cpp-ast) &optional in)
    ;; Member access can be determined by the parent or by a preceding
    ;; sibling.
    (lret ((result (or (static-member-access node) in)))
      (reduce (lambda (in next)
                (member-access next in))
              (children node)
              :initial-value result))))

(defmethod attr-missing ((fn-name (eql 'member-access)) node)
  (member-access (genome (attrs-root*)) :public))

(defun public? (ast)
  (eql :public (member-access ast)))

(defun private? (ast)
  (eql :private (member-access ast)))

(defun protected? (ast)
  (eql :protected (member-access ast)))

(defmethod field-table ((typedef cpp-type-definition))
  "Given a typedef for a template type, recursively resolve the
templated definition's field table."
  (match typedef
    ((cpp-type-definition
      (cpp-type (and type (cpp-template-type))))
     (when-let (class (get-declaration-ast :type type))
       (field-table class)))
    (otherwise (call-next-method))))

(defmethod outer-declarations ((ast cpp-template-declaration))
  ;; TODO Store the template parameters somehow in the symbol table?
  (when-let ((definitions (filter (of-type '(or definition-ast declaration-ast))
                                  (direct-children ast))))
    (outer-declarations (only-elt definitions))))

(defmethod outer-declarations ((ast cpp-namespace-definition))
  (match ast
    ((cpp-namespace-definition
      (cpp-body
       (cpp-declaration-list (direct-children children))))
     (outer-declarations-merge children))))

(defmethod outer-defs ((ast cpp-namespace-definition))
  ;; Needed to conserve exports.
  (match ast
    ((cpp-namespace-definition
      (cpp-body
       (cpp-declaration-list (direct-children children))))
     (reduce (op (symbol-table-union ast _ _))
             (mapcar #'outer-defs children)))))

(defun const-field-declaration? (field-decl fn)
  "Is FN declared const in FIELD-DECL?"
  (match field-decl
    ;; Function definitions in field declaration lists aren't wrapped
    ;; with field-declaration ASTs.
    ((cpp-function-definition)
     (unless (source-text= (definition-name-ast field-decl) fn)
       (fail))
     (member "const"
             (specifier
              (canonicalize-type field-decl :software (attrs-root*)))
             :test #'source-text=))
    ;; TODO canonicalize-type needs to be extended for C++ to handle type
    ;; qualifiers on method function declarators.
    ((cpp-field-declaration
      (cpp-declarator declarator/s))
     (let ((declarators (ensure-list declarator/s)))
       (iter (for field-decl in declarators)
             (match field-decl
               ((and
                 (cpp-function-declarator
                  (cpp-declarator (source-text= fn)))
                 ;; TODO Should the type-qualifiers be in a
                 ;; post-specifiers slot?
                 (access #'direct-children children))
                (thereis
                 (some (op (match _ ((cpp-type-qualifier :text "const") t)))
                       children)))))))))

(defun declared-const? (ast &key (software (attrs-root*)))
  "Is AST declared const?"
  (member "const"
          (specifier
           (canonicalize-type ast :software software))
          :test #'source-text=))

(defmethod resolve-overloads ((type t) (ast cpp-field-expression) &optional overloads)
  "Resolve const overloads on field expressions."
  (or (when (every (of-type 'cpp-field-declaration) overloads)
        (match ast
          ((cpp-field-expression
            (cpp-argument (and arg (cpp-identifier)))
            (cpp-field (and field (cpp-field-identifier)))
            (cpp-operator (source-text= ".")))
           (when-let (decl (get-declaration-ast :variable arg))
             (mvlet* ((const? (declared-const? decl))
                      (const-overloads
                       mutable-overloads
                       (partition (op (const-field-declaration? _ field))
                                  overloads))
                      (relevant-overloads
                       (if const?
                           const-overloads
                           mutable-overloads)))
               (cond ((null relevant-overloads)
                      (error "Invalid partitioning of overloads: ~a"
                             overloads))
                     ((single relevant-overloads)
                      (only-elt relevant-overloads))
                     (t (if (length= relevant-overloads overloads)
                            (call-next-method)
                            (resolve-overloads type ast relevant-overloads)))))))))
      (call-next-method)))

(defmethod resolve-overloads (type (ast cpp-qualified-identifier)
                              &optional overloads)
  (resolve-overloads type (cpp-name ast) overloads))

(defmethod resolve-overloads ((type (eql :type)) (ast cpp-template-type)
                              &optional overloads)
  ;; TODO Implement SFINAE rules.
  (let ((alist (mapcar (op (cons (definition-name _1) _1)) overloads)))
    (or (aget (source-text ast) alist :test #'equal)
        ;; The base template should have the shortest name (e.g.
        ;; `vector<T>` vs. `vector<boolean>`.
        (cdr (extremum alist #'length<= :key #'car)))))

(defmethod resolve-declaration-type ((decl cpp-ast)
                                     (ast cpp-ast)
                                     &aux (obj (attrs-root*)))
  (when-let (first-try (call-next-method))
    (or
     ;; If the first try is not auto, just return it.
     (unless (placeholder-type-p first-try)
       first-try)
     ;; If there is a surrounding init declarator, infer the type from
     ;; its RHS.
     (when-let (init
                (find-if (of-type 'c/cpp-init-declarator)
                         (get-parent-asts obj decl)))
       (when (or (eql decl init)
                 (ancestor-of-p obj decl (lhs init)))
         (infer-type (rhs init))))
     (match decl
       ((cpp-declaration
         (cpp-declarator (eql ast))
         (cpp-type type)
         (cpp-value value))
        (if (placeholder-type-p type)
            (infer-type value)
            type))
       ((cpp-declaration
         (cpp-declarator (and decls (type list))))
        (dolist (decl decls)
          (when (and (typep decl 'c/cpp-init-declarator)
                     (source-text= (lhs decl) ast))
            (return (infer-type (rhs decl)))))))
     ;; Go with the original result.
     first-try)))

(defmethod resolve-declaration-type :around ((decl cpp-field-declaration)
                                             (ast call-ast))
  "If AST is a call AST, and the declaration is a field declaration,
then the return type of the call is the return type of the field."
  (match ast
    ((call-ast
      (call-function
       (and field (cpp-field-expression))))
     (resolve-declaration-type decl field))
    (otherwise (call-next-method))))

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

(defmethod deref-type ((type cpp-qualified-identifier))
  ;; TODO This needs to be generalized.
  (let ((parts (qualified-name->list type)))
    (if (member (lastcar parts) '("iterator" "const_iterator")
                :test #'source-text=)
        (resolve-container-element-type type)
        (call-next-method))))

(defmethod deref-type ((type cpp-type-descriptor))
  "Dereference a reference type."
  (match type
    ((cpp-type-descriptor
      (cpp-declarator
       (cpp-abstract-reference-declarator))
      (cpp-type type))
     type)
    (otherwise (call-next-method))))

(defmethod expression-type ((ast cpp-compound-literal-expression))
  (cpp-type ast))

;;; Do I need to do something special to put this in the std namespace?
(defmethod expression-type ((ast cpp-sizeof-expression))
  ;; TODO -- check if std is visible in the namespace and use std::size_t if not
  (make 'cpp-primitive-type :text "size_t"))

(defmethod expression-type ((ast cpp-true))
  (make 'cpp-primitive-type :text "bool"))

(defmethod expression-type ((ast cpp-false))
  (make 'cpp-primitive-type :text "bool"))

(defmethod resolve-declaration-type ((decl-ast cpp-type-parameter-declaration)
                                     (ast t))
  (second (children ast)))

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

(defmethod infer-expression-type ((ast cpp-call-expression))
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
     (unless (equal (mapcar #'source-text (namespace-qualifiers name))
                    '("std"))
       (trivia.fail:fail))
     (infer-type arg))
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

(defmethod expression-type ((ast cpp-string-literal))
  ;; TODO: handle unicode types.
  (make 'cpp-type-descriptor
        :cpp-declarator (make 'cpp-abstract-array-declarator
                              :cpp-size
                              (make 'cpp-number-literal
                                    :text (princ-to-string
                                           (assure (integer 0)
                                             ;; -2 for the quotes, 1+
                                             ;; -for the null.
                                             (- (length (text ast)) 1)))))
        :cpp-type (make 'cpp-primitive-type :text "char")
        :cpp-pre-type-qualifiers
        (list
         (make 'cpp-type-qualifier :text "const"
                                   :after-text " "))))

(defmethod placeholder-type-p ((ast cpp-auto))
  t)

(defmethod placeholder-type-p ((ast cpp-placeholder-type-specifier))
  t)

(defmethod infer-type :context ((ast cpp-ast))
  (match (call-next-method)
    ;; Unwrap trivial type descriptors.
    ((cpp-type-descriptor
      (cpp-type (and type (cpp-primitive-type)))
      (cpp-declarator nil))
     type)
    ;; Should be "$TYPE1<@ARGS>::$TYPE2", but phasing.
    ((cpp-qualified-identifier
      :cpp-name type2
      :cpp-scope
      (cpp-template-type
       :cpp-name type1
       :cpp-arguments
       (cpp-template-argument-list
        :children args)))
     (when-let* ((type-def (get-declaration-ast :type type1))
                 (template
                  (find-enclosing 'cpp-template-declaration
                                  (attrs-root*)
                                  type-def))
                 (template-param-names
                  (mapcar #'parameter-name (cpp-parameters template)))
                 (offset
                  (position type2 template-param-names
                            :test #'source-text=)))
       (nth offset args)))
    (result result)))

(defmethod infer-type :context ((id cpp-identifier))
  "When computing the type of a C++ identifier, if the identifier is
evaluated (in an expression AST) then implicitly dereference reference
types."
  (match (call-next-method)
    ((and type
          (cpp-type-descriptor
           (cpp-declarator (cpp-abstract-reference-declarator))))
     (if (or (find-if (of-type 'expression-ast)
                      (get-parent-asts* (attrs-root*) id))
             (typep (get-parent-ast (attrs-root*) id)
                    'cpp-expression-statement))
         (deref-type type)
         (fail)))
    (result result)))

(defmethod infer-type :around ((ast cpp-field-expression))
  (let* ((field-type (call-next-method))
         (arg-type (infer-type (cpp-argument ast))))
    (if (and arg-type field-type)
        (let ((field-ns (namespace field-type)))
          (if (equal field-ns (qualify-declared-ast-name arg-type))
              ;; If the type of the argument is (modulo template
              ;; arguments) the same as the namespace of the field
              ;; type, then we synthesize a new AST from both of them
              ;; with template arguments intact.
              (lret ((qname
                      (list->qualified-name
                       (append (qualified-name->list arg-type)
                               (qualified-name->list field-type)))))
                (setf (attr-proxy qname) field-type))
              field-type))
        field-type)))

(defmethod infer-expression-type ((ast cpp-initializer-list))
  (match (get-parent-ast (attrs-root*) ast)
    ((cpp-compound-literal-expression
      (cpp-type type))
     type)
    (otherwise (call-next-method))))

(defmethod infer-expression-type :around ((ast cpp-initializer-list))
  (or (call-next-method)
      (infer-type-as-c/cpp-expression (attrs-root*) ast)))

;;; TODO Also use this to look up free variables (not just this) in a
;;; function.
(defun friend-function-class (fn)
  (match fn
    ((and (cpp-function-definition)
          (access #'definition-name-ast
                  (and id (cpp-qualified-identifier))))
     (get-declaration-ast
      :type
      (list->qualified-name
       (butlast
        (qualified-name->list id)))))))

(defmethod infer-expression-type ((ast cpp-this) &aux (obj (attrs-root*)))
  (if-let (type-ast (find-enclosing 'type-declaration-ast obj ast))
    (definition-name-ast type-ast)
    ;; Infer type of this for a friend function.
    (when-let (fn (find-enclosing 'cpp-function-definition obj ast))
      (friend-function-class fn))))

(defgeneric qualified-name->list (ast)
  ;; TODO Qualified type and field identifiers.
  (:method ((ast cpp-ast))
    (list ast))
  (:method ((ast cpp-qualified-identifier))
    (cons (cpp-scope ast)
          (qualified-name->list (cpp-name ast)))))

(define-condition unqualifiable-ast-error (error)
  ((asts :initarg :asts :type (soft-list-of ast)
         :reader unqualifiable-ast-error.asts)
   (error :initarg :error :type error))
  (:report (lambda (c s)
             (with-slots (asts error) c
               (format s "Cannot compose qualified identifier from ~s because:~%~a"
                       asts
                       error)))))

(defmethod convert ((to (eql 'cpp-identifier))
                    (id identifier-ast)
                    &key)
  (convert-terminal to id))

(defmethod convert ((to (eql 'cpp-type-identifier))
                    (id identifier-ast)
                    &key)
  (convert-terminal to id))

(defmethod convert ((to (eql 'cpp-namespace-identifier))
                    (id identifier-ast)
                    &key)
  (convert-terminal to id))

(defmethod convert ((to (eql 'cpp-primitive-type))
                    (id identifier-ast)
                    &key)
  (convert-terminal to id))

(-> list->qualified-name ((soft-list-of cpp-ast))
    (values cpp-ast &optional))
(defun list->qualified-name (list)
  "Compose a cpp-qualified-identifier from LIST, a list of identifiers.

Since the tree-sitter grammar treats the same text differently
depending on which side of the :: of a qualified name it appears on,
this involves handling some translations between types."
  (when (null list)
    (error "Empty lists cannot become qualified names!"))
  (labels ((type-id->ns-id (type-id)
             "Create a namespace identifier from a text identifier."
             (lret ((ns-id (convert 'cpp-namespace-identifier type-id)))
               (setf (attr-proxy ns-id) type-id)))
           (ns-id->type-id (ns-id)
             "Create a type identifier from a namespace identifier."
             (lret ((type-id (convert 'cpp-type-identifier ns-id)))
               (setf (attr-proxy type-id) ns-id)))
           (dependent-type->dependent-name (dtype)
             "Create a cpp-dependent-name from a cpp-dependent-type."
             ;; TODO cpp-dependent-type is an alias for
             ;; cpp-dependent-name; should this be automatic?
             (lret ((dname (change-class (copy dtype) 'cpp-dependent-name)))
               (setf (attr-proxy dname) dtype)))
           (fix-scope (scope)
             "Make sure SCOPE is an AST that can appear in the scope
              slot of a qualified identifier."
             (etypecase scope
               (cpp-type-identifier
                (type-id->ns-id scope))
               (cpp-dependent-type
                (dependent-type->dependent-name scope))
               ((or cpp-namespace-identifier
                    cpp-template-type
                    cpp-dependent-name)
                scope)))
           (fix-name (name)
             "Make sure NAME is an AST that can appear in the name
              slot of a qualified identifier."
             (etypecase name
               (cpp-namespace-identifier
                (ns-id->type-id name))
               (cpp-dependent-type
                (dependent-type->dependent-name name))
               (cpp-type-descriptor
                (cpp-type name))
               ((or cpp-dependent-name
                    cpp-type-identifier
                    cpp-qualified-identifier
                    cpp-template-function
                    cpp-identifier
                    cpp-operator-name
                    cpp-destructor-name)
                name)))
           (qualify (list)
             "Right-fold LIST into a qualified name."
             (reduce (lambda (scope name)
                       (cond ((typep name 'cpp-primitive-type) name)
                             ((no scope) name)
                             (t (make 'cpp-qualified-identifier
                                      :cpp-scope (fix-scope scope)
                                      :cpp-name (fix-name name)))))
                     list
                     :from-end t))
           (check-result (result-ast)
             "Check that RESULT-AST is printable."
             (prog1 nil
               (source-text result-ast))))
    (restart-case
        (handler-bind ((error
                        (lambda (e)
                          (error 'unqualifiable-ast-error
                                 :asts list
                                 :error e))))
          (lret ((result (qualify list)))
            ;; Check that it's valid.
            (check-result result)))
      (continue ()
        :report "Drop the first element"
        :test (lambda (c)
                (declare (ignore c))
                (rest list))
        (return-from list->qualified-name
          (list->qualified-name (rest list)))))))

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

(defgeneric implicit-namespace-qualifiers (ast)
  (:documentation "Namespace qualifiers derived from surrounding namespaces.")
  (:method ((ast cpp-ast))
    (split "::" (namespace ast))))

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
  (remove-if
   (conjoin #'stringp #'emptyp)
   (if-let ((tail (member :global explicit)))
     (rest tail)
     (if explicit
         (let ((index (search explicit implicit
                              :key #'source-text
                              :test #'equal
                              :from-end t)))
           (append (take (or index 0) implicit)
                   explicit))
         implicit))))

(defgeneric namespace-qualifiers (ast)
  (:documentation "Final namespace qualifiers, derived by resolving
  explicit (relative) namespace qualifiers relative to
  implicit (absolute) ones.")
  (:method ((ast cpp-ast))
    (combine-namespace-qualifiers
     (explicit-namespace-qualifiers ast)
     (implicit-namespace-qualifiers ast))))

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
  (:method ((ast cpp-nested-namespace-specifier))
    (car (children ast))))

(defmethod initializer-aliasee ((sw t)
                                (lhs cpp-reference-declarator)
                                (rhs cpp-pointer-expression))
  (with-attr-table sw
    (if (typep (cpp-operator rhs) 'cpp-*)
        (aliasee (cpp-argument rhs))
        (call-next-method))))

(defmethod initializer-aliasee ((sw t) (lhs cpp-reference-declarator) rhs)
  (with-attr-table sw
    (aliasee rhs)))

(defmethod wrap-type-descriptor ((d cpp-pointer-declarator) type)
  (make 'cpp-type-descriptor
        :cpp-declarator (make 'cpp-abstract-pointer-declarator)
        :cpp-type type))

(defmethod wrap-type-descriptor ((d cpp-array-declarator) type)
  (make 'cpp-type-descriptor
        :cpp-declarator (make 'cpp-abstract-array-declarator
                              :cpp-size (cpp-size d))
        :cpp-type type))

(defmethod wrap-type-descriptor ((d cpp-reference-declarator) type)
  ;; type
  (make 'cpp-type-descriptor
        :cpp-declarator (make 'cpp-abstract-reference-declarator
                              :text "&")
        :cpp-type type))


;;; Whitespace rules

(define-empty-whitespace-methods (:style c-style-indentation)
  cpp-namespace-identifier :|::|
  :|::| cpp-qualified-identifier
  :|::| cpp-type-identifier
  :|::| cpp-identifier
  cpp-type-identifier cpp-template-argument-list
  :< cpp-type-descriptor
  cpp-type-descriptor :>
  cpp-template-type :|::|
  cpp-identifier cpp-template-argument-list
  cpp-template-function cpp-argument-list
  :|:| cpp-module-name)

(defmethod whitespace-between/parent ((parent cpp-namespace-definition)
                                      (style c-style-indentation)
                                      (x ast)
                                      (y ast))
  #\Newline)


;;; Namespace Attr

(defun handle-namespace-as-scope (ast in)
  "Propagate a new namespace based on the `cpp-name' of AST to AST's
children."
  (let* ((name (source-text (cpp-name ast)))
         (out (cond
                ;; E.g. an anonymous namespace.
                ((emptyp name) in)
                ((emptyp in) name)
                (t (string+ in "::" name)))))
    ;; Prevent e.g. std::list::list in the symbol table.
    (unless (emptyp name)
      (namespace (cpp-name ast) in))
    (mapc (op (namespace _ out))
          (children ast))
    in))

(defmethod namespace ((ast cpp-namespace-definition)
                      &optional in)
  ;; NOTE: tree-sitter-cpp doesn't currently handle
  ;;       inline namespaces
  ;; TODO: look at implicit namespaces and incorporate or factor
  ;;       out what is needed from there.
  (handle-namespace-as-scope ast in))

(defmethod namespace ((ast cpp-class-specifier)
                      &optional in)
  (handle-namespace-as-scope ast in))

(defmethod namespace ((ast cpp-struct-specifier)
                      &optional in)
  (handle-namespace-as-scope ast in))

(defmethod namespace ((ast cpp-enum-specifier)
                      &optional in)
  (if (not (cpp-scope ast))
      (call-next-method)
      (handle-namespace-as-scope ast in)))

(defvar-unbound *initial-namespace*
  "Bound by a qualified identifier to store the initial namespace.")

(defmethod namespace ((ast cpp-qualified-identifier) &optional in)
  "Handle two quirks of qualified identifiers:

1. No scope (e.g. `::x`) means the global scope.

2. The RHS is in the namespace of the LHS, unless the RHS is a
template function, in which case its arguments are in whatever IN was
for the outermost qualified namespace."
  (if (null (cpp-scope ast))
      (progn
        (namespace (cpp-name ast) "")
        "")
      (let* ((*initial-namespace*
              (or (bound-value '*initial-namespace*)
                  in))
             (ns (source-text (cpp-scope ast)))
             (out (if (emptyp in)
                      ns
                      (string+ in "::" ns))))
        (declare (special initial*))
        (namespace (cpp-scope ast) in)
        (namespace (cpp-name ast) out)
        in)))

(defmethod namespace ((ast cpp-template-type) &optional in)
  "Don't qualify cpp-template-type arguments."
  (namespace (cpp-name ast) in)
  (let ((in (or (bound-value '*initial-namespace*) in)))
    (namespace (cpp-arguments ast) in))
  in)

(defmethod namespace ((ast cpp-template-function) &optional in)
  "Don't qualify cpp-template-function arguments."
  (namespace (cpp-name ast) in)
  (let ((in (or (bound-value '*initial-namespace*) in)))
    (namespace (cpp-arguments ast) in))
  in)

(defmethod namespace ((ast cpp-preproc-def) &optional in)
  (declare (ignore in))
  (call-next-method ast ""))

(defmethod namespace ((ast cpp-preproc-function-def) &optional in)
  (declare (ignore in))
  (call-next-method ast ""))


;;; Symbol Table

(def +cpp-multi-declaration-keys+ '(:function :type)
  "A set of keys which indicate that several definitions for a symbol may be
available to use at any point in a C++ AST.")

(defmethod multi-declaration-keys ((root cpp-ast)) +cpp-multi-declaration-keys+)

(defmethod symbol-table ((ast cpp-ast) &optional in)
  ;; This shadows the handling of scope ASTs in the default method for
  ;; functional-tree-ast.
  (if (scope-ast-p ast)
      (propagate-exports-up (propagate-declarations-down ast in)
                            in)
      (call-next-method)))

(defun propagate-exports-up (scope-final in)
  (let ((new-exports (@ scope-final :export)))
    (if (no new-exports) in
        (with in :export new-exports))))

(defmethod symbol-table ((node cpp-namespace-definition) &optional in)
  (propagate-declarations-down node in))

(defmethod qualify-declared-ast-name :around ((ast cpp-ast))
  ;; Strip template parameters for lookup.
  (strip-template-arguments (call-next-method)))

(defmethod namespace :around ((ast cpp-ast) &optional in)
  (declare (ignore in))
  ;; Strip template parameters for lookup.
  (let ((result (call-next-method)))
    (if (stringp result)
        (strip-template-arguments result)
        result)))

(defmethod qualify-declared-ast-name ((declared-ast cpp-ast))
  (let* ((source-text
          (or (declarator-name declared-ast)
              (source-text declared-ast)))
         (namespace (namespace declared-ast))
         (implicit (split "::" namespace))
         (parts (split "::" source-text))
         (explicit
          (append
           (and (string^= "::" source-text)
                (list :global))
           (butlast parts)))
         (combined
          (combine-namespace-qualifiers explicit implicit)))
    (string-join (append1 combined (lastcar parts))
                 "::")))

(defmethod qualify-declared-ast-names ((declared-ast cpp-ast))
  "E.g. x::y::z becomes `'(\"x::y::z\", \"x::z\", \"z\")'."
  (qualified-name-variants (qualify-declared-ast-name declared-ast)))

(defmethod qualify-declared-ast-name ((id cpp-type-identifier))
  (or (and-let* ((type (find-enclosing 'type-declaration-ast (attrs-root*) id))
                 (type-name (definition-name-ast type))
                 ((not (eql type-name id)))
                 ((source-text= type-name id)))
        (qualify-declared-ast-name type-name))
      (call-next-method)))

(defmethod inner-declarations ((fn cpp-lambda-expression))
  (let ((result (call-next-method)))
    (values result
            (mapcar (constantly :variable) result))))

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


;;; Modules

;;; Model the kinds of module units as a class hierarchy.

(defgeneric module-unit-full-name (module))

(defclass module-unit ()
  ((declaration :initarg :declaration :type ast :reader module-unit-declaration)
   (module-name :type string :initarg :module-name :reader module-unit-module-name))
  (:documentation "A translation unit with a module declaration.")
  (:metaclass abstract-class))

(defmethod module-unit-full-name ((m module-unit))
  (module-unit-module-name m))

(defmethod print-object ((self module-unit) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (module-unit-full-name self))))

(defclass importable-module-unit (module-unit)
  ()
  (:documentation "A module unit that can be imported (by clang).")
  (:metaclass abstract-class))

(defclass module-interface-unit (importable-module-unit)
  ()
  (:documentation "An exported module unit.")
  (:metaclass abstract-class))

(defclass implementation-unit (module-unit)
  ()
  (:metaclass abstract-class))

(defclass module-partition-unit (module-unit)
  ((partition-name
    :type string
    :initarg :partition-name
    :reader module-unit-partition-name))
  (:metaclass abstract-class))

(defmethod module-unit-full-name ((p module-partition-unit))
  (fmt "~a:~a"
       (module-unit-module-name p)
       (module-unit-partition-name p)))

(defclass primary-module-interface-unit (module-interface-unit)
  ()
  (:documentation "Module interface unit that is not a partition unit."))

(defclass anonymous-implementation-unit (implementation-unit)
  ()
  (:documentation "A module implementation unit that is not a partition."))

(defclass module-partition-interface-unit
    (module-partition-unit module-interface-unit)
  ())

(defclass module-partition-implementation-unit
    (importable-module-unit module-partition-unit implementation-unit)
  ())

(defun module? (ast)
  "If AST is a module, return its classification."
  (when-let (decl
             (find-if (of-type 'cpp-module-declaration)
                      (genome ast)))
    (classify-module-declaration decl)))

(defun classify-module-declaration (decl)
  (let* ((exported? (find-if (of-type 'cpp-export-specifier) (direct-children decl)))
         (name-ast (find-if (of-type 'cpp-module-qualified-name) decl))
         (name (source-text name-ast))
         (partition-name-ast
          (find-if (of-type 'cpp-module-partition) decl))
         (partition-name
          (source-text partition-name-ast))
         (partition? (string^= ":" partition-name))
         (class (eif partition?
                     (eif exported?
                          (find-class 'module-partition-interface-unit)
                          (find-class 'module-partition-implementation-unit))
                     (eif exported?
                          (find-class 'primary-module-interface-unit)
                          (find-class 'anonymous-implementation-unit)))))
    (multiple-value-call #'make class
      :declaration decl
      :module-name name
      (if partition?
          (values :partition-name (drop-prefix ":" partition-name))
          (values)))))

(defun exported? (ast &key (check-decls t))
  "Is AST exported?

AST is exported if it has an export specifier, or is
within an export block or an exported namespace."
  (labels ((directly-exported? (ast)
             (find-if (of-type 'cpp-export-specifier)
                      (direct-children ast)))
           (exported-from-parents? (ast)
             (when-let ((decl (find-enclosing 'declaration-ast (attrs-root*) ast)))
               (or (directly-exported? ast)
                   (match (get-parent-asts* (attrs-root*) ast)
                     ((list* (cpp-declaration-list)
                             (cpp-export-block) _)
                      t)
                     ((list* (cpp-declaration-list)
                             (and (cpp-namespace-definition)
                                  (not (satisfies cpp-name)))
                             _)
                      nil)
                     ((list* (cpp-declaration-list)
                             (and ns (cpp-namespace-definition))
                             (cpp-declaration-list)
                             (cpp-export-block)
                             _)
                      (cpp-name ns))
                     ((list* (cpp-declaration-list)
                             (and ns (cpp-namespace-definition)) _)
                      (directly-exported? ns))))))
           (exported-from-declaration? (ast)
             (when-let* ((decl-type (relevant-declaration-type ast))
                         (decl (get-declaration-ast decl-type ast)))
               (unless (eql decl ast)
                 (cond ((typep decl 'cpp-declaration)
                        ;; The declaration is exported.
                        (exported? decl))
                       ((typep decl 'cpp-field-declaration)
                        (and (public? decl)
                             (exported? (find-enclosing 'class-ast (attrs-root*)
                                                        decl)))))))))
    (or (directly-exported? ast)
        (exported-from-parents? ast)
        (when check-decls
          (exported-from-declaration? ast)))))

(defmethod symbol-table-union ((root cpp-ast) table-1 table-2 &key &allow-other-keys)
  "Recursively union the maps under the :export key, if there are any."
  (let* ((exports1 (@ table-1 :export))
         (exports2 (@ table-2 :export)))
    (if (nor exports1 exports2) (call-next-method)
        (with (call-next-method root
                                (less table-1 :export)
                                (less table-2 :export))
              :export
              (if (and exports1 exports2)
                  (symbol-table-union root exports1 exports2)
                  (or exports1 exports2))))))

(defun handle-exports (ast outer-defs)
  "Conditionally mark the outer definitions of AST as exports."
  (if (exported? ast :check-decls nil)
      (if (empty? outer-defs) outer-defs
          (if (@ outer-defs :export)
              (error "Already has exports")
              (with outer-defs :export outer-defs)))
      outer-defs))

(defmethod outer-defs :around ((ast cpp-exportable-ast))
  (handle-exports ast (call-next-method)))

) ; #+:TREE-SITTER-CPP
