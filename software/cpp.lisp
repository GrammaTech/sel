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

(eval-always
  (uiop:add-package-local-nickname
   :cpp :software-evolution-library/software/cpp))

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

(defconst +smart-pointer-type-names+
  '("unique_ptr" "shared_ptr" "weak_ptr" "auto_ptr"))

(defun smart-pointer-type-name? (string)
  "Is STRING the name of a smart pointer class?"
  (declare (string string))
  (string-case string
    (#.+smart-pointer-type-names+ t)
    (t nil)))

(define-language-alias-mappings
    cpp ("c plus plus" "c++" "c-plus-plus" "cc" "cp" "cpp" "cxx" "hpp"
         ;; Module unit extensions. Visual Studio uses .ixx, Clang
         ;; uses the extensions ending with -m.
         "ixx" "cppm" "ccm" "cxxm" "c++m"))

(deftype private () '(eql :private))
(deftype protected () '(eql :protected))
(deftype public () '(eql :public))

(deftype member-access ()
  "Whether a member is public, private, or protected.
`nil' means the member is not visible at all."
  '(or null public private protected))

(defgeneric strip-template-arguments (template)
  (:documentation "Strip template arguments (in angle brackets) from TEMPLATE.")
  (:method ((string string))
    (with-string-dispatch () string
      (if (not (position #\< string)) string
          (nlet rec ((pos 0)
                     (bracket-count 0)
                     (acc nil))
            (declare (array-index pos)
                     ;; bracket-count is not an index since it can go
                     ;; below 0 in pathalogical cases.
                     (fixnum bracket-count)
                     (list acc))
            (if (length>= pos string)
                (if (> bracket-count 0)
                    ;; Not actually delimiters. E.g. operator<.
                    string
                    (let ((s (make-array (length acc) :element-type 'character)))
                      (replace s acc)
                      (nreverse s)))
                (case-let (char (vref string pos))
                  (#\< (rec (1+ pos) (1+ bracket-count) acc))
                  (#\> (rec (1+ pos) (1- bracket-count) acc))
                  (t (if (> bracket-count 0)
                         (rec (1+ pos) bracket-count acc)
                         (rec (1+ pos) bracket-count (cons char acc)))))))))))

(defun qualified-name-lookup-variants (qname)
  "List variants under which to look up QNAME.
For example, for a qualified name like `x::y::z', we might also want
to look it up as `x::z' or just `z'."
  (unless (search "::" qname)
    (return-from qualified-name-lookup-variants
      (list qname)))
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

(defgeneric morally-noexcept-parent? (ast)
  (:method ((ast t))
    nil))

(defgeneric morally-noexcept? (fn)
  (:method ((ast t))
    nil))

#+:TREE-SITTER-CPP
(progn

(defun sort-qualifiers (qualifiers)
  "Sort the potential qualifiers of a function canonically."
  (stable-sort (copy-seq qualifiers)
               (load-time-value
                (ordering '(cpp-attribute-specifier
                            cpp-type-qualifier
                            cpp-ref-qualifier
                            cpp-throw-specifier
                            cpp-empty-throw-specifier
                            cpp-noexcept
                            cpp-attribute-declaration
                            cpp-trailing-return-type
                            cpp-virtual-specifier
                            cpp-requires-clause))
                t)
               :key #'tree-sitter-class-name))

(defmethod initialize-instance :after
    ((cpp cpp-function-declarator) &key &allow-other-keys)
  (callf #'sort-qualifiers (direct-children cpp)))

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
               ((typep target-ast 'cpp-source-text-fragment-variation-point)
                target-ast)
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
             (or
              (when-let ((definition
                          (find-if (of-type 'cpp-function-definition)
                                   parents)))
                (shares-path-of-p root ast (cpp-declarator definition)))
              ;; void fn() = delete does not need contextualizing.
              (when-let* ((decl
                            (find-if (of-type 'cpp-init-declarator) parents))
                          (value (cpp-value decl)))
                (or (typep value 'cpp-delete-expression)
                    (source-text= value "delete")))))
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

(defmethod canonicalize-declarator ((declarator cpp-abstract-reference-declarator))
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

(defmethod canonicalize-type ((declaration cpp-variadic-parameter-declaration)
                              &key ast-type canonical-type)
  (make canonical-type
        :specifier (get-specifier-list ast-type declaration)
        :declarator (canonicalize-declarator (c/cpp-declarator declaration))))

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

(defmethod child-slots ((ast cpp-declaration))
  "Work around unusual alias in cpp-condition-clause.
See SEL issue #359."
  (let ((child-slots (call-next-method)))
    (assert (not (find 'cpp-value child-slots :key #'car))
      () "Workaround is obsolete")
    (if (slot-value ast 'cpp-value)
        (let ((tail (member 'children child-slots :key #'car)))
          (append (ldiff child-slots tail)
                  (list '(cpp-value . 1))
                  tail))
        child-slots)))

(defmethod child-slot-specifiers ((ast cpp-declaration))
  "Work around unusual alias in cpp-condition-clause.
See SEL issue #359."
  (let ((specs (call-next-method)))
    (if (slot-value ast 'cpp-value)
        ;; Don't add it if it's already there.
        (if (find 'cpp-value specs :key #'ft::slot-specifier-slot)
            specs
            (cons (load-time-value
                   (make 'ft::slot-specifier
                         :class t
                         :slot 'cpp-value
                         :arity 1))
                  specs))
        specs)))


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

(defmethod relevant-declaration-type ((ast cpp-namespace-identifier))
  'namespace-declaration-ast)

(defmethod relevant-declaration-type ((ast cpp-nested-namespace-specifier))
  'namespace-declaration-ast)

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

(defmethod outer-declarations ((decl cpp-reference-declarator))
  (values (get-nested-declaration decl)
          '(:variable)))

(defmethod outer-declarations ((field cpp-field-declaration))
  (match field
    ;; Functions.
    ((cpp-field-declaration
      (cpp-declarator
       (list (and declarator (cpp-function-declarator)))))
     (outer-declarations declarator))
    ((cpp-field-declaration
      (cpp-declarator
       (list (and id (cpp-field-identifier)))))
     (values (list id) '(:variable)))
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

(defmethod scope-ast-p ((ast cpp-namespace-definition))
  "Namespace definitions are not (lexical) scopes."
  nil)
(defmethod scope-ast-p ((ast cpp-export-block))
  "Export blocks are not scopes."
  nil)
(defmethod scope-ast-p ((ast cpp-declaration-list))
  (match (get-parent-ast (attrs-root*) ast)
    ((cpp-namespace-definition) nil)
    ((cpp-export-block) nil)
    (otherwise (call-next-method))))

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

(defmethod extract-nested-class ((decl cpp-field-declaration))
  (when (typep decl 'cpp-field-declaration)
    (let ((type (cpp-type decl)))
      (when (typep type 'type-declaration-ast)
        type))))

(defun nested-classes (ast)
  "Extract nested class definitions in AST."
  (when-let (body (cpp-body ast))
    (filter #'extract-nested-class
            (children body))))

(defun static-member? (ast)
  (match ast
    ((cpp-field-declaration
      :cpp-pre-specifiers pre-specifiers)
     (find-if (op (source-text= "static" _))
              pre-specifiers))))

(defun static-members (ast)
  (when-let (body (cpp-body ast))
    (filter #'static-member?
            (children body))))

(defmethod field-adjoin ((field cpp-field-declaration) map)
  "Add nested classes and their exports (e.g. enum members) to the field
table."
  (let ((map-out (call-next-method)))
    (if-let (type (cpp-type field))
      (field-adjoin type map-out)
      map-out)))

(defun export-static-members (ast decls namespaces)
  "Export appropriate static members in symbol table leaving AST.
Static here means both static members and subclasses; anything that
can be accessed by using the class as a namespace."
  (let ((static-fields
          (append (nested-classes ast)
                  (static-members ast))))
    (multiple-value-bind (new-decls new-decl-namespaces)
        (outer-declarations-merge static-fields)
      (values (append new-decls decls)
              (append new-decl-namespaces namespaces)))))

(defmethod outer-declarations :around ((ast cpp-class-specifier))
  (multiple-value-bind (decls decl-namespaces) (call-next-method)
    (export-static-members ast decls decl-namespaces)))

(defmethod outer-declarations :around ((ast cpp-struct-specifier))
  (multiple-value-bind (decls decl-namespaces) (call-next-method)
    (export-static-members ast decls decl-namespaces)))

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

(defgeneric contextual-member-access (ast)
  (:documentation "If AST changes the member access, return the new member access.")
  (:method-combination standard/context)
  (:method :context ((ast cpp-ast))
    (assure member-access
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
Returns one of `:public', `:private', `:protected' (or `nil').

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
    (lret ((result (or (contextual-member-access node) in)))
      (reduce (lambda (in next)
                (member-access next in))
              (children node)
              :initial-value result))))

(defmethod attr-missing ((fn-name (eql 'member-access)) node)
  (declare (ignore node))
  (member-access (genome (attrs-root*)) :public))

(defun public? (ast)
  (eql :public (member-access ast)))

(defun private? (ast)
  (eql :private (member-access ast)))

(defun protected? (ast)
  (eql :protected (member-access ast)))

(define-field-key cpp::field-access cpp::+access+)

(define-field-key cpp::field-virtual? cpp::+virtual+)

(defmethod field-table ((typedef cpp-type-definition))
  "Given a typedef for a template type, recursively resolve the
templated definition's field table."
  (match typedef
    ((cpp-type-definition
      (cpp-type (and type (cpp-template-type))))
     (when-let (class (get-declaration-ast :type type))
       (field-table class)))
    (otherwise (call-next-method))))

(-> cpp::inherited-member-access (member-access member-access) member-access)
(defun cpp::inherited-member-access (as-inherited as-defined)
  "Compute the final visibility of a member give how it is inherited in
the derived class (AS-INHERITED) and how it is defined in the base
class (AS-DEFINED)."
  (dispatch-case ((as-inherited member-access)
                  (as-defined member-access))
    ((* null) nil)
    ((null *) nil)
    ;; The final visibility of inheritance of a private field is
    ;; always null.
    ((* private) nil)
    ;; The final visibility of private inheritance of a field is
    ;; always private.
    ((private *) :private)
    ;; The final visibility of public inheritance of a public field is
    ;; public.
    ((public public) :public)
    ;; The final visibility of public inheritance of a protected field
    ;; is protected.
    ((public protected) :protected)
    ;; The final visibility of protected inheritance of a
    ;; public/protected field is protected.
    ((protected (or public protected)) :protected)))

(defun cpp::derived-class? (class)
  "If CLASS is a derived class, return the base class clause."
  (and (typep class 'c/cpp-classoid-specifier)
       (find-if (of-type 'cpp-base-class-clause)
                (children class))))

(defun cpp::base-class-alist (class)
  "Return an alist from the base classes of CLASS to their
qualifiers (public/private/protected/virtual). Maintain textual
order."
  (labels ((update-base-class-alist (alist ast)
             (if (typep ast '(or cpp-type-identifier
                              cpp-template-type
                              cpp-qualified-identifier))
                 (acons ast '() alist)
                 (ematch alist
                   ((cons (cons last-class quals) alist)
                    (acons last-class (cons ast quals)
                           alist))))))
    (when-let (clause (cpp::derived-class? class))
      (let ((alist
              (reduce (flip #'update-base-class-alist)
                      (children clause)
                      :from-end t
                      :initial-value nil)))
        (mapcar (op (cons (car _1) (nreverse (cdr _1))))
                alist)))))

(defun cpp::declared-virtual? (ast)
  "Is AST declared virtual?"
  (find-if (of-type 'cpp-virtual)
           (slot-value-safe ast 'cpp-pre-specifiers)))

(defun cpp::field-table-collect-properties (field-table)
  "For all the fields in FIELD-TABLE, record properties (member access,
virtuality) from class where they are declared."
  (labels ((field-ast-virtual? (field)
             (and (eql :function (@ field +ns+))
                  (let ((ast (@ field +id+)))
                    (when-let* ((decl
                                 (find-enclosing
                                  'function-declaration-ast
                                  (attrs-root*)
                                  ast)))
                      (cpp::declared-virtual? decl)))))
           (collect-properties (field)
             (let* ((field
                      (if (cpp::field-access field) field
                          (let ((access
                                  (member-access
                                   (field-id field))))
                            (cpp::field-access field access)))))
               (if (nth-value 1 (cpp::field-virtual? field))
                   field
                   (if (field-ast-virtual? field)
                       (cpp::field-virtual? field t)
                       field)))))
    (iter (for (name fields) in-map field-table)
          (map-collect name (mapcar #'collect-properties fields)))))

(defmethod direct-field-table :around ((ast cpp-ast))
  (when-let (map (call-next-method))
    (cpp::field-table-collect-properties map)))

(defun cpp::base-class-access (derived-class quals)
  "Determine the base class access based on DERIVED-CLASS and QUALIFIERS.
QUALIFIERS could contain a public, private, or protected qualifier;
otherwise use the default (public for a struct, private for a class)."
  (assure member-access
    (iter (for qual in quals)
          (thereis
           (cond ((find-if (of-type 'cpp-public) qual)
                  :public)
                 ((find-if (of-type 'cpp-private) qual)
                  :private)
                 ((find-if (of-type 'cpp-protected) qual)
                  :protected)))
          (finally
           (return
             (econd
               ((typep derived-class 'cpp-class-specifier)
                :private)
               ((typep derived-class 'cpp-struct-specifier)
                :public)))))))

(defun cpp::single-inheritance (derived-field-table
                                derived-class
                                base-class
                                quals)
  "Add the fields of BASE-CLASS into DERIVED-CLASS.
Note that because of C++'s name hiding rules, the fields of
DERIVED-CLASS shadow those of BASE-CLASS, except for overrides of
virtual methods."
  (declare (fset:map derived-field-table)
           (ast derived-class base-class))
  (let ((base-access (cpp::base-class-access derived-class quals)))
    (labels ((field-private? (field)
               "Is FIELD defined as private."
               (eql :private (@ field cpp::+access+)))
             (field-virtual? (field)
               "Is FIELD defined as virtual?"
               (@ field cpp::+virtual+))
             (set-final-visibility (field)
               "Return a copy of FIELD with final visibility.
                The final visibility is the combination of the field's
                visibility where it was defined and the declared
                visibility of the base class where it was inherited."
               (let ((final-visibility
                       (cpp::inherited-member-access
                        base-access
                        (@ field cpp::+access+))))
                 (with field cpp::+access+ final-visibility)))
             (filter-fields (base-fields)
               "Remove non-virtual private fields from BASE-FIELDS."
               (mapcar #'set-final-visibility
                       (remove-if
                        (lambda (field)
                          ;; Remove private base-fields, unless they're
                          ;; virtual.
                          (and (field-private? field)
                               (not (field-virtual? field))))
                        base-fields)))
             (mark-virtual (field)
               "Return a copy of FIELD marked as virtual."
               (if (field-virtual? field) field
                   (with field cpp::+virtual+ t)))
             (update-derived-field-table (field-table key base-fields)
               "Add BASE-FIELDS into FIELD-TABLE under KEY."
               ;; C++ uses "name hiding": if a derived class specifies
               ;; a name, overloads for that name in the parent class
               ;; disappear. But that doesn't apply to virtual
               ;; methods.
               (let ((derived-class-value (@ field-table key))
                     ;; Note that virtuality doesn't care if a field is
                     ;; private.
                     (base-fields (filter-fields base-fields))
                     (virtual (some #'field-virtual? base-fields)))
                 (cond ((no derived-class-value)
                        ;; If the derived class doesn't define a name,
                        ;; inherit it.
                        (with field-table key (filter-fields base-fields)))
                       ;; If the derived class defines a name, only
                       ;; inherit it if it's virtual. Also mark all
                       ;; the base-fields virtual. TODO: This should
                       ;; only happen if the signatures match.
                       (virtual
                        (with field-table key
                              (mapcar #'mark-virtual
                                      (append derived-class-value
                                              (filter-fields base-fields)))))
                       (t field-table)))))
      (iter (for (key base-fields) in-map (field-table base-class))
            (for new-field-table
                 initially derived-field-table
                 then (update-derived-field-table
                       new-field-table
                       key
                       base-fields))
            (finally (return new-field-table))))))

(defun cpp::multiple-inheritance (class field-table)
  "Extend FIELD-TABLE with the members of all classes FIELD-TABLE
inherits from."
  (let ((class-name (definition-name-ast class)))
    (labels ((inherit-from-base-class-definition (field-table id quals)
               (if-let ((base-class (get-declaration-ast :type id)))
                 (cpp::single-inheritance field-table
                                          class
                                          base-class
                                          quals)
                 (progn
                   (dbg:note :debug
                             "No definition found for base class ~a of ~a"
                             (source-text id)
                             (source-text class-name))
                   field-table)))
             (inherit-from-base-class-specifiers
                 (field-table base-class-specifiers)
               ;; todo C3 linearization? Left classes do take
               ;; precedence over right.
               (reduce (lambda (field-table id.quals)
                         (destructuring-bind (id . quals) id.quals
                           (inherit-from-base-class-definition
                            field-table id quals)))
                       base-class-specifiers
                       :initial-value field-table))
             (inherit-from-base-classes (class field-table)
               (let ((base-class-alist (cpp::base-class-alist class)))
                 (cond ((no base-class-alist)
                        field-table)
                       ((has-attribute-p class 'symbol-table)
                        (inherit-from-base-class-specifiers
                         field-table base-class-alist))
                       (t
                        (error "Cannot inherit into ~a without a symbol table"
                               class-name))))))
      (inherit-from-base-classes class field-table))))

(defun cpp::field-table-with-inheritance (class)
  "Return CLASS's field table, with inheritance if possible."
  (let ((direct-field-table (direct-field-table class)))
    (if (has-attribute-p class 'symbol-table)
        (cpp::multiple-inheritance class direct-field-table)
        (progn
          (when (cpp::derived-class? class)
            (dbg:lazy-note :debug
                           "Cannot inherit without a symbol table: ~a"
                           (definition-name-ast class)))
          direct-field-table))))

(defmethod field-table ((struct cpp-struct-specifier))
  (cpp::field-table-with-inheritance struct))

(defmethod field-table ((class cpp-class-specifier))
  (cpp::field-table-with-inheritance class))

(defmethod outer-declarations ((ast cpp-template-declaration))
  ;; TODO Store the template parameters somehow in the symbol table?
  (when-let ((definitions (filter (of-type '(or definition-ast declaration-ast))
                                  (direct-children ast))))
    (outer-declarations (only-elt definitions))))

(defmethod inner-declarations ((ast cpp-namespace-definition))
  (when-let (name (cpp-name ast))
    (values (list name) '(:namespace))))

(defmethod inner-declarations ((ast cpp-declaration-list))
  (if (typep (get-parent-ast (attrs-root*) ast) 'cpp-namespace-definition)
      (values nil nil)
      (call-next-method)))

(defmethod outer-declarations ((ns cpp-namespace-definition))
  (match ns
    ((cpp-namespace-definition
      (cpp-body
       (cpp-declaration-list (direct-children children))))
     (if-let (name (cpp-name ns))
       (receive (child-decls child-namespaces)
           (outer-declarations-merge children)
         (values
          (cons (cpp-name ns) child-decls)
          (cons :namespace child-namespaces)))
       (outer-declarations-merge children)))))

(defun conserve-outer-def-exports (ast)
  "Add the outer definitions of AST's children to AST's outer
definitions."
  (match (cpp-body ast)
    ((cpp-declaration-list (direct-children children))
     (reduce (op (symbol-table-union ast _ _))
             (mapcar #'outer-defs children)
             :initial-value (empty-map)))))

(defmethod outer-defs ((ast cpp-namespace-definition))
  (if-let (exports (conserve-outer-def-exports ast))
    ;; Add the namespace itself.
    (symbol-table-union ast exports (call-next-method))
    (call-next-method)))

(defmethod outer-defs ((ast cpp-export-block))
  (or (conserve-outer-def-exports ast)
      (call-next-method)))

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
                      ;; We can call const methods on a mutable
                      ;; object, but not v.v.
                      (if (not const?)
                          overloads
                          (error "Invalid partitioning of overloads: ~a"
                                 overloads))
                      relevant-overloads)
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

(defmethod resolve-overloads ((type (eql :type)) (ast cpp-ast)
                              &optional overloads)
  "Remove bare declarations of types (e.g. `struct mytype;') from
consideration as overloads."
  (if (every (of-type 'c/cpp-classoid-specifier) overloads)
      (match (remove-if-not #'cpp-body overloads)
        ((list only) only)
        (t (call-next-method)))
      (call-next-method)))

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

(-> smart-pointer-type-arg (cpp-ast) (or cpp-ast null))
(defun smart-pointer-type-arg (ast)
  "If AST is a smart pointer, extract its type argument."
  (match ast
    ((cpp-qualified-identifier)
     (smart-pointer-type-arg
      (lastcar (qualified-name->list ast))))
    ((cpp-template-type
      (cpp-name
       (cpp-type-identifier
        :text (satisfies smart-pointer-type-name?)))
      (cpp-arguments
       (cpp-template-argument-list :children (list type))))
     (and (equal (namespace ast) "std")
          type))))

(defmethod deref-type ((type cpp-qualified-identifier))
  ;; TODO This needs to be generalized.
  (or (smart-pointer-type-arg type)
      (let ((parts (qualified-name->list type)))
        (if (member (lastcar parts) '("iterator" "const_iterator")
                    :test #'source-text=)
            (resolve-container-element-type type)
            (call-next-method)))))

(defmethod deref-type ((type cpp-template-type))
  (or (smart-pointer-type-arg type)
      (call-next-method)))

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
       (fail))
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

(defmethod placeholder-type-p ((ast cpp-auto))
  t)

(defmethod placeholder-type-p ((ast cpp-placeholder-type-specifier))
  t)

(defmethod placeholder-type-p ((ast cpp-type-descriptor))
  (placeholder-type-p (cpp-type ast)))

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
        (let ((field-ns (namespace field-type))
              (qualified-arg-type (qualify-declared-ast-name arg-type)))
          (if (equal field-ns qualified-arg-type)
              ;; If the type of the argument is (modulo template
              ;; arguments) the same as the namespace of the field
              ;; type, then we synthesize a new AST from both of them
              ;; with template arguments intact.
              (let ((new-arg-type (tree-copy arg-type))
                    (new-field-type (tree-copy field-type)))
                (setf (attr-proxy new-arg-type) arg-type
                      (attr-proxy new-field-type) field-type)
                (lret ((qname
                        (list->qualified-name
                         (append (qualified-name->list new-arg-type)
                                 (qualified-name->list new-field-type)))))
                  (setf (attr-proxy qname) field-type)))
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
      (lret ((qname
              (tree-copy
               (list->qualified-name
                (butlast
                 (qualified-name->list id))))))
        (setf (attr-proxy qname) id))))))

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
          (qualified-name->list (cpp-name ast))))
  (:method ((ast cpp-dependent-type))
    ;; "The keyword typename may only be used in this way before
    ;; qualified names (e.g. T::x)."
    (mappend #'qualified-name->list
             (children ast))))

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

(defmethod convert ((to (eql 'cpp-field-identifier))
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

(-> list->qualified-name ((soft-list-of (or null cpp-ast)))
    (values cpp-ast &optional))
(defun list->qualified-name (list)
  "Compose a cpp-qualified-identifier from LIST, a list of identifiers.

It is also possible for `nil' to appear in the list, meaning the
global scope (e.g. `::ns::x`).

Since the tree-sitter grammar for C++ treats the same text differently
depending on which side of the :: of a qualified name it appears on,
this involves handling some translations between types."
  (when (null list)
    (error "Empty lists cannot become qualified names!"))
  (labels ((type-id->ns-id (type-id)
             "Create a namespace identifier from a text identifier."
             (lret ((ns-id (tree-copy (convert 'cpp-namespace-identifier type-id))))
               (setf (attr-proxy ns-id) type-id)))
           (ns-id->type-id (ns-id)
             "Create a type identifier from a namespace identifier."
             (lret ((type-id (tree-copy (convert 'cpp-type-identifier ns-id))))
               (setf (attr-proxy type-id) ns-id)))
           (fix-scope (scope)
             "Make sure SCOPE is an AST that can appear in the scope
              slot of a qualified identifier."
             (etypecase scope
               (cpp-type-identifier
                (type-id->ns-id scope))
               ((or cpp-namespace-identifier
                    cpp-template-type
                    cpp-dependent-name
                    ;; Global scope.
                    null)
                scope)))
           (fix-name (name)
             "Make sure NAME is an AST that can appear in the name
              slot of a qualified identifier."
             (etypecase name
               (cpp-namespace-identifier
                (ns-id->type-id name))
               (cpp-type-descriptor
                (cpp-type name))
               ((or cpp-dependent-name
                    cpp-type-identifier
                    cpp-qualified-identifier
                    cpp-template-function
                    cpp-identifier
                    cpp-operator-name
                    cpp-destructor-name
                    cpp-template-type)
                name)))
           (qualify (list)
             "Right-fold LIST into a qualified name."
             (reduce (lambda (scope name)
                       (cond ((primitive-type-p name) name)
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

(defgeneric unqualified-name (name &key count)
  (:documentation "Remove namespace qualifications from NAME.
COUNT controls how many levels of qualification to remove \(if for
instance we only want to remove one).")
  (:method ((ast cpp-ast) &key count)
    (declare (ignore count))
    (if (primitive-type-p ast)
        ast
        (call-next-method)))
  (:method ((ast cpp-identifier) &key count)
    (declare (ignore count))
    ast)
  (:method ((ast cpp-field-identifier) &key count)
    (declare (ignore count))
    ast)
  (:method ((ast cpp-namespace-identifier) &key count)
    (declare (ignore count))
    ast)
  (:method ((ast cpp-type-identifier) &key count)
    (declare (ignore count))
    ast)
  (:method ((ast cpp-template-type) &key count)
    (declare (ignore count))
    ast)
  (:method ((ast cpp-qualified-identifier) &key (count most-positive-fixnum))
    (declare (optimize (debug 0))
             ((and fixnum unsigned-byte) count))
    (if (zerop count) ast
        (unqualified-name (cpp-name ast) :count (1- count))))
  (:method ((ast cpp-nested-namespace-specifier) &key (count most-positive-fixnum))
    (declare ((and fixnum unsigned-byte) count))
    (if (zerop count) ast
        (car (children ast))))
  (:method ((ast cpp-template-function) &key (count most-positive-fixnum))
    (if (zerop count) ast
        (unqualified-name (cpp-name ast) :count (1- count))))
  (:method ((ast cpp-destructor-name) &key count)
    (declare (ignore count))
    ast)
  (:method ((ast cpp-primitive-type) &key count)
    (declare (ignore count))
    ast)
  (:method ((ast cpp-operator-name) &key count)
    (declare (ignore count))
    ast))

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

(defmethod ltr-eval-ast-p ((ast cpp-binary-expression))
  (or (member (operator ast) '(:<< :>>))
      (call-next-method)))

(defparameter *morally-noexcept*
  (fset:set "static_cast" "next"
            "begin" "end"
            "rbegin" "rend"
            "cbegin" "cend"
            "swap")
  "List of STL functions, methods, and operators that are morally noexcept, Lakos Rule notwithstanding.")

(defmethod morally-noexcept? ((fn-name identifier-ast))
  (and (member (namespace fn-name) '("std" "") :test #'equal)
       (or (lookup *morally-noexcept*
                   (source-text (unqualified-name fn-name :count 1)))
           (some #'morally-noexcept-parent?
                 (get-parent-asts* (attrs-root*) fn-name)))))

(defmethod morally-noexcept? ((fn cpp-field-expression))
  ;; TODO Based on namespace.
  (morally-noexcept? (cpp-field fn)))

(defmethod morally-noexcept? ((fn cpp-function-definition))
  (morally-noexcept? (definition-name-ast fn)))

(defmethod morally-noexcept? ((fn cpp-function-declarator))
  (morally-noexcept? (declarator-name-ast fn)))

(defmethod morally-noexcept? ((decl cpp-declaration))
  (morally-noexcept? (cpp-declarator decl)))

(defmethod morally-noexcept? ((decl cpp-operator-name))
  ;; Operators are not always noexcept. E.g. you can enable exceptions
  ;; for operator<< with the .exceptions method on iostreams.
  (lookup *morally-noexcept* (source-text decl)))

(defgeneric noexcept-specifier? (ast)
  (:method ((ast t)) nil)
  (:method ((ast cpp-noexcept)) t)
  (:method ((ast cpp-empty-throw-specifier)) t))

(defun specified-noexcept? (ast)
  (find-if #'noexcept-specifier?
           (direct-children (cpp-declarator ast))))

(defmethod exception-set ((ast cpp-field-initializer-list))
  ;; TODO Get the exception set of the initializer of each member.
  (let ((args
          (iter (for field-initializer in
                     ;; Comments in a field initializer list appear as
                     ;; children.
                     (filter (of-type 'cpp-field-initializer)
                             (children ast)))
                (destructuring-bind (field arglist)
                    (children field-initializer)
                  (declare (ignore field))
                  (appending (children arglist))))))
    (reduce #'exception-set-union
            args
            :key #'exception-set
            :initial-value +exception-bottom-type+)))

(defmethod exception-set ((ast cpp-function-definition))
  (cond-let found
    ((specified-noexcept? ast)
     ;; NB A noexcept function can contain a throw statement, but it
     ;; can't escape the function.
     +exception-bottom-type+)
    ((morally-noexcept? (definition-name-ast ast))
     +exception-bottom-type+)
    ((find-if (of-type 'cpp-field-initializer-list) ast)
     (exception-set-union
      (exception-set found)
      (if-let (body (cpp-body ast))
        (exception-set body)
        +exception-bottom-type+)))
    ((cpp-body ast)
     (exception-set found))
    ((find-if (of-type 'cpp-default-method-clause) ast)
     ;; TODO Handle default exception set semantics for
     ;; constructors/destructors.
     +exception-bottom-type+)
    (t +exception-bottom-type+)))

(defun declared-function-exception-set (declaration)
  "Helper function to compute the exception set of DECLARATION, if
DECLARATION declares a function.

The exception set of the declaration is either empty, if the
declaration is `noexcept', or the union of the exception sets of all
the definitions of the declared function."
  (match (cpp-declarator declaration)
    ((list
      (or (and declarator (cpp-function-declarator))
          (cpp-reference-declarator
           :children (list (and declarator (cpp-function-declarator))))))
     (cond ((find-if #'noexcept-specifier?
                     (direct-children declarator))
            +exception-bottom-type+)
           ((morally-noexcept? declarator)
            +exception-bottom-type+)
           (t
            (if-let ((definitions (c/cpp-function-declaration-definitions declaration)))
              (reduce #'exception-set-union
                      definitions
                      :key #'exception-set
                      :initial-value +exception-bottom-type+)
              +exception-top-type+))))))

(defmethod exception-set ((ast cpp-declaration))
  (or (declared-function-exception-set ast)
      (call-next-method)))

(defmethod exception-set ((ast cpp-field-declaration))
  (or (declared-function-exception-set ast)
      (call-next-method)))

(defun cpp-catch-clause-handled-exception-set (catch-clause)
  (declare (cpp-catch-clause catch-clause))
  (or (when-let* ((params (find-if (of-type 'parameters-ast) catch-clause))
                  (param (first (children params))))
        (typecase param
          ;; TODO This should be parsed as a terminal.
          (cpp-variadic-declaration +exception-top-type+)
          (parameter-ast
           (list 'or (parameter-type param)))))
      +exception-bottom-type+))

(defmethod exception-set ((ast cpp-try-statement))
  (let* ((catch-clause (find-if (of-type 'cpp-catch-clause) ast))
         (handled-exception-set
          (cpp-catch-clause-handled-exception-set catch-clause))
         ;; Filtered exceptions from the try body.
         (filtered-exceptions
          (exception-set-difference (exception-set (cpp-body ast))
                                    handled-exception-set))
         ;; Rethrows from the catch clause body.
         (rethrows (exception-set (cpp-body catch-clause))))
    (exception-set-union filtered-exceptions rethrows)))

(defmethod exception-set ((ast cpp-throw-statement))
  (if (no (children ast))
      (cpp-catch-clause-handled-exception-set
       (or (find-enclosing 'cpp-catch-clause (attrs-root*) ast)
           (error "No enclosing catch clause for throw without argument")))
      (call-next-method)))

(defmethod function-exception-set :around ((ast cpp-ast))
  (if (morally-noexcept? ast) '(or)
      (call-next-method)))

(defmethod function-exception-set ((ast cpp-template-function))
  (function-exception-set (cpp-name ast)))

(defun find-enclosing-template (ast)
  (find-enclosing 'cpp-template-declaration (attrs-root*) ast))

(def-attr-fun template-specializations ()
  (:documentation "Return all specializations of a template.
Specializations are both template types and calls of template
functions.")
  (:method ((template cpp-template-declaration) &aux (root (attrs-root*)))
    (nest
     (labels ((specialized? (arguments-ast)
                (let* ((args (children arguments-ast))
                       (arg-decls
                         (mapcar (op (get-declaration-ast :type _))
                                 args)))
                  ;; NB This excludes optional type parameters.
                  (notevery (of-type 'cpp-type-parameter-declaration)
                            arg-decls)))))
     (iter (for node in-tree (genome root)))
     (unless (parent-ast-p (attrs-root*) template node))
     (match node
       ;; Collect specializations from types.
       ((cpp-template-type
         (cpp-name type)
         (cpp-arguments arguments))
        (and-let* ((type-def (get-declaration-ast :type type))
                   ((eql template (find-enclosing-template type-def)))
                   ((specialized? arguments)))
          (collect node)))
       ;; Collect specializations from invocations.
       ((call-ast
         (call-function (cpp-field-expression)))
        nil)
       ((call-ast
         (call-function
          (and fn
               (cpp-template-function)))
         (call-arguments-ast arguments))
        (when (specialized? arguments)
          (collect fn)))
       ((call-ast
         (call-function (and fn (identifier-ast))))
        (unless (get-declaration-id :macro fn)
          (when-let* ((id (get-declaration-id :function fn))
                      (template (find-enclosing-template id)))
            (collect fn))))))))

(defun param-possible-types (decl &optional default)
  (labels ((param-offset (template decl)
             (position decl (children (cpp-parameters template))))
           (specialization-type-arguments (specialization)
             "Get the type arguments from a template specialization. "
             (match specialization
               ((or (cpp-template-type
                     (cpp-arguments type-args))
                    (cpp-template-function
                     (cpp-arguments type-args)))
                (children type-args))
               ;; Handle implicit specialization of a
               ;; call.
               ((and id (identifier-ast))
                (let ((call (find-enclosing 'call-ast (attrs-root*) id)))
                  (mapcar #'infer-type (call-arguments call)))))))
    (when-let* ((template (find-enclosing-template decl))
                (offset (param-offset template decl)))
      (let* ((specializations (template-specializations template))
             (specialization-argument-lists
               (mapcar #'specialization-type-arguments specializations))
             (parameter-arguments
               ;; TODO Handle defaulting.
               (filter-map (op (or (nth offset _) default))
                           specialization-argument-lists))
             ;; TODO Circularity?
             (ids (mappend
                   (lambda (type)
                     (or (get-declaration-ids :type type)
                         (match type
                           ((or (cpp-primitive-type)
                                (cpp-type-descriptor
                                 (cpp-type (cpp-primitive-type))))
                            (list type)))))
                   parameter-arguments)))
        (nub ids)))))

(def-attr-fun possible-types ()
  (:documentation "Get the possible types of a template parameter declaration.")
  (:method ((decl cpp-type-parameter-declaration))
    (param-possible-types decl))
  (:method ((decl cpp-optional-type-parameter-declaration))
    (param-possible-types decl (cpp-default-type decl))))

(defmethod get-declaration-ids :around ((type (eql :type)) (ast cpp-ast))
  "When the declaration of a type is a type parameter, include the
set of possible concrete specializations of that type."
  (let ((results (call-next-method type ast)))
    (if-let (type-params
             (filter-map
              (op (find-enclosing 'cpp-type-parameter-declaration (attrs-root*) _))
              results))
      (append results
              (mappend #'possible-types type-params))
      results)))

(defmethod find-enclosing-declaration ((type (eql 'function-declaration-ast))
                                       root
                                       (id cpp-destructor-name))
  (match (get-parent-asts* root id)
    ((list*
      (cpp-function-declarator (cpp-declarator decl-name))
      (and decl (cpp-declaration))
      _)
     (unless (eql decl-name id)
       (fail))
     decl)
    (otherwise (call-next-method))))

(defmethod find-enclosing-declaration ((type (eql 'function-declaration-ast))
                                       root
                                       (id cpp-operator-name))
  (match (get-parent-asts* root id)
    ((list*
      (cpp-function-declarator (cpp-declarator decl-name))
      (and decl (cpp-field-declaration))
      _)
     (unless (eql decl-name id)
       (fail))
     decl)
    (otherwise (call-next-method))))


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
  :|:| cpp-module-name
  :|::| cpp-ast
  cpp-ast :|::|
  cpp-identifier cpp-.
  cpp-. cpp-field-identifier
  cpp-field-expression cpp-argument-list
  cpp-identifier cpp-initializer-list
  cpp-module-qualified-name cpp-module-partition
  cpp-this cpp-->
  cpp-this cpp-.
  cpp-field-identifier cpp-parameter-list
  cpp-& cpp-identifier
  :|operator| cpp-ast
  cpp-ast cpp-abstract-reference-declarator
  cpp-module-name :|.|
  :|.| cpp-module-name)

(defmethod whitespace-between/parent ((parent cpp-namespace-definition)
                                      (style c-style-indentation)
                                      (x ast)
                                      (y ast))
  #\Newline)

(defmethod whitespace-between/parent ((parent cpp-reference-declarator)
                                      (style c-style-indentation)
                                      (x cpp-&)
                                      (y cpp-ast))
  (if (emptyp (before-text parent)) " " ""))

(defmethod whitespace-between :around ((style c-style-indentation)
                                       (l cpp-ast)
                                       (r cpp-reference-declarator))
  (if (emptyp (before-text (second (children r)))) " " ""))


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
      (propagate-exports-up ast (propagate-declarations-down ast in)
                            in)
      (call-next-method)))

(defun propagate-exports-up (ast scope-final in)
  (declare (ignore ast))
  (let ((new-exports (@ scope-final :export)))
    (if (no new-exports) in
        (with in :export new-exports))))

(defmethod symbol-table ((node cpp-namespace-definition) &optional in)
  (propagate-declarations-down node in))

(defmethod symbol-table ((node cpp-export-block) &optional in)
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

(defmethod plausible-macro-name? ((ast cpp-operator-name))
  nil)

(defmethod plausible-macro-name? ((ast cpp-destructor-name))
  nil)

(defmethod plausible-macro-name? ((ast cpp-qualified-identifier))
  nil)

(defmethod plausible-macro-name? ((ast cpp-this))
  nil)

(defmethod qualify-declared-ast-name ((type cpp-primitive-type))
  (source-text type))

(defun qualify-declared-ast-name/namespaces (declared-ast)
  (labels ((enough-source-text (declared-ast)
             "Get enough of the source text of DECLARED-AST.
              Put a reasonable limit on it in case something absurd has
              been passed in. E.g. callers may try to get the
              declaration of a definition by looking up the definition
              itself. Some compilers limit maximum name lengths, so
              code that goes beyond this would be invalid
              anyway (MISRA only allows 31!)."
             (or (declarator-name declared-ast)
                 (source-text-take 2048 declared-ast)))
           (qualify-declared-ast-name (declared-ast)
             (let* ((source-text (enough-source-text declared-ast))
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
                            "::"))))
    (if (macro-name? declared-ast)
        (enough-source-text declared-ast)
        (qualify-declared-ast-name declared-ast))))

(defmethod qualify-declared-ast-name ((declared-ast cpp-ast))
  (qualify-declared-ast-name/namespaces declared-ast))

(defmethod qualify-declared-ast-name ((id cpp-namespace-identifier))
  (qualify-declared-ast-name/namespaces id))

(defmethod qualify-declared-ast-names-for-lookup ((declared-ast cpp-ast))
  "E.g. x::y::z becomes `'(\"x::y::z\", \"x::z\", \"z\")'."
  (qualified-name-lookup-variants (qualify-declared-ast-name declared-ast)))

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

(defmethod inner-declarations ((catch-clause cpp-catch-clause))
  (let ((params
          (mappend #'parameter-names
                   (children (cpp-parameters catch-clause)))))
    (values params
            (mapcar (constantly :variable) params))))

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
  (:metaclass abstract-standard-class))

(defmethod module-unit-full-name ((m module-unit))
  (module-unit-module-name m))

(defmethod print-object ((self module-unit) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (module-unit-full-name self))))

(defclass importable-module-unit (module-unit)
  ()
  (:documentation "A module unit that can be imported (by clang).")
  (:metaclass abstract-standard-class))

(defclass module-interface-unit (importable-module-unit)
  ()
  (:documentation "An exported module unit.")
  (:metaclass abstract-standard-class))

(defclass implementation-unit (module-unit)
  ()
  (:metaclass abstract-standard-class))

(defclass module-partition-unit (module-unit)
  ((partition-name
    :type string
    :initarg :partition-name
    :reader module-unit-partition-name))
  (:metaclass abstract-standard-class))

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

AST is exported if:
- It has an export specifier.
- It is defined in an export block.
- It is defined in a namespace that satisfies `exported?'.
- It is defined as public in a class that satisfies `exported?'.
- It is a definition and its declaration satisfies
  one of the above."
  (labels ((directly-exported? (ast)
             (find-if (of-type 'cpp-export-specifier)
                      (direct-children ast)))
           (exported-from-parents? (ast)
             (when-let ((decl (find-enclosing 'declaration-ast (attrs-root*) ast)))
               (match (get-parent-asts* (attrs-root*) ast)
                 ;; Export block.
                 ((list* (cpp-declaration-list)
                         (cpp-export-block) _)
                  t)
                 ;; Unnamed namespace.
                 ((list* (cpp-declaration-list)
                         (and (cpp-namespace-definition)
                              (not (satisfies cpp-name)))
                         _)
                  nil)
                 ;; Exported namespace.
                 ((list* (cpp-declaration-list)
                         (and ns (cpp-namespace-definition)) _)
                  ;; Pass :check-decls nil to prevent circularity
                  ;; during attribute computation.
                  (exported? ns :check-decls nil))
                 ;; Exported class.
                 ((list* (cpp-field-declaration-list)
                         (and class
                              ;; Struct, class, enum, union, etc.
                              (c/cpp-classoid-specifier)) _)
                  (and (public? decl)
                       (exported? class))))))
           (exported-from-declaration? (ast)
             (when-let* ((decl-type (relevant-declaration-type ast))
                         (decls (get-declaration-asts decl-type ast)))
               (dolist (decl (remove ast decls))
                 (cond ((typep decl 'cpp-declaration)
                        ;; The declaration is exported.
                        (return (exported? decl)))
                       ((typep decl 'cpp-field-declaration)
                        (return
                          (and (public? decl)
                               (exported?
                                (find-enclosing 'c/cpp-classoid-specifier
                                                (attrs-root*)
                                                decl))))))))))
    (or (directly-exported? ast)
        (exported-from-parents? ast)
        (and check-decls
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
