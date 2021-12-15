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

#+:TREE-SITTER-CPP
(progn

(defmethod initialize-instance :after ((cpp cpp)
                                       &key &allow-other-keys)
  "If no compiler was specified, default to cc."
  (unless (compiler cpp)
    (setf (compiler cpp) "c++")))

(defmethod ast-for-match ((language (eql 'cpp))
                          string software context)
  (@ (convert (language-ast-class language)
              (concatenate 'string string ";")
              :deepest t)
     '(0)))

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
    ((language (eql :cpp)) (class (eql 'cpp-operator-name)) parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    (#.(mapcar #'make-keyword +cpp-operator-names+)
       (label-as :name))))

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

(defmethod definition-name ((ast cpp-class-specifier))
  (source-text (cpp-name ast)))

(defmethod definition-name ((ast cpp-namespace-definition))
  (source-text (cpp-name ast)))


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

(defmethod outer-declarations ((ast cpp-function-declarator))
  (list (cpp-declarator ast)))

(def +unnamed-namespace-ast+
  (make 'cpp-ast)
  "Dummy AST for an unnamed namespace.")

(defmethod outer-declarations ((ast cpp-namespace-definition))
  (let ((name (cpp-name ast)))
    (etypecase name
      ;; TODO An unnamed namespace exposes all its definitions.
      (null nil)
      (cpp-identifier (list name))
      ;; E.g. namespace A::B {}
      (cpp-namespace-definition-name
       (children name)))))

(defmethod inner-declarations ((ast cpp-namespace-definition))
  (let ((name (cpp-name ast)))
    (etypecase name
      (null (list +unnamed-namespace-ast+))
      (cpp-identifier (list name))
      ;; E.g. namespace A::B {}
      (cpp-namespace-definition-name
       (children name)))))

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

(defmethod extract-declaration-type ((obj cpp) (decl cpp-ast))
  (or
   ;; Look for a surrounding variable declaration.
   (when-let ((declaration
               (find-if (of-type '(and variable-declaration-ast
                                   (not cpp-init-declarator)))
                        ;; Inclusive of AST.
                        (get-parent-asts obj decl))))
     (cpp-type declaration))
   ;; If the declaration is for a function, return that
   ;; function's type.
   (and-let* ((function (find-enclosing 'function-ast obj decl))
              ((eql decl (cpp-declarator function))))
     (cpp-type function))))

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

(defmethod expression-type ((ast cpp-number-literal))
  ;; NB There are no negative integer literals in C++; they are
  ;; handed through implicit conversion with the unary minus
  ;; operator (TODO).
  (match
      ;; C++ does not care about case (in hex numbers) and allows ' as
      ;; a separator.
      (remove #\' (string-downcase (text ast)))
    ;; TODO Unfinished. See
    ;; https://en.cppreference.com/w/cpp/language/integer_literal and
    ;; https://en.cppreference.com/w/cpp/language/floating_literal.
    ((and string (ppcre "^[0-9]+$"))
     (let ((int (parse-integer string)))
       (econd
        ;; TODO Allow configuring the thresholds? Extract them from
        ;; the environment?
        ((< int (expt 2 16))
         (make 'cpp-primitive-type :text "int"))
        ((< int (expt 2 32))
         (ast-from-template "$1 x;" 'cpp-ast "long int"))
        ((< int (expt 2 64))
         (ast-from-template "$1 x;" 'cpp-ast "long long int")))))
    ((ppcre "^[0-9]+\\.[0-9]*$")
     (make 'cpp-primitive-type :text "double"))
    ((ppcre "^[0-9]+\\.[0-9]*f$")
     (make 'cpp-primitive-type :text "float"))))

(defmethod infer-expression-type ((obj cpp) (ast cpp-parenthesized-expression))
  (infer-expression-type obj (only-elt (children ast))))

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
    (("int" "float") type1)
    ((x y) (and (equal x y) type1))))

(defmethod infer-expression-type ((obj cpp) (ast expression-ast))
  (match (take 2 (get-parent-asts* obj ast))
    ((list (type cpp-init-declarator)
           (and decl (type cpp-declaration)))
     (cpp-type decl))))

(defgeneric explicit-namespace-qualifiers (ast)
  (:documentation "Explicit namespace qualifiers (e.g. A::x).")
  (:method ((ast cpp-ast)))
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
    (reverse
     (iter (for ns in (find-all-enclosing 'cpp-namespace-definition obj ast))
           (match ns
             ;; Unnamed namespace.
             ((cpp-namespace-definition :cpp-name nil))
             ((cpp-namespace-definition
               :cpp-name (and name (type cpp-identifier)))
              (collecting name))
             ((cpp-namespace-definition
               :cpp-name (and name (type cpp-namespace-definition-name)))
              (appending (children name))))))))

(defgeneric namespace-qualifiers (obj ast)
  (:documentation "Final namespace qualifiers, derived by resolving
  explicit (relative) namespace qualifiers relative to
  implicit (absolute) ones.")
  (:method ((obj cpp) ast)
    (let ((explicit (explicit-namespace-qualifiers ast)))
      (if-let ((tail (member :global explicit)))
        (rest tail)
        (let ((implicit (implicit-namespace-qualifiers obj ast)))
          (if explicit
              (let ((index (search explicit implicit
                                   :key #'source-text
                                   :test #'equal
                                   :from-end t)))
                (append (take (or index 0) implicit)
                        explicit))
              implicit))))))

(defgeneric unqualified-name (name)
  (:documentation "Remove namespace qualifications from NAME.")
  (:method ((ast cpp-identifier))
    ast)
  (:method ((ast cpp-template-type))
    ast)
  (:method ((ast cpp-qualified-identifier))
    (declare (optimize (debug 0)))
    (unqualified-name (cpp-name ast)))
  (:method ((ast cpp-namespace-definition-name))
    (lastcar (children ast))))

(defmethod get-declaration-ast ((obj cpp) (ast ast))
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
                           (equal
                            full-qualifiers
                            (mapcar #'source-text
                                    (namespace-qualifiers obj decl)))))))
                 (scope-tree obj))))
          (aget :decl scope)))))


;;; Whitespace rules


) ; #+:TREE-SITTER-CPP
