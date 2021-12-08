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


;;; Methods common to all software objects


;;;; Methods for tree-sitter generics

(defmethod outer-declarations ((ast cpp-function-declarator))
  (list (cpp-declarator ast)))

(def +unnamed-namespace-ast+
  (make 'cpp-ast)
  "Dummy AST for an unnamed namespace.")

(defmethod outer-declarations ((ast cpp-namespace-definition))
  (let ((name (cpp-name ast)))
    (etypecase name
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
  (remove-if (of-type 'cpp-namespace-identifier)
             (identifiers ast)))


;;; Whitespace rules


) ; #+:TREE-SITTER-CPP
