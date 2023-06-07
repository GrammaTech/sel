;;;; tree-sitter-code-gen.lisp --- parsing and processing of grammars to generate
;;;;                               tree-sitter AST representations.
;;; Dummy Package
(defpackage :software-evolution-library/software/tree-sitter-code-gen
  (:nicknames :sel/software/ts-code-gen :sel/sw/ts-code-gen)
  (:use :gt/full
        :cl-json
        :software-evolution-library
   :software-evolution-library/software/tree-sitter)
  (:import-from :overlord
                :depends-on
                :file-target
                :use-threads-p))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)


;;; Shared object set-up
(eval-always
  ;; TODO: REMOVE THIS AFTER ALL DOWNSTREAM CIs HAVE BEEN UPDATED
  (unless (member :sel/structured-text *features*)
    (push :sel/structured-text *features*))

  (define-condition matching-error (error)
    ())

  (define-condition rule-matching-error (matching-error)
    ((rule :initarg :rule-matching-error-rule :initform nil
           :reader rule-matching-error-rule)
     (ast :initarg :rule-matching-error-ast :initform nil
          :reader rule-matching-error-ast))
    (:documentation "Error thrown when a rule fails to match on an AST.")
    (:report
     (lambda (condition stream)
       (format stream "Unable to match~%~a~%on AST of type ~%~a"
               (rule-matching-error-rule condition)
               (type-of (rule-matching-error-ast condition))))))

  (define-condition parse-tree-matching-error (matching-error)
    ((superclass :initarg :superclass :initform nil
                 :reader parse-tree-matching-error-superclass)
     (parse-tree :initarg :parse-tree :initform nil
                 :reader parse-tree-matching-error-parse-tree)
     (subclasses :initarg :subclasses :initform nil
                 :reader parse-tree-matching-error-subclasses)
     (child-types :initarg :child-types :initform nil
                  :reader :parse-tree-matching-error-child-types))
    (:documentation "Error when a parse tree can't be matched.")
    (:report
     (lambda (condition stream)
       (with-slots (superclass parse-tree subclasses) condition
         (let ((*print-circle* nil))
           (format stream
                   "Unable to match tree to any subclass of ~s:~%~s~2%Candidates: ~s"
                   superclass parse-tree subclasses))))))

  (defstruct (parse-tree (:type list) (:copier nil))
    type range children)

  (defun copy-parse-tree (parse-tree
                          &key
                            (type (parse-tree-type parse-tree))
                            (range (parse-tree-range parse-tree))
                            (children (parse-tree-children parse-tree)))
    (make-parse-tree :type type :range range :children children))

  (defpattern parse-tree (type range children)
    `(list ,type ,range ,children))

  (defvar *superclass->language* (make-hash-table)
    "Maps an AST superclass to its tree-sitter language. When
convert is called, the superclass can then be used to look up
which language--and its relevant shared object--should be used
to parse the string.")

  (defvar *tree-sitter-language-directories*
    (or (when-let ((env (getenv "SEL_TREE_SITTER_LANGUAGE_DIR")))
          (split-sequence #\, env))
        (remove-if-not
         #'probe-file `("/usr/share/tree-sitter/"
                        "/usr/local/share/tree-sitter/"
                        ,@(when-let (home (getenv "HOME"))
                            (list (string+ home "/.local/share/tree-sitter/")))))
        (prog1 nil (format *error-output* "No tree-sitter language directory found.")))
    "A list of directories that hold directories of json files
defining supported tree-sitter languages.  These directories are
searched to populate `*tree-sitter-language-files*'.")

  (defun collect-tree-sitter-language-files
      (&optional (directories *tree-sitter-language-directories*) &aux results)
    "Collect tree-sitter language definition files."
    (dolist (dir directories results)
      (walk-directory
       dir
       (lambda (file)
         (let* ((base (pathname-directory-pathname file))
                (name (string-trim '(#\/) (pathname-relativize dir base))))
           (push (list name
                       (merge-pathnames "grammar.json" base)
                       (merge-pathnames "node-types.json" base))
                 results)))
       :test [{string= "node-types"} #'namestring #'pathname-name])))

  (defvar *tree-sitter-language-files* (collect-tree-sitter-language-files)
    "Files defining tree sitter languages.")


  (define-software c/cpp (include-paths-mixin)
    ()
    (:documentation "Mix-in class for tree-sitter c and cpp classes")
    (:default-initargs :include-paths nil))

  (define-software ecma ()
    ()
    (:documentation "Mix-in class for tree-sitter ECMAScript variants."))

  (define-software typescript (ecma)
    ()
    (:documentation "Mix-in class for tree-sitter TypeScript variants."))

  ;; It might be useful to break this down into mixins.
  (define-software c-like-syntax ()
    ()
    (:documentation "Mix-in class for software that uses C-like syntax.

C-like syntax means semicolons for delimiters, braces for blocks,
parentheses after the function name for function application, block
comments with /* and */, etc."))

  (defclass ecma-ast ()
    ()
    (:documentation "Mix-in class for tree-sitter ECMAScript ASTs."))

  (defclass typescript-ast (ecma-ast)
    ()
    (:documentation "Mix-in class for tree-sitter TypeScript ASTs."))

  (defclass c/cpp-ast ()
    ()
    (:documentation "Mix-in class for C and C++ ASTs."))

  (defclass c-like-syntax-ast ()
    ()
    (:documentation "Mix-in class for ASTs in languages that use C-like syntax.

See `c-like-syntax' for more discussion of what \"C-like\" means."))

  (defclass normal-scope-ast ()
    ()
    (:documentation "Mixin for ASTs whose languages use \"normal scope\"."))

  (defparameter *tree-sitter-software-superclasses*
    '((:c compilable normal-scope c/cpp c-like-syntax)
      (:cpp compilable normal-scope c/cpp c-like-syntax)
      (:golang normal-scope c-like-syntax)
      (:java c-like-syntax)
      (:javascript normal-scope ecma c-like-syntax)
      (:rust c-like-syntax normal-scope)
      (:typescript-ts typescript c-like-syntax)
      (:typescript-tsx typescript c-like-syntax)))

  (defparameter *tree-sitter-software-direct-slots* '()
    "Alist of direct slots for software classes, such as `python' or
    `c'.")

  (defparameter *tree-sitter-base-ast-superclasses*
    '((:c c/cpp-ast c-like-syntax-ast normal-scope-ast)
      (:cpp c/cpp-ast c-like-syntax-ast normal-scope-ast)
      (:golang c-like-syntax-ast normal-scope-ast)
      (:java c-like-syntax-ast)
      (:javascript ecma-ast c-like-syntax-ast normal-scope-ast)
      (:rust c-like-syntax-ast normal-scope-ast)
      (:typescript-ts typescript-ast c-like-syntax-ast)
      (:typescript-tsx typescript-ast c-like-syntax-ast))
    "Alist of superclasses for the base class of a language (e.g.
    `python-ast').")

  (defparameter *tree-sitter-ast-extra-slots*
    '((:c
       (c-parameter-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (c-field-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (c-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (c-function-definition
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (c-sized-type-specifier (:modifiers (:multiple . t)))
       (c-type-descriptor
        (:pre-type-qualifiers (:multiple . t))
        (:post-type-qualifiers (:multiple . t)))
       (c-case-statement
        (:statements (:multiple . t)))
       (c-labeled-statement (:statement)))
      (:cpp
       (cpp-parameter-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-field-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-enum-specifier (:scope))
       (cpp-function-definition
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-for-range-loop
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-operator-cast
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-operator-name (:name) (:array) (:suffix-identifier))
       (cpp-template-instantiation
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-optional-parameter-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-variadic-parameter-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-type-descriptor
        (:pre-type-qualifiers (:multiple . t))
        (:post-type-qualifiers (:multiple . t)))
       (cpp-case-statement
        (:statements (:multiple . t)))
       (cpp-labeled-statement (:statement))
       (cpp-sized-type-specifier (:modifiers (:multiple . t)))
       (cpp-access-specifier (:keyword))
       (cpp-type-parameter-declaration (:keyword))
       (cpp-reference-declarator (:valueness))
       (cpp-variadic-reference-declarator (:valueness)))
      (:java
       (java-modifiers (:modifiers (:multiple . t)))
       (java-module-declaration (:open)))
      (:python
       (python-function-definition (:async))
       (python-for-statement (:async))
       (python-with-statement (:async)))
      (:rust
       (rust-closure-expression (:move))
       (rust-function-modifiers (:modifiers (:multiple . t)))
       (rust-let-declaration (:mutable-specifier))
       (rust-self-parameter (:borrow))
       (rust-reference-expression (:mutable-specifier))
       (rust-reference-type (:mutable-specifier))
       (rust-parameter (:mutable-specifier))
       (rust-unary-expression (:operator))
       (rust-generic-type (:turbofish-operator))
       (rust-generic-type-with-turbofish (:turbofish-operator))
       (rust-range-expression (:operator)))
      (:javascript
       (javascript-function-declaration (:async))
       (javascript-export-statement (:default)))
      (:typescript-ts
       (typescript-ts-export-statement (:default))
       (typescript-ts-method-definition (:getter-setter) (:optional) (:async))
       (typescript-ts-public-field-definition (:declare) (:modifiers) (:optional))
       (typescript-ts-method-signature (:optional))
       (typescript-ts-abstract-method-signature (:optional))
       (typescript-ts-property-signature (:optional))
       (typescript-ts-member-expression (:operator))
       (typescript-ts-subscript-expression (:operator))
       (typescript-ts-call-expression (:operator))
       (typescript-ts-arrow-function (:async))
       (typescript-ts-function (:async))
       (typescript-ts-function-declaration (:async))
       (typescript-ts-function-signature (:async))
       (typescript-ts-generator-function-declaration (:async))
       (typescript-ts-enum-declaration (:kind))
       (typescript-ts-required-parameter (:modifiers (:multiple . t)))
       (typescript-ts-optional-parameter (:modifiers (:multiple . t))))
      (:typescript-tsx
       (typescript-tsx-export-statement (:default))
       (typescript-tsx-method-definition (:getter-setter) (:optional) (:async))
       (typescript-tsx-public-field-definition (:declare) (:modifiers) (:optional))
       (typescript-tsx-method-signature (:getter-setter) (:optional))
       (typescript-tsx-abstract-method-signature (:optional))
       (typescript-tsx-property-signature (:optional))
       (typescript-tsx-member-expression (:operator))
       (typescript-tsx-subscript-expression (:operator))
       (typescript-tsx-call-expression (:operator))
       (typescript-tsx-arrow-function (:async))
       (typescript-tsx-function (:async))
       (typescript-tsx-function-declaration (:async))
       (typescript-tsx-function-signature (:async))
       (typescript-tsx-generator-function-declaration (:async))
       (typescript-tsx-enum-declaration (:kind))
       (typescript-tsx-required-parameter (:modifiers (:multiple . t)))
       (typescript-tsx-optional-parameter (:modifiers (:multiple . t)))))
    "Alist from languages to classes with extra slots.
The form should be the same as the fields in the note-types.json
for the language.")

  (defparameter *tree-sitter-ast-extra-slot-options*
    '((:c
       (c-init-declarator
        (c-declarator :initarg :lhs :reader lhs)
        (c-value :initarg :rhs :reader rhs))
       (c-assignment-expression
        (c-operator :initarg :operator :reader operator)
        (c-left :initarg :lhs :reader lhs :reader assignee)
        (c-right :initarg :rhs :reader rhs))
       (c-call-expression
        (c-function :reader call-function :initarg :function)
        (c-arguments :reader call-arguments :initarg :arguments))
       (c-while-statement
        (c-body :reader body :initarg :body))
       (c-do-statement
        (c-body :reader body :initarg :body))
       (c-for-statement
        (c-body :reader body :initarg :body))
       (c-if-statement
        ;; TODO Is it worth shadowing `cl:condition'? `test' would
        ;; also cause package conflicts with test frameworks.
        (c-condition :initarg :condition :reader condition)
        (c-consequence :initarg :consequence :reader consequence)
        (c-alternative :initarg :alternative :reader alternative))
       (c-conditional-expression
        ;; TODO Is it worth shadowing `cl:condition'? `test' would
        ;; also cause package conflicts with test frameworks.
        (c-condition :initarg :condition :reader condition)
        (c-consequence :initarg :consequence :reader consequence)
        (c-alternative :initarg :alternative :reader alternative))
       (c-update-expression
        (c-argument :reader assignee))
       (c-binary-expression
        (c-left :reader lhs :initarg :lhs)
        (c-operator :reader operator :initarg :operator)
        (c-right :reader rhs :initarg :rhs))
       (c-comma-expression
        (c-left :reader lhs :initarg :lhs)
        (c-right :reader rhs :initarg :rhs)))
      (:cpp
       (cpp-init-declarator
        (cpp-declarator :initarg :lhs :reader lhs)
        (cpp-value :initarg :rhs :reader rhs))
       (cpp-assignment-expression
        (cpp-operator :initarg :operator :reader operator)
        (cpp-left :initarg :lhs :reader lhs :reader assignee)
        (cpp-right :initarg :rhs :reader rhs))
       (cpp-call-expression
        (cpp-function :reader call-function :initarg :function)
        (cpp-arguments :reader call-arguments :initarg :arguments))
       (cpp-while-statement
        (cpp-body :reader body :initarg :body))
       (cpp-do-statement
        (cpp-body :reader body :initarg :body))
       (cpp-for-statement
        (cpp-body :reader body :initarg :body))
       (cpp-for-range-loop
        (cpp-body :reader body :initarg :body))
       (cpp-function-definition
        (cpp-body :reader function-body))
       (cpp-if-statement
        (cpp-condition :initarg :condition :reader condition)
        (cpp-consequence :initarg :consequence :reader consequence)
        (cpp-alternative :initarg :alternative :reader alternative))
       (cpp-conditional-expression
        (cpp-condition :initarg :condition :reader condition)
        (cpp-consequence :initarg :consequence :reader consequence)
        (cpp-alternative :initarg :alternative :reader alternative))
       (cpp-update-expression
        (cpp-argument :reader assignee)
        (cpp-operator :initarg :operator :reader operator))
       (cpp-binary-expression
        (cpp-left :reader lhs :initarg :lhs)
        (cpp-operator :reader operator :initarg :operator)
        (cpp-right :reader rhs :initarg :rhs))
       (cpp-template-declaration
        (cpp-parameters :reader function-parameters))
       (cpp-comma-expression
        (cpp-left :reader lhs :initarg :lhs)
        (cpp-right :reader rhs :initarg :rhs)))
      (:golang
       (golang-const-spec
        (golang-name :reader definition-name-ast))
       (golang-function-declaration
        (golang-body :reader function-body))
       (golang-method-declaration
        (golang-body :reader function-body)))
      (:java
       (java-method-declaration
        (java-name :reader function-name)
        (java-parameters :reader function-parameters)
        (java-body :reader function-body))
       (java-method-invocation
        (java-name :reader call-function)
        (java-arguments :reader call-arguments)))
      (:javascript
       (javascript-switch-case
        (javascript-body :reader body))
       (javascript-assignment-expression
        (javascript-left :reader lhs :reader assignee)
        (javascript-right :reader rhs))
       (javascript-augmented-assignment-expression
        (javascript-left :reader lhs :reader assignee)
        (javascript-right :reader rhs))
       (javascript-assignment-pattern
        (javascript-left :reader lhs :reader assignee)
        (javascript-right :reader rhs))
       (javascript-update-expression
        (javascript-argument :reader assignee)))
      (:python
       (python-call
        (python-function :reader call-function)
        (python-arguments :reader call-arguments))
       (python-assignment
        (python-left :initarg :lhs :reader lhs)
        (python-right :initarg :rhs :reader rhs))
       (python-augmented-assignment
        (python-left :initarg :lhs :reader lhs)
        (python-right :initarg :rhs :reader rhs))
       (python-binary-operator
        (python-left :initarg :lhs :reader lhs)
        (python-right :initarg :rhs :reader rhs)
        (python-operator :initarg :operator :reader operator))
       (python-boolean-operator
        (python-left :initarg :lhs :reader lhs)
        (python-right :initarg :rhs :reader rhs)
        (python-operator :initarg :operator :reader operator))
       (python-function-definition
        (python-body :reader body)
        (python-return-type :reader return-type))
       (python-keyword-argument
        (python-name :initarg :lhs :reader lhs)
        (python-value :initarg :rhs :reader rhs))
       (python-unary-operator
        (python-operator :initarg :operator :reader operator))
       (python-while-statement
        (python-body :reader body)))
      (:rust
       (rust-assignment-expression
        (rust-left :initarg :lhs :reader lhs)
        (rust-right  :initarg :rhs :reader rhs))
       (rust-compound-assignment-expr
        (rust-operator :reader operator :initarg :operator)
        (rust-left :initarg :lhs :reader lhs)
        (rust-right :initarg :rhs :reader rhs))
       (rust-binary-expression
        (rust-left :initarg :lhs :reader lhs)
        (rust-operator :initarg :operator :reader operator)
        (rust-right :initarg :rhs :reader rhs))
       (rust-if-expression
        (rust-condition :initarg :condition :reader condition)
        (rust-consequence :initarg :consequence :reader consequence)
        (rust-alternative :initarg :alternative :reader alternative))
       (rust-let-declaration
        (rust-pattern :initarg :lhs :reader lhs)
        (rust-value :initarg :rhs :reader rhs))
       (rust-function-item
        (rust-name :reader function-name :reader definition-name-ast)
        (rust-parameters :reader function-parameters)
        (rust-body :reader function-body))
       (rust-call-expression
        (rust-function :reader call-function :initarg :function)
        (rust-arguments :reader call-arguments :initarg :arguments))
       (rust-parameter
        (rust-type :reader parameter-type))
       (rust-range-expression
        (rust-operator :reader operator :initarg :operator))
       (rust-for-expression
        (rust-body :reader body :initarg :body))
       (rust-loop-expression
        (rust-body :reader body :initarg :body))
       (rust-while-expression
        (rust-body :reader body :initarg :body))
       (rust-while-let-expression
        (rust-body :reader body :initarg :body)))
      ((:typescript-ts :typescript-tsx)
       ;; Anonymous function (function keyword).
       (typescript-ts-function
        (typescript-ts-return-type
         :reader return-type))
       (typescript-tsx-function
        (typescript-tsx-return-type
         :reader return-type))
       ;; Function declaration.
       (typescript-ts-function-declaration
        (typescript-ts-return-type
         :reader return-type)
        (typescript-ts-name
         :reader typescript-name))
       (typescript-tsx-function-declaration
        (typescript-tsx-return-type
         :reader return-type)
        (typescript-tsx-name
         :reader typescript-name))
       ;; Function signature (overload).
       (typescript-ts-function-signature
        (typescript-ts-return-type
         :reader return-type))
       (typescript-ts-function-signature
        (typescript-ts-return-type
         :reader return-type))
       ;; Arrow function.
       (typescript-ts-arrow-function
        (typescript-ts-return-type
         :reader return-type))
       (typescript-tsx-arrow-function
        (typescript-tsx-return-type
         :reader return-type))
       ;; Optional parameter.
       (typescript-ts-optional-parameter
        (typescript-ts-type
         :reader parameter-type))
       (typescript-tsx-optional-parameter
        (typescript-tsx-type
         :reader parameter-type))
       ;; Required parameter.
       (typescript-ts-required-parameter
        (typescript-ts-type
         :reader parameter-type))
       (typescript-tsx-required-parameter
        (typescript-tsx-type
         :reader parameter-type))
       (typescript-ts-assignment-expression
        (typescript-ts-left :reader lhs :reader assignee)
        (typescript-ts-right :reader rhs))
       (typescript-tsx-assignment-expression
        (typescript-tsx-left :reader lhs :reader assignee)
        (typescript-tsx-right :reader rhs))
       (typescript-ts-assignment-pattern
        (typescript-ts-left :reader lhs :reader assignee)
        (typescript-ts-right :reader rhs))
       (typescript-tsx-assignment-pattern
        (typescript-tsx-left :reader lhs :reader assignee)
        (typescript-tsx-right :reader rhs))
       (typescript-ts-update-expression
        (typescript-ts-argument :reader assignee))
       (typescript-tsx-update-expression
        (typescript-tsx-argument :reader assignee))))
    "Alist from languages to classes with extra slot options.")

  (defparameter *tree-sitter-ast-superclasses*
    '((:c
       ;; A subclass of a declaration AST that isn't really a
       ;; declaration.
       (:degenerate-declaration-ast
        c-struct-tag-specifier
        c-enum-tag-specifier
        c-union-tag-specifier)
       (:c-tag-specifier
        c-struct-tag-specifier
        c-enum-tag-specifier
        c-union-tag-specifier))
      (:cpp
       (:definition-ast cpp-class-specifier cpp-namespace-definition)
       (:type-declaration-ast cpp-class-specifier cpp-alias-declaration cpp-type-parameter-declaration)
       (:composite-type-ast cpp-class-specifier)
       (:namespace-declaration-ast cpp-namespace-definition)
       (:string-ast cpp-raw-string-literal)
       (:variable-declaration-ast
        cpp-optional-parameter-declaration
        cpp-variadic-parameter-declaration)
       (:variable-initialization-ast cpp-optional-parameter-declaration)
       (:identifier-ast cpp-namespace-identifier
        cpp-qualified-identifier
        cpp-type-identifier
        cpp-primitive-type
        cpp-operator-name)
       (:catch-ast cpp-catch-clause)
       (:loop-ast cpp-for-range-loop)
       (:parameter-ast
        cpp-optional-parameter-declaration
        cpp-optional-type-parameter-declaration
        cpp-template-template-parameter-declaration
        cpp-type-parameter-declaration
        cpp-variadic-parameter-declaration
        cpp-variadic-type-parameter-declaration)
       (:parameters-ast cpp-template-parameter-list)
       (:type-ast cpp-template-type)
       (:lambda-ast cpp-lambda-expression))
      ((:c :cpp)
       (:c/cpp-+ c-+ cpp-+)
       (:c/cpp-- c-- cpp--)
       (:c/cpp-* c-* cpp-*)
       (:c/cpp-/ c-/ cpp-/)
       (:c/cpp-! c-! cpp-!)
       (:c/cpp-&& c-&& cpp-&&)
       (:|C/CPP-\|\|| |C-\|\|| |CPP-\|\||)
       (:c/cpp-& c-& cpp-&)
       (:|C/CPP-\|| |C-\|| |CPP-\||)
       (:c/cpp-<< c-<< cpp-<<)
       (:c/cpp->> c->> cpp->>)
       (:c/cpp-~ c-~ cpp-~)
       (:c/cpp-^ c-^ cpp-^)
       (:c/cpp-% c-% cpp-%)
       (:c/cpp-== c-== cpp-==)
       (:c/cpp-<= c-<= cpp-<=)
       (:c/cpp->= c->= cpp->=)
       (:c/cpp-!= c-!= cpp-!=)
       (:c/cpp-< c-< cpp-<)
       (:c/cpp-> c-> cpp->)
       (:c/cpp--> c--> cpp-->)
       (:increment-operator-ast c-++ cpp-++)
       (:c/cpp-++ c-++ cpp-++)
       (:decrement-operator-ast c--- cpp---)
       (:c/cpp--- c--- cpp---)
       (:c/cpp-#define c-#define cpp-#define)
       (:c/cpp-#else c-#else cpp-#else)
       (:c/cpp-#elif c-#elif cpp-#elif)
       (:c/cpp-#endif c-#endif cpp-#endif)
       (:c/cpp-#if c-#if cpp-#if)
       (:c/cpp-#ifdef c-#ifdef cpp-#ifdef)
       (:c/cpp-#ifndef c-#ifndef cpp-#ifndef)
       (:c/cpp-#include c-#include cpp-#include)
       (:c/cpp-abstract-array-declarator c-abstract-array-declarator
        cpp-abstract-array-declarator)
       (:c/cpp-abstract-function-declarator c-abstract-function-declarator
        cpp-abstract-function-declarator)
       (:c/cpp-abstract-pointer-declarator c-abstract-pointer-declarator
        cpp-abstract-pointer-declarator)
       (:c/cpp-argument-list c-argument-list cpp-argument-list)
       (:c/cpp-array-declarator c-array-declarator cpp-array-declarator)
       (:c/cpp-assignment-expression c-assignment-expression
        cpp-assignment-expression)
       (:c/cpp-binary-expression c-binary-expression cpp-binary-expression)
       (:c/cpp-break-statement c-break-statement cpp-break-statement)
       (:c/cpp-call-expression c-call-expression cpp-call-expression)
       (:c/cpp-case-statement c-case-statement cpp-case-statement)
       (:c/cpp-cast-expression c-cast-expression cpp-cast-expression)
       (:c/cpp-char-literal c-char-literal cpp-char-literal)
       ;; This is a common superclass for all three ways of defining a
       ;; class in C++ -- class, struct, and union -- and their C
       ;; equivalents (when they exist).
       (:c/cpp-classoid-specifier
        c-struct-specifier
        c-union-specifier
        cpp-struct-specifier
        cpp-class-specifier
        cpp-union-specifier)
       (:c/cpp-comma-expression c-comma-expression cpp-comma-expression)
       (:c/cpp-comment c-comment cpp-comment)
       (:c/cpp-compound-statement c-compound-statement cpp-compound-statement)
       (:c/cpp-condition-clause c-condition-clause cpp-condition-clause)
       (:c/cpp-conditional-expression c-conditional-expression cpp-conditional-expression)
       (:c/cpp-continue-statement c-continue-statement cpp-continue-statement)
       (:c/cpp-declaration c-declaration cpp-declaration)
       (:c/cpp-declarator c--declarator cpp--declarator)
       (:c/cpp-do-statement c-do-statement cpp-do-statement)
       (:c/cpp-empty-statement c-empty-statement cpp-empty-statement)
       (:c/cpp-enum-specifier c-enum-specifier cpp-enum-specifier)
       (:c/cpp-enumerator c-enumerator cpp-enumerator)
       (:c/cpp-enumerator-list c-enumerator-list cpp-enumerator-list)
       (:c/cpp-error c-error cpp-error)
       (:c/cpp--expression c--expression cpp--expression)
       (:c/cpp-expression-statement
        c-expression-statement cpp-expression-statement)
       (:c/cpp-field-declaration c-field-declaration cpp-field-declaration)
       (:c/cpp-field-declaration-list c-field-declaration-list cpp-field-declaration-list)
       (:c/cpp-field-expression c-field-expression cpp-field-expression)
       (:c/cpp-field-identifier c-field-identifier cpp-field-identifier)
       (:c/cpp-for-statement c-for-statement cpp-for-statement)
       (:c/cpp-function-declarator c-function-declarator cpp-function-declarator)
       (:c/cpp-function-definition c-function-definition cpp-function-definition)
       (:c/cpp-identifier c-identifier cpp-identifier)
       (:c/cpp-if-statement c-if-statement cpp-if-statement)
       (:c/cpp-init-declarator c-init-declarator cpp-init-declarator)
       (:c/cpp-initializer-pair c-initializer-pair cpp-initializer-pair)
       (:c/cpp-labeled-statement c-labeled-statement cpp-labeled-statement)
       (:c/cpp-macro-forward-declaration
        c-macro-forward-declaration cpp-macro-forward-declaration)
       (:c/cpp-number-literal c-number-literal cpp-number-literal)
       (:c/cpp-parameter-declaration c-parameter-declaration
        cpp-parameter-declaration)
       (:c/cpp-parameter-list c-parameter-list cpp-parameter-list)
       (:c/cpp-parenthesized-declarator c-parenthesized-declarator
        cpp-parenthesized-declarator)
       (:c/cpp-parenthesized-expression c-parenthesized-expression
        cpp-parenthesized-expression)
       (:c/cpp-pointer-declarator c-pointer-declarator cpp-pointer-declarator)
       (:c/cpp-pointer-expression c-pointer-expression cpp-pointer-expression)
       (:c/cpp-preproc-arg c-preproc-arg cpp-preproc-arg)
       (:c/cpp-preproc-def c-preproc-def cpp-preproc-def)
       (:c/cpp-preproc-elif c-preproc-elif cpp-preproc-elif)
       (:c/cpp-preproc-else c-preproc-else cpp-preproc-else)
       (:c/cpp-preproc-function-def
        c-preproc-function-def
        cpp-preproc-function-def)
       (:c/cpp-preproc-if c-preproc-if cpp-preproc-if)
       (:c/cpp-preproc-ifdef c-preproc-ifdef cpp-preproc-ifdef)
       (:c/cpp-preproc-include c-preproc-include cpp-preproc-include)
       (:c/cpp-preproc-params c-preproc-params cpp-preproc-params)
       (:c/cpp-primitive-type c-primitive-type cpp-primitive-type)
       (:c/cpp-return-statement c-return-statement cpp-return-statement)
       (:c/cpp-signed c-signed cpp-signed)
       (:c/cpp-sized-type-specifier
        c-sized-type-specifier cpp-sized-type-specifier)
       (:c/cpp-sizeof-expression c-sizeof-expression cpp-sizeof-expression)
       (c/cpp--statement c--statement cpp--statement)
       (:c/cpp-storage-class-specifier c-storage-class-specifier
        cpp-storage-class-specifier)
       (:c/cpp-string-literal c-string-literal cpp-string-literal)
       (:c/cpp-struct-specifier c-struct-specifier cpp-struct-specifier)
       (:c/cpp-subscript-expression
        c-subscript-expression cpp-subscript-expression)
       (:c/cpp-switch-statement c-switch-statement cpp-switch-statement)
       (:c/cpp-system-lib-string c-system-lib-string cpp-system-lib-string)
       (:c/cpp-translation-unit c-translation-unit cpp-translation-unit)
       (:c/cpp-type-definition c-type-definition cpp-type-definition)
       (:c/cpp-type-descriptor c-type-descriptor cpp-type-descriptor)
       (:c/cpp-type-forward-declaration
        c-type-forward-declaration cpp-type-forward-declaration)
       (:c/cpp-type-identifier c-type-identifier cpp-type-identifier)
       (:c/cpp-type-qualifier c-type-qualifier cpp-type-qualifier)
       (:c/cpp-unary-expression c-unary-expression cpp-unary-expression)
       (:c/cpp-union-specifier c-union-specifier cpp-union-specifier)
       (:c/cpp-update-expression c-update-expression cpp-update-expression)
       (:c/cpp-while-statement c-while-statement cpp-while-statement))
      (:cl
       (:literal-ast cl-char-lit cl-kwd-lit cl-list-lit cl-map-lit
        cl-package-lit cl-path-lit cl-quoting-lit cl-read-cond-lit
        cl-sym-lit cl-syn-quoting-lit cl-unquote-splicing-lit cl-unquoting-lit
        cl-var-quoting-lit cl-vec-lit)
       (:string-ast cl-str-lit)
       (:loop-ast cl-loop-macro)
       (:number-ast cl-num-lit cl-complex-num-lit cl-imaginary))
      (:golang
       (:root-ast golang-source-file)
       (:comment-ast golang-comment)
       (:compound-ast golang-block)
       (:declaration-ast golang-field-declaration golang-type-declaration)
       (:definition-ast
        ;; TODO: Everything here also declaration?
        golang-type-declaration
        )
       (:function-declaration-ast
        golang-function-declaration golang-method-declaration)
       (:identifier-ast golang-identifier golang-field-identifier)
       (:loop-ast golang-for-statement)
       (:string-ast )
       (:type-ast golang-qualified-type golang-pointer-type golang-struct-type
        golang-interface-type golang-array-type golang-slice-type
        golang-map-type golang-channel-type golang-function-type)
       (:type-identifier-ast golang-type-identifier)
       (:variable-declaration-ast
        golang-var-declaration golang-const-declaration))
      (:java
       (:arguments-ast java-argument-list)
       (:assignment-ast java-assignment-expression java-update-expression)
       (:binary-ast java-binary-expression)
       (:boolean-false-ast java-false)
       (:boolean-true-ast java-true)
       (:catch-ast java-catch-clause)
       (:class-ast java-class-declaration)
       (:comment-ast java-comment)
       (:control-flow-ast java-switch-expression java-try-statement)
       (:expression-ast java-expression)
       (:field-asst java-field-access)
       (:identifier-ast java-identifier)
       (:if-ast java-if-statement)
       (:lambda-ast java-lambda-expression)
       (:literal-ast java-class-literal java-decimal-integer-literal
        java-hex-integer-literal java-octal-integer-literal
        java-binary-integer-literal java-decimal-floating-point-literal
        java-hex-floating-point-literal null-literal)
       (:loop-ast java-for-statement java-while-statement java-do-statement
        java-enhanced-for-statement)
       (:parenthesized-expression-ast java-parenthesized-expression)
       (:return-ast java-return-statement)
       (:root-ast java-program)
       (:statement-ast java-statement)
       (:string-ast java-string-literal)
       (:type-identifier-ast java-type-identifier)
       (:type-ast java-generic-type)
       (:unary-ast java-unary-expression)
       (:variable-declaration-ast java-local-variable-declaration
        java-variable-declarator)
       (:while-ast java-while-statement))
      (:javascript
       (:root-ast javascript-program)
       (:comment-ast javascript-comment)
       (:class-ast javascript-class-declaration)
       (:control-flow-ast
        javascript-switch-statement javascript-try-statement)
       (:if-ast javascript-if-statement)
       (:while-ast javascript-while-statement)
       (:expression-ast javascript--expression)
       (:parenthesized-expression-ast javascript-parenthesized-expression)
       (:compound-ast javascript-statement-block)
       (:boolean-true-ast javascript-true)
       (:boolean-false-ast javascript-false)
       (:function-declaration-ast javascript-function-declaration)
       (:lambda-ast javascript-function javascript-arrow-function)
       (:parameters-ast javascript-formal-parameters)
       (:variable-declaration-ast javascript-variable-declaration-ast)
       (:identifier-ast
        javascript-identifier javascript-property-identifier
        javascript-shorthand-property-identifier
        javascript-shorthand-property-identifier-pattern)
       (:field-ast javascript-member-expression)
       (:subscript-ast javascript-subscript-expression)
       (:float-ast javascript-number)
       (:string-ast javascript-string)
       (:loop-ast
        javascript-for-statement javascript-do-statement
        javascript-while-statement)
       (:statement-ast javascript--statement javascript-statement)
       (:expression-statement-ast javascript-expression-statement)
       (:call-ast javascript-call-expression)
       (:arguments-ast javascript-arguments)
       (:unary-ast javascript-unary-expression)
       (:binary-ast javascript-binary-expression)
       (:return-ast javascript-return-statement)
       (:catch-ast javascript-catch-clause))
      ((:javascript :typescript-ts :typescript-tsx)
       (:ecma-comment
        javascript-comment
        typescript-ts-comment
        typescript-tsx-comment)
       (:ecma-error
        javascript-error
        typescript-ts-error
        typescript-tsx-error)
       (:ecma-variable-declarator
        javascript-variable-declarator
        typescript-ts-variable-declarator
        typescript-tsx-variable-declarator)
       (:ecma-assignment-expression
        javascript-assignment-expression
        typescript-ts-assignment-expression
        typescript-tsx-assignment-expression)
       (:ecma-assignment-pattern
        javascript-assignment-pattern
        typescript-ts-assignment-pattern
        typescript-tsx-assignment-pattern)
       (:assignment-ast
        javascript-assignment-expression
        javascript-augmented-assignment-expression
        javascript-assignment-pattern
        javascript-update-expression
        typescript-ts-assignment-expression
        typescript-ts-augmented-assignment-expression
        typescript-ts-assignment-pattern
        typescript-ts-update-expression
        typescript-tsx-assignment-expression
        typescript-tsx-augmented-assignment-expression
        typescript-tsx-assignment-pattern
        typescript-tsx-update-expression)
       (:ecma-rest-pattern
        javascript-rest-pattern
        typescript-ts-rest-pattern
        typescript-tsx-rest-pattern)
       (:ecma-call-expression
        javascript-call-expression
        typescript-ts-call-expression
        typescript-tsx-call-expression)
       (:ecma-arguments
        javascript-arguments
        typescript-ts-arguments
        typescript-tsx-arguments)
       (:ecma-parenthesized-expression
        javascript-parenthesized-expression
        typescript-ts-parenthesized-expression
        typescript-tsx-parenthesized-expression)
       (:ecma-member-expression
        javascript-member-expression
        typescript-ts-member-expression
        typescript-tsx-member-expression)
       (:decrement-operator-ast
        javascript---
        typescript-ts---
        typescript-tsx---)
       (:increment-operator-ast
        javascript-++
        typescript-ts-++
        typescript-tsx-++)
       (:semicolon-ast
        javascript-\;
        typescript-ts-\;
        typescript-tsx-\;))
      (:python
       (:root-ast python-module)
       (:comment-ast python-comment)
       (:class-ast python-class-definition)
       (:control-flow-ast
        python-try-statement python-conditional-expression
        python-list-comprehension python-set-comprehension
        python-generator-expression python-dictionary-comprehension)
       (:if-ast python-if-statement)
       (:while-ast python-while-statement)
       (:expression-ast python-expression)
       (:parenthesized-expression-ast python-parenthesized-expression)
       (:function-declaration-ast python-function-definition)
       (:parameters-ast python-parameters python-lambda-parameters)
       (:boolean-true-ast python-true)
       (:boolean-false-ast python-false)
       (:identifier-ast python-identifier)
       (:field-ast python-attribute)
       (:subscript-ast python-subscript)
       (:lambda-ast python-lambda)
       (:integer-ast python-integer)
       (:float-ast python-float)
       (:string-ast python-string)
       (:loop-ast
        python-while-statement python-for-statement python-for-in-clause)
       (:statement-ast python--compound-statement python--simple-statement)
       (:expression-statement-ast python-expression-statement)
       (:compound-ast python-block)
       (:call-ast python-call)
       (:arguments-ast python-argument-list)
       (:unary-ast python-unary-operator python-not-operator)
       (:binary-ast python-binary-operator python-boolean-operator)
       (:return-ast python-return-statement)
       (:variable-declaration-ast python-assignment python-keyword-argument)
       (:parameter-ast python-parameter)
       (:assignment-ast python-assignment python-augmented-assignment)
       (:catch-ast python-except-clause))
      (:rust
       (:arguments-ast rust-arguments)
       (:assignment-ast
        rust-assignment-expression rust-compound-assignment-expr)
       (:binary-ast rust-binary-expression)
       (:boolean-ast rust-boolean-literal)
       (:call-ast rust-call-expression)
       (:comment-ast rust-line-comment rust-block-comment)
       (:compound-ast rust-block rust-declaration-list)
       (:expression-ast rust--expression)
       (:float-ast rust-float-literal)
       (:function-declaration-ast rust-function-item)
       (:identifier-ast
        rust-field-identifier
        rust-identifier
        rust-primitive-type)
       (:integer-ast rust-integer-literal)
       (:loop-ast
        rust-for-expression
        rust-loop-expression
        rust-while-expression
        rust-while-let-expression)
       (:namespace-declaration-ast rust-mod-item)
       (:parameter-ast rust-parameter rust-self-parameter)
       (:parameters-ast rust-parameters)
       (:parenthesized-expression-ast rust-parenthesized-expression)
       (:return-ast rust-return-expression rust-implicit-return-expression)
       (:root-ast rust-source-file)
       (:statement-ast rust--declaration-statement rust-expression-statement)
       (:string-ast rust-string-literal)
       (:subscript-ast rust-index-expression)
       (:type-ast rust--type)
       (:type-declaration-ast rust-struct-item)
       (:type-identifier-ast rust-type-identifier)
       (:unary-ast rust-unary-expression)
       (:variable-declaration-ast rust-let-declaration))
      ((:typescript-ts :typescript-tsx)
       (:root-ast
        typescript-ts-program
        typescript-tsx-program)
       (:class-ast
        typescript-ts-class-declaration
        typescript-tsx-class-declaration)
       (:boolean-true-ast
        typescript-ts-true
        typescript-tsx-true)
       (:boolean-false-ast
        typescript-ts-false
        typescript-tsx-false)
       (:identifier-ast
        typescript-ts-identifier
        typescript-tsx-identifier
        typescript-ts-property-identifier
        typescript-tsx-property-identifier
        typescript-ts-shorthand-property-identifier
        typescript-tsx-shorthand-property-identifier
        typescript-ts-shorthand-property-identifier-pattern
        typescript-tsx-shorthand-property-identifier-pattern)
       (:call-ast
        typescript-ts-call-expression
        typescript-tsx-call-expression)
       (:control-flow-ast
        typescript-ts-switch-statement
        typescript-tsx-switch-statement
        typescript-ts-while-statement
        typescript-tsx-while-statement)
       (:function-declaration-ast
        typescript-ts-function-declaration
        typescript-tsx-function-declaration
        typescript-ts-function-signature
        typescript-tsx-function-signature)
       (:function-ast
        typescript-ts-function
        typescript-tsx-function)
       (:lambda-ast
        typescript-ts-arrow-function
        typescript-tsx-arrow-function)
       (:parameter-ast
        typescript-ts-required-parameter
        typescript-tsx-required-parameter
        typescript-ts-optional-parameter
        typescript-tsx-optional-parameter)
       (:typescript-program
        typescript-ts-program
        typescript-tsx-program)
       (:typescript-type-annotation
        typescript-ts-type-annotation
        typescript-tsx-type-annotation)
       (:typescript-parameter
        typescript-ts-required-parameter
        typescript-tsx-required-parameter
        typescript-ts-optional-parameter
        typescript-tsx-optional-parameter)
       (:typescript-required-parameter
        typescript-ts-required-parameter
        typescript-tsx-required-parameter)
       (:typescript-optional-parameter
        typescript-ts-optional-parameter
        typescript-tsx-optional-parameter)
       (:typescript-rest-pattern
        typescript-ts-rest-pattern
        typescript-tsx-rest-pattern)
       (:typescript-assignment-pattern
        typescript-ts-assignment-pattern
        typescript-tsx-assignment-pattern)
       (:typescript-function-declaration
        typescript-ts-function-declaration
        typescript-tsx-function-declaration)
       (:typescript-function-signature
        typescript-ts-function-signature
        typescript-tsx-function-signature)
       (:typescript-variable-declarator
        typescript-ts-variable-declarator
        typescript-tsx-variable-declarator)
       (:typescript-assignment-expression
        typescript-ts-assignment-expression
        typescript-tsx-assignment-expression)
       (:typescript-identifier
        typescript-ts-identifier
        typescript-tsx-identifier)
       (:typescript-tuple-type
        typescript-ts-tuple-type
        typescript-tsx-tuple-type)
       (:typescript-array-type
        typescript-ts-array-type
        typescript-tsx-array-type)
       (:typescript-intersection-type
        typescript-ts-intersection-type
        typescript-tsx-intersection-type)
       (:typescript-union-type
        typescript-ts-union-type
        typescript-tsx-union-type)
       (:typescript-type-identifier
        typescript-ts-type-identifier
        typescript-tsx-type-identifier)
       (:type-identifier-ast
        typescript-ts-type-identifier
        typescript-tsx-type-identifier)
       (:typescript-generic-type
        typescript-ts-generic-type
        typescript-tsx-generic-type)
       (:typescript-object-type
        typescript-ts-object-type
        typescript-tsx-object-type)
       (:typescript-index-signature
        typescript-ts-index-signature
        typescript-tsx-index-signature)
       (:typescript-property-signature
        typescript-ts-property-signature
        typescript-tsx-property-signature)
       (:typescript-property-identifier
        typescript-ts-property-identifier
        typescript-tsx-property-identifier)
       (:typescript-flow-maybe-type
        typescript-ts-flow-maybe-type
        typescript-tsx-flow-maybe-type)
       (:typescript-literal-type
        typescript-ts-literal-type
        typescript-tsx-literal-type)
       (:typescript-type-arguments
        typescript-ts-type-arguments
        typescript-tsx-type-arguments)
       (:typescript-type-argument
        typescript-ts-type-argument
        typescript-tsx-type-argument)
       (:typescript-predefined-type
        typescript-ts-predefined-type
        typescript-tsx-predefined-type)
       (:return-ast
        typescript-ts-return-statement
        typescript-tsx-return-statement)))
    "Specifies which classes should inherit from which mixins.
An alist from a language (or list of languages) to an alist of mixins
and tree-sitter AST classes that should inherit from them.

A language may appear multiple times; in this case all the mixins for that language apply.

Note that mixins used here will be automatically exported later, and
those that do not have separate class definitions will be given stub
definitions.")

  (defparameter *tree-sitter-mixin-ast-superclasses*
    '((:c/cpp
       (c/cpp-argument-list arguments-ast)
       (c/cpp-assignment-expression assignment-ast)
       (c/cpp-binary-expression binary-ast)
       (c/cpp-call-expression call-ast)
       (c/cpp-case-statement control-flow-ast)
       (c/cpp-char-literal char-ast)
       (c/cpp-conditional-expression if-ast)
       (c/cpp-comma-expression expression-ast)
       (c/cpp-comment comment-ast)
       (c/cpp-compound-statement compound-ast)
       (c/cpp-declaration statement-ast variable-declaration-ast)
       (c/cpp-do-statement loop-ast)
       (c/cpp--expression expression-ast)
       (c/cpp-expression-statement expression-statement-ast)
       (c/cpp-enum-specifier definition-ast type-declaration-ast)
       (c/cpp-enumerator variable-declaration-ast)
       (c/cpp-error parse-error-ast)
       (c/cpp-false boolean-false-ast)
       (c/cpp-field-expression field-ast)
       (c/cpp-field-declaration definition-ast)
       (c/cpp-field-identifier identifier-ast)
       (c/cpp-function-definition definition-ast function-declaration-ast statement-ast)
       (c/cpp-for-statement loop-ast)
       (c/cpp-goto-statement goto-ast)
       (c/cpp-identifier identifier-ast)
       (c/cpp-if-statement if-ast)
       (c/cpp-init-declarator variable-initialization-ast)
       (c/cpp-macro-forward-declaration macro-declaration-ast)
       (c/cpp-number-literal number-ast)
       (c/cpp-parameter-declaration parameter-ast variable-declaration-ast)
       (c/cpp-parameter-list parameters-ast)
       (c/cpp-parenthesized-expression parenthesized-expression-ast)
       (c/cpp-preproc-def definition-ast macro-declaration-ast)
       (c/cpp-preproc-function-def definition-ast macro-declaration-ast)
       (c/cpp-primitive-type type-ast)
       (c/cpp-return-statement return-ast)
       (c/cpp--statement statement-ast)
       (c/cpp-sized-type-specifier type-ast)
       (c/cpp-string-literal string-ast)
       (c/cpp-struct-specifier composite-type-ast definition-ast type-declaration-ast)
       (c/cpp-subscript-expression subscript-ast)
       (c/cpp-switch-statement control-flow-ast)
       (c/cpp-translation-unit root-ast)
       (c/cpp-true boolean-true-ast)
       (c/cpp-type-forward-declaration type-declaration-ast)
       (c/cpp-type-definition definition-ast type-declaration-ast)
       (c/cpp-type-descriptor type-ast)
       (c/cpp-type-identifier type-identifier-ast)
       (c/cpp-update-expression assignment-ast)
       (c/cpp-unary-expression unary-ast)
       (c/cpp-union-specifier composite-type-ast definition-ast type-declaration-ast)
       (c/cpp-while-statement loop-ast while-ast)))
    "Specifies superclasses for mixin ASTs.")

  ;; TODO: it may make sense to have a way to 'rebind' a subclass when
  ;;       the subclass no longer applies after a mutation.
  (defparameter *tree-sitter-choice-expansion-subclasses*
    '((:c
       ;; TODO: this should be moved over to using the pruned-rule before
       ;;       merging into master.
       (c-update-expression
        (c-update-expression-prefix
         (:seq (:field c-operator c--- c-++) (:field c-argument c--expression)))
        (c-update-expression-postfix
         (:seq (:field c-argument c--expression) (:field c-operator c--- c-++))))
       (c-expression-statement
        (c-expression-statement-
         (:seq (:child c--expression c-comma-expression)))
        (c-empty-expression-statement
         (:seq)))
       (c-struct-specifier
        (c-struct-tag-specifier
         (:seq (:field c-name c-type-identifier))))
       (c-union-specifier
        (c-union-tag-specifier
         (:seq (:field c-name c-type-identifier))))
       (c-enum-specifier
        (c-enum-tag-specifier
         (:seq (:field c-name c-type-identifier)))))
      (:cpp
       (cpp-update-expression
        (cpp-update-expression-prefix
         (:seq (:field cpp-operator cpp--- cpp-++) (:field cpp-argument cpp--expression)))
        (cpp-update-expression-postfix
         (:seq (:field cpp-argument cpp--expression) (:field cpp-operator cpp--- cpp-++))))
       (cpp-expression-statement
        (cpp-expression-statement-
         (:seq (:child cpp--expression cpp-comma-expression)))
        (cpp-empty-expression-statement
         (:seq))))
      (:python
       (python-tuple
        (python-empty-tuple
         (:seq (:slot python-internal-asts-0))))
       (python-dictionary
        (python-empty-dictionary
         (:seq (:slot python-internal-asts-0))))
       (python-parameters
        (python-empty-parameters
         (:seq (:slot python-internal-asts-0))))
       (python-argument-list
        (python-empty-argument-list
         (:seq (:slot python-internal-asts-1)))))
      (:rust
       ;; See
       ;; <https://doc.rust-lang.org/reference/expressions/range-expr.html>.
       ;; Note RangeInclusiveExpr and RangeToInclusiveExpr are not
       ;; currently distinguished.
       (rust-range-expression
        (rust-range-expr
         (:seq (:child rust--expression) (:field rust-operator rust-.. rust-... rust-..=) (:child rust--expression)))
        (rust-range-from-expr
         (:seq (:child rust--expression) (:field rust-operator rust-..)))
        (rust-range-to-expr
         (:seq (:field rust-operator rust-..) (:child rust--expression)))
        (rust-range-full-expr
         (:field rust-operator rust-..)))))
    "Give names to choice expansion subclasses having a certain collapsed rule.
This isn't just for convenience; anonymous choice expansion subclasses
are ignored by templates, whereas named ones are preserved.")

  (defparameter *tree-sitter-json-rule-substitutions*
    '(((:c :cpp)
       ;; These are common to C and C++.
       ;; NOTE: remove this if this is patched upstream.
       (:PREPROC-PARAMS (:TYPE . "SEQ")
        ;; Adds variadic declaration nodes in place of literal ellipses.
        (:MEMBERS
         ((:TYPE . "IMMEDIATE_TOKEN")
          (:CONTENT (:TYPE . "STRING") (:VALUE . "(")))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "CHOICE")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "identifier"))
               ((:TYPE . "SYMBOL") (:NAME . "variadic_declaration"))))
             ((:TYPE . "REPEAT")
              (:CONTENT
               (:TYPE . "SEQ")
               (:MEMBERS
                ((:TYPE . "STRING") (:VALUE . ","))
                ((:TYPE . "CHOICE")
                 (:MEMBERS
                  ((:TYPE . "SYMBOL") (:NAME . "identifier"))
                  ((:TYPE . "SYMBOL")
                   (:NAME . "variadic_declaration")))))))))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ")"))))
       (:SIZED-TYPE-SPECIFIER (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "REPEAT1")
          (:CONTENT
           (:TYPE . "FIELD") (:NAME . "modifiers")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "STRING") (:VALUE . "signed"))
             ((:TYPE . "STRING") (:VALUE . "unsigned"))
             ((:TYPE . "STRING") (:VALUE . "long"))
             ((:TYPE . "STRING") (:VALUE . "short"))))))
         ((:TYPE . "FIELD") (:NAME . "type")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "CHOICE")
             (:MEMBERS
              ((:TYPE . "PREC_DYNAMIC")
               (:VALUE . -1)
               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_identifier")))
              ((:TYPE . "SYMBOL") (:NAME . "primitive_type"))))
            ((:TYPE . "BLANK")))))))
       (:TYPE-DESCRIPTOR
        (:TYPE . "SEQ")
        (:MEMBERS
         ;; Wrap a field around the type qualifier.
         ((:TYPE . "REPEAT")
          (:CONTENT
           (:TYPE . "FIELD") (:NAME . "pre_type_qualifiers")
           (:CONTENT
            (:TYPE . "SYMBOL") (:NAME . "type_qualifier"))))
         ;; Wrap a field around the type specifier.
         ((:TYPE . "FIELD")
          (:NAME . "type")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_specifier")))
         ((:TYPE . "REPEAT")
          (:CONTENT
           ;; Wrap a field around the post type qualifiers.
           (:TYPE . "FIELD") (:NAME . "post_type_qualifiers")
           (:CONTENT
            (:TYPE . "SYMBOL") (:NAME . "type_qualifier"))))
         ((:TYPE . "FIELD")
          (:NAME . "declarator")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "SYMBOL") (:NAME . "_abstract_declarator"))
            ((:TYPE . "BLANK")))))))
       (:CASE-STATEMENT
        (:TYPE . "PREC_RIGHT")
        (:VALUE . 0)
        (:CONTENT
         (:TYPE . "SEQ")
         (:MEMBERS
          ((:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "SEQ")
             (:MEMBERS
              ((:TYPE . "STRING") (:VALUE . "case"))
              ((:TYPE . "FIELD")
               (:NAME . "value")
               (:CONTENT
                (:TYPE . "SYMBOL")
                (:NAME . "_expression")))))
            ((:TYPE . "STRING") (:VALUE . "default"))))
          ((:TYPE . "STRING") (:VALUE . ":"))
          ((:TYPE . "FIELD")
           (:NAME . "STATEMENTS")
           (:CONTENT
            (:TYPE . "REPEAT")
            (:CONTENT
             (:TYPE . "CHOICE")
             (:MEMBERS
              ((:TYPE . "ALIAS")
               (:CONTENT
                (:TYPE . "SYMBOL")
                (:NAME . "attributed_non_case_statement"))
               (:NAMED . T)
               (:VALUE . "attributed_statement"))
              ((:TYPE . "SYMBOL") (:NAME . "_non_case_statement"))
              ((:TYPE . "SYMBOL") (:NAME . "declaration"))
              ((:TYPE . "SYMBOL") (:NAME . "type_definition")))))))))
       (:LABELED-STATEMENT
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "FIELD")
          (:NAME . "label")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement_identifier")))
         ((:TYPE . "STRING") (:VALUE . ":"))
         ((:TYPE . "FIELD")
          (:NAME . "statement")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement")))))
       ;; This is to get around _EMPTY_DECLARATION inlining a semicolon
       ;; without any further information to reproduce it.
       (:EMPTY-STATEMENT
        (:TYPE . "STRING") (:value . ";"))
       ;; Replaces semicolon string with empty_statement.
       ;; This is currently only use for compound-statement, so
       ;; a parse-tree transformation is currently only used for that.
       (:-EMPTY-DECLARATION
        ;; NOTE: this should probably be a SEQ, but it would unfortunately
        ;;       require backtracking which isn't implemented currently, so
        ;;       a CHOICE is used instead.
        (:TYPE . "CHOICE")
        (:MEMBERS
         ((:TYPE . "SYMBOL") (:NAME . "_type_specifier"))
         ((:TYPE . "SYMBOL") (:NAME . "empty_statement"))
         ((:TYPE . "SEQ")
          (:MEMBERS
           ((:TYPE . "SYMBOL") (:NAME . "_type_specifier"))
           ((:TYPE . "SYMBOL") (:NAME . "empty_statement"))))))
       ;; TODO: add type-forward-declaration and macro-forward-declaration here.
       ;;       They'll be very similar to expression-statement. Make sure that
       ;;       _statement is added to their superclasses to ensure this will
       ;;       work.
       (:TYPE-FORWARD-DECLARATION
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "SYMBOL") (:NAME . "_expression"))
         ((:TYPE . "STRING") (:VALUE . ";"))))
       (:MACRO-FORWARD-DECLARATION
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "SYMBOL") (:NAME . "_expression"))
         ((:TYPE . "STRING") (:VALUE . ";")))))
      (:c
       ;; These are specific to C.
       (:-DECLARATION-SPECIFIERS (:TYPE . "SEQ")
        (:MEMBERS
         ;; Inline _declaration_modifiers and wrap a field around it.
         ((:TYPE . "FIELD") (:NAME . "pre-specifiers")
          (:CONTENT
           (:TYPE . "REPEAT")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SYMBOL") (:NAME . "storage_class_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "type_qualifier"))
             ((:TYPE . "SYMBOL") (:NAME . "attribute_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "attribute_declaration"))
             ((:TYPE . "SYMBOL") (:NAME . "ms_declspec_modifier"))))))
         ;; Wrap a field around the type specifier.
         ((:TYPE . "FIELD") (:NAME . "type")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_specifier")))
         ;; Inline declaration-modifiers and wrap a field around it.
         ((:TYPE . "FIELD") (:NAME . "post-specifiers")
          (:CONTENT
           (:TYPE . "REPEAT")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SYMBOL") (:NAME . "storage_class_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "type_qualifier"))
             ((:TYPE . "SYMBOL") (:NAME . "attribute_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "attribute_declaration"))
             ((:TYPE . "SYMBOL") (:NAME . "ms_declspec_modifier"))))))))
       (:STRUCT-SPECIFIER (:TYPE . "SEQ")
        (:MEMBERS ((:TYPE . "STRING") (:VALUE . "struct"))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "ms_declspec_modifier"))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "FIELD")
              (:NAME . "name")
              (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_identifier")))
             ;; Move the choice from inside the body to around the body.
             ((:type . "CHOICE")
              (:members
               ((:TYPE . "FIELD")
                (:NAME . "body")
                (:CONTENT (:TYPE . "SYMBOL") (:NAME . "field_declaration_list")))
               ((:type . "BLANK"))))))
           ((:TYPE . "FIELD")
            (:NAME . "body")
            (:CONTENT
             (:TYPE . "SYMBOL") (:NAME . "field_declaration_list")))))))
       (:UNION-SPECIFIER
        (:TYPE . "SEQ")
        (:MEMBERS ((:TYPE . "STRING") (:VALUE . "union"))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "ms_declspec_modifier"))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "FIELD")
              (:NAME . "name")
              (:CONTENT
               (:TYPE . "SYMBOL")
               (:NAME . "_type_identifier")))
             ;; Move the choice from inside the body to around the body.
             ((:type . "CHOICE")
              (:members
               ((:TYPE . "FIELD")
                (:NAME . "body")
                (:CONTENT
                 (:TYPE . "SYMBOL")
                 (:NAME . "field_declaration_list")))
               ((:type . "BLANK"))))))
           ((:TYPE . "FIELD")
            (:NAME . "body")
            (:CONTENT (:TYPE . "SYMBOL")
                      (:NAME . "field_declaration_list")))))))
       (:ENUM-SPECIFIER
        (:TYPE . "SEQ")
        (:MEMBERS ((:TYPE . "STRING") (:VALUE . "enum"))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "FIELD")
              (:NAME . "name")
              (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_identifier")))
             ;; Move the choice from inside the body to around the body.
             ((:type . "CHOICE")
              (:members
               ((:TYPE . "FIELD")
                (:NAME . "body")
                (:CONTENT
                 (:TYPE . "SYMBOL")
                 (:NAME . "enumerator_list")))
               ((:type . "BLANK")))
              )))
           ((:TYPE . "FIELD")
            (:NAME . "body")
            (:CONTENT (:TYPE . "SYMBOL") (:NAME . "enumerator_list")))))))
       (:LABELED-STATEMENT
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "FIELD")
          (:NAME . "label")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement_identifier")))
         ((:TYPE . "STRING") (:VALUE . ":"))
         ((:TYPE . "FIELD")
          (:NAME . "statement")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement")))))
       (:-NON-CASE-STATEMENT (:TYPE . "CHOICE")
        (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "labeled_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "compound_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "expression_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "if_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "switch_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "do_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "while_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "for_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "return_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "break_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "continue_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "goto_statement"))
         ;; Add support for type and macro forward declarations.
         ((:TYPE . "SYMBOL") (:NAME . "type_forward_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "macro_forward_declaration")))))
      (:cpp
       ;; These are specific to C++.
       (:REFERENCE-DECLARATOR
        (:TYPE . "PREC_DYNAMIC") (:VALUE . 1)
        (:CONTENT (:TYPE . "PREC_RIGHT") (:VALUE . 0)
         (:CONTENT (:TYPE . "SEQ")
          (:MEMBERS
           ;; Wrap in a field.
           ((:type . "FIELD")
            (:name . "valueness")
            (:content
             (:TYPE . "CHOICE")
             ;; Convert strings to symbols.
             (:MEMBERS ((:TYPE . "SYMBOL") (:name . "&"))
                       ((:TYPE . "SYMBOL") (:name . "&&")))))
           ((:TYPE . "SYMBOL") (:NAME . "_declarator"))))))
       (:VARIADIC-REFERENCE-DECLARATOR
        (:TYPE . "SEQ")
        (:MEMBERS
         ;; Wrap in a field.
         ((:type . "FIELD")
          (:name . "valueness")
          (:content
           (:TYPE . "CHOICE")
           ;; Convert strings to symbols.
           (:MEMBERS ((:TYPE . "SYMBOL") (:name . "&"))
                     ((:TYPE . "SYMBOL") (:name . "&&")))))
         ((:TYPE . "SYMBOL") (:NAME . "variadic_declarator"))))
       (:REFERENCE-FIELD-DECLARATOR
        (:TYPE . "PREC_DYNAMIC") (:VALUE . 1)
        (:CONTENT (:TYPE . "PREC_RIGHT") (:VALUE . 0)
         (:CONTENT (:TYPE . "SEQ")
          (:MEMBERS
           ;; Wrap in a field.
           ((:type . "FIELD")
            (:name . "valueness")
            (:content
             (:TYPE . "CHOICE")
             ;; Convert strings to symbols.
             (:MEMBERS ((:TYPE . "SYMBOL") (:name . "&"))
                       ((:TYPE . "SYMBOL") (:name . "&&")))))
           ((:TYPE . "SYMBOL") (:NAME . "_field_declarator"))))))
       (:-DECLARATION-SPECIFIERS (:TYPE . "SEQ")
        (:MEMBERS
         ;; Inline _declaration_modifiers and wrap a field around it.
         ((:TYPE . "FIELD") (:NAME . "pre-specifiers")
          (:CONTENT
           (:TYPE . "REPEAT")
           (:CONTENT
            (:TYPE . "SYMBOL")
            (:NAME . "_declaration_modifiers"))))
         ;; Wrap a field around the type specifier.
         ((:TYPE . "FIELD") (:NAME . "type")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_specifier")))
         ;; Inline declaration-modifiers and wrap a field around it.
         ((:TYPE . "FIELD") (:NAME . "post-specifiers")
          (:CONTENT
           (:TYPE . "REPEAT")
           (:CONTENT
            (:TYPE . "SYMBOL")
            (:NAME . "_declaration_modifiers"))))))
       (:-CONSTRUCTOR-SPECIFIERS (:TYPE . "REPEAT1")
        (:CONTENT (:TYPE . "PREC_RIGHT") (:VALUE . 0)
         (:CONTENT
          (:TYPE . "FIELD")
          (:NAME . "pre_specifiers")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "SYMBOL") (:NAME . "storage_class_specifier"))
            ((:TYPE . "SYMBOL") (:NAME . "type_qualifier"))
            ((:TYPE . "SYMBOL") (:NAME . "attribute_specifier"))
            ((:TYPE . "SYMBOL") (:NAME . "virtual"))
            ((:TYPE . "SYMBOL") (:NAME . "explicit_function_specifier")))))))
       ;; TODO This one could probably be upstreamed.
       (:OPERATOR-NAME
        (:TYPE . "PREC") (:VALUE . 1)
        (:CONTENT (:TYPE . "SEQ")
         (:MEMBERS
          ((:TYPE . "STRING") (:VALUE . "operator"))
          ;; Wrap a "name" field around the operators.
          ((:TYPE . "CHOICE")
           (:MEMBERS
            ((:type . "FIELD")
             (:name . "name")
             (:content
              (:TYPE . "CHOICE")
              (:MEMBERS ((:TYPE . "STRING") (:VALUE . "co_await"))
                        ((:TYPE . "STRING") (:VALUE . "+")) ((:TYPE . "STRING") (:VALUE . "-"))
                        ((:TYPE . "STRING") (:VALUE . "*")) ((:TYPE . "STRING") (:VALUE . "/"))
                        ((:TYPE . "STRING") (:VALUE . "%")) ((:TYPE . "STRING") (:VALUE . "^"))
                        ((:TYPE . "STRING") (:VALUE . "&")) ((:TYPE . "STRING") (:VALUE . "|"))
                        ((:TYPE . "STRING") (:VALUE . "~")) ((:TYPE . "STRING") (:VALUE . "!"))
                        ((:TYPE . "STRING") (:VALUE . "=")) ((:TYPE . "STRING") (:VALUE . "<"))
                        ((:TYPE . "STRING") (:VALUE . ">")) ((:TYPE . "STRING") (:VALUE . "+="))
                        ((:TYPE . "STRING") (:VALUE . "-=")) ((:TYPE . "STRING") (:VALUE . "*="))
                        ((:TYPE . "STRING") (:VALUE . "/=")) ((:TYPE . "STRING") (:VALUE . "%="))
                        ((:TYPE . "STRING") (:VALUE . "^=")) ((:TYPE . "STRING") (:VALUE . "&="))
                        ((:TYPE . "STRING") (:VALUE . "|=")) ((:TYPE . "STRING") (:VALUE . "<<"))
                        ((:TYPE . "STRING") (:VALUE . ">>")) ((:TYPE . "STRING") (:VALUE . ">>="))
                        ((:TYPE . "STRING") (:VALUE . "<<=")) ((:TYPE . "STRING") (:VALUE . "=="))
                        ((:TYPE . "STRING") (:VALUE . "!=")) ((:TYPE . "STRING") (:VALUE . "<="))
                        ((:TYPE . "STRING") (:VALUE . ">=")) ((:TYPE . "STRING") (:VALUE . "&&"))
                        ((:TYPE . "STRING") (:VALUE . "||")) ((:TYPE . "STRING") (:VALUE . "++"))
                        ((:TYPE . "STRING") (:VALUE . "--")) ((:TYPE . "STRING") (:VALUE . ","))
                        ((:TYPE . "STRING") (:VALUE . "->*")) ((:TYPE . "STRING") (:VALUE . "->"))
                        ((:TYPE . "STRING") (:VALUE . "()")) ((:TYPE . "STRING") (:VALUE . "[]")))))
            ;; Add fields for name and array
            ((:TYPE . "SEQ")
             (:MEMBERS
              ((:type . "FIELD")
               (:name . "name")
               (:content
                (:TYPE . "CHOICE")
                (:MEMBERS ((:TYPE . "STRING") (:VALUE . "new"))
                          ((:TYPE . "STRING") (:VALUE . "delete")))))

              ((:type . "FIELD")
               (:name . "array")
               (:content
                (:TYPE . "CHOICE")
                (:MEMBERS ((:TYPE . "STRING") (:VALUE . "[]")) ((:TYPE . "BLANK")))))))
            ;; Add fields for name and suffix_identifier
            ((:TYPE . "SEQ")
             (:MEMBERS
              ((:type . "FIELD")
               (:name . "name")
               (:content
                (:TYPE . "STRING") (:VALUE . "\"\"")))
              ((:type . "FIELD")
               (:name . "suffix_identifier")
               (:content
                (:TYPE . "SYMBOL") (:NAME . "identifier"))))))))))
       (:-NON-CASE-STATEMENT (:TYPE . "CHOICE")
        (:MEMBERS
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "labeled_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "compound_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "expression_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "if_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "switch_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "do_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "while_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "for_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "return_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "break_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "continue_statement"))
           ((:TYPE . "SYMBOL") (:NAME . "goto_statement"))))
         ((:TYPE . "SYMBOL") (:NAME . "co_return_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "co_yield_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "for_range_loop"))
         ((:TYPE . "SYMBOL") (:NAME . "try_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "throw_statement"))
         ((:TYPE . "SYMBOL") (:NAME . "type_forward_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "macro_forward_declaration"))))
       (:PARAMETER-LIST (:TYPE . "SEQ")
        (:MEMBERS ((:TYPE . "STRING") (:VALUE . "("))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "CHOICE")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "parameter_declaration"))
               ((:TYPE . "SYMBOL") (:NAME . "optional_parameter_declaration"))
               ((:TYPE . "SYMBOL") (:NAME . "variadic_parameter_declaration"))
               ;; NOTE: change ... to variadic_declaration
               ((:TYPE . "SYMBOL") (:NAME . "variadic_declaration"))))
             ((:TYPE . "REPEAT")
              (:CONTENT
               (:TYPE . "SEQ")
               (:MEMBERS
                ((:TYPE . "STRING") (:VALUE . ","))
                ((:TYPE . "CHOICE")
                 (:MEMBERS
                  ((:TYPE . "SYMBOL") (:NAME . "parameter_declaration"))
                  ((:TYPE . "SYMBOL") (:NAME . "optional_parameter_declaration"))
                  ((:TYPE . "SYMBOL") (:NAME . "variadic_parameter_declaration"))
                  ;; NOTE: change ... to variadic_declaration
                  ((:TYPE . "SYMBOL") (:NAME . "variadic_declaration")))))))))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ")")))))
      (:java
       (:MODIFIERS (:TYPE . "REPEAT1")
        (:CONTENT
         ;; Add modifiers field around everything
         (:TYPE . "FIELD") (:NAME . "modifiers")
         (:CONTENT (:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "_annotation"))
           ((:TYPE . "STRING") (:VALUE . "public"))
           ((:TYPE . "STRING") (:VALUE . "protected"))
           ((:TYPE . "STRING") (:VALUE . "private"))
           ((:TYPE . "STRING") (:VALUE . "abstract"))
           ((:TYPE . "STRING") (:VALUE . "static"))
           ((:TYPE . "STRING") (:VALUE . "final"))
           ((:TYPE . "STRING") (:VALUE . "strictfp"))
           ((:TYPE . "STRING") (:VALUE . "default"))
           ((:TYPE . "STRING") (:VALUE . "synchronized"))
           ((:TYPE . "STRING") (:VALUE . "native"))
           ((:TYPE . "STRING") (:VALUE . "transient"))
           ((:TYPE . "STRING") (:VALUE . "volatile"))))))
       ;; This is to get around statement allowing a semicolon
       ;; without any further information to reproduce it.
       (:EMPTY-STATEMENT
        (:TYPE . "STRING") (:value . ";"))
       (:-CLASS-BODY-DECLARATION (:TYPE . "CHOICE")
        (:MEMBERS
         ((:TYPE . "SYMBOL") (:NAME . "field_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "record_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "method_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "class_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "interface_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "annotation_type_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "enum_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "block"))
         ((:TYPE . "SYMBOL") (:NAME . "static_initializer"))
         ((:TYPE . "SYMBOL") (:NAME . "constructor_declaration"))
         ((:TYPE . "SYMBOL") (:NAME . "empty_statement"))))
       (:MODULE-DECLARATION (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "REPEAT")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_annotation")))
         ;; Add field around "open" so that it can be reproduced.
         ((:TYPE . "FIELD") (:NAME . "open")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "STRING") (:VALUE . "open"))
            ((:TYPE . "BLANK")))))
         ((:TYPE . "STRING") (:VALUE . "module"))
         ((:TYPE . "FIELD") (:NAME . "name")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_name")))
         ((:TYPE . "FIELD") (:NAME . "body")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "module_body"))))))
      (:python
       ;; NOTE: this removes semicolons. This can be further amended if it
       ;;       becomes problematic.
       (:-SIMPLE-STATEMENTS (:TYPE . "SYMBOL") (:NAME . "_simple_statement"))
       (:FUNCTION-DEFINITION (:TYPE . "SEQ")
        (:MEMBERS
         ;; Add a field for async.
         ((:TYPE . "FIELD") (:NAME . "async")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "STRING") (:VALUE . "async")) ((:TYPE . "BLANK")))))
         ((:TYPE . "STRING") (:VALUE . "def"))
         ((:TYPE . "FIELD") (:NAME . "name")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "identifier")))
         ((:TYPE . "FIELD") (:NAME . "parameters")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "parameters")))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS ((:TYPE . "STRING") (:VALUE . "->"))
                      ((:TYPE . "FIELD")
                       (:NAME . "return_type")
                       (:CONTENT (:TYPE . "SYMBOL") (:NAME . "type")))))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ":"))
         ((:TYPE . "FIELD") (:NAME . "body")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_suite")))))
       (:FOR-STATEMENT (:TYPE . "SEQ")
        (:MEMBERS
         ;; Add a field for async.
         ((:TYPE . "FIELD") (:NAME . "async")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "STRING") (:VALUE . "async")) ((:TYPE . "BLANK")))))
         ((:TYPE . "STRING") (:VALUE . "for"))
         ((:TYPE . "FIELD") (:NAME . "left")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_left_hand_side")))
         ((:TYPE . "STRING") (:VALUE . "in"))
         ((:TYPE . "FIELD") (:NAME . "right")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_expressions")))
         ((:TYPE . "STRING") (:VALUE . ":"))
         ((:TYPE . "FIELD") (:NAME . "body")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_suite")))
         ((:TYPE . "FIELD") (:NAME . "alternative")
          (:CONTENT (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "else_clause"))
                     ((:TYPE . "BLANK")))))))
       (:WITH-STATEMENT (:TYPE . "SEQ")
         (:MEMBERS
          ;; Add a field for async.
          ((:TYPE . "FIELD") (:NAME . "async")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "STRING") (:VALUE . "async")) ((:TYPE . "BLANK")))))
          ((:TYPE . "STRING") (:VALUE . "with"))
          ((:TYPE . "SYMBOL") (:NAME . "with_clause"))
          ((:TYPE . "STRING") (:VALUE . ":"))
          ((:TYPE . "FIELD") (:NAME . "body")
           (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_suite")))))
       (:POSITIONAL-ONLY-SEPARATOR (:TYPE . "STRING") (:VALUE . "/"))
       (:KEYWORD-ONLY-SEPARATOR (:TYPE . "STRING") (:VALUE . "*"))
       ;; NOTE: remove this if backtracking is implemented for
       ;;       match-parsed-children.
       (:COMPARISON-OPERATOR (:TYPE . "PREC_LEFT") (:VALUE . 2)
        (:CONTENT (:TYPE . "SEQ")
         (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "primary_expression"))
          ((:TYPE . "REPEAT1")
           (:CONTENT
            (:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "FIELD")
              (:NAME . "operators")
              (:CONTENT
               (:TYPE . "CHOICE")
               (:MEMBERS
                ((:TYPE . "STRING") (:VALUE . "<"))
                ((:TYPE . "STRING") (:VALUE . "<="))
                ((:TYPE . "STRING") (:VALUE . "=="))
                ((:TYPE . "STRING") (:VALUE . "!="))
                ((:TYPE . "STRING") (:VALUE . ">="))
                ((:TYPE . "STRING") (:VALUE . ">"))
                ((:TYPE . "STRING") (:VALUE . "<>"))
                ((:TYPE . "SEQ")
                 (:MEMBERS
                  ((:TYPE . "STRING") (:VALUE . "not"))
                  ((:TYPE . "STRING") (:VALUE . "in"))))
                ((:TYPE . "STRING") (:VALUE . "in"))
                ;; The next two clauses are swapped.
                ((:TYPE . "SEQ")
                 (:MEMBERS ((:TYPE . "STRING") (:VALUE . "is"))
                           ((:TYPE . "STRING") (:VALUE . "not"))))
                ((:TYPE . "STRING") (:VALUE . "is")))))
             ((:TYPE . "SYMBOL") (:NAME . "primary_expression"))))))))
       (:TUPLE
        (:TYPE . "SEQ")
        (:MEMBERS ((:TYPE . "STRING") (:VALUE . "("))
         ((:TYPE . "CHOICE")
          ;; inline _collection_elements and modify it to require a comma
          ;; in certain cases.
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "CHOICE")
              (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "expression"))
                        ((:TYPE . "SYMBOL") (:NAME . "yield"))
                        ((:TYPE . "SYMBOL") (:NAME . "list_splat"))
                        ((:TYPE . "SYMBOL") (:NAME . "parenthesized_list_splat"))))
             ((:TYPE . "STRING") (:VALUE . ","))))
           ((:TYPE . "SYMBOL") (:NAME . "_collection_elements"))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ")"))))
       (:TUPLE-PATTERN
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "STRING") (:VALUE . "("))
         ((:TYPE . "CHOICE")
          (:MEMBERS
          ;; inline _patterns and modify it to require a comma
          ;; in certain cases.
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "SYMBOL") (:NAME . "pattern"))
             ((:TYPE . "STRING") (:VALUE . ","))))
           ((:TYPE . "SYMBOL") (:NAME . "_patterns"))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ")")))))
      (:rust
       (:MATCH-BLOCK (:TYPE . "SEQ")
        (:MEMBERS ((:TYPE . "STRING") (:VALUE . "{"))
         ;; Remove the 'SEQ' and move the 'ALIAS' into the 'REPEAT'.
         ;; Also remove the 'BLANK' as it is no longer needed.
         ((:TYPE . "REPEAT")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "SYMBOL") (:NAME . "match_arm"))
            ((:TYPE . "ALIAS")
             (:CONTENT (:TYPE . "SYMBOL") (:NAME . "last_match_arm"))
             (:NAMED . T) (:VALUE . "match_arm")))))
         ((:TYPE . "STRING") (:VALUE . "}"))))
       (:_
        (:TYPE . "STRING") (:VALUE . "_"))
       (:FUNCTION-MODIFIERS
        ;; Add field around everything.
        (:TYPE . "FIELD") (:NAME . "modifiers")
        (:CONTENT
         (:TYPE . "REPEAT1")
         (:CONTENT (:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "STRING") (:VALUE . "async"))
           ((:TYPE . "STRING") (:VALUE . "default"))
           ((:TYPE . "STRING") (:VALUE . "const"))
           ((:TYPE . "STRING") (:VALUE . "unsafe"))
           ((:TYPE . "SYMBOL") (:NAME . "extern_modifier"))))))
       (:TYPE-ARGUMENTS (:TYPE . "SEQ")
        (:MEMBERS
         ;; Remove token around this which causes it to be seen as computed-text.
         ((:TYPE . "PREC") (:VALUE . 1)
          (:CONTENT (:TYPE . "STRING") (:VALUE . "<")))
         ((:TYPE . "SEQ")
          (:MEMBERS
           ((:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SYMBOL") (:NAME . "_type"))
             ((:TYPE . "SYMBOL") (:NAME . "type_binding"))
             ((:TYPE . "SYMBOL") (:NAME . "lifetime"))
             ((:TYPE . "SYMBOL") (:NAME . "_literal"))
             ((:TYPE . "SYMBOL") (:NAME . "block"))))
           ((:TYPE . "REPEAT")
            (:CONTENT
             (:TYPE . "SEQ")
             (:MEMBERS
              ((:TYPE . "STRING") (:VALUE . ","))
              ((:TYPE . "CHOICE")
               (:MEMBERS
                ((:TYPE . "SYMBOL") (:NAME . "_type"))
                ((:TYPE . "SYMBOL") (:NAME . "type_binding"))
                ((:TYPE . "SYMBOL") (:NAME . "lifetime"))
                ((:TYPE . "SYMBOL") (:NAME . "_literal"))
                ((:TYPE . "SYMBOL") (:NAME . "block")))))))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . ",")) ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ">"))))
       (:BLOCK
           (:TYPE . "SEQ")
         (:MEMBERS
          ((:TYPE . "STRING") (:VALUE . "{"))
          ((:TYPE . "REPEAT") (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement")))
          ((:TYPE . "CHOICE")
           (:MEMBERS
            ;; Replace _expression with new class.
            ((:TYPE . "SYMBOL") (:NAME . "implicit_return_expression"))
            ((:TYPE . "BLANK"))))
          ((:TYPE . "STRING") (:VALUE . "}"))))
       ;; Wrapper class.
       (:IMPLICIT-RETURN-EXPRESSION
        (:TYPE . "SYMBOL")
        (:NAME . "_expression"))
       ;; Reformat rule such that the trailing comma isn't preferred.
       (:TUPLE-EXPRESSION
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "STRING") (:VALUE . "("))
         ((:TYPE . "REPEAT")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "attribute_item")))
         ((:TYPE . "SYMBOL") (:NAME . "_expression"))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "STRING") (:VALUE . ","))
           ((:TYPE . "REPEAT")
            (:CONTENT
             (:TYPE . "SEQ")
             (:MEMBERS
              ((:TYPE . "STRING") (:VALUE . ","))
              ((:TYPE . "SYMBOL") (:NAME . "_expression")))))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "BLANK")) ((:TYPE . "STRING") (:VALUE . ","))))
         ((:TYPE . "STRING") (:VALUE . ")"))))
       (:GENERIC-TYPE-WITH-TURBOFISH (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "FIELD")
          (:NAME . "type")
          (:CONTENT (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "_type_identifier"))
                     ((:TYPE . "SYMBOL") (:NAME . "scoped_identifier")))))
         ;; Add a field around the "::" of the turbofish operator.
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "FIELD")
            (:NAME . "turbofish_operator")
            (:CONTENT
             (:TYPE . "STRING") (:VALUE . "::")))
           ((:TYPE . "STRING") (:VALUE . "::"))))
         ((:TYPE . "FIELD") (:NAME . "type_arguments")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "type_arguments"))))))
      ((:javascript :typescript-ts :typescript-tsx)
       (:-SEMICOLON (:TYPE . "CHOICE")
        (:MEMBERS
         ;; Put the string before the symbol.
         ((:TYPE . "STRING") (:VALUE . ";"))
         ((:TYPE . "SYMBOL") (:NAME . "_automatic_semicolon")))))
      (:javascript
       ;; TODO: add a substitution for javascript-array.
       ;;       This will add a javascript-blank-ast between any two
       ;;       consecutive commas via a parse tree transformation.
       (:FUNCTION-DECLARATION
        (:TYPE . "PREC_RIGHT") (:VALUE . "declaration")
        (:CONTENT
         (:TYPE . "SEQ")
         (:MEMBERS
          ;; Add a field for `async'.
          ((:TYPE . "FIELD") (:NAME . "async")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "STRING") (:VALUE . "async"))
             ((:TYPE . "BLANK")))))
          ((:TYPE . "STRING") (:VALUE . "function"))
          ((:TYPE . "FIELD") (:NAME . "name")
           (:CONTENT
            (:TYPE . "SYMBOL") (:NAME . "identifier")))
          ((:TYPE . "SYMBOL") (:NAME . "_call_signature"))
          ((:TYPE . "FIELD") (:NAME . "body")
           (:CONTENT
            (:TYPE . "SYMBOL") (:NAME . "statement_block")))
          ((:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "_automatic_semicolon"))
                     ((:TYPE . "BLANK")))))))
       (:EXPORT-STATEMENT (:TYPE . "CHOICE")
        (:MEMBERS
         ((:TYPE . "SEQ")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "export"))
           ((:TYPE . "CHOICE")
            (:MEMBERS
             ;; NOTE: the order here has been changed from the tree-sitter rule.
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "namespace_import"))
               ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "STRING") (:VALUE . "*"))
               ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))))
         ((:TYPE . "SEQ")
          (:MEMBERS
           ((:TYPE . "REPEAT")
            (:CONTENT
             (:TYPE . "FIELD") (:NAME . "decorator")
             (:CONTENT (:TYPE . "SYMBOL") (:NAME . "decorator"))))
           ((:TYPE . "STRING") (:VALUE . "export"))
           ((:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "FIELD")
                (:NAME . "default")
                (:CONTENT (:TYPE . "STRING") (:VALUE . "default")))
               ((:TYPE . "CHOICE")
                (:MEMBERS
                 ((:TYPE . "FIELD") (:NAME . "declaration")
                                    (:CONTENT (:TYPE . "SYMBOL") (:NAME . "declaration")))
                 ((:TYPE . "SEQ")
                  (:MEMBERS
                   ((:TYPE . "FIELD") (:NAME . "value")
                                      (:CONTENT (:TYPE . "SYMBOL") (:NAME . "expression")))
                   ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))))
             ((:TYPE . "FIELD")
              (:NAME . "declaration")
              (:CONTENT (:TYPE . "SYMBOL") (:NAME . "declaration"))))))))))
      ((:typescript-ts :typescript-tsx)
       (:-PARAMETER-NAME
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "REPEAT")
          (:CONTENT (:TYPE . "FIELD") (:NAME . "decorator")
           (:CONTENT (:TYPE . "SYMBOL") (:NAME . "decorator"))))
         ;; Group these as a modifiers field.
         ((:type . "FIELD")
          (:name . "modifiers")
          (:content
           (:type . "SEQ")
           (:members
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "accessibility_modifier"))
                       ((:TYPE . "BLANK"))))
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "override_modifier"))
                       ((:TYPE . "BLANK"))))
            ((:TYPE . "CHOICE")
             ;; Upstream tree-sitter has this as a STRING instead of a
             ;; symbol, which means it is always printed.
             (:MEMBERS ((:type . "SYMBOL") (:name . "readonly")) ((:TYPE . "BLANK")))))))
         ((:TYPE . "FIELD") (:NAME . "pattern")
          (:CONTENT (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "pattern"))
                     ((:TYPE . "SYMBOL") (:NAME . "this")))))))
       (:EXPORT-STATEMENT
        (:TYPE . "CHOICE")
        (:MEMBERS
         ;; The following is the same as the Javascript above, but
         ;; substituted for the original.
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS ((:TYPE . "STRING") (:VALUE . "export"))
                      ((:TYPE . "CHOICE")
                       (:MEMBERS
                        ;; NOTE: the order here has been changed from the tree-sitter rule.
                        ((:TYPE . "SEQ")
                         (:MEMBERS
                          ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
                        ((:TYPE . "SEQ")
                         (:MEMBERS
                          ;; This has been changed into an explicit alias.
                          ((:TYPE . "ALIAS")
                           (:CONTENT (:TYPE . "SYMBOL")
                                     (:NAME . "namespace_import_export"))
                           (:NAMED . T) (:VALUE . "namespace_export"))
                          ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
                        ((:TYPE . "SEQ")
                         (:MEMBERS
                          ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
                        ((:TYPE . "SEQ")
                         (:MEMBERS
                          ((:type . "FIELD")
                           (:name . "default")
                           (:content
                            (:TYPE . "STRING") (:VALUE . "*")))
                          ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))))
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "REPEAT")
              (:CONTENT
               (:TYPE . "FIELD") (:NAME . "decorator")
               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "decorator"))))
             ((:TYPE . "STRING") (:VALUE . "export"))
             ((:TYPE . "CHOICE")
              (:MEMBERS
               ((:TYPE . "SEQ")
                (:MEMBERS
                 ((:TYPE . "FIELD")
                  (:NAME . "default")
                  (:CONTENT (:TYPE . "STRING") (:VALUE . "default")))
                 ((:TYPE . "CHOICE")
                  (:MEMBERS
                   ((:TYPE . "FIELD") (:NAME . "declaration")
                                      (:CONTENT (:TYPE . "SYMBOL") (:NAME . "declaration")))
                   ((:TYPE . "SEQ")
                    (:MEMBERS
                     ((:TYPE . "FIELD") (:NAME . "value")
                                        (:CONTENT (:TYPE . "SYMBOL") (:NAME . "expression")))
                     ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))))
               ((:TYPE . "FIELD")
                (:NAME . "declaration")
                (:CONTENT (:TYPE . "SYMBOL") (:NAME . "declaration")))))))))
         ((:TYPE . "SEQ")
          (:MEMBERS
           ((:TYPE . "STRING") (:VALUE . "export"))
           ((:TYPE . "SYMBOL") (:NAME . "type"))
           ((:TYPE . "SYMBOL") (:NAME . "export_clause"))))
         ((:TYPE . "SEQ")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "export"))
           ((:type . "FIELD")
            (:name . "default")
            (:content
             (:TYPE . "STRING") (:VALUE . "=")))
           ((:TYPE . "SYMBOL") (:NAME . "identifier"))
           ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
         ((:TYPE . "SEQ")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "export"))
           ((:type . "FIELD")
            (:name . "default")
            (:content
             (:type . "SEQ")
             (:members
              ((:TYPE . "SYMBOL") (:NAME . "as"))
              ((:TYPE . "SYMBOL") (:NAME . "namespace")))))
           ((:TYPE . "SYMBOL") (:NAME . "identifier"))
           ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))
       (:method-definition
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "accessibility_modifier"))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "static")) ((:TYPE . "BLANK"))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "override_modifier"))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "readonly")) ((:TYPE . "BLANK"))))
         ;; Add a field for async.
         ((:type . "FIELD")
          (:name . "async")
          (:content
           (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "STRING") (:VALUE . "async")) ((:TYPE . "BLANK")))))
         ;; Added a new field to hold get/set.
         ((:type . "FIELD")
          (:name . "getter-setter")
          (:content
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "STRING") (:VALUE . "get"))
                       ((:TYPE . "STRING") (:VALUE . "set"))
                       ((:TYPE . "STRING") (:VALUE . "*"))))
            ((:TYPE . "BLANK")))))
         ((:TYPE . "FIELD") (:NAME . "name")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_property_name")))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "?")) ((:TYPE . "BLANK"))))
         ((:TYPE . "SYMBOL") (:NAME . "_call_signature"))
         ((:TYPE . "FIELD") (:NAME . "body")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "statement_block")))))
       (:public-field-definition
        (:TYPE . "SEQ")
        (:MEMBERS
         ;; Add a field for declare.
         ((:type . "FIELD")
          (:name . "declare")
          (:content
           (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "STRING") (:VALUE . "declare")) ((:TYPE . "BLANK")))))
         ;; Added a "modifiers" field.
         ((:type . "FIELD")
          (:name . "modifiers")
          (:content
           (:type . "SEQ")
           (:members
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "accessibility_modifier"))
                       ((:TYPE . "BLANK"))))
            ((:TYPE . "CHOICE")
             (:MEMBERS
              ((:TYPE . "SEQ")
               (:MEMBERS
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "STRING") (:VALUE . "static")) ((:TYPE . "BLANK"))))
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "override_modifier"))
                           ((:TYPE . "BLANK"))))
                ((:TYPE . "CHOICE")
                 ;; string -> symbol
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "readonly"))
                           ((:TYPE . "BLANK"))))))
              ((:TYPE . "SEQ")
               ;; Convert abstract and readonly from strings to symbols.
               (:MEMBERS
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "abstract"))
                           ((:TYPE . "BLANK"))))
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "readonly"))
                           ((:TYPE . "BLANK"))))))
              ((:TYPE . "SEQ")
               (:MEMBERS
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "readonly"))
                           ((:TYPE . "BLANK"))))
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "abstract"))
                           ((:TYPE . "BLANK")))))))))))
         ((:TYPE . "FIELD") (:NAME . "name")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_property_name")))
         ((:type . "FIELD")
          (:name . "optional")
          (:content
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "STRING") (:VALUE . "?"))
                       ((:TYPE . "STRING") (:VALUE . "!"))))
            ((:TYPE . "BLANK")))))
         ((:TYPE . "FIELD") (:NAME . "type")
          (:CONTENT (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "type_annotation"))
                     ((:TYPE . "BLANK")))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "_initializer"))
           ((:TYPE . "BLANK"))))))
       (:call-expression
        (:TYPE . "CHOICE")
        (:MEMBERS
         ;; Swap order.
         ((:TYPE . "PREC") (:VALUE . "member")
          (:CONTENT (:TYPE . "SEQ")
           (:MEMBERS
            ((:TYPE . "FIELD") (:NAME . "function")
                               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "primary_expression")))
            ;; Wrap with operator field.
            ((:type . "FIELD")
             (:name . "operator")
             (:content
              (:TYPE . "STRING") (:VALUE . "?.")))
            ((:TYPE . "FIELD") (:NAME . "type_arguments")
                               (:CONTENT (:TYPE . "CHOICE")
                                         (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "type_arguments"))
                                                   ((:TYPE . "BLANK")))))
            ((:TYPE . "FIELD") (:NAME . "arguments")
                               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "arguments"))))))
         ((:TYPE . "PREC") (:VALUE . "call")
          (:CONTENT (:TYPE . "SEQ")
           (:MEMBERS
            ((:TYPE . "FIELD") (:NAME . "function")
                               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "expression")))
            ((:TYPE . "FIELD") (:NAME . "type_arguments")
                               (:CONTENT (:TYPE . "CHOICE")
                                         (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "type_arguments"))
                                                   ((:TYPE . "BLANK")))))
            ((:TYPE . "FIELD") (:NAME . "arguments")
                               (:CONTENT (:TYPE . "CHOICE")
                                         (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "arguments"))
                                                   ((:TYPE . "SYMBOL") (:NAME . "template_string"))))))))))))
    "A mapping of JSON rule substitutions to be performed on the JSON file
before class generation and analysis.

Using this variable allows you to override the definitions of rules in
the `grammar.json' file for a given language.

Most commonly this is used to insert fields in places where
tree-sitter would otherwise lose data. Note that in that case you will
also need to (1) add a slot for the field in
`*tree-sitter-ast-extra-slots*' above and (2) define a corresponding
`transform-parse-tree' method to postprocess the output of
tree-sitter.")

  (defparameter *tree-sitter-json-rule-patches*
    '((:cpp
       (:field-expression
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "."))
           ((:TYPE . "STRING") (:VALUE . "->"))))
         :as "operator"))
       (:access-specifier
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "public"))
           ((:TYPE . "STRING") (:VALUE . "private"))
           ((:TYPE . "STRING") (:VALUE . "protected"))))
         :as "keyword"))
       (:type-parameter-declaration
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "typename"))
           ((:TYPE . "STRING") (:VALUE . "class"))))
         :as "keyword"))
       (:enum-specifier
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "CHOICE")
            (:MEMBERS ((:TYPE . "STRING") (:VALUE . "class"))
                      ((:TYPE . "STRING") (:VALUE . "struct"))))
           ((:TYPE . "BLANK"))))
         :as "scope")))
      (:rust
       (:arguments
        (:replace
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . ",")) ((:TYPE . "BLANK"))))
         :with
         ;; Put blank first to avoid generating a comma in an empty
         ;; argument list.
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "BLANK")) ((:VALUE . ",") (:TYPE . "STRING"))))))
       (:let-declaration
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "mutable_specifier"))
           ((:TYPE . "BLANK"))))
         :as "mutable_specifier"))
       (:parameter
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "mutable_specifier"))
           ((:TYPE . "BLANK"))))
         :as "mutable_specifier"))
       (:parameters
        (:replace
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . ",")) ((:TYPE . "BLANK"))))
         :with
         ;; Put blank first to avoid generating a comma in an empty
         ;; parameter list.
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "BLANK")) ((:VALUE . ",") (:TYPE . "STRING"))))))
       (:range-expression
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . ".."))
           ((:TYPE . "STRING") (:VALUE . "..."))
           ((:TYPE . "STRING") (:VALUE . "..="))))
         :as "operator")
        (:label
         ((:TYPE . "STRING") (:VALUE . ".."))
         :as "operator"))
       (:reference-expression
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "mutable_specifier"))
           ((:TYPE . "BLANK"))))
         :as "mutable_specifier"))
       (:reference-type
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "mutable_specifier"))
           ((:TYPE . "BLANK"))))
         :as "mutable_specifier"))
       (:unary-expression
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "-"))
           ((:TYPE . "STRING") (:VALUE . "*"))
           ((:TYPE . "STRING") (:VALUE . "!"))))
         :as "operator"))
       (:self-parameter
        (:label
         ((:TYPE . "STRING") (:VALUE . "&"))
         :as "borrow"))
       (:closure-expression
        (:label
         ((:TYPE . "STRING") (:VALUE . "move"))
         :as "move")))
      ((:typescript-ts :typescript-tsx)
       (:property-signature
        (:replace
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "?"))
           ((:TYPE . "BLANK"))))
         :with
         ((:type . "FIELD")
          (:name . "optional")
          (:content
           (:TYPE . "CHOICE")
           ;; String to symbol.
           (:MEMBERS ((:type . "SYMBOL") (:name . "?"))
                     ((:TYPE . "BLANK")))))))
       ;; TODO JS too?
       (:member-expression
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "."))
           ((:TYPE . "STRING") (:VALUE . "?."))))
         :as "operator"))
       (:subscript-expression
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "?.")) ((:TYPE . "BLANK"))))
         :as "operator"))
       ((:arrow-function
         :function
         :function-declaration
         :function-signature
         :generator-function-declaration)
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "async"))
           ((:TYPE . "BLANK"))))
         :as "async"))
       (:enum-declaration
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "const"))
           ((:TYPE . "BLANK"))))
         :as "kind"))
       (:class-body
        (:replace
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SYMBOL")
            (:NAME . "_function_signature_automatic_semicolon"))
           ((:TYPE . "STRING") (:VALUE . ","))))
         :with
         ((:TYPE . "CHOICE")
          ;; The function_signature_automatic_semicolon token from the
          ;; external scanner confuses children-parser.
          (:MEMBERS
           ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))
           ((:TYPE . "STRING") (:VALUE . ","))))))))
    "Nested alist of patches to JSON rules.
Organized first by relevant language (or list of relevant languages)
and then by AST types.

Supported patch syntaxes are:
- `(:replace AST1 :with AST2)' Replace every instance of AST1 with AST2.
- `(:wrap AST1 :with AST2)' Replace every instance of AST1 with
  AST2, substituting AST1 for every instance of `_' in AST2.
- `(:label AST1 :as NAME)` Wrap every instance of AST1 with a field
  named NAME.

All tests are done with `EQUAL'.")

  (defparameter *tree-sitter-json-node-type-substitutions*
    '((:c
       ;; This is to get around _EMPTY_DECLARATION inlining a semicolon
       ;; without any further information to reproduce it.
       ((:type . "empty_statement") (:named . T))
       ((:type . "type_forward_declaration") (:named . T))
       ((:type . "macro_forward_declaration") (:named . T))
       ((:TYPE . "#define") (:NAMED . T))
       ((:TYPE . "#else") (:NAMED . T))
       ((:TYPE . "#elif") (:NAMED . T))
       ((:TYPE . "#endif") (:NAMED . T))
       ((:TYPE . "#if") (:NAMED . T))
       ((:TYPE . "#ifdef") (:NAMED . T))
       ((:TYPE . "#ifndef") (:NAMED . T))
       ((:TYPE . "#include") (:NAMED . T))
       ((:TYPE . "_statement") (:NAMED . T)
        (:SUBTYPES ((:TYPE . "break_statement") (:NAMED . T))
         ((:TYPE . "case_statement") (:NAMED . T))
         ((:TYPE . "compound_statement") (:NAMED . T))
         ((:TYPE . "continue_statement") (:NAMED . T))
         ((:TYPE . "do_statement") (:NAMED . T))
         ((:TYPE . "expression_statement") (:NAMED . T))
         ((:TYPE . "for_statement") (:NAMED . T))
         ((:TYPE . "goto_statement") (:NAMED . T))
         ((:TYPE . "if_statement") (:NAMED . T))
         ((:TYPE . "labeled_statement") (:NAMED . T))
         ((:TYPE . "return_statement") (:NAMED . T))
         ((:TYPE . "switch_statement") (:NAMED . T))
         ((:TYPE . "while_statement") (:NAMED . T))
         ;; Add type and macro forward declarations
         ((:TYPE . "type_forward_declaration") (:NAMED . T))
         ((:TYPE . "macro_forward_declaration") (:NAMED . T)))))
      (:cpp
       ;; This is to get around _EMPTY_DECLARATION inlining a semicolon
       ;; without any further information to reproduce it.
       ((:type . "empty_statement") (:named . T))
       ((:type . "type_forward_declaration") (:named . T))
       ((:type . "macro_forward_declaration") (:named . T))
       ((:TYPE . "#define") (:NAMED . T))
       ((:TYPE . "#else") (:NAMED . T))
       ((:TYPE . "#elif") (:NAMED . T))
       ((:TYPE . "#endif") (:NAMED . T))
       ((:TYPE . "#if") (:NAMED . T))
       ((:TYPE . "#ifdef") (:NAMED . T))
       ((:TYPE . "#ifndef") (:NAMED . T))
       ((:TYPE . "#include") (:NAMED . T))
       ((:TYPE . "_statement") (:NAMED . T)
        (:SUBTYPES
         ((:TYPE . "break_statement") (:NAMED . T))
         ((:TYPE . "case_statement") (:NAMED . T))
         ((:TYPE . "co_return_statement") (:NAMED . T))
         ((:TYPE . "co_yield_statement") (:NAMED . T))
         ((:TYPE . "compound_statement") (:NAMED . T))
         ((:TYPE . "continue_statement") (:NAMED . T))
         ((:TYPE . "do_statement") (:NAMED . T))
         ((:TYPE . "expression_statement") (:NAMED . T))
         ((:TYPE . "for_range_loop") (:NAMED . T))
         ((:TYPE . "for_statement") (:NAMED . T))
         ((:TYPE . "goto_statement") (:NAMED . T))
         ((:TYPE . "if_statement") (:NAMED . T))
         ((:TYPE . "labeled_statement") (:NAMED . T))
         ((:TYPE . "return_statement") (:NAMED . T))
         ((:TYPE . "switch_statement") (:NAMED . T))
         ((:TYPE . "throw_statement") (:NAMED . T))
         ((:TYPE . "try_statement") (:NAMED . T))
         ((:TYPE . "while_statement") (:NAMED . T))
         ;; Add type and macro forward declarations
         ((:TYPE . "type_forward_declaration") (:NAMED . T))
         ((:TYPE . "macro_forward_declaration") (:NAMED . T)))))
      (:java
       ;; This is to get around STATEMENT allowing a semicolon
       ;; without any further information to reproduce it.
       ((:type . "empty_statement") (:named . T))
       ;; Put empty statement in statement superclass.
       ((:TYPE . "statement") (:NAMED . T)
        (:SUBTYPES
         ((:TYPE . "empty_statement") (:NAMED . T))
         ((:TYPE . "assert_statement") (:NAMED . T))
         ((:TYPE . "block") (:NAMED . T))
         ((:TYPE . "break_statement") (:NAMED . T))
         ((:TYPE . "continue_statement") (:NAMED . T))
         ((:TYPE . "declaration") (:NAMED . T))
         ((:TYPE . "do_statement") (:NAMED . T))
         ((:TYPE . "enhanced_for_statement") (:NAMED . T))
         ((:TYPE . "expression_statement") (:NAMED . T))
         ((:TYPE . "for_statement") (:NAMED . T))
         ((:TYPE . "if_statement") (:NAMED . T))
         ((:TYPE . "labeled_statement") (:NAMED . T))
         ((:TYPE . "local_variable_declaration") (:NAMED . T))
         ((:TYPE . "return_statement") (:NAMED . T))
         ((:TYPE . "switch_expression") (:NAMED . T))
         ((:TYPE . "synchronized_statement") (:NAMED . T))
         ((:TYPE . "throw_statement") (:NAMED . T))
         ((:TYPE . "try_statement") (:NAMED . T))
         ((:TYPE . "try_with_resources_statement") (:NAMED . T))
         ((:TYPE . "while_statement") (:NAMED . T))
         ((:TYPE . "yield_statement") (:NAMED . T)))))
      (:python
       ((:TYPE . "positional_only_separator") (:NAMED . T))
       ((:TYPE . "keyword_only_separator") (:NAMED . T))
       ((:TYPE . "parameter") (:NAMED . T)
        (:SUBTYPES
         ((:TYPE . "positional_only_separator") (:NAMED . T))
         ((:TYPE . "keyword_only_separator") (:NAMED . T))
         ((:TYPE . "default_parameter") (:NAMED . T))
         ((:TYPE . "dictionary_splat_pattern") (:NAMED . T))
         ((:TYPE . "identifier") (:NAMED . T))
         ((:TYPE . "list_splat_pattern") (:NAMED . T))
         ((:TYPE . "tuple_pattern") (:NAMED . T))
         ((:TYPE . "typed_default_parameter") (:NAMED . T))
         ((:TYPE . "typed_parameter") (:NAMED . T)))))
      (:rust
       ((:TYPE . "implicit_return_expression") (:NAMED . T)))
      ;; Both the TSX and TypeScript node-types.json have duplicate
      ;; number types, one named and one not named, but in the
      ;; opposite orders. This makes them consistent (and allows TSX
      ;; to parse numbers!).
      ((:typescript-ts :typescript-tsx)
       ((:type . "number") (:named . t)))
      (:rust
       ((:type . "_") (:named . t))))
    "A mapping of JSON node type substitutions to be performed on the JSON file
before class generation and analysis. This effectively allows the definition
of new classes.")

  (defparameter *tree-sitter-json-subtree-choice-resolver* nil
    ;; Currently unused.
    "A mapping of functions for resolving which choice branch should be
chosen when gathering a string representation of a JSON subtree.")

  (defparameter *tree-sitter-json-field-transformations*
    `((:c
       (:symbol-names
        ("#if" "#ifdef" "#ifndef")
        :slot-name "if-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :if-terminal '(:|#IF| :|#IFDEF| :|#IFNDEF|)})
       (:symbol-names
        ("#else")
        :slot-name "else-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :else-terminal '(:|#ELSE|)})
       (:symbol-names
        ("#elif")
        :slot-name "elif-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :elif-terminal '(:|#ELIF|)})
       (:symbol-names
        ("#endif")
        :slot-name "endif-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :endif-terminal '(:|#ENDIF|)})
       (:symbol-names
        ("#define")
        :slot-name "define-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :define-terminal '(:|#DEFINE|)})
       (:symbol-names
        ("#include")
        :slot-name "include-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :include-terminal '(:|#INCLUDE|)}))
      (:cpp
       (:symbol-names ("#if" "#ifdef" "#ifndef")
        :slot-name "if-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :if-terminal
                    '(:|#IF| :|#IFDEF| :|#IFNDEF|)})
       (:symbol-names
        ("#else")
        :slot-name "else-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :else-terminal '(:|#ELSE|)})
       (:symbol-names
        ("#elif")
        :slot-name "elif-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :elif-terminal '(:|#ELIF|)})
       (:symbol-names
        ("#endif")
        :slot-name "endif-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :endif-terminal '(:|#ENDIF|)})
       (:symbol-names
        ("#define")
        :slot-name "define-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :define-terminal '(:|#DEFINE|)})
       (:symbol-names
        ("#include")
        :slot-name "include-terminal"
        :arity 1
        :predicate ,(constantly t)
        :transform {terminal->field-transform _
                    :include-terminal '(:|#INCLUDE|)}))
      ((:javascript :typescript-ts :typescript-tsx)
       (:symbol-names ("_semicolon")
        :slot-name "semicolon"
        :predicate ,(constantly t)
        :transform
        (lambda (parse-tree)
          (copy-parse-tree
           parse-tree
           :children
           (mapcar
            (lambda (child-tree &aux (child-type (parse-tree-type child-tree)))
              (cond
                ((eql child-type #.(make-keyword ";"))
                 (cons (list :semicolon child-type) (cdr child-tree)))
                (t child-tree)))
            (parse-tree-children parse-tree))))))
      ;; This is needed to handle case in TypeScript where it allows
      ;; commas to be used in place of semicolons (e.g. :object-type).
      ((:typescript-ts :typescript-tsx)
       (:symbol-names (",")
        :slot-name "comma"
        :predicate ,(constantly t)
        :transform
        (lambda (parse-tree)
          (with-modify-parse-tree (parse-tree)
            ((:|,|) (label-as :comma)))))))
    "A mapping of tree-sitter symbol names that should have fields wrapped
around them. It is also followed by the slot that should be added to the relevant
class, a predicate that determines whether a subtree should be encapsulated and
the body of a parse tree transformation for each class that uses it.")

  (defparameter *tree-sitter-superclass-additions* nil
    "A mapping of tree-sitter language to additional supertypes.")

  (defparameter *tree-sitter-ast-superclass-table*
    (lret ((table (make-hash-table)))
      (iter
       (for (langs . alist) in *tree-sitter-ast-superclasses*)
       (iter (for lang in (ensure-list langs))
             (let* ((lang (intern (string lang) :sel/sw/ts))
                    (lang-table
                     (ensure-gethash lang table (make-hash-table))))
               (iter
                (for (mixin . subclasses) in alist)
                (let ((mixin (find-symbol (string mixin) :sel/sw/ts)))
                  (dolist (subclass subclasses)
                    (push subclass (gethash mixin lang-table)))))))))
    "Nested hash table from language and mixin to a list of classes
    that inherit from that mixin.")

  (defparameter *tree-sitter-ast-extra-prefixes*
    '((:c c/cpp)
      (:cpp c/cpp)
      (:typescript-ts typescript ecma)
      (:typescript-tsx typescript ecma)
      (:javascript ecma))
    "Alist of languages and extra prefixes.
For every extra prefix, every slot will get an extra reader and an
extra initarg with that prefix.")

  (defparameter *tree-sitter-computed-text-asts*
    '((:c c-#define c-#else c-#elif c-#endif c-#if c-#ifdef c-#ifndef c-#include)
      (:cpp cpp-#define cpp-#else cpp-#elif cpp-#endif cpp-#if cpp-#ifdef
       cpp-#ifndef cpp-#include)
      (:python python-string)
      (:javascript javascript-template-string))
    "Alist of languages and their classes which should be computed-text ASTs
but aren't detected as such. This is usually due to insufficient information
stored on the AST or external rules.")

  (defun aget-all (key alist)
    "Like `aget', but with two differences:
1. It also matches on conses where the car is a list contains KEY.
2. It collects *all* the matching clauses and appends them."
    (mappend #'cdr
             (filter (lambda (cons)
                       (member key (ensure-list (car cons))))
                     alist)))

  (defun tree-sitter-ast-classes (name grammar-file node-types-file)
    (nest
     (flet ((alternate-class-name (name)
              (string-case name
                ("GO" "GOLANG")
                ("TYPESCRIPT-TYPESCRIPT" "TYPESCRIPT-TS")
                ("COMMONLISP" "CL")
                (t name)))))
     (let* ((path-name (replace-all name "/" "-"))
            (class-name (alternate-class-name (string-upcase path-name)))
            (class-keyword (make-keyword class-name))))
     `((register-tree-sitter-language
        ,(string-join (list "tree-sitter" path-name) #\-)
        ,class-keyword
        ',(intern (string-join (list class-name "AST") #\-)
                  :software-evolution-library/software/tree-sitter))
       ,(create-tree-sitter-classes
         node-types-file
         grammar-file
         class-name
         :ast-superclasses
         (aget-all class-keyword *tree-sitter-ast-superclasses*)
         :base-ast-superclasses
         (aget-all class-keyword *tree-sitter-base-ast-superclasses*)
         :software-superclasses
         (aget-all class-keyword *tree-sitter-software-superclasses*)
         :software-direct-slots
         (aget-all class-keyword *tree-sitter-software-direct-slots*)
         :ast-extra-slot-options
         (aget-all class-keyword *tree-sitter-ast-extra-slot-options*)
         :ast-extra-slots
         (aget-all class-keyword *tree-sitter-ast-extra-slots*)
         :node-type-substitutions
         (aget-all class-keyword *tree-sitter-json-node-type-substitutions*)
         :json-subtree-choice-resolver
         (car (aget-all class-keyword *tree-sitter-json-subtree-choice-resolver*))
         :json-field-transformations
         (aget-all class-keyword *tree-sitter-json-field-transformations*)
         :additional-supertypes
         (aget-all class-keyword *tree-sitter-superclass-additions*))))))

(-> ast-mixin-subclasses ((or symbol class) (or symbol class)) list)
(defun ast-mixin-subclasses (class language)
  "Return a list of AST classes for LANGUAGE that inherit from CLASS."
  (declare ((and symbol (not keyword)) class language))
  (let ((table *tree-sitter-ast-superclass-table*))
    (when-let (language-table (gethash language table))
      (gethash class language-table))))

(defun terminal->field-transform (parse-tree field-name terminal-types)
  (nest (copy-parse-tree parse-tree :children)
        (mapcar (lambda (child-tree &aux (child-type (parse-tree-type child-tree)))
                  (cond
                    ((member child-type terminal-types)
                     (cons (list field-name child-type) (cdr child-tree)))
                    (t child-tree))))
        (parse-tree-children parse-tree)))

(defmacro register-tree-sitter-language (lib-name language ast-superclass)
  "Setup LANGUAGE to map to AST-SUPERCLASS and use LIB-NAME for parsing."
  (let ((register-language #.(when (asdf:find-system :cl-tree-sitter nil) t)))
    `(eval-always
       (handler-case
           (progn
             (when ,register-language
               (handler-case
                   (progn
                     (when ,register-language
                       (register-language
                        ,language ,lib-name
                        ,@(string-case lib-name
                            ("tree-sitter-typescript-typescript"
                             '(:fn-name "tree_sitter_typescript"))
                            ("tree-sitter-typescript-tsx"
                             '(:fn-name "tree_sitter_tsx")))))
                     (setf (gethash ,ast-superclass *superclass->language*) ,language))
                 ;; Try again with an augmented library search path.
                 (load-foreign-library-error ()
                   (register-language ,language ,(concatenate 'string "/usr/lib/" lib-name)))))
             (setf (gethash ,ast-superclass *superclass->language*) ,language))
         (load-foreign-library-error ()
           (format
            *error-output*
            "Failed to load '~a'. Support for '~a' will not be available.~%"
            ,lib-name ,language))))))


;;; Defining tree-sitter classes

(defmacro define-template-builder (class ast-class)
  (let ((class* (intern (string+ class '*)
                        (symbol-package class))))
    `(progn
       (export/tree-sitter ',class*)

       (defun ,class
           (template &rest args)
         "Short for (ast-template TEMPLATE python-ast ARGS...)."
         (apply #'ast-template template ',ast-class args))

       (defun ,class*
           (template &rest args)
         "Short for (ast-template* TEMPLATE python-ast ARGS...)."
         (apply #'ast-template* template ',ast-class args))

       ;; Define compiler macro so the template can be
       ;; statically checked.
       (define-compiler-macro ,class (template &rest args)
         (list* 'ast-template template '',ast-class args))

       (define-compiler-macro ,class* (template &rest args)
         (list* 'ast-template* template '',ast-class args))

       (defpattern ,class (template &rest args)
         (list* 'ast-template template ',ast-class args))

       (defpattern ,class* (template &rest args)
         (list* 'ast-template* template ',ast-class args)))))

(eval-always
 ;; TODO The methods on null for before-text and after-text are insurance
 ;; against tree-manipulation methods that might affect the slots if they
 ;; contain conflict ASTs.

 (defgeneric before-text (ast)
   (:method :around (ast)
     (let ((result (call-next-method)))
       (if (null result) "" result))))

 (defgeneric after-text (ast)
   (:method :around (ast)
     (let ((result (call-next-method)))
       (if (null result) "" result))))

  ;; TODO: let over a basic string for empty strings?
  (defclass structured-text ()
    ((before-text
      :accessor before-text
      :initarg :before-text
      :initform ""
      :documentation "The text before the first token of an AST.")
     ;; NOTE: the primary usage of this slot is for AST text, like
     ;;       identifiers, which doesn't belong in the before or after slot.
     (text
      :accessor text
      :initarg :text
      :initform nil
      ;; NOTE: it's hard to name this slot descriptively such that its usage
      ;;       is obvious.
      :documentation "The text that can vary between ASTs of the same class. It
stores the part that is computed from the variable definition. This should be
stored as a list of interleaved text. This should ideally only be used for leaf
 nodes.")
     (after-text
      :accessor after-text
      :initarg :after-text
      :initform ""
      :documentation "The text after the last token of an AST.")
     (before-asts
      :accessor before-asts
      :initarg :before-asts
      :initform nil
      :documentation
      "A list of comments and errors that precede the before text of an AST.")
     (after-asts
      :accessor after-asts
      :initarg :after-asts
      :initform nil
      :documentation
      "A list of comments and errors that procede the after text of an AST.")
     (ordered-children
      :reader ordered-children
      :documentation
      "A cache slot for ordered children to speed up retrieving the children
of an AST."))
    (:documentation "Mix-in for structured text ASTs."))

  (defclass tree-sitter-ast (indentation
                             structured-text
                             functional-tree-ast)
    ()
    (:documentation "AST for input from tree-sitter."))

  (defclass computed-text ()
    ()
    (:documentation "A mixin for computed text ASTs."))

 (defmethod source-text :around ((ast computed-text) &key stream (trim nil trim-supplied?))
   "Avoid needless copying when calling `source-text' on a `computed-text' ast."
   (declare (ignore trim))
   (or (and
        ;; Not invoked recursively.
        (no stream)
        ;; Not computing indentation.
        (not trim-supplied?)
        ;; Has text.
        (text ast))
       (call-next-method)))

  (defclass text-fragment (tree-sitter-ast)
    ((text
      :accessor text
      :initarg :text
      :initform nil)
     (child-slot-specifiers
      :initform nil
      :allocation :class))
    (:documentation "A wrapper class for text fragments in computed text ASTs."))

  (defclass source-text-fragment (computed-text)
    ((text :accessor text
           :initform ""
           :initarg :text)
     (choice-subclasses
      :initform nil
      :reader choice-subclasses
      :allocation :class))
    (:documentation "A mixin for ASTs that represent fragments of source text.")
    (:default-initargs :indent-adjustment 0))

  (defclass variation-point (alternative-ast)
    ()
    (:documentation "A mixin to represent variations"))

  (defclass error-variation-point (variation-point)
    ()
    (:documentation "Node for error variation points."))

  (defclass source-text-fragment-variation-point (variation-point)
    ()
    (:documentation "Node for source-text fragment variation points."))

  (defclass error-tree (parse-error-ast)
    ()
    (:documentation "Node for errors represented as a tree."))

  (defclass inner-whitespace (computed-text text-fragment)
    ()
    (:documentation "An AST that represents whitespace between two
terminal tokens."))

  (defclass blot (computed-text text-fragment)
    ()
    (:documentation "A node that represents blotted sections of code.
These sections are generally considered problematic and have been hidden
from the parser."))

  (define-node-class inner-parent (tree-sitter-ast)
    ((children
      :initform nil
      :initarg :children
      :accessor children)
     (child-slots
      :initform '((children . 0))
      :reader child-slots
      :allocation :class))
    (:documentation "A container class that holds multiple children which occur
between the same two terminal tokens."))

  (defclass comment-ast (ast) ()
    (:documentation "Mix-in for AST classes that are comments.

Superclass of every generated LANGUAGE-comment class."))

  (defclass ecma-comment (comment-ast) ()
    (:documentation "Mix-in for comments in dialects of ECMAScript."))

 (defclass root-ast (ast subroot) ()
    (:documentation "Mix-in for AST classes which are roots."))

  (defclass statement-ast (ast) ()
    (:documentation "Mix-in for AST classes that are statements."))

 (defclass expression-statement-ast (statement-ast) ()
    (:documentation "Mix-in for AST classes that are expression statements."))

  (defclass expression-ast (ast) ()
    (:documentation "Mix-in for AST classes that are expressions."))

 (defclass parenthesized-expression-ast (expression-ast) ()
    (:documentation "Mix-in for AST classes that are parenthesized
    expressions."))

 (defclass field-ast (expression-ast) ()
   (:documentation "Mix-in for AST classes that are field
    expressions (not field declarations)."))

 (defclass compound-ast (ast) ()
    (:documentation "Mix-in for AST classes that are compounds."))

  (defclass conditional-ast (ast) ()
    (:documentation "Mix-in for AST classes that have a conditional."))

  (defclass control-flow-ast (ast) ()
    (:documentation "Mix-in for AST classes that have control flow."))

  (defclass if-ast (control-flow-ast conditional-ast) ()
    (:documentation "Mix-in for AST classes that are ifs."))

  (defclass while-ast (control-flow-ast conditional-ast) ()
    (:documentation "Mix-in for AST classes that are whiles."))

 (defclass loop-ast (control-flow-ast) ()
    (:documentation "Mix-in for AST classes that are loops."))

 (defclass declaration-ast (ast) ()
   (:documentation "Mixin for AST classes that declare/define something."))

 (defclass degenerate-declaration-ast (ast) ()
   (:documentation "Mixin for AST classes that are subclasses of
   declaration ASTs, but don't actually declare/define anything."))

 (defclass definition-ast (declaration-ast) ()
   (:documentation "AST for something that associates a name with a

thing. The name string is obtained by by DEFINITION-NAME, and the AST by
DEFINITION-NAME-AST.

Note this is distinct from `declaration-ast' in that a declaration
need not associate a name with anything."))

 (defclass type-declaration-ast (declaration-ast) ()
   (:documentation "Mix-in for AST classes that are type declarations."))

 (defclass macro-declaration-ast (declaration-ast) ()
   (:documentation "Mix-in for AST classes that are macro declarations."))

 (defclass namespace-declaration-ast (declaration-ast) ()
   (:documentation "Mix-in for AST classes that are namespace declarations."))

 (defclass class-ast (type-declaration-ast) ()
   (:documentation "Mix-in for AST classes that are class declarations."))

 (defclass c/cpp-classoid-specifier (class-ast) ()
   (:documentation "Common superclass for all three ways of defining
    a class in C++ -- class, struct, and union -- and their C
    equivalents (when they exist)."))

  (defclass parse-error-ast (ast) ()
    (:documentation
     "Mix-in for AST classes that represent tree-sitter parsing errors.

Superclass of every generated LANGUAGE-error class."))

 (defclass function-ast (ast) ()
   (:documentation "Mix-in for AST classes that are functions.

An AST that declares a function should be a
`function-declaration-ast'; an anonymous function AST should be a
`lambda-ast'. Function ASTs that are not declarations or lambdas are
things like JavaScript function expressions (which have names but are
not declarations)."))

 ;; NB While function-declaration-ast and lambda-ast are disjoint
 ;; subtypes of function-ast they are not exhaustive.

 (defclass function-declaration-ast (function-ast declaration-ast) ()
   (:documentation "Mix-in for AST classes that are function declarations."))

 (defclass lambda-ast (function-ast) ()
   (:documentation "Mix-in for AST classes that are lambdas."))

  (defclass parameters-ast (ast) ()
    (:documentation "Mix-in for AST classes that are parameter lists."))

 (defclass parameter-ast (variable-declaration-ast) ()
   (:documentation "Mix-in for AST classes that are individual parameters."))

 (defclass variable-declaration-ast (declaration-ast) ()
    (:documentation "Mix-in for AST classes that are variable declarations."))

 (defclass variable-initialization-ast (variable-declaration-ast) ()
   (:documentation "Mix-in for AST classes that are variable initializers."))

 (defclass assignment-ast (ast) ()
   (:documentation "Mix-in for AST classes that are assignments."))

  (defclass identifier-ast (ast) ()
    (:documentation "Mix-in for AST classes that are identifiers."))

 (defclass type-ast (ast) ()
   (:documentation "Mixin for AST classes that designate types."))

 (defclass type-identifier-ast (identifier-ast type-ast) ()
   (:documentation "Mix-in for AST classes that are type identifiers \(when they are distinct)."))

  (defclass subscript-ast (ast) ()
    (:documentation "Mix-in for AST classes that are subscripts."))

  (defclass literal-ast (ast) ()
    (:documentation "Mix-in for AST classes that are literals."))

  (defclass boolean-ast (literal-ast) ()
    (:documentation "Mix-in for AST classes that are booleans."))

  (defclass boolean-true-ast (boolean-ast) ()
    (:documentation "Mix-in for AST classes that are true booleans."))

  (defclass boolean-false-ast (boolean-ast) ()
    (:documentation "Mix-in for AST classes that are false booleans."))

  (defclass char-ast (literal-ast) ()
    (:documentation "Mix-in for AST classes that are literal chars."))

  (defclass string-ast (literal-ast) ()
    (:documentation "Mix-in for AST classes that are literal strings."))

  (defclass number-ast (literal-ast) ()
    (:documentation "Mix-in for AST classes that are literal numbers."))

  (defclass integer-ast (number-ast) ()
    (:documentation "Mix-in for AST classes that are literal integers."))

  (defclass float-ast (number-ast) ()
    (:documentation "Mix-in for AST classes that are literal floats."))

  (defclass call-ast (expression-ast) ()
    (:documentation "Mix-in for AST classes that are calls."))

  (defclass arguments-ast (ast) ()
    (:documentation "Mix-in for AST classes that are lists of arguments."))

  (defclass unary-ast (expression-ast) ()
    (:documentation "Mix-in for AST classes that are unary expressions."))

  (defclass binary-ast (expression-ast) ()
    (:documentation "Mix-in for AST classes that are binary expressions."))

  (defclass return-ast (statement-ast) ()
    (:documentation "Mix-in for AST classes that are return statements."))

  (defclass goto-ast (statement-ast) ()
    (:documentation "Mix-in for AST classes that are goto statements."))

  (defclass catch-ast (statement-ast) ()
    (:documentation "Mix-in for AST classes that are error catch clauses."))

 (defclass operator-ast (ast) ()
   (:documentation "Mixin for an operator AST."))

 (defclass semicolon-ast (operator-ast) ()
   (:documentation "Mix-in for a semicolon."))

 (defclass increment-operator-ast (operator-ast) ()
   (:documentation "Mix-in for ++."))

 (defclass decrement-operator-ast (operator-ast) ()
   (:documentation "Mix-in for --."))

  (defclass terminal-symbol ()
    ()
    (:documentation "Mix-in for terminal symbols. Note that this won't fully
cover every terminal symbol, only the ones that aren't named.")
    (:default-initargs :indent-adjustment 0))

  (defgeneric convert-name (language name-string)
    (:method (language name-string)
      (substitute #\- #\_  (string-upcase (string name-string))))
    (:method ((language (eql :rust)) name-string)
      (if (equal "_" (string name-string))
          name-string
          (call-next-method)))
    (:method :around (language name-string)
      (if (keywordp language)
          (call-next-method)
          (convert-name (make-keyword language) name-string))))

  (defun translate-to-slot-name (name prefix)
    "Translate NAME into a slot name that is unlikely
     to collide with inherited slot names by prepending
     PREFIX. If NAME is 'children', the prefix is not
     attached."
    (cond
      ((string= name :children)
       'children)
      (t
       ;; (format-symbol 'sel/sw/ts "~a-~a" prefix name)
       (values
        (intern (concatenate 'string (string prefix) "-" (string name))
                'sel/sw/ts)))))

  ;; NOTE: while a :child-order annotation is currently being generated
  ;;       for every ast converted from a string, having the slot order
  ;;       is useful for converting from a list where the :child-order
  ;;       annotation would need to be generated and slot order is likely
  ;;       already correct except in a few rare cases.
  (defun slot-order (language-prefix name expected-fields grammar-rules
                     &aux dependencies fields
                       (expected-fields (mapcar #'car expected-fields)))
    "Return the slot order of the fields in the production specified
by NAME. If NIL is returned, there are either no fields or the order
of fields needs to be determined at parse-time."
    (labels ((add-dependency (preceding-fields field)
               "Add a dependency for each on each item
              in PRECEDING-FIELDS for field."
               ;; NOTE: this can potentially add duplicate dependencies
               ;;       though this likely isn't much of a problem.
               (mapc
                (lambda (preceding-field)
                  (unless (equal preceding-field field)
                    (push (list preceding-field field) dependencies)))
                preceding-fields))
             (add-field (name)
               "Add NAME to the list of used fields."
               ;; NOTE: avoid adding the same field more than once.
               ;;       This can occur with 'CHOICE' rules.
               (pushnew name fields :test #'equal))
             (handle-choice (rule &optional preceding-fields visited-rules)
               "Handle RULE as a 'CHOICE' rule."
               (remove-duplicates
                (iter
                  (for member in (aget :members rule))
                  (appending
                   (handle-rule member preceding-fields visited-rules)))
                :test #'equal))
             (handle-seq (rule &optional preceding-fields visited-rules)
               "Handle RULE as a 'SEQ' rule."
               (iter
                 (for member in (aget :members rule))
                 (for preceding
                      initially preceding-fields
                      then (or (handle-rule member preceding visited-rules)
                               preceding))
                 (finally (return preceding))))
             (handle-repeat (rule &optional preceding-fields visited-rules)
               "Handle RULE as a 'REPEAT' rule."
               ;; NOTE: perform twice to loop the ending field of the repeat
               ;;       back to the front of the repeat. This will create
               ;;       an inconsistency if one exists. Also note that
               ;;       a dependency of a field on itself is ignored.
               (iter
                 (repeat 3)
                 (for preceding
                      initially preceding-fields
                      then (or (handle-rule
                                (aget :content rule) preceding visited-rules)
                               preceding))
                 (finally (return preceding))))
             (handle-field (rule &optional preceding-fields
                            &aux (name (aget :name rule)))
               "Handle RULE as a 'FIELD' rule and add a dependency from
              the field to PRECEDING-FIELDS if it exists."
               (when (member (make-keyword (convert-name language-prefix name))
                             expected-fields)
                 (add-field name)
                 (add-dependency preceding-fields name)
                 (list name)))
             (handle-rule (rule &optional preceding-fields visited-rules)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               (string-ecase (aget :type rule)
                 (("ALIAS" "BLANK" "IMMEDIATE_TOKEN" "TOKEN" "PATTERN" "STRING"))
                 ("CHOICE" (handle-choice rule preceding-fields visited-rules))
                 ("FIELD" (handle-field rule preceding-fields))
                 (("PREC" "PREC_DYNAMIC" "PREC_LEFT" "PREC_RIGHT")
                  ;; pass-through
                  (handle-rule
                   (aget :content rule) preceding-fields visited-rules))
                 (("REPEAT" "REPEAT1")
                  (handle-repeat rule preceding-fields visited-rules))
                 ("SEQ" (handle-seq rule preceding-fields visited-rules))
                 ("SYMBOL"
                  (let* ((name-string (aget :name rule))
                         (name (make-keyword (convert-name language-prefix
                                                           name-string))))
                    ;; NOTE: the rules starting with an #\_ are special
                    ;;       and are the only ones that should be considered
                    ;;       when searching for fields that may be down the line
                    ;;       in different rules.
                    (when-let ((name (and (eql #\_ (aref name-string 0))
                                          (not (member name visited-rules))
                                          (aget name grammar-rules))))
                      (handle-rule name
                                   preceding-fields
                                   (cons name visited-rules))))))))
      ;; NOTE: tree-sitter/cli/src/generate/grammar-schema.json
      ;;       The grammar schema contains information on the
      ;;       possible rule types.
      (let* ((name-keyword (make-keyword (convert-name language-prefix name)))
             (name-rule (aget name-keyword grammar-rules)))
        (when name-rule
          (handle-rule name-rule nil (list name-keyword))
          (mapcar
           #'make-keyword
           (mapcar
            {convert-name language-prefix}
            (handler-case (sort fields (toposort dependencies :test #'equal))
              (inconsistent-graph ()
                ;; NOTE: the order doesn't matter as a :child-order
                ;;       annotation will be used instead of it.
                ;;       This is only provided for #'sorted-children
                ;;       to use.
                fields))))))))

  ;; STRUCTURED TEXT
  ;;
  ;; structured-text uses the JSON files provided by tree-sitter to generate
  ;; methods that reconstruct what the source code should look like for an AST.
  ;;
  ;; The methods primarily operate on a "transformed" JSON rule and its
  ;; equivalent "pruned" rule. The transformed JSON rule is an alist
  ;; representation which has removed any precedence rules, inlined every
  ;; rule starting with an underscore that isn't a supertype, distributed fields
  ;; such that they are less ambiguous, added choices for ambiguous alias rules,
  ;; and added extra slots to store information between terminal tokens.
  ;; The pruned rule transforms the transformed JSON rule by turning it into a
  ;; list instead of an alist, and replaces any subtree that does not have a slot
  ;; usage with a nil.
  ;;
  ;; The structured-text class has 5 slots--before-text stores text that comes
  ;; before the node, after-text stores text that comes after the node,
  ;; 'text' stores any text that can vary between different instances
  ;; of a class, before-asts stores comment and error ASTs that occur before
  ;; the before-text, and after-asts stores comment and error ASTs that occur
  ;; after the after-text. The text slot will also contain the text for any
  ;; class that doesn't have any slot usages. This allows for classes, such as
  ;; primitive_type in C, that don't store anything but have variable text to
  ;; still produce something meaningful without needing to modify the rule for
  ;; the AST.
  ;;
  ;; There are two main generated methods: output-transformation and parse-order.
  ;; output-transformation accepts an AST and returns a list of strings and its
  ;; children. This format has been chosen so that indentation can be reinserted
  ;; later. parse-order returns a list of an AST's children in-order. It also
  ;; contains "bookmarks" that indicate which "CHOICE" branch was taken or
  ;; how many times a "REPEAT" should be taken.
  ;;
  ;; There are two parsers: children-parser and match-parsed-children.
  ;; children-parser is a hack and should probably be replaced by something
  ;; generated from a parser generator. It is the back-bone of parse-order
  ;; and is what actually assembles its return value. match-parsed-children
  ;; matches a rule against a parse tree returned by cl-tree-sitter:parse-string.
  ;;
  ;; To disambiguate between different choice branches that a rule can take,
  ;; several different 'CHOICE expansion' subclasses are generated. Each one of
  ;; these expands a different branch in a 'CHOICE' present in the rule such that
  ;; they have a unique output transformation. The subclasses have names
  ;; generated based on the base class with a number added to the end of it.
  ;;
  ;; NOTE: these are some notes on the functions that may or may not be
  ;;       up-to-date
  ;;
  ;; transform-json transforms a json-rule by removing any precedent rules and
  ;; inlining any rule that starts with an underscore.
  ;;
  ;; prune-rule-tree transforms a json list representation into an easier to
  ;; work with list and prunes any subtree that doesn't include something that
  ;; will go into a slot--these are either field or child rules
  ;;
  ;; collapse-rule-tree further transforms the rule tree by collapsing duplicate
  ;; rules on each other, such as a sequence nested inside a sequence, and removes
  ;; any nils from the rule tree. Ideally, none of the code generation should use
  ;; this representation and prefer the pruned representation instead. Since the
  ;; collapsed representation was used early on, some code needs refactored to
  ;; account for this.
  ;;
  ;; structured-rule-p checks whether a rule is "structured" which I've deemed as
  ;; a fancy way of saying that it's unlikely to be problematic reproducing the
  ;; source text of the class associated with this rule. This should be useful at
  ;; compile time so that we know immediately which problems are likely to be
  ;; problematic. Note that it won't find problems with information that should be
  ;; stored in its own class but is currently treated as interleaved text by
  ;; tree-sitter.
  ;;
  ;; expand-choice-branches expands all choice subtrees outside of repeats into
  ;; their own rules without the choice rule. This allows for what I'm referring to
  ;; as "choice expansion" subclassing which allows for different slot orderings to
  ;; be implicit based on the AST subclass. There are currently some major
  ;; performance issues with this function on some languages.
  ;;
  ;; generate-computed-text-method determines if an AST class is largely variable
  ;; text, such as identifiers or literals, and generates a predicate method that
  ;; indicates whether the node should be read in specially--the variable text is
  ;; stored when read in.
  ;;
  ;; children-parser is largely a hack at the moment. It takes a collapsed rule
  ;; tree that has converted its strings to relevant class and slot names and an
  ;; AST. It then traverses the rule and pulls the children from the relevant slot
  ;; as they are encountered in the rule. This function does not back track right
  ;; now, and based on a cursory look at the rules, I haven't seen an instance
  ;; where it would be needed, so I'm putting that off until it becomes a problem.
  ;;
  ;; get-json-subtree-string takes a subtree that doesn't contain any slot uses
  ;; and generates a string representation of it. This is used for reproducing
  ;; the interleaved text. It prioritizes "BLANK" rules with "CHOICE" branches
  ;; and doesn't take any "REPEAT" rules.
  (defun map-json (map-fun tree &key (traversal :postorder))
    "Maps MAP-FUN over TREE as if it were a list representation of JSON
and returns the result."
    (map-tree
     (lambda (subtree)
       ;; TODO: this appears to call map-fun with the cdr of :members
       ;;       which isn't an alist that is use-able.
       (cond
         ;; first five clauses filter out anything that can't
         ;; be used as an alist. This is assumed based on the
         ;; structure of the json read in. NOTE that we may get
         ;; a non-alist still, but it should prevent any errors
         ;; that may occur from using something like #'aget.
         ((atom subtree) subtree)
         ((atom (cdr subtree)) subtree)
         ((not (listp (car subtree))) subtree)
         ((eql (car subtree) :members) subtree)
         ((eql (car subtree) :content) subtree)
         ((funcall map-fun subtree))))
     tree :tag 'prune :traversal traversal))

  (defun walk-json (function tree &key (traversal :preorder))
    "Walks FUNCTION over TREE as if it were a list representation of JSON."
    (walk-tree
     (lambda (subtree)
       (cond
         ;; first five clauses filter out anything that can't
         ;; be used as an alist. This is assumed based on the
         ;; structure of the json read in. NOTE that we may get
         ;; a non-alist still, but it should prevent any errors
         ;; that may occur from using something like #'aget.
         ((atom subtree))
         ((atom (cdr subtree)))
         ((not (every 'consp subtree))) ; all list elements must be conses
                                        ; in alist
         ((funcall function subtree))))
     tree :tag 'prune :traversal traversal))

  (defun substitute-json-rules (language rules)
    "Update rules in RULES based on mappings found for LANGUAGE.

First patches (from `*tree-sitter-json-rule-patches*') are applied,
then whole substitutions (from
`*tree-sitter-json-rule-substitutions*')."
    ;; NOTE: this will become inefficient with a lot of rule
    ;;       substitutions.
    (labels ((patch->cons (patch)
               "Expand PATCH into a cons that can be passed to sublis."
               (ematch patch
                 ((lambda-list &key replace with)
                  (cons replace with))
                 ((lambda-list &key wrap with)
                  (cons wrap (subst wrap '_ with)))
                 ((lambda-list &key label as)
                  (unless (stringp as) (fail))
                  (patch->cons
                   `(:wrap ,label
                     :with ((:type . "FIELD")
                            (:name . ,as)
                            (:content . _)))))))
             (patch-rule (rule patches)
               (let ((alist (mapcar #'patch->cons patches)))
                 (sublis alist rule :test #'equal)))
             (patch-rules (rules)
               (let ((patches (aget-all (make-keyword language)
                                        *tree-sitter-json-rule-patches*)))
                 (iter (for (rule-type . rule) in rules)
                       (if-let ((relevant-patches (aget-all rule-type patches)))
                         (collect (cons rule-type
                                        (patch-rule rule relevant-patches))
                                  into patched)
                         (collect (cons rule-type rule) into unchanged))
                       ;; Preserve the property that the changed rules
                       ;; are moved to the front of the list.
                       (finally (return (nconc patched unchanged))))))
             (substitute-rules (rules)
               (let ((substitutions
                      (aget-all (make-keyword language)
                                *tree-sitter-json-rule-substitutions*)))
                 (reduce
                  (lambda (rules substitution)
                    (areplace (car substitution) (cdr substitution) rules))
                  substitutions :initial-value rules))))
      (patch-rules (substitute-rules rules))))

  (defun substitute-json-node-types (substitutions node-types)
    "Substitute types in NODE-TYPES based on mappings found for LANGUAGE."
    (let* ((node-types
            ;; NOTE: this will become inefficient with a lot of node
            ;;       type substitutions.
            (reduce
             (lambda (node-types substitution)
               (cons substitution (remove (cdar substitution) node-types
                                          :key #'cdar :test #'equal)))
             substitutions :initial-value (copy-list node-types)))
           (constraints
            (iter constraints
                  (for node-type in node-types)
                  (for type = (cdar node-type))
                  (when-let (subtypes (aget :subtypes node-type))
                    (iter (for subtype in subtypes)
                          (in constraints
                              (collect (list type (cdar subtype))))))))
           (ordering (toposort constraints :test #'equal)))
      (stable-sort node-types ordering :key #'cdar)))

  (defun add-aliased-rules
      (language-prefix rules
       &aux (rule-name-set
             (alist-hash-table (mapcar (op (cons (car _) t))
                                       rules))))
    "Add rules for named types which are only used as aliases in RULES.
This is to prevent certain classes from being seen as terminal symbols."
    (labels ((superset-alias-p
                 (subset superset &aux (subtype (aget :type subset))
                                    (supertype (aget :type superset)))
               "RETURN T if SUPERSET is a superset of SUBSET."
               ;; NOTE: this only considers supersets that start with a "CHOICE".
               (cond
                 ((and (equal subtype "CHOICE") (equal subtype supertype))
                  (iter
                    (for branch in (aget :members subset))
                    (always
                     (find-if (op (equal _ branch)) (aget :members superset)))))
                 ((equal supertype "CHOICE")
                  (find-if (op (equal _ subset)) (aget :members superset)))))
             (collect-strictly-aliased-types
                 (&aux (alias->content (make-hash-table :test #'equal)))
               "Collects all named aliases that have a :VALUE which
                doesn't exist as a rule."
               (walk-json
                (lambda (alist &aux (content (aget :content alist)))
                  (when (and
                         (aget :named alist)
                         (equal "ALIAS" (aget :type alist))
                         (not
                          (gethash
                           (make-keyword (convert-name
                                          language-prefix (aget :value alist)))
                           rule-name-set)))
                    (symbol-macrolet
                        ((hash-value (gethash (aget :value alist)
                                              alias->content)))
                      (cond
                        ((or (not hash-value)
                             (superset-alias-p hash-value content))
                         ;; add if it is the first time seeing this.
                         ;; NOTE: that it is updated if hash-value is a
                         ;;       subset or equal to content.
                         (setf hash-value content))
                        ;; do nothing when a superset is already stored.
                        ((superset-alias-p content hash-value))
                        ;; when there isn't a super set, a new choice needs
                        ;; created.
                        ;; NOTE: there's a possibility that the choice will be
                        ;;       misordered, but if there's the generated choice
                        ;;       is seen later in the rules, it will be updated
                        ;;       by the first clause to be in the order seen
                        ;;       in the rules.
                        (t
                         (let ((previous (if (equal (aget :type hash-value)
                                                    "CHOICE")
                                             (aget :members hash-value)
                                             (list hash-value)))
                               (current (if (equal (aget :type content)
                                                   "CHOICE")
                                            (aget :members content)
                                            (list content))))
                           (setf hash-value
                                 `((:type . "CHOICE")
                                   (:members
                                    ,@(filter-map
                                       (distinct :test #'equal)
                                       (append previous current)))))))))))
                rules)
               alias->content)
             (expand-alias-content (content)
               "Expands CONTENT into a rule."
               (map-json
                (lambda (alist)
                  ;;TODO: only interested in symbols. Shouldn't follow nested
                  ;;      aliases; just prune. Sould only expand once.
                  (if (aget :type alist)
                      (string-case (aget :type alist)
                        ;; TODO: check prune here. nil may be incorrect.
                        ("ALIAS"
                         (throw 'prune alist))
                        ("SYMBOL"
                         (aget (make-keyword (convert-name language-prefix
                                                           (aget :name alist)))
                               rules))
                        (t alist))
                      alist))
                content))
             (create-aliased-rules (alias->content)
               "Create rules for every alias in ALIAS->CONTENT."
               (maphash-return
                (lambda (name content)
                  (cons (make-keyword (convert-name language-prefix name))
                        (expand-alias-content content)))
                alias->content)))
      (append rules (create-aliased-rules (collect-strictly-aliased-types)))))

  (defun combine-aliased-rules (language-prefix rule-table)
    "Search each rule for aliases. When an alias is found, add a 'CHOICE'
around the relevant rule and have a choice branch for the original rule and
a branch for the alias. This should help get around several issues that come
up due to aliases."
    (labels ((ensure-choice (rule)
               "Ensure that RULE begins with a 'CHOICE'."
               (if (string= "CHOICE" (aget :type rule))
                   rule
                   `((:TYPE . "CHOICE") (:MEMBERS ,rule))))
             (add-aliases (rule aliased-content)
               ;; NOTE: an error here indicates a case that hasn't
               ;;       been considered yet.
               ;; TODO: this is a hack. string-ecase should be used.
               (string-case (aget :type aliased-content)
                 (("BLANK" "STRING" "PATTERN")
                  `((:TYPE . "CHOICE")
                    (:MEMBERS
                     ,@(aget :members rule)
                     ,aliased-content)))
                 ("SYMBOL"
                  `((:TYPE . "CHOICE")
                    (:MEMBERS
                     ,@(aget :members rule)
                     ,(gethash
                        (make-keyword
                         (convert-name language-prefix
                                       (aget :name aliased-content)))
                        rule-table))))
                 ("CHOICE"
                  (reduce #'add-aliases (aget :members aliased-content)
                          :initial-value rule))))
             (update-rule
                 (rule-name aliased-content
                  &aux (rule-key (make-keyword (convert-name language-prefix
                                                             rule-name))))
               "Update the rule specified by RULE-NAME such that it considers
                ALIASED-CONTENT to be a valid, match-able state of the rule."
               (symbol-macrolet ((rule (gethash rule-key rule-table)))
                 (let ((result (add-aliases (ensure-choice rule)
                                            aliased-content)))
                   ;; NOTE: this is working around using string-case above
                   ;;       and is purposefully avoiding certain alias content.
                   (unless (or (member nil result)
                               (null result))
                     (setf rule result))))))
      (iter
        (for (nil rule) in-hashtable rule-table)
        (walk-json (lambda (subtree)
                     (when (and (string= "ALIAS" (aget :type subtree))
                                (aget :named subtree))
                       (update-rule
                        (aget :value subtree) (aget :content subtree))))
                   rule))
      rule-table))

  (defun insert-internal-ast-slots
      (language-prefix transformed-json-rule insert-paths
       class-name class-name->class-definition
       &key top-level-rule
       &aux (internal-asts-postfix -1))
    "Insert internal-asts slots into TRANSFORMED-JSON-RULE for each path in
INSERT-PATHS.
TOP-LEVEL-RULE indicates that TRANSFORMED-JSON-RULE is the rule that the parser
matches as the root of the AST. Internal AST slots are added around the rule when
it is true."
    ;; TODO: at some point, prevent the insertion of any internal-asts slots in
    ;;       in rules that represent computed text ASTs.
    (labels ((trim-paths (paths)
               "Remove the first number from every path."
               (mapcar #'cdr paths))
             (order-paths (paths)
               "Order paths such that they're assorted by the first number in
                their path."
               (sort (assort paths :key #'car) #'< :key #'caar))
             (generate-internal-asts-slot
                 (&aux (slot-name (format nil "INTERNAL-ASTS-~a"
                                          (incf internal-asts-postfix)))
                    (slot-symbol (format-symbol :sel/sw/ts "~a-~a"
                                                language-prefix slot-name)))
               "Generate a new internal-asts slot for the current rule and
                add the slot to the class definition."
               (add-slot-to-class-definition
                class-name
                class-name->class-definition
                `(,slot-symbol
                  :accessor ,slot-symbol
                  :initarg ,(make-keyword slot-symbol)
                  :initform nil)
                :add-to-child-slots t)
               `((:TYPE . "SLOT")
                 (:NAME . ,slot-name)))
             (add-top-level-slot (rule-tree)
               "Add a proceding internal AST slot for RULE-TREE.
                This is a special edge case."
               ;; NOTE: at some point, there may need to be a preceding
               ;;       slot too, but it will add more complexity to the
               ;;       implementation. There aren't any examples where
               ;;       it would be useful to do this at the moment.
               (if (equal (aget :type rule-tree) "SEQ")
                   `((:TYPE . "SEQ")
                     (:MEMBERS
                      ,@(aget :members rule-tree)
                      ,(generate-internal-asts-slot)))
                   `((:TYPE . "SEQ")
                     (:MEMBERS
                      ,rule-tree
                      ,(generate-internal-asts-slot)))))
             (add-preceding-internal-asts-field (subtree)
               "Add a preceding internal-asts slot to SUBTREE and return
                the result."
               `((:TYPE . "SEQ")
                 (:MEMBERS
                  ,(generate-internal-asts-slot)
                  ,subtree)))
             (add-succeeding-internal-asts-field (subtree)
               "Add a proceding internal-asts slot to SUBTREE and return
                the result."
               `((:TYPE . "SEQ")
                 (:MEMBERS
                  ,subtree
                  ,(generate-internal-asts-slot))))
             (add-surrounding-internal-asts-fields (subtree)
               "Add a surrounding internal-asts slot to SUBTREE and return
                the result."
               `((:TYPE . "SEQ")
                 (:MEMBERS
                  ,(generate-internal-asts-slot)
                  ,subtree
                  ,(generate-internal-asts-slot))))
             (handle-choice
                 (rule paths &aux (ordered-paths (order-paths paths)))
               "Handle RULE as a 'CHOICE' rule."
               `((:TYPE . "CHOICE")
                 (:MEMBERS
                  ,@(iter
                      (for i upfrom 0)
                      (for member in (aget :members rule))
                      (if (eql i (caaar ordered-paths))
                          (collect (handle-rule
                                    member (trim-paths (pop ordered-paths))))
                          (collect member))))))
             (handle-seq (rule paths &aux (ordered-paths (order-paths paths)))
               "Handle RULE as a 'SEQ' rule."
               `((:TYPE . "SEQ")
                 (:MEMBERS
                  ,@(iter
                      (for i upfrom 0)
                      (for member in (aget :members rule))
                      (if-let ((current-paths (and (eql i (caaar ordered-paths))
                                                   (pop ordered-paths))))
                        (cond
                          ((length= 1 (car current-paths))
                           (collect (generate-internal-asts-slot))
                           (collect member))
                          (t (collect (handle-rule
                                       member (trim-paths current-paths)))))
                        (collect member))))))
             (handle-repeat (rule paths)
               "Handle RULE as a 'REPEAT' rule."
               `((:TYPE . "REPEAT")
                 (:CONTENT
                  ,@(handle-rule (aget :CONTENT rule) (trim-paths paths)))))
             (handle-alias (rule paths)
               "Handle RULE as an 'ALIAS' rule."
               ;; NOTE: assume that internal-asts slots will only occur before this.
               (if (member nil paths)
                   (add-preceding-internal-asts-field rule)
                   rule))
             (handle-string (rule paths)
               "HANDLE RULE as a string."
               ;; TODO: handle-string is a workaround for an upstream bug that
               ;;       causes newline terminals to have a range which spans
               ;;       multiple newlines. Remove this and have string be
               ;;       considered a terminal once it is fixed. To check if it
               ;;       has been fixed, try #'cl-tree-sitter:parse-string with a
               ;;       c-preproc-include and check if the newline spans multiple
               ;;       newlines.
               (let ((newline-p (member '(newline) paths :test #'equal))
                     (preceding-internal-asts-p (member nil paths)))
                 (cond
                   ((and newline-p preceding-internal-asts-p)
                    (add-surrounding-internal-asts-fields rule))
                   (newline-p
                    (add-succeeding-internal-asts-field rule))
                   (preceding-internal-asts-p
                    (add-preceding-internal-asts-field rule))
                   (rule))))
             (handle-terminal (rule paths)
               "Handle RULE as a terminal."
               (if (member nil paths)
                   (add-preceding-internal-asts-field rule)
                   rule))
             (handle-rule (rule paths)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               (string-ecase (aget :type rule)
                 ("ALIAS" (handle-alias rule paths))
                 ("CHOICE" (handle-choice rule paths))
                 ("REPEAT" (handle-repeat rule paths))
                 ("SEQ" (handle-seq rule paths))
                 (("PATTERN" "TOKEN") (handle-terminal rule paths))
                 ("STRING" (handle-string rule paths)))))
      (let ((internal-asts-rule
              (if insert-paths
                  (handle-rule transformed-json-rule insert-paths)
                  transformed-json-rule)))
        (if top-level-rule
            (add-top-level-slot internal-asts-rule)
            internal-asts-rule))))

  (defun add-internal-ast-slots
      (language-prefix transformed-json-rule class-name class-name->class-definition
       &key top-level-rule
       &aux insert-paths in-field-flag*)
    "Return a modified version of TRANSFORMED-JSON-RULE with internal-asts slots
added in between consecutive terminal symbols.
TOP-LEVEL-RULE indicates that TRANSFORMED-JSON-RULE is the rule that the parser
matches as the root of the AST."
    (declare (special in-field-flag*))
    (labels ((handle-choice (rule path &optional preceding-terminal?)
               "Handle RULE as a 'CHOICE' rule."
               ;; TODO: prefer one internal slot before the choice or inlined
               ;;       into an enclosing sequence. Address this if having more
               ;;       than one internal slot becomes a problem; it is likely
               ;;       non-trivial to implement.
               (find-if
                #'identity
                (iter
                  (for i upfrom 0)
                  (for member in (aget :members rule))
                  (collect
                      ;; NOTE: handle-rule will recursively traverse the tree
                      ;;       pushing the position of any place that requires
                      ;;       an internal AST slot to insert-paths. Because of
                      ;;       this, handle-rule needs to be called on every
                      ;;       child of the choice rule without exiting early.
                      (handle-rule member (cons i path) preceding-terminal?)))))
             (handle-seq (rule path &optional preceding-terminal?)
               "Handle RULE as a 'SEQ' rule."
               (iter
                 (for i upfrom 0)
                 (for member in (aget :members rule))
                 (for preceding
                      initially preceding-terminal?
                      then (handle-rule member (cons i path) preceding))
                 (finally (return preceding))))
             (handle-repeat (rule path &optional preceding-terminal?)
               "Handle RULE as a 'REPEAT' rule."
               ;; NOTE: perform twice to loop the ending field of the repeat
               ;;       back to the front of the repeat. This will determine
               ;;       if a repeat will need a slot before or after certain
               ;;       branches.
               (iter
                 (repeat 3)
                 (for preceding
                      initially preceding-terminal?
                      then (or (handle-rule
                                (aget :content rule)
                                (cons 0 path)
                                preceding)
                               preceding))
                 ;; NOTE: take into account preceding-terminal? for cases
                 ;;       where the repeat is blank.
                 (finally (return (or preceding preceding-terminal?)))))
             (handle-alias (rule path &optional preceding-terminal?)
               "Handle RULE as an 'ALIAS' rule."
               (unless (aget :named rule)
                 (handle-terminal path preceding-terminal?)))
             (handle-terminal (path &optional preceding-terminal?)
               "Handle RULE as a terminal."
               ;; NOTE: when inside the content of a FIELD, a BLANK is the only
               ;;       thing that will still be considered a terminal.
               (when (and (not in-field-flag*) preceding-terminal?)
                 (push (reverse path) insert-paths))
               (not in-field-flag*))
             (handle-string (rule path &optional preceding-terminal?)
               "Handle RULE as a string."
               ;; TODO: handle-string is a workaround for an upstream bug that
               ;;       causes newline terminals to have a range which spans
               ;;       multiple newlines. Remove this and have string be
               ;;       considered a terminal once it is fixed. To check if it
               ;;       has been fixed, try #'cl-tree-sitter:parse-string with a
               ;;       c-preproc-include and check if the newline spans multiple
               ;;       newlines.
               (when (not in-field-flag*)
                 (cond-every
                   (preceding-terminal? (push (reverse path) insert-paths))
                   ((equal #.(fmt "~%") (aget :value rule))
                    (push (reverse (cons 'newline path)) insert-paths))))
               (not in-field-flag*))
             (handle-field (rule path &optional preceding-terminal?
                            &aux (in-field-flag* t))
               (declare (special in-field-flag*))
               (handle-rule
                (aget :content rule)
                (cons 0 path)
                preceding-terminal?))
             (handle-rule (rule path &optional preceding-terminal?
                           &aux (type (aget :type rule)))
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               (when type
                 (string-ecase type
                   ("ALIAS" (handle-alias rule path preceding-terminal?))
                   ;; NOTE: immediate tokens shouldn't have internal-asts before them.
                   ;;       Blanks will be surrounded by internal-asts slots if it
                   ;;       is treated normally.
                   ("BLANK" t)
                   ("CHOICE" (handle-choice rule path preceding-terminal?))
                   ("FIELD" (handle-field rule path preceding-terminal?))
                   ("IMMEDIATE_TOKEN" (not in-field-flag*))
                   (("PATTERN" "TOKEN")
                    (handle-terminal path preceding-terminal?))
                   ("REPEAT" (handle-repeat rule path preceding-terminal?))
                   ("SEQ" (handle-seq rule path preceding-terminal?))
                   ("STRING" (handle-string rule path preceding-terminal?))
                   ("SYMBOL")))))
      (handle-rule transformed-json-rule nil)
      (list
       (insert-internal-ast-slots
        language-prefix
        transformed-json-rule
        insert-paths
        class-name
        class-name->class-definition
        :top-level-rule top-level-rule))))

  (defun transform-json-rule (language-prefix rule grammar rule-key-stack)
    "Expand inline rules base on GRAMMAR and :repeat1's in RULE.
RULE-KEY-STACK is a stack of rules that have already been visited in the
recursive calls for this function. Returns a transformed version of RULE."
    (labels ((propagate-field (tree &aux (field-name (aget :name tree)))
               "Return a modified version of TREE such that its field is
                propagated to its relevant subtrees and remove the enclosing
                field."
               (map-json
                (lambda (subtree &aux (type (aget :type subtree)))
                  (if type
                    (string-case type
                      (("STRING" "SYMBOL" "ALIAS" "BLANK")
                       (throw 'prune `((:TYPE . "FIELD")
                                       (:NAME . ,field-name)
                                       (:CONTENT ,@subtree))))
                      (t subtree))
                    subtree))
                (aget :content tree)
                :traversal :preorder))
             (propagate-field-p (tree)
               "Return T if TREE is a field rule that should be replaced
                with a transformed version of its content that transfers
                the field rule to the relevant subtrees."
               (when (equal "FIELD" (aget :type tree))
                 (walk-json
                  (lambda (subtree)
                    ;; TODO: determine if anything else would cause this.
                    (when (member (aget :type subtree) '("REPEAT" "SEQ")
                                  :test #'equal)
                      (return-from propagate-field-p t)))
                  tree)))
             (propagate-and-collapse-fields (tree)
               "Return a modified version of TREE with every field that
                satisfies propagate-field-p removed and replaced with
                a propagated version of its content."
               (map-json
                (lambda (subtree)
                  (if (propagate-field-p subtree)
                      (throw 'prune (propagate-field subtree))
                      subtree))
                tree
                :traversal :preorder))
             (expand-repeat1s (tree)
               "Expand all :REPEAT1 rules in an equivalent :SEQ and :REPEAT."
               (map-json (lambda (alist)
                           (if-let ((content (and (equal (aget :type alist)
                                                         "REPEAT1")
                                                  (aget :content alist))))
                             `((:type . "SEQ")
                               (:members
                                ,content
                                ((:type . "REPEAT")
                                 (:content ,@content))))
                             alist))
                         tree))
             (expand-inline-rules
                 (tree &aux (inline-rules (aget :inline grammar)))
               "Expand all inline rules."
               (map-json
                (lambda (alist)
                  (let* ((name-string (aget :name alist))
                         (name-key (make-keyword
                                    (convert-name language-prefix name-string)))
                         (inlinep (member name-string inline-rules
                                          :test #'equal)))
                    ;; NOTE: it appears that all "SYMBOL"s that start with
                    ;;       an underscore are inlined.
                    ;;       DON'T inline the supertype rules.
                    (cond-let result
                      ;; Not inlineable
                      ((not (and (equal (aget :type alist) "SYMBOL")
                                 (or
                                  (and (eql #\_ (aref name-string 0))
                                       ;; Prevent infinite recursion on
                                       ;; rules already expanded.
                                       (not (member name-key rule-key-stack)))
                                  ;; Python has one inline rule without
                                  ;; an underscore.
                                  inlinep)
                                 ;; Don't inline supertypes unless they are
                                 ;; explicitly inlined.
                                 (or (not (member name-string
                                              (aget :supertypes grammar)
                                                  :test #'equal))
                                     inlinep)))
                       alist)
                      ((aget name-key (aget :rules grammar))
                       ;; Transform it again before inlining.
                       (transform-json-rule
                        language-prefix result grammar
                        (cons name-key rule-key-stack)))
                      ((member name-string (aget :externals grammar)
                               :test #'equal
                               :key {aget :name})
                       ;; Remove it from consideration.
                       `((:type . "BLANK")))
                      (t alist))))
                tree))
             (remove-prec-rules (tree)
               "Removes all precedent rules from TREE, replacing them with
                their content."
               (map-json (lambda (alist)
                           (if (member (aget :type alist)
                                       '("PREC" "PREC_DYNAMIC"
                                         "PREC_LEFT" "PREC_RIGHT")
                                       :test #'equal)
                               (aget :content alist)
                               alist))
                         tree))
             (branch-comparison-form (subtree)
               "Map SUBTREE to a 'branch comparison form'. This
                removes any types from 'SYMBOL' and 'FIELD' alists such that
                the types won't be considered for equality when an #'equal
                comparison is made. Named 'ALIAS' subtrees are also replaced
                with a 'SYMBOL' alist as they behave the same."
               ;; NOTE: it is unlikely that this will become a bottleneck, but
               ;;       a custom walk-tree in unison function could be written
               ;;       instead of remapping the subtree here.
               (map-json (lambda (alist)
                           (cond
                             ((equal (aget :type alist) "SYMBOL")
                              (adrop '(:name) alist))
                             ((equal (aget :type alist) "FIELD")
                              (adrop '(:content) alist))
                             ((and (equal (aget :type alist) "ALIAS")
                                   (aget :name alist))
                              `((:type . "SYMBOL")))
                             (t alist)))
                         subtree))
             (branch-similar-p (branch1 branch2)
               "Compare BRANCH1 with BRANCH2 and return T if they have
                identical branch comparison forms."
               (equal (branch-comparison-form branch1)
                      (branch-comparison-form branch2)))
             (collect-branch-types (branch &aux type-stack)
               "Collect the relevant type information from BRANCH
                that can be merged into a single branch."
               (walk-json
                (lambda (alist)
                  (cond
                    ((equal (aget :type alist) "SYMBOL")
                     (push (aget :name alist) type-stack))
                    ((equal (aget :type alist) "FIELD")
                     ;; NOTE: this should be expanded into a
                     ;;       choice inside the field content.
                     (push (aget :content alist) type-stack)
                     (throw 'prune nil))
                    ((and (equal (aget :type alist) "ALIAS")
                          (aget :named alist))
                     (push (aget :value alist) type-stack)
                     (throw 'prune nil))
                    (t alist)))
                branch :traversal :preorder)
               (reverse type-stack))
             (merge-choice-branch (similar-branches)
               "Merge SIMILAR-BRANCHES into one branch by
                adding multiple types onto the end of the relevant
                cons in the alist."
               (if (length< 1 similar-branches)
                   (let ((types-stack
                           ;; NOTE: creating a stack of types that can
                           ;;       be used to markup the initial branch
                           ;;       while it mapped.
                           (apply
                            {mapcar #'list}
                            (mapcar #'collect-branch-types
                                    (cdr similar-branches)))))
                     (map-json
                      (lambda (alist)
                        (cond
                          ((equal (aget :type alist) "SYMBOL")
                           (throw 'prune
                             (areplace
                              :name
                              (append (ensure-cons (aget :name alist))
                                      ;; This flatten is a bit of a hack.
                                      (flatten (pop types-stack)))
                              alist)))
                          ((equal (aget :type alist) "FIELD")
                           ;; NOTE: this should be expanded into a
                           ;;       choice inside the field content.
                           (throw 'prune
                             (areplace
                              :content
                              ;; TODO: don't create a new CHOICE every time,
                              ;;       only create it if it is needed.
                              `((:type . "CHOICE")
                                (:members
                                 ,(aget :content alist)
                                 ,@(pop types-stack)))
                              alist)))
                          ((and (equal (aget :type alist) "ALIAS")
                                (aget :named alist))
                           (throw 'prune
                             (areplace
                              :value
                              (append (ensure-cons (aget :value alist))
                                      ;; This flatten is a bit of a hack.
                                      (flatten (pop types-stack)))
                              alist)))
                          (t alist)))
                      (car similar-branches)
                      :traversal :preorder))
                   ;; Just return the only branch.
                   (car similar-branches)))
             (merge-choice-branches (subtree)
               "Return SUBTREE with its choice branches merged."
               (areplace
                :members
                (mapcar #'merge-choice-branch
                        (assort (aget :members subtree)
                                :test #'branch-similar-p))
                subtree))
             (merge-similar-choice-branches (tree)
               "Merge choice branches with identical output transformations in
                TREE."
               (map-json
                (lambda (alist)
                  (if (equal "CHOICE" (aget :type alist))
                      (merge-choice-branches alist)
                      alist))
                tree)))
      (propagate-and-collapse-fields
       (merge-similar-choice-branches
        (expand-repeat1s
         (remove-prec-rules
          (expand-inline-rules rule)))))))

  (defun substitute-field-symbols
      (json-rule class-name language-prefix symbol-substitutions
       class-name->class-definition class-name->parse-tree-transforms)
    "Substitute symbols in TRANSFORMED-JSON that should be treated as fields.
A slot is added to the relevant definition in CLASS-NAME->CLASS-DEFINITION and
the list of parse tree transforms is updated for the relevant class in
CLASS-NAME->PARSE-TREE-TRANSFORMS."
    (labels ((update-class-definition
                 (symbol-substitution
                  &aux (slot-name (format-symbol
                                   'sel/sw/ts "~a-~a"
                                   language-prefix
                                   (convert-name
                                    language-prefix
                                    (getf symbol-substitution
                                          :slot-name)))))
               "Update class-name->class-definition with information from
                SYMBOL-SUBSTITUTION."
               ;; TODO: add slot name to symbols that need exported.
               (add-slot-to-class-definition
                (format-symbol 'sel/sw/ts "~a-~a" language-prefix class-name)
                class-name->class-definition
                `(,slot-name :accessor ,slot-name
                             :initarg ,(make-keyword slot-name)
                             :initform nil)
                :arity (or (getf symbol-substitution :arity) 0)
                :add-to-child-slots t))
             (update-class-transforms (symbol-substitution)
               "Update class-name->parse-tree-transforms with a cons of
                slot name and the function used for transforming the parse tree."
               (symbol-macrolet ((class-transforms
                                   (gethash class-name
                                            class-name->parse-tree-transforms)))
                 (setf class-transforms
                       (cons (getf symbol-substitution :transform)
                             class-transforms))))
             (encapsulate-symbol (subtree symbol-substitution)
               "Encapsulates SUBTREE with a field and updates the relevant
                hash tables."
               ;; Some rules don't have a corresponding class if they are
               ;; aliased.
               (when (update-class-definition symbol-substitution)
                 (update-class-transforms symbol-substitution))
               `((:TYPE . "FIELD")
                 (:NAME . ,(getf symbol-substitution :slot-name))
                 (:CONTENT
                  ,@subtree))))
      (if symbol-substitutions
          (map-json
           (lambda (subtree)
             (cond-let substitution
               ((not (member (aget :type subtree) '("SYMBOL" "ALIAS" "STRING")
                             :test #'equal))
                subtree)
               ((find-if
                 (lambda (symbol-substitution
                          &aux (symbol-name (or (aget :name subtree)
                                                (aget :value subtree))))
                   (member symbol-name symbol-substitution :test #'equal))
                 symbol-substitutions :key (op (getf _ :symbol-names)))
                ;; Check if the filter function passes.
                (if (funcall (getf substitution :predicate)
                             class-name json-rule subtree)
                    (encapsulate-symbol subtree substitution)
                    subtree))
               (t subtree)))
           json-rule)
          json-rule)))

  (defun prune-rule-tree (transformed-json-rule)
    (labels ((gather-field-types (content)
               "Return a list of symbols that CONTENT could be for a field."
               ;; TODO: this WHEN check should not be necessary
               (when (listp content) ; ignore if not a list
                 ;; TODO: remove duplicates from the gathered field types.
                 (remove-duplicates
                  (string-case (aget :type content)
                    ;; TODO: may need to add more things here.
                    ;;       It also might make sense to use walk-json here
                    ;;       since it wasn't available when this was originally
                    ;;       written.
                    ("STRING"
                     (list (aget :value content)))
                    ("SYMBOL"
                     (list (aget :name content)))
                    ("ALIAS"
                     (list (aget :value content)))
                    (("CHOICE" "SEQ")
                     (mappend #'gather-field-types (aget :members content)))
                    ("REPEAT"
                     (gather-field-types (aget :content content)))
                    ("BLANK"
                     (list 'null)))
                  :test #'equal)))
             (handle-seq (rule)
               "Handle RULE as a 'SEQ', 'REPEAT', 'REPEAT1', or 'CHOICE' rule."
               (let ((children (if-let ((members (aget :members rule)))
                                 ;; seq and choice
                                 (mapcar #'handle-rule members)
                                 ;; repeat and repeat1
                                 (list (handle-rule (aget :content rule))))))
                 (unless (every #'null children)
                   (cons (make-keyword (aget :type rule)) children))))
             (handle-field (rule)
               "Handle RULE as a 'FIELD' rule."
               `(:field
                 ,(aget :name rule)
                 ,@(flatten (gather-field-types (aget :content rule)))))
             (handle-symbol (rule)
               "Handle RULE as a 'SYMBOL' rule. This checks if
                it is part of the AST's children."
               `(:child ,@(ensure-cons (aget :name rule))))
             (handle-alias (rule)
               ;; NOTE: this assumes that all named aliases go into
               ;;       the children slot.
               (when (aget :named rule)
                 `(:child ,@(ensure-cons (aget :value rule)))))
             (handle-slot (rule)
               "Handle RULE as a 'SLOT' rule. Note that slot rules
                are not part of tree-sitter and are added by
                add-internal-ast-slots."
               `(:slot ,@(ensure-cons (aget :name rule))))
             (handle-rule (rule)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               ;; TODO: this WHEN check of the rule should not be necessary
               (when rule
                   (string-ecase (aget :type rule)
                     ("ALIAS" (handle-alias rule))
                     (("BLANK" "IMMEDIATE_TOKEN" "TOKEN"
                               "PATTERN" "STRING"))
                     (("CHOICE" "SEQ" "REPEAT" "REPEAT1")
                      (handle-seq rule))
                     ("FIELD" (handle-field rule))
                     ("SLOT" (handle-slot rule))
                     ("SYMBOL"
                      ;; NOTE: this assumes that all 'SYMBOL's that are seen
                      ;;       going into the children slot. Also NOTE that
                      ;;       the inline rules should be inlined before entering
                      ;;       this function.
                      (handle-symbol rule))))))
      (if transformed-json-rule ; watch for null rule
          (handle-rule transformed-json-rule))))

  ;; TODO: update doc string
  (defun collapse-rule-tree (rule-tree)
    "Return a new version of TREE that removes empty subtrees,
expands :REPEAT1 rules, and collapses contiguous, nested
:SEQ rules."
    ;; NOTE: the collapsed representation makes analysis much easier
    ;;       especially when dealing with nested choices.
    (labels ((collapse-on (tree value)
               "Collapse subtrees of TREE that start with VALUE."
               (cond
                 ((atom tree) tree)
                 ((eql (car tree) value)
                  (iter
                    (for child in tree)
                    (let ((result (collapse-on child value)))
                      (cond
                        ((atom result) (collect result))
                        ((eql (car result) value)
                         (mapcar (lambda (child) (collect child)) (cdr result)))
                        (t (collect result))))))
                 (t (mapcar {collapse-on _ value} tree))))
             (collapse-sequences (tree)
               "Collapse nested sequences into a single sequence."
               (collapse-on tree :seq))
             (collapse-choices (tree)
               "Collapse nested choices into a single choice."
               (collapse-on tree :choice))
             (prune-empty-subtrees (tree)
               "Remove subtrees without :field's or :child's."
               (prune-if (lambda (node)
                           (or (null node)
                               (and (listp node)
                                    (length= node 1))))
                         tree)))
      (collapse-sequences
       (collapse-choices
        (prune-empty-subtrees rule-tree)))))

  (defun collect-rule-tree (predicate tree &aux accumulator)
    "Collect every subtree in TREE that satisfies PREDICATE."
    (walk-tree
     (lambda (subtree)
       (when (and (listp subtree)
                  (funcall predicate subtree))
         (push subtree accumulator)))
     tree)
    accumulator)

  (defun collect-rule-slots (tree &key include-slots)
    "Collect all slots used in TREE."
    (let ((slot-identifiers `(:field :child ,@(when include-slots '(:slot)))))
      (collect-rule-tree
       (lambda (subtree)
         (member (car subtree) slot-identifiers))
       tree)))

  ;; TODO: at some point, this may need to treat interal-asts slots as fields
  ;;       when considering whether they are problematic.
  (defun structured-rule-p (collapsed-rule pruned-rule)
    ;; NOTE: this will only operate on fields and children.
    ;;       There may be potential for interleaved text to have their own issues,
    ;;       but if they don't have their own field then it's an issue with the
    ;;       parser.

    ;; TODO: At some point, maybe consider variations on the strings
    ;;       (non-slot fields) too, but it might not be reasonable (or it's
    ;;       difficult with repeats) to do this?
    (labels ((collect-repeats (tree)
               "Collect all repeat subtrees in TREE."
               (collect-rule-tree
                (lambda (subtree)
                  ;; NOTE: :repeat1's should be expanded at this point.
                  (eql (car subtree) :repeat))
                tree))
             (collect-choices (tree)
               "Collect all choice subtrees in TREE."
               (collect-rule-tree
                (lambda (subtree)
                  (eql (car subtree) :choice))
                tree))
             (collect-branch-slots (choice)
               "Collect the slots in each branch of CHOICE."
               (iter
                 (for branch in (cdr choice))
                 (when-let ((slots (collect-rule-slots branch)))
                   (collect slots))))
             (incompatible-choice-p (branch-slots-1 branch-slots-2)
               "Compare BRANCH-SLOTS-1 to BRANCH-SLOTS-2 and return T
                if they are 'incompatible'."
               ;; NOTE: :child pairs will contain a type string in the
               ;;        #'cadr position that doesn't matter since it
               ;;        will go into the same slot, so the :child types
               ;;        are stripped here before comparison.
               ;; TODO: it may be the case that a :choice with an empty
               ;;       branch is problematic too or we can assume the
               ;;       empty branch isn't interesting when reproducing the
               ;;       text.
               (let* ((slots-1 (mapcar (lambda (slot-pair)
                                         (if (eql (car slot-pair) :child)
                                             :child
                                             (cadr slot-pair)))
                                       branch-slots-1))
                      (slots-2 (mapcar (lambda (slot-pair)
                                         (if (eql (car slot-pair) :child)
                                             :child
                                             (cadr slot-pair)))
                                       branch-slots-2))
                      (comparison-func {equal (car slots-1)}))
                 (not
                  (if (every comparison-func slots-1)
                      ;; NOTE: all the same. This covers the case where
                      ;;       there's just one slot usage.
                      (every comparison-func slots-2)
                      ;; NOTE: this maintains the same order otherwise.
                      (equal slots-1 slots-2)))))
             (incompatible-choice-in-repeat-p (rule)
               "Return T if there is an incompatible choice in a repeat."
               ;; NOTE: this should only consider a choice in a repeat problematic
               ;;       if the choice has two separate slots in two separate
               ;;       branches, there are multiple slots in a different order,
               ;;       or a branch has a slot while another only has terminals.
               ;;       This causes an issue trying to reproduce the ordering.
               (iter
                 (for choice in (mappend #'collect-choices (collect-repeats rule)))
                 (for slots-in-branches = (collect-branch-slots choice))
                 ;; NOTE: compare all branches to the first branch. If one
                 ;;       matches the first, then it should be the same as
                 ;;       comparing to the first again.
                 (thereis (or
                           (and slots-in-branches (some #'null choice))
                           (some {incompatible-choice-p (car slots-in-branches)}
                                 slots-in-branches)))))
             (use-after-repeat-p (tree &key in-repeat? repeat-alist)
               "Return non-NIL if TREE contains a problematic usage of a slot after
                a repeat on that slot."
               ;; NOTE: it isn't strictly the case that a use after repeat will
               ;;       be problematic; it can be handled by counting the number of
               ;;       uses after the repeat that are needed for a specific type.
               ;;       It is problematic any time a slot occurs in two separate
               ;;       repeats.
               (when (listp tree)
                 (case (car tree)
                   (:seq
                    (iter
                      (for subtree in tree)
                      (for (values subtree-repeat-alist use-after?)
                           first (values repeat-alist)
                           then (use-after-repeat-p
                                 subtree :in-repeat? in-repeat?
                                 :repeat-alist subtree-repeat-alist))
                      (when use-after?
                        (leave (values nil use-after?)))
                      (finally
                       (return (values subtree-repeat-alist)))))
                   (:repeat
                    (iter
                      (for subtree in tree)
                      (for (values subtree-repeat-alist use-after?)
                           first (values repeat-alist)
                           then (use-after-repeat-p
                                 subtree :in-repeat? tree
                                 :repeat-alist subtree-repeat-alist))
                      (when use-after?
                        (leave (values nil use-after?)))
                      (finally
                       (return (values subtree-repeat-alist)))))
                   (:choice
                    (iter
                      (for subtree in tree)
                      (for (values subtree-repeat-alist use-after?)
                           = (use-after-repeat-p
                              subtree :in-repeat? in-repeat?
                              :repeat-alist repeat-alist))
                      (if use-after?
                          (leave (values nil use-after?))
                          (collect subtree-repeat-alist into choice-alists))
                      (finally
                       ;; NOTE: if we're in a repeat, there could be a use after
                       ;;       a repeat in separate choice branches, but the check
                       ;;       for this would have already been done in
                       ;;       incompatible-choice-in-repeat-p, so don't make the
                       ;;       same check again.
                       (return
                         (remove-duplicates
                          (reduce #'append choice-alists)
                          :key #'car)))))
                   ((:field :child)
                    (let ((types
                            (if (eql (car tree) :field)
                                (cddr tree)
                                (cdr tree))))
                      (cond-let result
                        ((some (lambda (type)
                                 (aget type repeat-alist :test #'equal))
                               types)
                         (values nil result))
                        (in-repeat?
                         (append (mapcar {cons _ in-repeat?} types)
                                 repeat-alist))
                        (t repeat-alist))))))))
      (not (or (incompatible-choice-in-repeat-p pruned-rule)
               (nth-value 1 (use-after-repeat-p collapsed-rule))))))

  (defun expand-choice-branches (pruned-rule transformed-json)
    "Expand the choice branches the PRUNED-RULE for NODE-TYPE has
outside of repeats."
    (labels ((get-path (tree path)
               "Get the subtree in TREE at PATH."
               ;; This will blowup if a bad path is given to it.
               (if (listp path)
                   (reduce (lambda (tree position) (nth position tree)) path
                           :initial-value tree)
                   tree))
             (replace-at (list index value)
               "Replace the value at INDEX in list, copying as little as possible."
               (let ((tail (nthcdr index list)))
                 (if (eql value (first tail))
                     list
                     (nconc (ldiff list tail)
                            (list value)
                            (rest tail)))))
             (replace-at-path (tree path value)
               (if (endp path) value
                   (replace-at tree (first path)
                               (replace-at-path (nth (first path) tree)
                                                (rest path)
                                                value))))
             (get-json-path (json-tree path)
               "Get the subtree in JSON-TREE at path."
               (if (listp path)
                   (reduce (lambda (json-tree position)
                             (nth (1- position) (or (aget :members json-tree)
                                                    (aget :content json-tree))))
                           path
                           :initial-value json-tree)
                   json-tree))
             (replace-at-json-path (json-tree path value)
               (if (endp path) value
                   (let* ((entry
                           (or (assoc :members json-tree)
                               (assoc :content json-tree)))
                          (new-cdr
                           (replace-at (cdr entry)
                                       (1- (car path))
                                       (replace-at-json-path
                                        (nth (1- (car path))
                                             (cdr entry))
                                        (cdr path)
                                        value)))
                          (new-entry
                           (reuse-cons (car entry) new-cdr entry)))
                     (if (eql entry new-entry) json-tree
                         (substitute new-entry entry json-tree :count 1)))))
             (get-leaf-choice (tree &optional (path nil))
               "Gets the first 'CHOICE' in TREE that doesn't have any nested
               'CHOICE's within it."
               (cond
                 ((not (listp tree)) nil)
                 ;; exclude choices in repeats from consideration.
                 ((eql (car tree) :repeat) nil)
                 (t
                  (iter
                    (for child in tree)
                    (for i upfrom 0)
                    (collect (get-leaf-choice child (cons i path))
                      into nested-choices)
                    (finally (return (or (find-if-not #'null nested-choices)
                                         (and (eql (car tree) :choice)
                                              (reverse path)))))))))
             (expand-choice (tree path)
               "Expands the 'CHOICE' at PATH in TREE into several different trees."
               (let ((choice (get-path tree path)))
                 (assert (eql (car choice) :choice))
                 (if path
                     (mapcar (op (replace-at-path tree path _))
                             (cdr choice))
                     (cdr choice))))
             (expand-json-choice (json-tree path)
               "Expands the 'CHOICE' at PATH in JSON-TREE into several different
                trees."
               (let ((choice (get-json-path json-tree path)))
                 (assert (equal (aget :type choice) "CHOICE"))
                 (if path
                     (mapcar (op (replace-at-json-path json-tree path _))
                             (aget :members choice))
                     (aget :members choice))))
             (remove-duplicate-rules (pruned-branches json-branches)
               "Remove the duplicates from PRUNED-BRANCHES while also
                removing the 'same' item from JSON-BRANCHES. The two
                modified lists are returned as values."
               (let ((removed-duplicates
                      (filter-map (distinct :key #'car :test #'equal)
                                  (mapcar #'cons pruned-branches json-branches))))
                 (values
                  (mapcar #'car removed-duplicates)
                  (mapcar #'cdr removed-duplicates))))
             (expand-choices (tree json-tree)
               "Expand all nested choices into their own branches."
               (if (eql (car tree) :choice)
                   (values (expand-choice tree nil)
                           (expand-json-choice json-tree nil))
                   (iter
                     (for expansion-stack initially (list tree)
                          then (mappend (lambda (expansion)
                                          (expand-choice expansion choice-path))
                                        expansion-stack))
                     (for json-expansion-stack initially (list json-tree)
                          then (mappend
                                (lambda (expansion)
                                  (expand-json-choice expansion choice-path))
                                json-expansion-stack))
                     (for choice-path = (get-leaf-choice (car expansion-stack)))
                     (while choice-path)
                     (finally
                      (return (remove-duplicate-rules
                               expansion-stack json-expansion-stack)))))))
      (expand-choices pruned-rule transformed-json)))

  (defun computed-text-ast-p (language class-name json-rule &aux children?)
    "Return T if JSON-RULE and TYPE represent an AST that contains variable text."
    ;; TODO: update this to be more intelligent and not mark everything without
    ;;       children as a computed-text node.
    (or
     (member class-name
             (aget (make-keyword language) *tree-sitter-computed-text-asts*))
     (walk-json
      (lambda (subtree)
        ;(format t "subtree: ~A~%" subtree)
        ;; TODO: maybe also look at node types to see if it has any children
        ;;       default to computed text node p if it doesn't?
        (when-let (type (and (listp subtree) (aget :type subtree)))
          ;; TODO: this WHEN check shouldn't be necessary as type should always
          ;; be a string if it is found at all.
          (when (stringp type)
              (string-case type
                ;; TODO: figure out a way to remove the token rules from this.
                (("PATTERN" "TOKEN" "IMMEDIATE_TOKEN")
                 ;; PATTERN indicates that there
                 ;; is variable text.
                 ;; TOKEN and IMMEDIATE_TOKEN don't
                 ;; necessarily indicate variable text,
                 ;; but they generally have a CHOICE that
                 ;; selects from some default values, and
                 ;; since this isn't stored in a slot, it's
                 ;; generally a good idea to store it since
                 ;; things break otherwise. An example of this
                 ;; can be seen with language-provided types in
                 ;; C.
                 (return-from computed-text-ast-p t))
                ("ALIAS"
                 (if (and (listp subtree) (aget :named subtree))
                     ;; A named alias should be treated like a symbol.
                     (setf children? t)
                     ;; Aliases can have patterns in them,
                     ;; but they are recast to something else,
                     ;; so they do not need to be stored.
                     (throw 'prune nil)))
                (("FIELD" "SYMBOL" "SLOT")
                 ;; The presence of any of these indicate that there are
                 ;; children.
                 (setf children? t))))))
      json-rule)
     (not children?)))

  ;;; TODO: clean up everything that calls this. This no longer generates
  ;;;       anything but instead adds a superclass to a relevant class
  ;;;       definition. All of the callsites should be updated to reflect
  ;;;       this, and it should be renamed.
  ;;;
  ;;;       This was created before the class-name->class-definition hash table
  ;;;       was available.
  (defun generate-computed-text-method
      (transformed-json-rule class-name language class-name->class-definition
       &key skip-checking-json)
    "Generate an input transformation method for RULE if one is needed.
CLASS-NAME is used as the specialization for the generated method.
The definition of CLASS-NAME is updated to include the computed-text mixin."
    (when (or skip-checking-json
              (computed-text-ast-p language class-name transformed-json-rule))
      (add-superclass-to-class-definition
       class-name class-name->class-definition 'computed-text)
      nil))


  (defun add-slot-to-class-definition
      (class-name class-name->class-definition slot-spec
       &key add-to-child-slots (arity 0))
    "Destructively add SLOT-SPEC to CLASS-NAME's definition in
CLASS-NAME->CLASS-DEFINITION. Return NIL on failure and non-NIL on success."
    (labels ((update-child-slots (slots)
               "Update SLOTS such that child-slots contains the new
                slot-spec."
               (let* ((child-slots (assoc 'child-slots slots))
                      (slot-list (lastcar (getf (cdr child-slots) :initform))))
                 (acons
                  'child-slots
                  `(:initform
                    ',(append (butlast slot-list)
                              `((,(car slot-spec) . ,arity))
                              (last slot-list))
                    :allocation :class)
                  (remove child-slots slots)))))
      (when-let ((class-definition (gethash class-name class-name->class-definition)))
        (symbol-macrolet ((slots (cadddr class-definition)))
          (unless (aget (car slot-spec) slots)
            (setf slots
                  (cons slot-spec
                        (if add-to-child-slots
                            (update-child-slots slots)
                            slots))))))))

  (defun add-superclass-to-class-definition
      (class-name class-name->class-definition superclass)
    "Destructively add SUPERCLASS to CLASS-NAME's definition in
CLASS-NAME->CLASS-DEFINITION."
    (when-let ((class-definition (gethash class-name class-name->class-definition)))
      (symbol-macrolet ((supers (caddr class-definition)))
        (pushnew superclass supers))))

  (defun generate-children-method
      (pruned-rule json-rule class-name class-name->class-definition)
    ;; TODO: rename this method now that it doesn't generate any methods.
    "Add slots for PRUNED-RULE and JSON-RULE to the definition of CLASS-NAME in
CLASS-NAME->CLASS-DEFINITION."
    (mapc {add-slot-to-class-definition
           class-name class-name->class-definition}
          `((pruned-rule
             :accessor pruned-rule
             :initform ',pruned-rule
             :allocation :class
             :documentation
             "A rule used to order the children of this class.")
            (json-rule
             :initform ',json-rule
             :reader json-rule
             :allocation :class
             :documentation
             "A rule used to determine where inner ASTs are assigned."))))

  (defun get-json-subtree-string (json-subtree choice-resolver)
    "Get the string representation of JSON-SUBTREE. This assumes that there aren't
any slot usages in JSON-SUBTREE."
    ;; NOTE: assume that token, immediate_token, and pattern automatically
    ;;       mean that a node has variable text. Thus, if any subtree has one of
    ;;       these rules, the AST itself should be printed specially using the
    ;;       computed-text ('text') slot.
    (labels ((handle-alias (rule)
               ;; NOTE: only consider unnamed nodes unless it becomes problematic
               ;;       to not be considering the named ones.
               (if (aget :named rule)
                   ;; TODO: remove this before committing?
                   ;;
                   ;; If we reach here, we may have a problem with
                   ;; pruning rules?
                   (error "unhandled named alias")
                   (aget :value rule)))
             (handle-blank ()
               "")
             (handle-choice (rule &aux (branches (aget :members rule)))
               ;; Prefer blank branches
               (cond
                 (choice-resolver
                  (rule-handler (or (funcall choice-resolver branches)
                                    (car branches))))
                 (t (rule-handler (or (find-if
                                       (lambda (branch)
                                         (equal "BLANK" (aget :type branch)))
                                       branches)
                                      (car branches))))))
             (handle-repeat ()
               "")
             (handle-seq (rule)
               (mapcar #'rule-handler (aget :members rule)))
             (handle-string (rule)
               (aget :value rule))
             (rule-handler (rule)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if an unexpected rule is
               ;;       encountered.
               (string-ecase (aget :type rule)
                 ("ALIAS" (handle-alias rule))
                 ("BLANK" (handle-blank))
                 ("CHOICE" (handle-choice rule))
                 ("REPEAT" (handle-repeat))
                 ("SEQ" (handle-seq rule))
                 ("SLOT" "")
                 ("STRING" (handle-string rule)))))
      (apply #'string+ (flatten (rule-handler json-subtree)))))

  (defun generate-output-transformation
      (pruned-rule transformed-json-rule language class-name
       class-name->class-definition choice-resolver)
    (labels ((generate-choice (json-rule pruned-rule)
               "Generate a quoted form which handles choices."
               ;; NOTE: coming in, the in-order children stack should have a
               ;;       list as its top item. This will be of the form
               ;;       (:choice branch-number). The correct branch can be chosen
               ;;       based on branch number.
               `(ecase (cadr (pop parse-stack))
                  ,@(iter
                      (for branch in (aget :members json-rule))
                      (for pruned-branch in (cdr pruned-rule))
                      (for i upfrom 0)
                      (collect
                          (list
                           i (generate-rule-handler branch pruned-branch))))))
             (generate-field ()
               "Generate a quoted form which handles fields."
               `(pop parse-stack))
             (generate-repeat (json-rule pruned-rule)
               "Generate a quoted form which handles repeats."
               `(iter
                  (ecase (car (pop parse-stack))
                    ((:repeat :continue)
                     (collect ,(generate-rule-handler (aget :content json-rule)
                                                      (cadr pruned-rule))))
                    (:end-repeat (finish)))))
             (generate-seq (json-rule pruned-rule)
               "Generate a quoted form which handles sequences."
               `(list
                 ,@(mapcar #'generate-rule-handler
                           (aget :members json-rule) (cdr pruned-rule))))
             (generate-symbol ()
               "Generate a quoted form which handles symbols."
               `(pop parse-stack))
             (generate-slot ()
               "Generate a quoted form which handles slots."
               ;; TODO: there may be some issues here.
               `(pop parse-stack))
             (generate-rule-handler (json-rule pruned-rule)
               "Handles dispatching JSON-RULE to its relevant rule handler."
               (if pruned-rule
                   ;; NOTE: this will throw an error if the json schema for
                   ;;       the grammar.json files has changed or the form of
                   ;;       the json-rule or pruned-rule change.
                   ;;       This can also throw an error if pruning trees is
                   ;;       incorrectly done.
                   (string-ecase (aget :type json-rule)
                     ("ALIAS" (generate-symbol))
                     ("CHOICE" (generate-choice json-rule pruned-rule))
                     ("REPEAT" (generate-repeat json-rule pruned-rule))
                     ("FIELD" (generate-field))
                     ("SEQ" (generate-seq json-rule pruned-rule))
                     ("SLOT" (generate-slot))
                     ("SYMBOL" (generate-symbol)))
                   (get-json-subtree-string json-rule choice-resolver)))
             (generate-ast-list (json-rule pruned-rule)
               "Generate the form that creates a list of ASTs and strings."
               (generate-rule-handler json-rule pruned-rule))
             (generate-body (transformed-json-rule pruned-rule)
               "Generate the body of the output transformation method."
               `(list
                 (before-text ast)
                 ;; Expand all rules here.
                 ,(generate-ast-list transformed-json-rule pruned-rule)
                 (after-text ast)))
             (generate-method (transformed-json-rule pruned-rule)
               "Generate the output transformation method."
               (cond
                 ((computed-text-ast-p
                   language class-name transformed-json-rule)
                  nil)
                 ((every #'null (cdr pruned-rule))
                  ;; This is the case where we have a string representation for
                  ;; the full thing.
                  (add-slot-to-class-definition
                   class-name class-name->class-definition
                   `(text ; computed-text
                     :accessor text
                     :initarg :text
                     :allocation :class
                     :initform
                     ',(get-json-subtree-string transformed-json-rule
                                                 choice-resolver)))
                  nil)
                 (t `(defmethod output-transformation
                       ((ast ,class-name) &rest rest &key &aux (parse-stack (parse-order ast)))
                     (declare (ignorable parse-stack rest))
                     (flatten ,(generate-body transformed-json-rule pruned-rule)))))))
      (generate-method transformed-json-rule pruned-rule)))

  (defgeneric convert-to-lisp-type (prefix type-string)
    (:documentation "Convert TYPE-STRING to a lisp type for the language
PREFIX.")
    (:method (prefix type-string)
      (format-symbol 'sel/sw/ts "~a-~a"
                     prefix (convert-name prefix type-string)))
    (:method ((prefix symbol) (type-string symbol))
      (values (intern (concatenate 'string (symbol-name prefix) "-" (symbol-name type-string))
                      'sel/sw/ts)))
    (:method :around (prefix type-string)
      (if (keywordp prefix)
          (call-next-method)
          (convert-to-lisp-type (make-keyword prefix) type-string))))

  (defun generate-input/output-handling
      (pruned-rule json-rule superclass language-prefix child-types
       class-name->class-definition choice-resolver computed-text-ast?
       &key symbols-to-export
         subtype->supertypes
       &aux (subclass-counter -1)
         (subclasses
          (aget superclass
                (aget (make-keyword language-prefix)
                      *tree-sitter-choice-expansion-subclasses*))))
    "Generate a method for a type of AST that returns a choice expansion
subclass based on the order of the children were read in."
    (declare (hash-table subtype->supertypes))
    (labels ((report-problematic-rule ()
               "Reports 'unstructured' rules to *error-output*."
               (unless (or computed-text-ast?
                           (structured-rule-p
                            (collapse-rule-tree pruned-rule) pruned-rule))
                 (format *error-output* "Problematic Rule: ~a~%" superclass)))
             (get-subclass-name (collapsed-rule)
               "Get the subclass name for COLLAPSED-RULE. If one isn't in
                subclasses, generate a new name."
               (assure (and symbol (not null))
                 (or
                  (lret ((name
                          (car (find-if (lambda (pair)
                                          (equal collapsed-rule (cadr pair)))
                                        subclasses))))
                    (when name
                      (ensure-gethash name symbols-to-export t)))
                  (format-symbol :sel/sw/ts "~a-~a"
                                 ;; If there are name collisions from the counter,
                                 ;; move back to the gensym-based approach.
                                 superclass (incf subclass-counter)))))
             (convert-to-lisp-types (rule)
               "Converts all strings in RULE to lisp types."
               (map-tree
                (lambda (node)
                  (if (typep node 'string)
                      (convert-to-lisp-type language-prefix node)
                      node))
                rule))
             (add-subclass-list-slot (subclass-pairs)
               "Add a slot to superclass which contains a list of subclasses
                based on SUBCLASS-PAIRS."
               (add-slot-to-class-definition
                superclass
                class-name->class-definition
                `(choice-subclasses
                  :initform ',(mapcar #'car subclass-pairs)
                  :reader choice-subclasses
                  :allocation :class)))
             (generate-subclass (subclass-pair
                                 &aux (class-name (car subclass-pair)))
               "Generate a defclass form for SUBCLASS-PAIR."
               ;; TODO: Would additional slots and slot options also
               ;; be useful?
               (push class-name
                     (gethash 'class-order class-name->class-definition))
               (setf (gethash class-name class-name->class-definition)
                     `(defclass ,class-name
                          (,superclass
                           ,@(gethash class-name subtype->supertypes))
                        ((rule
                          :initform ',(cadr subclass-pair)
                          :reader rule
                          :allocation :class)
                         (choice-superclass
                          :initform ',superclass
                          :reader choice-superclass
                          :allocation :class)
                         (choice-subclasses
                          :initform nil
                          :reader choice-subclasses
                          :allocation :class)))))
             (generate-subclasses (subclass-pairs)
               "Generate a defclass forms for SUBCLASS-PAIRS."
               ;; TODO: rename this function since it doesn't generate anything
               ;;       internal to this function anymmore.
               (map nil #'generate-subclass subclass-pairs))
             (generate-children-methods
                 (subclass-pairs json-expansions expansions?)
               "Generate the methods for handling children for
                every subclass pair in SUBCLASS-PAIRS."
               ;; TODO: rename this function since it doesn't generate anything
               ;;       anymore.
               (mapc
                (lambda (subclass-pair json-expansion)
                  (generate-children-method
                   (caddr subclass-pair)
                   json-expansion
                   (car subclass-pair)
                   class-name->class-definition))
                (if expansions?
                    ;; Generate children information for the superclass so it
                    ;; can be used as a default class.
                    (cons
                     (cons superclass (cdr (car subclass-pairs)))
                     subclass-pairs)
                    subclass-pairs)
                (if expansions?
                    (cons json-rule json-expansions)
                    json-expansions)))
             (generate-input-subclass-dispatch (json-expansions subclass-pairs)
               "Generate a method to return the name of the subclass
                to be used by the parse-tree returned by tree-sitter."
               `(defmethod get-choice-expansion-subclass
                    ((class (eql ',superclass)) parse-tree
                     &aux (child-types ',child-types))
                  (cond
                   ,@(mapcar
                      (lambda (json-rule subclass-pair)
                        `((match-parsed-children
                           ,language-prefix
                           ',json-rule
                           ',(caddr subclass-pair) child-types parse-tree)
                          ',(car subclass-pair)))
                      json-expansions
                      subclass-pairs)
                   (t
                    (error 'parse-tree-matching-error
                           :superclass ',superclass
                           :parse-tree parse-tree
                           :child-types ',child-types
                           :subclasses ',(mapcar #'car subclass-pairs))))))
             (generate-computed-text-methods (json-expansions subclass-pairs)
               "Generate the variable text methods for the rules in
                JSON-EXPANSION."
               (map nil
                    (op (generate-computed-text-method
                         _ (car _) language-prefix class-name->class-definition))
                    json-expansions subclass-pairs))
             (generate-output-transformations
                 (pruned-expansions json-expansions subclass-pairs)
               "Generate the output transformations for each subclass."
               (mapcar (lambda (pruned-rule json-rule class-name)
                         (generate-output-transformation
                          pruned-rule json-rule language-prefix class-name
                          class-name->class-definition choice-resolver))
                       pruned-expansions
                       json-expansions
                       (mapcar #'car subclass-pairs)))
             (generate-superclass-slot-usage ()
               "Generate the slot-usage slot for the superclass."
               ;; NOTE: store slot usage at the superclass level so that
               ;;       #'children-parser is able to tell when any slot that
               ;;       a subclass could use hasn't been used.
               (let ((slots
                       (remove-duplicates
                        (mapcar
                         (lambda (cons)
                           (if (member (car cons) '(:field :slot))
                               (cadr cons)
                               'children))
                         (collect-rule-slots
                          (convert-to-lisp-types
                           (collapse-rule-tree pruned-rule))
                          :include-slots t)))))
                 (add-slot-to-class-definition
                  superclass
                  class-name->class-definition
                  `(slot-usage
                    :accessor slot-usage
                    :initform ',slots
                    :allocation :class
                    :documentation
                    "A set of slots that are used in the pruned-rule.")))))
      (report-problematic-rule)
      ;; TODO: refactor this and children-parser as it accepts a
      ;;       pruned rule and wasn't before.
      (if (computed-text-ast-p language-prefix superclass json-rule)
          ;; Don't expand choices if the super class is computed text.
          `(progn
             ,(generate-computed-text-method
               json-rule superclass language-prefix
               class-name->class-definition))
          (mvlet* ((pruned-rule-expansions
                    ;; NOTE: if json-expansions has 1 item, it does NOT mean that
                    ;;       it is the same as json-rule! This is important
                    ;;       because the rule slot needs to be set with the item
                    ;;       in json-expansions to match pruned-rule.
                    json-expansions
                    (expand-choice-branches pruned-rule json-rule))
                   (expansions? (not (length= 1 pruned-rule-expansions)))
                   (collapsed-rule-expansions
                    (mapcar [#'convert-to-lisp-types #'collapse-rule-tree]
                            pruned-rule-expansions))
                   (subclass-pairs
                    (if expansions?
                        (mapcar (lambda (collapsed-rule pruned-rule)
                                  (list (get-subclass-name collapsed-rule)
                                        ;; TODO: get rid of this?
                                        collapsed-rule
                                        pruned-rule))
                                collapsed-rule-expansions
                                (mapcar
                                 #'convert-to-lisp-types pruned-rule-expansions))
                        `((,superclass
                           ,(car collapsed-rule-expansions)
                           ,(convert-to-lisp-types
                             (car pruned-rule-expansions)))))))
            (when expansions?
              (generate-subclasses subclass-pairs)
              (add-subclass-list-slot subclass-pairs))
            (generate-superclass-slot-usage)
            (generate-children-methods
             subclass-pairs json-expansions expansions?)
            `(progn
               ,(and expansions? (generate-input-subclass-dispatch
                                  json-expansions subclass-pairs))
               ;; TODO: at some point, determine how to remove this.
               ,@(generate-computed-text-methods json-expansions subclass-pairs)
               ,@(generate-output-transformations
                  pruned-rule-expansions json-expansions subclass-pairs))))))

  (defun generate-structured-text-methods
      (grammar types language-prefix
       class-name->class-definition choice-resolver
       json-field-transformations
       &key symbols-to-export
         subtype->supertypes
       &aux (class-name->parse-tree-transforms (make-hash-table)))
    (declare (hash-table subtype->supertypes))
    ;; NOTE: it might make sense to integrate the loop into
    ;;       the class generation above at some point.

    ;; TODO: NOTE: figure out how to handle the superclasses instead of
    ;;             treating them as terminal nodes.
    ;;
    ;;             There's a top-level "superclasses" form in the grammar.
    ;;             can probably ignore based on that.

    ;; NOTE: things we know at this point:
    ;;        - If a choice occurs in a repeat, all branches share the same slots.
    ;;        - Choices outside of repeats will be expanded if any of the branches
    ;;          contain different slots or ordering of slots.
    (labels ((generate-code
                 (transformed-json type-json
                  &aux (class-name (convert-to-lisp-type
                                    language-prefix (aget :type type-json))))
               "Generate the code for TRANSFORMED-JSON that is of the type
                specified by TYPE-JSON."
               (generate-input/output-handling
                (prune-rule-tree transformed-json)
                transformed-json
                class-name
                ;; NOTE: this should be a keyword.
                language-prefix
                (mapcar [{convert-to-lisp-type language-prefix}
                         {aget :type}]
                        (aget :types (aget :children type-json)))
                class-name->class-definition
                choice-resolver
                (computed-text-ast-p
                 language-prefix class-name transformed-json)
                :symbols-to-export symbols-to-export
                :subtype->supertypes subtype->supertypes))
             (generate-terminal-code (type-string class-name)
               "Generate the code for a terminal symbol."
               ;; TODO: rename this function
               ;; destructively modify the class definition.
               (add-slot-to-class-definition
                class-name class-name->class-definition
                `(text
                  :accessor text
                  :initarg :text
                  :initform ',type-string)))
             (generate-parse-tree-transform (class-name transforms)
               "Generate a method for generated-transform-parse-tree based on
                class-name and transforms."
               `(defmethod generated-transform-parse-tree
                    ((language (eql ',(make-keyword language-prefix)))
                     (class (eql ',(format-symbol :sel/sw/ts "~a-~a"
                                    language-prefix class-name)))
                     parse-tree)
                  (reduce
                   (lambda (tree transform)
                     (funcall transform tree))
                   ,(cons 'list transforms)
                   :initial-value parse-tree)))
             (get-transformed-json-table ()
               "Get a hash table containing transformed JSON rules."
               (let* ((rules (add-aliased-rules
                              language-prefix
                              (substitute-json-rules
                               language-prefix
                               (aget :rules grammar))))
                      (new-grammar (areplace :rules rules grammar))
                      (rule-table (alist-hash-table
                                   (mapcar (lambda (rule)
                                             (cons (car rule) (cdr rule)))
                                           rules))))
                 (iter
                   (iter:with root-rule-name = (caar (aget :rules grammar)))
                   (for (key value) in-hashtable
                        (combine-aliased-rules language-prefix rule-table))
                   (setf (gethash key rule-table)
                         (add-internal-ast-slots
                          language-prefix
                          (transform-json-rule
                           language-prefix
                           (substitute-field-symbols
                            value
                            key language-prefix
                            json-field-transformations
                            class-name->class-definition
                            class-name->parse-tree-transforms)
                           new-grammar (list key))
                          (convert-to-lisp-type language-prefix key)
                          class-name->class-definition
                          :top-level-rule (eq root-rule-name key))))
                 rule-table))
             (get-superclasses-set ()
               "Get a hash set containing the names of superclasses for the
                language."
               (alist-hash-table
                (mapcar {cons _ t} (aget :supertypes grammar))
                :test #'equal))
             (get-parse-tree-transforms ()
               "Get a list of generated-transform-parse-tree methods based on
                values in class-name->class-definition."
               (maphash-return #'generate-parse-tree-transform
                               class-name->parse-tree-transforms)))
      `(progn
         ,@(iter
             (iter:with super-classes-set = (get-superclasses-set))
             (iter:with rule-table = (get-transformed-json-table))
             (iter:with type-set = (make-hash-table :test #'equal))
             (for type in types)
             (for type-string = (aget :type type))
             (for converted-type = (convert-name language-prefix type-string))
             (for lisp-type = (convert-to-lisp-type
                               language-prefix converted-type))
             (for terminal-lisp-type = (format-symbol 'sel/sw/ts "~a-~a"
                                                      lisp-type 'terminal))
             (cond-let result
               ;; skip superclass interfaces.
               ;; TODO: at some point, the interfaces will always be at the top,
               ;;       so they can be counted out and skipped instead of this.
               ((gethash type-string super-classes-set))
               ((gethash converted-type type-set)
                ;; If the name is already in the type set, that means a rule
                ;; has the same name as a language keyword. As an example,
                ;; Python has a 'lambda' rule but also has a 'lambda' keyword
                ;; as part of the language.
                (generate-terminal-code type-string terminal-lisp-type))
               ((gethash (make-keyword converted-type) rule-table)
                ;; Only add to the type-set here since there won't be a
                ;; name-clash for terminal symbols.
                (setf (gethash converted-type type-set) t)
                (collect (generate-code (car result) type)))
               ((aget :named type)
                ;; If a type is named and doesn't have a rule present assume
                ;; that it is a computed text node. This is an edge case
                ;; that shouldn't happen often and should cover external
                ;; named nodes.
                (generate-computed-text-method
                 nil lisp-type language-prefix class-name->class-definition
                 :skip-checking-json t))
               (t
                ;; If a type doesn't have a rule and is unnamed, it is considered
                ;; a terminal symbol.
                (generate-terminal-code
                 type-string
                 (if (gethash terminal-lisp-type class-name->class-definition)
                     terminal-lisp-type
                     lisp-type)))))
         ,@(get-parse-tree-transforms))))

  (defun create-tree-sitter-classes
      (node-types-file grammar-file name-prefix
       &key ast-superclasses base-ast-superclasses
         software-superclasses software-direct-slots
         ast-extra-slot-options
         ast-extra-slots
         node-type-substitutions
         json-subtree-choice-resolver
         json-field-transformations
         additional-supertypes
       &aux (subtype->supertypes (make-hash-table))
         (symbols-to-export (make-hash-table))
         (class->extra-slot-options (make-hash-table))
         (class->extra-slots (make-hash-table))
         (ast-superclass (symbolicate
                          name-prefix
                          "-"
                          (convert-name name-prefix "ast")))
         (class-name->class-definition (make-hash-table))
         (*json-identifier-name-to-lisp* {convert-name name-prefix})
         (node-types
          (substitute-json-node-types
           node-type-substitutions
           (decode-json-from-string
            (file-to-string node-types-file))))
         (grammar (decode-json-from-string (file-to-string grammar-file)))
         ;; TODO: consider turning this into a hash table.
         (grammar-rules (aget :rules grammar)))
    "Create the classes for a tree-sitter language.

Creates one class for a software object (named NAME-PREFIX) and many
classes for ASTs, using NAME-PREFIX as their prefix.

AST-SUPERCLASSES is an alist of superclasses and the names of the AST
classes that should inherit from them.

BASE-AST-SUPERCLASSES is a list of superclasses for the base
class (`X-ast') of the language's ASTs.

SOFTWARE-SUPERCLASSES is a list of superclass names for the software
object.

SOFTWARE-DIRECT-SLOTS is a list of slots to be added to the created
sofware class.

AST-EXTRA-SLOT-OPTIONS is an alist from classes to extra options for
their slots.

AST-EXTRA-SLOTS is an alist from classes to extra slots."
    (labels ((add-additional-supertypes (additional-supertypes)
               "Destructively add ADDITIONAL-SUPERTYPES to the supertypes of
                grammar."
               (when additional-supertypes
                 (symbol-macrolet ((supertypes (aget :supertypes grammar)))
                   (setf supertypes (append additional-supertypes supertypes)))))
             (initialize-subtype->supertypes ()
               "Initialize subtype->supertypes with the super types that
                aren't parsed from the json files."
               (mapc
                (lambda
                    (types-list &aux (supertype (symbolicate (car types-list))))
                  (mapc
                   (lambda (subtype)
                     (push supertype (gethash subtype subtype->supertypes)))
                   (cdr types-list)))
                ast-superclasses)
               ;; Add super class into all of these to ensure it's present. When
               ;; add-supertypes-to-subtypes is called, it will need to remove
               ;; it.
               (maphash-keys
                (lambda (subtype)
                  (symbol-macrolet ((subtype-hash
                                      (gethash subtype subtype->supertypes)))
                    (setf subtype-hash (append1 subtype-hash ast-superclass))))
                subtype->supertypes)
               ;; Return for easier debugging.
               subtype->supertypes)
             (initialize-class->extra-slot-options ()
               (iter (for (class . fields) in ast-extra-slot-options)
                     (setf (gethash class class->extra-slot-options) fields))
               ;; Return for easier debugging.
               class->extra-slot-options)
             (initialize-class->extra-slots ()
               (iter (for (class . slots) in ast-extra-slots)
                 (setf (gethash class class->extra-slots) slots))
               ;; Return for easier debugging.
               class->extra-slots)
             (populate-supertypes ()
               "Populate the subtypes to supertypes with supertypes. This is
                to have the information available if the definition occurs after
                its first usage."
               (mapc
                (lambda
                    (supertype
                     &aux (type (find-if (op (equal supertype (aget :type _)))
                                         node-types)))
                  ;; TODO: add error output if type is nil.
                  (when type
                    (add-supertype-to-subtypes
                     (aget :type type) (aget :subtypes type))))
                (aget :supertypes grammar)))
             (make-class-name (&optional name-string)
               "Create a class name based on NAME-STRING and add it to the
                symbols that need exported."
               ;; NOTE: this has the potential for name clashes
               ;;       though it's probably unlikely.
               (lret* ((*package* (find-package :sel/sw/ts))
                       (name
                        (if name-string
                            (convert-to-lisp-type name-prefix name-string)
                            (symbolicate name-prefix))))
                 (ensure-gethash name symbols-to-export t)))
             (make-accessor-name (prefix name-keyword)
               "Create an accessor name based on NAME-KEYWORD and
                PREFIX and add it to the symbols that need exported."
               (lret ((name (symbolicate
                             prefix
                             "-"
                             name-keyword)))
                 (ensure-gethash name symbols-to-export t)))
             (make-node-initargs (prefix initarg)
               "Return a list of two initargs, suitable for splicing
                into a node definition. One is just INITARG; the other
                other is INITARG prefixed with NAME-PREFIX."
               ;; `ft:tree-copy' relies on every slot having a keyword
               ;; initarg that is string= to the slot's name.
               (list :initarg initarg
                     :initarg (make-keyword (string+ prefix "-" initarg))))
             (make-accessor-names (name-keyword)
               "Create accessor names based on NAME-PREFIX and
                `*tree-sitter-ast-extra-prefixes*` and add them to the
                symbols that need exporting."
               (cons (make-accessor-name name-prefix name-keyword)
                     (mapcar (op (make-accessor-name _ name-keyword))
                             (aget name-prefix
                                   *tree-sitter-ast-extra-prefixes*
                                   :test #'string=))))
             (get-supertypes-for-type (type)
               "Retrieve the supertypes of TYPE."
               (gethash (make-class-name type) subtype->supertypes))
             (add-supertype-to-subtypes (supertype subtypes)
               "Add SUPERTYPE to the list of superclasses for
                each type in SUBTYPES."
               (mapc
                (lambda (subtype &aux (name (aget :type subtype)))
                  (symbol-macrolet ((subtype-hash (gethash (make-class-name name)
                                                           subtype->supertypes)))
                    (setf subtype-hash
                          (cons (make-class-name supertype)
                                ;; Remove ast-superclass to prevent
                                ;; circular class dependency.
                                (remove ast-superclass subtype-hash)))))
                subtypes))
             (create-slot (type field)
               "Create a slot for TYPE based on FIELD."
               (let ((names (make-accessor-names (car field))))
                 `(,(first names)
                   ,@(mappend (op `(:accessor ,_)) names)
                   ;; Prefixed initargs.
                   ,@(mappend (op `(:initarg ,(make-keyword _))) names)
                   :initform nil
                   ,@(lookup-extra-slot-options type (first names)))))
             (lookup-extra-slot-options (type field-name)
               (declare (symbol type field-name))
               (aget field-name (gethash type class->extra-slot-options)))
             (create-slots (type fields)
               "Create the slots for a new class of TYPE based on
                FIELDS and CHILDREN. Currently, slot types aren't
                supported, but there is enough information to limit
                slots to certain types."
               (declare (symbol type) (list fields))
               ;; NOTE: there is a small possibility for name overlaps when
               ;;       generating these slots.
               (mapcar {create-slot type} fields))
             (create-supertype-class (type
                                      &aux (class-name (make-class-name type)))
               "Create a new class for subtypes to inherit from."
               `(defclass ,class-name
                    (,@(or (get-supertypes-for-type type)
                           `(,ast-superclass)))
                  ()
                  (:documentation ,(format nil "Generated for ~a." type))))
             (create-type-class (type fields grammar-rules
                                 &aux (class-name (make-class-name type)))
               "Create a new class for TYPE using FIELDS and CHILDREN for slots."
               (let* ((extra-fields (gethash class-name class->extra-slots))
                      (all-fields (append fields extra-fields))
                      (child-slot-order
                        (append
                         (when all-fields
                           (mapcar
                            (lambda (slot-keyword)
                              (cons
                               (translate-to-slot-name slot-keyword name-prefix)
                               (if (aget :multiple
                                         (aget slot-keyword all-fields))
                                   0
                                   1)))
                            (slot-order
                             name-prefix type all-fields grammar-rules)))
                         (when extra-fields
                           ;; Assume an arity of 0 for now.
                           (mapcar
                            (op (cons
                                 (translate-to-slot-name (car _1) name-prefix)
                                 (if (aget :multiple (cdr _1))
                                     0
                                     1)))
                            extra-fields)))))
                 `(define-node-class
                      ,class-name
                      (,@(or (get-supertypes-for-type type)
                             `(,ast-superclass)))
                    (,@(create-slots class-name all-fields)
                     (child-slots
                      :initform
                      ',(append '((before-asts . 0))
                                child-slot-order
                                '((children . 0))
                                '((after-asts . 0)))
                      :allocation :class))
                    ;; NOTE: this is primarily for determing which rule this
                    ;;       was generated for.
                    (:documentation ,(format nil "Generated for ~a." type))
                    (:method-options :skip-children-definition))))
             (create-terminal-symbol-class (type)
               "Create a new class that represents a terminal symbol.
                In the case that there's a non-terminal with the same name,
                append '-terminal' to the end of it."
               `(define-node-class
                    ,(if (gethash
                          (format-symbol 'sel/sw/ts "~a-~a"
                                         name-prefix
                                         (convert-name name-prefix type))
                          class-name->class-definition)
                         (make-class-name
                          (format-symbol 'sel/sw/ts "~a-~a"
                                         (string-upcase type) 'terminal))
                         (make-class-name type))
                    (,@(or (get-supertypes-for-type type)
                           `(,ast-superclass))
                     terminal-symbol)
                  ()
                  (:documentation
                   ,(format nil "Generated for terminal symbol '~a'" type))))
             (create-node-class
                 (grammar-rules node-type
                  &aux (type (aget :type node-type))
                    (subtypes (aget :subtypes node-type))
                    (named-p (aget :named node-type)))
               "Create a class for  NODE-TYPE."
               (let* ((class-definition
                        (cond
                          (subtypes (create-supertype-class type))
                          (named-p
                           (create-type-class
                            type
                            (aget :fields node-type)
                            grammar-rules))
                          ;; Terminal Symbol
                          (t (create-terminal-symbol-class type))))
                      (class-name (cadr class-definition)))
                 (push class-name
                       (gethash 'class-order class-name->class-definition))
                 (setf (gethash class-name class-name->class-definition)
                       class-definition)))
             (create-external-class (name)
               "Create a class for an external rule."
               (when name
                 `(define-node-class ,(make-class-name name) (,ast-superclass) ())))
             (create-external-classes (grammar)
               "Create classes for the external rules for the grammar file."
               (filter-map (op (create-external-class (aget :name _)))
                           (aget :externals grammar)))
             (generate-variation-point-code ()
               "Generate the code for source-text-fragment and error
                variation-points."
               ;; TODO: need to make sure that the slots and accessors are
               ;;       exported so that match can access the slots for
               ;;       templates.
               `(progn
                  (define-node-class ,(make-class-name "error-variation-point")
                      ,(remove-duplicates
                        `(error-variation-point
                          ,ast-superclass
                          ,@(get-supertypes-for-type "error-variation-point"))
                        :from-end t)
                    ((#1=,(convert-to-lisp-type name-prefix "parse-error-ast")
                         :accessor ,(make-accessor-name name-prefix 'parse-error-ast)
                         :accessor parse-error-ast
                         :initform nil
                         ,@(make-node-initargs name-prefix :parse-error-ast))
                     (,(convert-to-lisp-type name-prefix  "error-tree")
                      :accessor ,(make-accessor-name name-prefix  'error-tree)
                      :accessor error-tree
                      :initform nil
                      ,@(make-node-initargs name-prefix :error-tree))
                     (child-slots :initform '((#1# . 1))
                                  :allocation :class))
                    (:documentation "Generated for error variation points."))

                  (define-node-class
                      ,(make-class-name "error-variation-point-tree")
                      ,(remove-duplicates
                        `(error-variation-point
                          ,ast-superclass
                          ,@(get-supertypes-for-type
                             "error-variation-point-tree"))
                        :from-end t)
                    ((,(convert-to-lisp-type name-prefix  "parse-error-ast")
                      :accessor ,(make-accessor-name name-prefix 'parse-error-ast)
                      :accessor parse-error-ast
                      :initform nil
                      ,@(make-node-initargs name-prefix :parse-error-ast))
                     (#2=,(convert-to-lisp-type name-prefix  "error-tree")
                         :accessor ,(make-accessor-name name-prefix 'error-tree)
                         :accessor error-tree
                         :initform nil
                         ,@(make-node-initargs name-prefix :error-tree))
                     (child-slots :initform '((#2# . 1))
                                  :allocation :class))
                    (:documentation "Generated for error variation points."))

                  (define-node-class
                      ,(make-class-name "source-text-fragment-variation-point")
                      ,(remove-duplicates
                        `(source-text-fragment-variation-point
                          ,ast-superclass
                          ,@(get-supertypes-for-type
                             "source-text-fragment-variation-point"))
                        :from-end t)
                    ((#3=,(convert-to-lisp-type
                           name-prefix "source-text-fragment")
                         :accessor ,(make-accessor-name name-prefix 'source-text-fragment)
                         :accessor source-text-fragment
                         :initform nil
                         ,@(make-node-initargs name-prefix :source-text-fragment))
                     (,(convert-to-lisp-type
                        name-prefix  "source-text-fragment-tree")
                      :accessor ,(make-accessor-name name-prefix 'source-text-fragment-tree)
                      :accessor source-text-fragment-tree
                      :initform nil
                      ,@(make-node-initargs name-prefix :source-text-fragment-tree))
                     (child-slots :initform '((#3# . 1))
                                  :allocation :class))
                    (:documentation
                     "Generated for source text fragment variation points."))

                  (define-node-class
                      ,(make-class-name
                        "source-text-fragment-variation-point-tree")
                      ,(remove-duplicates
                        `(source-text-fragment-variation-point
                          ,ast-superclass
                          ,@(get-supertypes-for-type
                             "source-text-fragment-variation-point"))
                        :from-end t)
                    ((,(convert-to-lisp-type name-prefix  "source-text-fragment")
                      :accessor ,(make-accessor-name name-prefix 'source-text-fragment)
                      :accessor source-text-fragment
                      :initform nil
                      ,@(make-node-initargs name-prefix :source-text-fragment))
                     (#4=,(convert-to-lisp-type
                           name-prefix  "source-text-fragment-tree")
                         :accessor ,(make-accessor-name name-prefix
                                     'source-text-fragment-tree)
                         :accessor source-text-fragment-tree
                         :initform nil
                         ,@(make-node-initargs name-prefix
                                               :source-text-fragment-tree))
                     (child-slots :initform '((#4# . 1))
                                  :allocation :class))
                    (:documentation
                     "Generated for source text fragment variation points."))

                  (define-node-class ,(make-class-name "error-tree")
                      ,(remove-duplicates
                        `(error-tree
                          ,ast-superclass
                          ,@(get-supertypes-for-type "error-tree"))
                        :from-end t)
                    ((child-slots :initform '((children . 0))
                                  :allocation :class))
                    (:documentation "Generated for error trees."))

                  (define-node-class
                      ,(make-class-name "source-text-fragment-tree")
                      ,(remove-duplicates
                        `(error-tree
                          ,ast-superclass
                          ,@(get-supertypes-for-type
                             "source-text-fragment-tree"))
                        :from-end t)
                    ((child-slots :initform '((children . 0))
                                  :allocation :class))
                    (:documentation
                     "Generated for source-text-fragment trees.")))))
      (add-additional-supertypes additional-supertypes)
      (initialize-subtype->supertypes)
      (initialize-class->extra-slot-options)
      (initialize-class->extra-slots)
      (populate-supertypes)
      ;; populate hash table of tree-sitter classes.
      (map nil {create-node-class grammar-rules} node-types)
      (let ((structured-text-code
              (generate-structured-text-methods
               grammar node-types name-prefix class-name->class-definition
               json-subtree-choice-resolver json-field-transformations
               :symbols-to-export symbols-to-export
               :subtype->supertypes subtype->supertypes))
            (root-rule-name (caar (aget :rules grammar))))
        `(progn
           (eval-always
             (define-software ,(make-class-name) (tree-sitter
                                                  ,@software-superclasses)
               (,@software-direct-slots)
               (:documentation
                ,(format nil "~a tree-sitter software representation."
                         name-prefix)))

             (define-template-builder ,(make-class-name)
                 ,(make-class-name "ast"))

             (define-node-class ,(make-class-name "ast")
                 (,@(mapcar [#'car #'ensure-list]
                            base-ast-superclasses)
                  tree-sitter-ast)
               ;; NOTE: ensure there is always a children slot.
               ;;       This is important for classes that don't have
               ;;       it but can have comments mixed in.
               ((children ,@(mappend (op `(:accessor ,_))
                                     (make-accessor-names :children))
                          :accessor direct-children
                          :documentation
                          "Returns all language-specific children.
Unlike the `children` methods which collects all children of an AST from any slot."
                          :initarg :children
                          :initarg :direct-children
                          :initform nil)
                (child-slots :allocation :class
                             :initform '((children . 0))))
               (:documentation
                ,(format nil "AST for ~A from input via tree-sitter."
                         name-prefix))
               (:method-options :skip-children-definition))

             (defmethod language-ast-class ((lang (eql ',(make-class-name))))
               ',(make-class-name "ast"))

             (defmethod language-ast-class ((lang ,(make-class-name)))
               ',(make-class-name "ast"))

             (defmethod ast-language-class ((ast ,(make-class-name "ast")))
               ',(make-class-name))

             (defmethod ast-language-class ((ast (eql ',(make-class-name "ast"))))
               ',(make-class-name))

             ;; TODO: the error and comment classes may be created by some
             ;;       languages?
             ;; NOTE: the following are to handle results returned from
             ;;       cl-tree-sitter.
             (define-node-class ,(make-class-name "comment")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "comment")
                     comment-ast)
                   :from-end t)
               ()
               (:documentation "Generated for parsed comments."))

             (define-node-class ,(make-class-name "error")
                 ,(remove-duplicates
                   `(,ast-superclass
                     computed-text
                     ,@(get-supertypes-for-type "error")
                     parse-error-ast)
                   :from-end t)
               ((children
                 :initarg :children
                 :initarg :direct-children
                 :initform nil)
                (child-slots :initform '((children . 0))
                             :allocation :class))
               (:documentation "Generated for parsing errors."))

             (define-node-class ,(make-class-name "inner-whitespace")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "inner-whitespace")
                     inner-whitespace)
                   :from-end t)
               ()
               (:documentation "Generated for inner whitespace."))

             (define-node-class ,(make-class-name "source-text-fragment")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "source-text-fragment")
                     source-text-fragment)
                   :from-end t)
               ()
               (:documentation "Generated for source text fragments."))

             (define-node-class ,(make-class-name "blot")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "blot")
                     blot)
                   :from-end t)
               ()
               (:documentation "Generated for source text fragments."))

             ,(generate-variation-point-code)

             ,@(create-external-classes grammar)

             ;; NOTE: we want to maintain the order of the classes as they
             ;;       were created. Since hash tables are unordered, a stack
             ;;       is kept at the key 'class-order so that they can be
             ;;       retrieved in order.
             ,@(iter
                 (for
                  class-name in
                  (reverse (gethash 'class-order class-name->class-definition)))
                 (collect (gethash class-name class-name->class-definition)))

             (define-mutation ,(make-class-name "mutation") (parseable-mutation)
               ()
               (:documentation
                ,(format nil "Mutation interface for ~a software objects."
                         name-prefix)))

             (export/tree-sitter
              ',(iter
                 (for (symbol) in-hashtable symbols-to-export)
                 (collect symbol))))

           (defmethod convert
               ((to-type (eql ',ast-superclass)) (spec ,ast-superclass)
                &key &allow-other-keys)
             spec)

           (defmethod convert ((to-type (eql ',ast-superclass)) (spec list)
                               &rest args &key &allow-other-keys)
             (apply #'convert 'tree-sitter-ast spec :superclass to-type args))

           (defmethod convert ((to-type (eql ',ast-superclass)) (string string)
                               &rest args &key &allow-other-keys)
             (apply #'convert 'tree-sitter-ast string :superclass to-type args))

           (defmethod parse-asts ((obj ,(make-class-name))
                                  &optional (source (genome-string obj)))
             (convert ',(make-class-name "ast") source))

           (defmethod root-rule-ast-p ((ast ,(make-class-name root-rule-name)))
             t)

           (defmethod root-rule-ast-p
               ((name (eql ,(make-keyword
                             (convert-name name-prefix root-rule-name)))))
             t)

           ;; This works around a bug with ranges for newlines in tree-sitter
           ;; and should be removed when fixed upstream.
           (defmethod transform-parse-tree
               ((language (eql ',(make-keyword name-prefix)))
                (class (eql ',(format-symbol :sel/sw/ts "~a-~%" name-prefix)))
                parse-tree
                &rest rest &key lines
                &aux (start-range (caadr parse-tree))
                  (start-line (cadr start-range)))
             (declare (ignorable rest))
             (if (equal start-range (cadadr parse-tree))
                 ;; Don't modify zero-width tokens.
                 parse-tree
                 `(,(car parse-tree)
                   ,(list
                     ;; TODO: will this become an issue with unicode?
                     (list (position (char-code #\newline) (aref lines start-line)
                                     :start (car start-range))
                           start-line)
                     (list 0 (1+ start-line)))
                   ,(caddr parse-tree))))

           ;; This provides access to extra ASTs, such as comments.
           (defmethod extra-asts ((language (eql ',(make-keyword name-prefix))))
             '(:error-variation-point
               :error-variation-point-tree
               :inner-whitespace
               :blot
               ,@(iter
                  (for extra in (aget :extras grammar))
                  (when (equal (aget :type extra) "SYMBOL")
                    (collect
                        (make-keyword
                         (convert-name name-prefix (aget :name extra))))))))

           ,structured-text-code)))))

(defmacro define-and-export-all-mixin-classes ()
  "Ensure that all mixin classes are defined and exported."
  (let ((classes
         (nest (remove-duplicates)
               (mapcar #'car)
               (mappend #'cdr)
               *tree-sitter-ast-superclasses*))
        (class->superclasses
          (nest (alist-hash-table)
                (mappend #'cdr *tree-sitter-mixin-ast-superclasses*))))
    `(progn
       ,@(iter (for class in classes)
               (let ((class (intern (string class) :sel/sw/ts))
                     (description
                      (nest
                       (string-downcase)
                       (substitute #\Space #\-)
                       (if (string$= '-ast class)
                           (drop -4 (string class)))
                       (string class))))
                 (collect `(progn
                             (export/tree-sitter ',class)
                             (unless (find-class ',class nil)
                               (defclass ,class
                                   (,@(or (gethash class class->superclasses)
                                          '(ast)))
                                 ()
                                 (:documentation
                                  ,(fmt "Mix-in for ~a AST classes."
                                        description)))))))))))

(eval-always
 (define-and-export-all-mixin-classes))

(setf (overlord:use-threads-p) nil)

(defmacro create-tree-sitter-language-cache
    (name &aux
            (upcase-name (string-upcase name))
            (cache-file-var
             (format-symbol :sel/sw/ts "+~A-CACHE-FILE+" upcase-name))
            (compiled-cache-file-var (format-symbol :sel/sw/ts "+~A-CACHE-FASL+" upcase-name))
            (load-canary
             (format-symbol :sel/sw/ts "+~a-LOAD-CANARY+" upcase-name))
            (name-string (string-downcase name))
            (lisp-cache
             (format nil ".cache/tree-sitter/~a/~a.lisp"
                     (uiop:implementation-identifier)
                     name-string))
            (compiled-cache
             (format nil ".cache/tree-sitter/~a/~a.~a"
                     (uiop:implementation-identifier)
                     name-string
                     (pathname-type
                      (compile-file-pathname "none")))))
  "Given the name (string) of the tree-sitter language, generate
 all the classes, methods and other artifacts that define the language."
  ;; NOTE: this comes from #'create-tree-sitter-language and is
  ;;       needed here so it is present at compile time.
  (when (find name *tree-sitter-language-files*
              :key 'car :test 'equal)
    (pushnew
     (intern (concatenate 'string "TREE-SITTER-" upcase-name)
             :keyword)
     *features*))
  `(eval-always
     ;; Generate lisp code.
     (overlord:file-target ,cache-file-var (:path ,lisp-cache :out out)
       (progn
         (with-output-to-file (s out :if-exists :supersede)
           (with-standard-io-syntax
             (let ((*package* (find-package :sel/sw/ts)))
               (format t "Expanding language definitions~%")
               (write
                (list 'progn
                      (macroexpand-1
                       '(create-tree-sitter-language ,name-string)))
                :stream s
                :readably t)
               (finish-output s)))))
       (overlord:depends-on
        (asdf:system-relative-pathname "software-evolution-library"
                                       "software/tree-sitter-code-gen.lisp")
        (pathname ,(format nil "/usr/share/tree-sitter/~a/node-types.json"
                           name-string))
        (pathname ,(format nil "/usr/share/tree-sitter/~a/grammar.json"
                           name-string))))
     ;; Generate FASL.
     (overlord:file-target ,compiled-cache-file-var
         (:path ,compiled-cache :out out)
       (overlord:depends-on ,cache-file-var)
       (format t "Compiling Language Definitions~%")
       (with-standard-io-syntax
         (let ((*package* (find-package :sel/sw/ts))
               ;; Leaving `*print-readably' at its standard T value
               ;; can cause problems with some SBCL versions.
               (*print-readably* nil))
           (handler-bind (#+sbcl (style-warning #'muffle-warning))
             (format t "Compile File: ~a~%"
                     (compile-file ,cache-file-var :output-file out))))))
     ;; Actually load in the tree-sitter code.
     (overlord:define-target-var ,load-canary
         (progn
           (format *error-output* "Loading compiled file for ~a~%" ,name-string)
           (load ,compiled-cache-file-var)
           (format *error-output* "Finished loading compiled file~%"))
       (overlord:depends-on ,compiled-cache-file-var))))

;;;
;;; This is used by the individual language files (c.lisp, python.lisp, etc.)
;;;
(defmacro create-tree-sitter-language (name)
  "Given the name (string) of the tree-sitter language, generate
 all the classes, methods and other artifacts that define the language."
  (when-let ((tree-sitter-files
               (find name
                     *tree-sitter-language-files*
                     :key 'car :test 'equal)))
    `(eval-always
      (encode-tree-sitter-version ,@tree-sitter-files)
       (progn
       ,@(apply 'tree-sitter-ast-classes
                tree-sitter-files)
       ;; add the language :TREE-SITTER-<name> to the *FEATURES* list
       (pushnew
        (intern (concatenate 'string
                             "TREE-SITTER-"
                             (string-upcase ,name)) :keyword)
        *features*)))))

(defun file-md5 (file)
  "Return the MD5 of FILE as a hex string."
  (with-output-to-string (out)
    (do-each (byte (md5:md5sum-file file) 'list)
      (format out "~(~2,'0x~)" byte))))

(defmacro encode-tree-sitter-version (name grammar-file node-types-file)
  "Warn at load time if GRAMMAR-FILE and NODE-TYPES-FILE have changed
 since the definitions were compiled."
  (let ((grammar-file-hash (file-md5 grammar-file))
        (node-types-file-hash (file-md5 node-types-file)))
    `(eval-when (:load-toplevel)
       (when (file-exists-p ,grammar-file)
         (unless (equal ,grammar-file-hash (file-md5 ,grammar-file))
           (warn "~a has changed, recompile ~a"
                 ,grammar-file
                 ',name)))
       (when (file-exists-p ,node-types-file)
         (unless (equal ,node-types-file-hash (file-md5 ,node-types-file))
           (warn "~a has changed, recompile ~a"
                 ,node-types-file
                 ',name))))))
