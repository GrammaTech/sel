(defpackage :software-evolution-library/software/c
  (:nicknames :sel/software/c :sel/sw/c)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template
        :software-evolution-library/software/c-cpp))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
;;; !! Language generated in c-cpp !!
;;;===================================================

(define-language-alias-mappings c ("c"))


#+:TREE-SITTER-C
(progn

(defmethod initialize-instance :after ((c c)
                                       &key &allow-other-keys)
  "If no compiler was specified, default to cc."
  (unless (compiler c)
    (setf (compiler c) "cc")))

(defmethod ext :around ((obj c)) (or (call-next-method) "c"))

(defmethod contextualize-ast :around (software (ast c-ast) context &rest rest
                                      &key ast-type &allow-other-keys)
  (if ast-type
      (call-next-method)
      (apply #'call-next-method software ast context :ast-type 'c-ast rest)))

(defclass c-variadic-declaration (c-parameter-declaration c-identifier)
  ((text :accessor text
         :initform "..."
         :initarg :text
         :allocation :class)
   (choice-subclasses
    :initform nil
    :reader choice-subclasses
    :allocation :class)))

(defmethod computed-text-node-p ((ast c-variadic-declaration)) t)

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-sized-type-specifier)) parse-tree &key)
  "Transform PARSE-TREE such that all modifiers are stored in the :modifiers
field."
  (with-modify-parse-tree (parse-tree)
    ((:error :comment) (ignore-types))
    (t (label-as :modifiers))))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-preproc-params)) parse-tree &key)
  (transform-c-style-variadic-parameter parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-function-definition)) parse-tree &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-declaration)) parse-tree &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-field-declaration)) parse-tree &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-parameter-declaration)) parse-tree &key)
  (transform-c-declaration-specifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-type-descriptor)) parse-tree &key)
  (transform-c-type-qualifiers parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-case-statement)) parse-tree &key)
  (transform-case-statement parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-labeled-statement)) parse-tree &key)
  (transform-labeled-statement parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-for-statement)) parse-tree &key)
  (transform-for-statement parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-compound-statement)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-translation-unit)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-preproc-if)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-preproc-ifdef)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-preproc-else)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-preproc-elif)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-declaration-list)) parse-tree &key)
  (transform-empty-statements parse-tree))

(defgeneric pointers (c-declarator)
  (:documentation "Return the number of pointers around C-DECLARATOR.")
  (:method ((ast c-parameter-declaration)) (pointers (c-declarator ast)))
  (:method ((ast c-pointer-declarator)) (1+ (pointers (c-declarator ast))))
  (:method ((ast c-identifier)) 0))

(defmethod parameter-type ((ast c-parameter-declaration))
  "Return format is (BASE-TYPE POINTER-DEPTH . QUALIFIERS)."
  (list* (source-text (c-type ast))
         (pointers ast)
         ;; This assumes that ordering doesn't matter for
         ;; _declaration_specifiers.
         (mapcar
          #'source-text
          (append (c-pre-specifiers ast) (c-post-specifiers ast)))))

(defmethod variable-name ((ast c-identifier)) (source-text ast))

(defmethod maybe-side-effect-p ((ast c-conditional-expression))
  (some #'maybe-side-effect-p (children ast)))
(defmethod maybe-side-effect-p ((ast c-binary-expression))
  (some #'maybe-side-effect-p (children ast)))
(defmethod maybe-side-effect-p ((ast c-unary-expression))
  (some #'maybe-side-effect-p (children ast)))
(defmethod maybe-side-effect-p ((ast c-comma-expression))
  (some #'maybe-side-effect-p (children ast)))
(defmethod maybe-side-effect-p ((ast c-field-expression))
  (some #'maybe-side-effect-p (children ast)))
(defmethod maybe-side-effect-p ((ast c-declaration))
  (some #'maybe-side-effect-p (c-declarator ast)))
(defmethod maybe-side-effect-p ((ast c-init-declarator))
  (maybe-side-effect-p (c-value ast)))

(defmethod expression-type ((ast c-number-literal))
  ;; There should be a global controlling the integer size model
  (match
   (string-downcase (text ast))
   ;; TODO: hex constants
   ((ppcre "^[0-9]+$")
    (make 'c-primitive-type :text "int"))
   ((ppcre "^[0-9]+u$")
    (c-type (convert 'c-ast "unsigned int a;" :deepest t)))
   ((ppcre "^[0-9]+l$")
    (c-type (convert 'c-ast "long int a;" :deepest t)))
   ((ppcre "^[0-9]+ul$")
    (c-type (convert 'c-ast "unsigned long int a;" :deepest t)))
   ;; TODO: ll constants
   ((ppcre "^[0-9]+\\.[0-9]*(|[ep][0-9]+)(|d)$" _ _)
    (make 'c-primitive-type :text "double"))
   ((ppcre "^[0-9]+\\.[0-9]*(|[ep][0-9]+)l$" _)
    (c-type (convert 'c-ast "long double a;" :deepest t)))
   ((ppcre "^[0-9]+\\.[0-9]*(|[ep][0-9]+)f$" _)
    (make 'c-primitive-type :text "float"))))

(defmethod expression-type ((ast c-string-literal))
  (make 'c-type-descriptor
        :c-type (make 'c-primitive-type :text "char")
        :c-declarator (make 'c-abstract-array-declarator)))

(defmethod expression-type ((ast c-concatenated-string))
  (make 'c-type-descriptor
        :c-type (make 'c-primitive-type :text "char")
        :c-declarator (make 'c-abstract-array-declarator)))

(defmethod expression-type ((ast c-char-literal))
  (make 'c-primitive-type :text "int"))

(defmethod expression-type ((ast c-sizeof-expression))
  (make 'c-primitive-type :text "size_t"))

(defmethod expression-type ((ast c-true))
  (make 'c-primitive-type :text "bool"))

(defmethod expression-type ((ast c-false))
  (make 'c-primitive-type :text "bool"))

(defmethod infer-type ((ast c-ast) &aux (obj (attrs-root*)))
  (let ((ancestors (get-parent-asts* obj ast))
        (prev ast))
    (iter (for a in ancestors)
          (typecase a
            ((or c-declaration c-parameter-declaration)
             (return
               (let ((d (c-declarator a)))
                 (if (typep d 'c-pointer-declarator)
                     d
                     (c-type a)))))
            (c-init-declarator
             (when (eql prev (c-value a))
               (return (call-next-method)))))
          (setf prev a)
          (finally (return (call-next-method))))))

(defun make-c-int-type ()
  (make-instance 'c-primitive-type :text "int"))

(defmethod infer-type-binary-expression ((op c-==) (ast c-binary-expression))
  (make-c-int-type))
(defmethod infer-type-binary-expression ((op c-!=) (ast c-binary-expression))
  (make-c-int-type))
(defmethod infer-type-binary-expression ((op c-<) (ast c-binary-expression))
  (make-c-int-type))
(defmethod infer-type-binary-expression ((op c-<=) (ast c-binary-expression))
  (make-c-int-type))
(defmethod infer-type-binary-expression ((op c->) (ast c-binary-expression))
  (make-c-int-type))
(defmethod infer-type-binary-expression ((op c->=) (ast c-binary-expression))
  (make-c-int-type))
(defmethod infer-type-binary-expression ((op c-&&) (ast c-binary-expression))
  (make-c-int-type))
(defmethod infer-type-binary-expression ((op c-\|\|) (ast c-binary-expression))
  (make-c-int-type))

;; TODO -- binary arithmetic, bit operators

(defmethod infer-type ((ast c-unary-expression))
  (if (typep (c-operator ast) 'c-!)
      (make-c-int-type)
      (call-next-method)))

(defun fix-nil-internal-asts-slots (ast)
  "Fix missing line endings in c preprocessor #if statements.
 If any slots named INTERNAL-ASTS-<nn> are null, set their values to a
 newline ast. This function is destructive.
 TODO: remove this hack when the problem is fixed."
  (labels ((find-in-rule (sym rule)
             (if (atom rule)
                 (eq sym rule)
                 (or (find-in-rule sym (first rule))
                     (find-in-rule sym (rest rule)))))
           (fixup-internal-asts (ast)
             (when (typep ast 'c-preproc-if)
               (do* ((count 0 (+ count 1))
                     (sym #1=(intern (format nil "C-INTERNAL-ASTS-~D" count))
                          #1#))
                    ((not (slot-exists-p ast sym)) ast)
                 (if (and (null (slot-value ast sym))
                          (find-in-rule sym (pruned-rule ast)))
                     (setf (slot-value ast sym)
                           (list (make-instance 'c-inner-whitespace
                                                :text (string #\newline)))))))))
    (mapc #'fixup-internal-asts ast)
    ast))

(defmethod to-file ((c c) file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let ((copy (copy c :genome (tree-copy (genome c)))))
      (patch-whitespace (genome copy) :recursive t)
      (fix-nil-internal-asts-slots (genome copy))
      (setf c copy)
      (call-next-method))))

(defmethod enclosing-definition ((sw c) (ast t))
  (find-enclosing '(or definition-ast c-primitive-type)
                  sw ast))

;; TODO: Convert other methods implemented for JavaScript but not C.

;; Implement the generic format-genome method for C objects.
(defmethod format-genome ((obj c) &key)
  (clang-format obj))

(defmethod equal? ((a c-identifier) (b c-identifier))
  (equal (first (text a)) (first (text b))))

(defclass c-canonical-type (c/cpp-canonical-type)
  ()
  (:documentation "C representation of canonical types."))

(defmethod canonicalize-type :around ((declaration c-ast) &rest rest
                                      &key &allow-other-keys)
  (multiple-value-call #'call-next-method
    declaration
    (values-list rest)
    :ast-type 'c-ast
    :canonical-type 'c-canonical-type))


;;; Methods common to all software objects

(defmethod get-function-from-function-call
    ((obj c) (callexpr c-ast))
  "Given a c software object and a call-expression, return the
 function definition."
  (when (typep callexpr 'c-expression-statement)
    (setf callexpr (first (children callexpr))))
  (match callexpr
    ((c-call-expression
      :c-function
      (c-identifier :text text))
     (enclosing-find-c-function obj callexpr text))))


;;;; Methods for tree-sitter generics

;;; TODO: add this for C++.
(defmethod statements-in-scope ((obj c) (scope c-for-statement) (ast c-ast))
  (iter
    (for c in (remove nil (append (children scope)
                                  (when (typep (body scope) 'c-compound-statement)
                                    (children (body scope))))))
    (while (path-later-p obj ast c))
    (collect c)))

;;; TODO: add this for C++.
(defmethod get-parent-decl ((obj c) (identifier c-ast))
  (labels ((get-parent-declarations ()
             "Return the first run of declarations in the parent ASTs of
              IDENTIFIER."
             (take-while (of-type '(or c--declarator variable-declaration-ast))
                         (drop-while (of-type 'identifier-ast)
                                     (get-parent-asts obj identifier)))))
    (or (lastcar (get-parent-declarations))
        identifier)))

(defmethod ast-to-scope-alist ((obj c) (scope c-ast) (ast c-ast))
  (let ((decl (get-parent-decl obj ast)))
    ;; NOTE: outer-declarations handles array and pointer declarations.
    `((:name . ,(source-text (or (car (outer-declarations ast))
                                 ast)))
      (:decl . ,(or decl ast))
      (:scope . ,(if (typep decl 'c-function-declarator)
                     (genome obj)
                     scope)))))

;;; TODO: add this for C++. It is likely more complicated with classes.
(defmethod child-variable-use-p
    ((obj c/cpp) (child identifier-ast) (parent c-field-expression)
     &key &allow-other-keys)
  (eq (c-argument parent) child))

;;; Special handling for tag specifiers to work around the fact that
;;; they share a superclass with actual declarations.

(defmethod field-table ((typedef c-type-definition))
  "Given a typedef for a struct defined elsewhere,
recursively resolve that struct's field table."
  (match typedef
    ((c-type-definition
      (c-type (and type (c-tag-specifier))))
     (when-let (class (get-declaration-ast :tag type))
       (field-table class)))
    (otherwise (call-next-method))))

(defun tag-specifier-outer-declarations (ast cc)
  (let ((parent (get-parent-ast (attrs-root*) ast)))
    (if (typep parent '(or compound-ast root-ast))
        ;; If the parent is a compound AST, this is a forward
        ;; declaration.
        (funcall cc)
        (values nil nil))))

(defmethod outer-declarations ((ast c-struct-tag-specifier))
  (tag-specifier-outer-declarations ast #'call-next-method))

(defmethod outer-declarations ((ast c-union-tag-specifier))
  (tag-specifier-outer-declarations ast #'call-next-method))

(defmethod outer-declarations ((ast c-enum-tag-specifier))
  (tag-specifier-outer-declarations ast #'call-next-method))

(defmethod get-declaration-ids ((ns (eql :type)) (ast c-tag-specifier))
  (get-declaration-ids :tag (c-name ast)))

(defmethod get-declaration-ids ((ns (eql :tag)) (ast c-tag-specifier))
  (get-declaration-ids :tag (c-name ast)))

(defmethod wrap-type-descriptor ((d c-pointer-declarator) type)
  (make 'c-type-descriptor
        :c-declarator (make 'c-abstract-pointer-declarator)
        :c-type type))

(defmethod wrap-type-descriptor ((d c-array-declarator) type)
  (make 'c-type-descriptor
        :c-declarator (make 'c-abstract-array-declarator
                            :c-size (c-size d))
        :c-type type))


;;; Symbol Table

(defun tag-specifier-outer-defs (node target-type)
  "Return the outer-defs for the declaration of type TARGET-TYPE which the
forward declaration NODE refers to. If the relevant declaration can not be found,
the outer-defs for NODE are returned instead."
  (mvlet* ((name (definition-name node))
           (declaration
            (or
             (find-if (lambda (ast)
                        (and (typep ast target-type)
                             (string= name (definition-name ast))))
                      (attrs-root*))
             node))
           (declarations namespaces (outer-declarations declaration)))
    (convert 'fset:map
             (convert-grouped-namespaces
              (group-by-namespace declarations namespaces)))))

(defmethod outer-defs ((node c-struct-tag-specifier))
  (tag-specifier-outer-defs node '(and c-struct-specifier
                                   (not c-struct-tag-specifier))))

(defmethod outer-defs ((node c-enum-tag-specifier))
  (tag-specifier-outer-defs node '(and c-enum-specifier
                                   (not c-enum-tag-specifier))))

(defmethod outer-defs ((node c-union-tag-specifier))
  (tag-specifier-outer-defs node '(and c-union-specifier
                                   (not c-union-tag-specifier))))

(defmethod outer-defs ((node c-type-definition))
    (let ((return-value (call-next-method))
          (type (c-type node)))
      (cond
        ((typep type '(or c-struct-tag-specifier c-enum-tag-specifier
                       c-union-tag-specifier))
         ;; TODO: find the definition and add it in.
         (symbol-table-union (attrs-root*) return-value (outer-defs type)))
        (t return-value))))


;;; C Utility

(defun c-functions (c-soft)
  "Returns the list of c functions in the C software object.
 Each returned function is a cons of the form (<function-name> . <ast>)
 where <function-name> is a string, and <ast> is a c-function-definition."
  (let ((funcs '()))
    (mapc (lambda (x)
            (if (typep x 'c-function-definition)
                (push (cons (function-name x) x) funcs)))
          c-soft)
    funcs))

(defun enclosing-find-c-function (obj start-ast function-name)
  "Find the C function with the name FUNCTION-NAME in OBJ."
  (declare (ignore start-ast))
  (cdr (find function-name (c-functions obj) :test 'equal :key 'car)))


;;; Whitespace rules

(define-empty-whitespace-methods ()
  c-ast (eql :|;|))

(defmethod whitespace-between ((style t)
                               (x c-preproc-include)
                               (y c-ast))
  (fmt "~%"))

(defmethod whitespace-between ((style t)
                               (y c-ast)
                               (x c-preproc-include))
  (whitespace-between style x y))

) ; #+:TREE-SITTER-C
