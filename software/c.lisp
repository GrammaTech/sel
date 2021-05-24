(defpackage :software-evolution-library/software/c
  (:nicknames :sel/software/c :sel/sw/c)
  (:use :gt/full
   :cl-json
   :software-evolution-library
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/c-cpp))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
;;; !! Language generated in c-cpp !!
;;;===================================================

(defmethod initialize-instance :after ((c c)
                                       &key &allow-other-keys)
  "If no compiler was specified, default to cc."
  (unless (compiler c)
    (setf (compiler c) "cc")))

(defmethod ext :around ((obj c)) (or (call-next-method) "c"))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-sized-type-specifier)) parse-tree)
  "Transform PARSE-TREE such that all modifiers are stored in the :modifiers
field."
  (append
   (butlast parse-tree)
   (list
    (mapcar
     (lambda (child-tree &aux (node-type (car child-tree)))
       (cond
         ((consp node-type) child-tree)
         ((member node-type '(:error :comment)) child-tree)
         (t (cons (list :modifiers node-type) (cdr child-tree)))))
     (lastcar parse-tree)))))

(defmethod transform-parse-tree
    ((language (eql ':c)) (class (eql 'c-preproc-ifdef)) parse-tree)
  "Transform PARSE-TREE such that all modifiers are stored in the :modifiers
field."
  (append
   (butlast parse-tree)
   (list
    (mapcar
     (lambda (child-tree)
       (cond
         ((member (car child-tree) '(:|#IFDEF| :|#IFNDEF|))
          (cons (list :operation (car child-tree)) (cdr child-tree)))
         (t child-tree)))
     (lastcar parse-tree)))))

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
         (mapcar #'source-text (slot-value ast 'children))))

(defmethod parameter-name ((ast c-parameter-declaration))
  (parameter-name (c-declarator ast)))
(defmethod parameter-name ((ast c-pointer-declarator))
  (parameter-name (c-declarator ast)))
(defmethod parameter-name ((ast c-identifier)) (source-text ast))

(defmethod variable-name ((ast c-identifier)) (source-text ast))

(defmethod no-fallthrough ((ast c-continue-statement)) t)
(defmethod no-fallthrough ((ast c-break-statement)) t)

(defmethod inner-declarations ((ast c-for-statement))
  (c-left (c-initializer ast)))

(defmethod type-in ((c c) (ast c-ast))
  (when-let ((decl (find-if «or {typep _ 'c-declaration}
                             {typep _ 'c-parameter-declaration}»
                             (get-parent-asts c ast))))
    (if (typep (c-declarator decl) 'c-pointer-declarator)
        :pointer
        (make-keyword (string-upcase (source-text (c-type decl)))))))

(defmethod enclosing-definition ((sw c) (ast t))
  (find-enclosing '(or definition-ast c-primitive-type)
                  sw ast))

(defmethod definition-name ((ast c-function-definition))
  (declarator-name (c-declarator ast)))
(defmethod definition-name ((ast c-struct-specifier))
  (source-text (c-name ast)))
(defmethod definition-name ((ast c-union-specifier))
  (source-text (c-name ast)))
(defmethod definition-name ((ast c-type-definition))
  (declarator-name (c-declarator ast)))
(defmethod definition-name ((ast c-preproc-def))
  (source-text (c-name ast)))
(defmethod definition-name ((ast c-preproc-function-def))
  (source-text (c-name ast)))

(defmethod declarator-name ((ast c-identifier))
  (source-text ast))
(defmethod declarator-name ((ast c-type-identifier))
  (source-text ast))
(defmethod declarator-name ((ast c-parenthesized-declarator))
  (source-text (car (children ast))))
(defmethod declarator-name ((ast c-pointer-declarator))
  (declarator-name (c-declarator ast)))
(defmethod declarator-name ((ast c-array-declarator))
  (declarator-name (c-declarator ast)))
(defmethod declarator-name ((ast c-function-declarator))
  (declarator-name (c-declarator ast)))

;; TODO: Convert other methods implemented for JavaScript but not C.

;; Implement the generic format-genome method for C objects.
(defmethod format-genome ((obj c) &key)
  (clang-format obj))

(defmethod equal? ((a c-identifier) (b c-identifier))
  (equal (first (text a)) (first (text b))))

(defmethod get-function-from-function-call
    ((obj c) (callexpr c-ast))
  "Given a c software object and a call-expression, return the
 function definition."
  (when (typep callexpr 'c-expression-statement)
    (setf callexpr (first (children callexpr))))
  (match callexpr
    ((c-call-expression
      :c-function
      (c-identifier text))
     (enclosing-find-c-function obj callexpr (first text)))))

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
