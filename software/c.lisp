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

#+:TREE-SITTER-C
(progn

(defmethod initialize-instance :after ((c c)
                                       &key &allow-other-keys)
  "If no compiler was specified, default to cc."
  (unless (compiler c)
    (setf (compiler c) "cc")))

(defmethod ext :around ((obj c)) (or (call-next-method) "c"))

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

(defmethod parameter-name ((ast c-parameter-declaration))
  (parameter-name (c-declarator ast)))
(defmethod parameter-name ((ast c-pointer-declarator))
  (parameter-name (c-declarator ast)))
(defmethod parameter-name ((ast c-identifier)) (source-text ast))

(defmethod variable-name ((ast c-identifier)) (source-text ast))

(defmethod no-fallthrough ((ast c-continue-statement)) t)
(defmethod no-fallthrough ((ast c-break-statement)) t)

(defmethod type-in ((c c) (ast c-ast))
  (when-let ((decl (find-if «or {typep _ 'c-declaration}
                             {typep _ 'c-parameter-declaration}»
                             (get-parent-asts c ast))))
    (if (typep (c-declarator decl) 'c-pointer-declarator)
        :pointer
        (make-keyword (string-upcase (source-text (c-type decl)))))))

(defun fix-nil-internal-asts-slots (ast)
  "Fix missing line endings in c preprocessor #if statements.
 If any slots named INTERNAL-ASTS-<nn> are null, set their values to a
 newline ast. This function is destructive.
 TODO: remove this hack when the problem is fixed."
  (labels ((fixup-internal-asts (ast)
             (when (typep ast 'sel/sw/ts::c-preproc-if)
               (do* ((count 0 (+ count 1))
                     (sym #1=(intern (format nil "C-INTERNAL-ASTS-~D" count)
                                     (find-package :sel/sw/ts)) #1#))
                    ((not (slot-exists-p ast sym)) ast)
                 (if (null (slot-value ast sym))
                     (setf (slot-value ast sym)
                           (list (make-instance 'sel/sw/ts::c-inner-whitespace
                                                :text (string #\newline)))))))))
    (mapcar #'fixup-internal-asts ast)
    ast))

(defmethod to-file ((c c) file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let ((copy (copy c)))
      (setf (genome copy)
            (fix-nil-internal-asts-slots (patch-whitespace (genome copy))))
      (setf c copy)
      (call-next-method))))

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
    (iter:with body = (car (direct-children scope)))
    (for c in (remove nil (append (children scope)
                                  (when (typep body 'c-compound-statement)
                                    (children body)))))
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

;;; TODO: add this for C++.
(defmethod outer-declarations ((ast c-function-declarator))
  ;; Special handling for uninitialized variables.
  (list (c-declarator ast)))

;;; TODO: add this for C++. It is likely more complicated with classes.
(defmethod child-variable-use-p
    ((obj c/cpp) (child identifier-ast) (parent c-field-expression)
     &key &allow-other-keys)
  (eq (c-argument parent) child))


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
