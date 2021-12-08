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
         (typecase d
           (c/cpp-identifier d)
           ((or c/cpp-array-declarator c/cpp-pointer-declarator)
            (outer-declarations d))
           ;; Special handling for uninitialized variables.
           (t (c/cpp-declarator d)))))))

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

(defmethod field-name ((ast c/cpp-field-declaration))
  (find-if (of-type 'c/cpp-field-identifier) ast))
(defmethod field-name ((ast c/cpp-enumerator))
  (c/cpp-name ast))

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
                        :attribute-specifier :ms-declspec-modifier
                        :virtual-function-specifier
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

 ) ; #+(or :tree-sitter-c :tree-sitter-cpp)
