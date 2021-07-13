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
  (source-text (c/cpp-declarator (c/cpp-declarator ast))))

(defmethod function-parameters ((ast c/cpp-function-definition))
  (children (c/cpp-parameters (c/cpp-declarator ast))))

(defmethod call-arguments ((node c/cpp-call-expression))
  (children (c/cpp-arguments node)))

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
  (c/cpp-name field-ast))

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
                        :attribute-specifier :ms-declspec-modifier))
          (cons (list position-slot (car child-tree))
                (cdr child-tree)))
         (t child-tree)))
     (lastcar parse-tree)))))

 ) ; #+(or :tree-sitter-c :tree-sitter-cpp)
