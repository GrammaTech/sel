(defpackage :software-evolution-library/software/java
  (:nicknames :sel/software/java :sel/sw/java)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "java")
;;;===================================================

#+:TREE-SITTER-JAVA
(progn

(defvar *empty-statement-asts*
  '(java-block java-labeled-statement java-do-statement java-if-statement
    java-while-statement java-enhanced-for-statement java-constructor-body
    java-program java-switch-block-statement-group java-class-body)
  "A list of types which contain semicolons that are to be transformed into
empty statements.")

(defun transform-java-empty-statements (parse-tree)
  "Transform the empty statements in PARSE-TREE such that they appear as empty
statements. This is to get around unnamed semicolons being inserted at the in
ASTs that can have multiple statements in their body."
  (with-modify-parse-tree (parse-tree :modification-style :all)
    ((:|;|) (wrap-with :empty-statement))))

(defmethod transform-parse-tree :around
    ((language (eql ':java)) (class symbol) parse-tree &key)
  (if (member class *empty-statement-asts*)
      (transform-java-empty-statements parse-tree)
      (call-next-method)))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-modifiers)) parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    ((:public :protected :private :abstract :static :final :strictfp :default
      :synchronized :native :transient :volatile :annotation :marker-annotation)
     (label-as :modifiers))))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-for-statement)) parse-tree &key)
  (with-modify-parse-tree (parse-tree :modification-style :slots)
    ((:|;|) (wrap-with/in-slot :empty-statement))))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-enum-body-declarations))
     parse-tree &key
     &aux semicolon-flag)
  (labels ((empty-statement-p (tree)
             (when (eql (parse-tree-type tree) :|;|)
               (prog1 semicolon-flag
                 (setf semicolon-flag t))))
           (wrap-after-first (tree)
             (when (empty-statement-p tree)
               `(:empty-statement ,(cadr tree) (,tree)))))
    (modify-parse-tree parse-tree #'wrap-after-first)))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-module-declaration)) parse-tree
     &key)
  (with-modify-parse-tree (parse-tree)
    ((:open) (label-as :open))))


;;; Methods for tree-sitter generics.

(defmethod declaration-type ((ast java-local-variable-declaration))
  (java-type ast))

(defmethod placeholder-type-p ((ast java-type-identifier))
  (equal (text ast) "var"))

) ; #+:TREE-SITTER-JAVA
