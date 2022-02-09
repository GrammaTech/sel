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

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-modifiers)) parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    ((:public :protected :private :abstract :static :final :strictfp :default
      :synchronized :native :transient :volatile :annotation :marker-annotation)
     (label-as :modifiers))))

(defun transform-java-empty-statements (parse-tree)
  "Transform the empty statements in PARSE-TREE such that they appear as empty
statements. This is to get around unnamed semicolons being inserted at the in
ASTs that can have multiple statements in their body."
  (with-modify-parse-tree (parse-tree :modification-style :all)
    ((:|;|) (wrap-with :empty-statement))))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-block)) parse-tree &key)
  (transform-java-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-labeled-statement)) parse-tree
     &key)
  (transform-java-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-do-statement)) parse-tree &key)
  (transform-java-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-if-statement)) parse-tree &key)
  (transform-java-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-while-statement)) parse-tree &key)
  (transform-java-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-for-statement)) parse-tree &key)
  ;; TODO
  (with-modify-parse-tree (parse-tree :modification-style :slots)
    ((:|;|) (wrap-with/in-slot :empty-statement))))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-enhanced-for-statement))
     parse-tree &key)
  (transform-java-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-constructor-body)) parse-tree
     &key)
  (transform-java-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-program)) parse-tree &key)
  (transform-java-empty-statements parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':java)) (class (eql 'java-switch-block-statement-group))
     parse-tree &key)
  (transform-java-empty-statements parse-tree))
