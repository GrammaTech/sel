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
