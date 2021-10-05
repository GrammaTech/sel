(defpackage :software-evolution-library/software/typescript
  (:nicknames :sel/software/typescript :sel/sw/typescript)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "typescript/tsx")
(create-tree-sitter-language "typescript/typescript")
;;;===================================================

(defmethod from-file ((class (eql 'typescript)) file)
  (from-file 'typescript-ts file))

(defmethod from-file ((software typescript) file)
  (from-file (change-class software 'typescript-ts) file))

(defmethod from-string ((class (eql 'typescript)) string)
  (from-string 'typescript-ts string))

(defmethod from-string ((software typescript) string)
  (from-string (change-class software 'typescript-ts) string))

#+:TREE-SITTER-TYPESCRIPT/TYPESCRIPT
(progn



)
