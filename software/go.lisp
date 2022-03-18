(defpackage :software-evolution-library/software/go
  (:nicknames #:sel/software/golang
              #:sel/software/go #:sel/sw/go #:sel/sw/golang)
  (:use
   #:gt/full
   #:software-evolution-library
   #:software-evolution-library/software/tree-sitter
   #:software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "go")
;;;===================================================

(define-language-alias-mappings golang ("go" "golang"))


#+:TREE-SITTER-GO
(progn
  
(defmethod function-name ((ast golang-function-declaration))
  (source-text (golang-name ast)))
(defmethod function-name ((ast golang-method-declaration))
  (source-text (golang-name ast)))

(defmethod function-parameters ((ast golang-function-declaration))
  (direct-children (golang-parameters ast)))
(defmethod function-parameters ((ast golang-method-declaration))
  (direct-children (golang-parameters ast)))

(defmethod call-arguments ((ast golang-call-expression))
  (direct-children (golang-arguments ast)))

(defmethod no-fallthrough ((ast golang-break-statement)) t)
(defmethod no-fallthrough ((ast golang-continue-statement)) t)
(defmethod no-fallthrough ((ast golang-goto-statement)) t)
(defmethod no-fallthrough ((ast golang-return-statement)) t)
(defmethod no-fallthrough ((ast golang-labeled-statement))
  (no-fallthrough (car (direct-children ast))))
(defmethod no-fallthrough ((ast golang-block))
  (when-let (c (direct-children ast))
    (no-fallthrough (lastcar c))))

(defmethod field-names ((ast golang-field-declaration))
  (collect-if (of-type 'golang-field-identifier) ast))
           
(defmethod enclosing-definition ((sw golang) (ast t))
  (find-enclosing '(or golang-type-spec
                    golang-function-declaration
                    golang-const-spec
                    golang-method-declaration
                    golang-short-var-spec
                    golang-type-alias
                    golang-var-spec)
                  sw ast))

(defun only-identifiers (lst)
  (remove-if-not (of-type 'golang-identifier) lst))

(defmethod definition-name-ast ((ast golang-type-declaration))
  (mappend #'definition-name-ast (children ast)))
(defmethod definition-name-ast ((ast golang-type-spec))
  (golang-name ast))
(defmethod definition-name-ast ((ast golang-type-alias))
  (list (golang-name ast)))
(defmethod definition-name-ast ((ast golang-short-var-declaration))
  (children (golang-left ast)))
(defmethod definition-name-ast ((ast golang-var-declaration))
  (definition-name-ast (first (children ast))))
(defmethod definition-name-ast ((ast golang-var-spec))
  (only-identifiers (golang-name ast)))
(defmethod definition-name-ast ((ast golang-const-declaration))
  (mappend #'definition-name-ast (children ast)))
(defmethod definition-name-ast ((ast golang-function-declaration))
  (list (golang-name ast)))
(defmethod definition-name-ast ((ast golang-labeled-statement))
  (list (golang-label ast)))

) ; #+:TREE-SITTER-GO
