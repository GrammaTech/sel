(defpackage :software-evolution-library/software/go
  (:nicknames #:sel/software/golang
              #:sel/software/go #:sel/sw/go #:sel/sw/golang)
  (:use
   #:gt/full
   #:software-evolution-library
   #:software-evolution-library/software/tree-sitter-base
   #:software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language-cache "go")
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


;;; Symbol Table


(defmethod outer-declarations ((ast golang-var-declaration))
  (labels ((collect-declarations (var-spec)
             (remove-if-not (of-type 'identifier-ast) (golang-name var-spec))))
    (iter
      (for var-spec in (children ast))
      (appending (collect-declarations var-spec) into declarations)
      (finally
       (return (values declarations
                       (repeat-sequence '(:variable)
                                        (length declarations))))))))

(defmethod outer-declarations ((ast golang-short-var-declaration))
  (let ((declarations (children (golang-left ast))))
    (values declarations
            (repeat-sequence '(:variable) (length declarations)))))

(defmethod outer-declarations ((ast golang-function-declaration))
  (values (list (golang-name ast)) '(:function)))

(defmethod outer-declarations ((ast golang-method-declaration))
  (values (list (golang-name ast)) '(:method)))

(defmethod outer-declarations ((ast golang-type-declaration))
  (iter
    (for child in (children ast))
    (collect (golang-name child) into types)
    (collect :type into namespaces)
    (finally
     (return (values types namespaces)))))

;;; TODO: Only handles named imports currently. Make it better.
(defmethod outer-declarations ((ast golang-import-declaration))
  ;; TODO: if a path is provided punt on it for now.
  ;; TODO: there are go docs upstream stream. It may be worth scraping them for
  ;;       stadard library information.
  (let* ((import-specs (collect-if (of-type 'golang-import-spec)
                                   ast))
         (named-imports (remove nil (mapcar #'golang-name import-specs))))
    (values named-imports
            (repeat-sequence '(:variable) (length named-imports)))))

(defun function-inner-declarations (ast)
  (labels ((collect-declarations (parameter-declaration)
             (remove-if-not (of-type 'identifier-ast)
                            (golang-name parameter-declaration)))
           (get-parameter-declarations (ast)
             (match ast
               ((golang-function-declaration
                 (golang-parameters
                  (golang-parameter-list
                   (children parameter-declarations))))
                parameter-declarations)))
           (get-return-parameter-declarations (ast)
             (match ast
               ((golang-function-declaration
                 (golang-result
                  (golang-parameter-list
                   (children return-parameter-declarations))))
                return-parameter-declarations))))
    (let* ((declarations
             (mappend #'collect-declarations
                      (append (get-parameter-declarations ast)
                              (get-return-parameter-declarations ast))))
           (declarations-namespaces
             (repeat-sequence '(:variable)
                              (length declarations))))
      (values (cons (golang-name ast) declarations)
              (cons :function declarations-namespaces)))))

(defmethod inner-declarations ((ast golang-function-declaration))
  (labels ((get-type-parameter-declarations (type-parameter-list)
             (mappend #'golang-name (children type-parameter-list))))
    (mvlet ((declarations
             namespaces
             (function-inner-declarations ast)))
      (if-let* ((type-parameter-list (golang-type-parameters ast))
                (type-declarations
                 (get-type-parameter-declarations type-parameter-list)))
        (values (append type-declarations declarations)
                (append (repeat-sequence '(:type) (length type-declarations))
                        namespaces))
        (values declarations namespaces)))))

(defmethod inner-declarations ((ast golang-method-declaration))
  (match ast
    ((golang-method-declaration
      (golang-receiver
       (golang-parameter-list
        (children
         (list
          (golang-parameter-declaration
           (golang-name
            (list receiver))))))))
     (mvlet ((declarations namespaces (function-inner-declarations ast)))
       (values (cons receiver declarations)
               (append '(:variable :method) (cdr namespaces)))))))

(defmethod inner-declarations ((ast golang-for-statement))
  (match ast
    ((golang-for-statement
      (children
       (list (golang-for-clause
              (golang-initializer
               (and declaration (not nil)))))))
     (outer-declarations declaration))))

(defmethod inner-declarations ((ast golang-if-statement))
  (match ast
    ((golang-if-statement
      (golang-initializer
       (and declaration (not nil))))
     (outer-declarations declaration))))

;;; NOTE: an initial short var declaration can happen before the actual value
;;;       to switch on. It's scope is the switch statement and the expression
;;;       being switched on.
(defmethod inner-declarations ((ast golang-expression-switch-statement))
  (when-let ((initializer (golang-initializer ast)))
    (outer-declarations initializer)))

(defmethod inner-declarations ((ast golang-type-switch-statement))
  (mvlet ((init-declarations
           init-namespaces
           (when-let ((initializer (golang-initializer ast)))
             (outer-declarations initializer))))
    (if-let ((alias (golang-alias ast)))
      (values (cons alias init-declarations)
              (cons :variable init-namespaces))
      (values init-declarations init-namespaces))))
) ; #+:TREE-SITTER-GO
