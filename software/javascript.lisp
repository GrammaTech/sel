(defpackage :software-evolution-library/software/javascript
  (:nicknames :sel/software/javascript :sel/sw/javascript)
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
(create-tree-sitter-language "javascript")
;;;===================================================

#+:TREE-SITTER-JAVASCRIPT
(progn

(defmethod transform-parse-tree
    ((language (eql ':javascript))
     (class (eql 'javascript-function-declaration))
     parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    (:async (label-as :async))))

(defmethod transform-parse-tree
    ((language (eql ':javascript))
     (class (eql 'javascript-export-statement))
     parse-tree &key)
  (with-modify-parse-tree (parse-tree)
    (:default (label-as :default))))


;;; Methods common to all software objects

(defmethod phenome ((obj javascript) &key (bin (temp-file-name)))
  (interpreted-phenome obj bin))

(defmethod function-parameters ((ast javascript-function-declaration))
  (javascript-children (javascript-parameters ast)))

(defmethod function-body ((ast javascript-function-declaration)) (javascript-body ast))

(defmethod enclosing-scope ((obj javascript) (ast javascript-ast))
  "Return the enclosing scope of AST in OBJ.
OBJ javascript software object
AST ast to return the enclosing scope for"
  (or (find-if (lambda (ast)
                 (typep ast
                        '(or
                          javascript-statement-block
                          javascript-function-declaration
                          javascript-program
                          javascript-arrow-function
                          javascript-for-statement
                          javascript-for-in-statement)))
               (get-parent-asts* obj ast))
      (genome obj)))

(defmethod inner-declarations ((ast javascript-arrow-function))
  (if-let ((parameter (javascript-parameter ast)))
    (identifiers parameter)
    (mappend #'identifiers (javascript-children (javascript-parameters ast)))))

(defmethod inner-declarations ((ast javascript-for-in-statement))
  (identifiers (javascript-left ast)))

(defmethod inner-declarations ((ast javascript-for-statement))
  (identifiers (javascript-initializer ast)))

(defmethod outer-declarations ((ast javascript-object-pattern))
  (mappend #'identifiers (javascript-children ast)))

(defmethod outer-declarations ((ast javascript-variable-declaration))
  (mappend #'outer-declarations (javascript-children ast)))

(defmethod outer-declarations ((ast javascript-array-pattern))
  (mappend #'identifiers (javascript-children ast)))

(defmethod outer-declarations ((ast javascript-rest-pattern))
  (identifiers ast))

(defmethod outer-declarations ((ast javascript-variable-declarator))
  (identifiers (javascript-name ast)))

(defmethod get-unbound-vals ((obj javascript) (ast javascript-ast) &key)
  "Return all variables used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound variables within"
  (labels ((get-unbound-vals-helper (obj parent ast)
             (remove-duplicates
              (apply #'append
                     (when (and (typep ast 'javascript-identifier)
                                (not
                                 (typep parent
                                        '(or
                                          javascript-call-expression
                                          javascript-member-expression
                                          javascript-function-declaration
                                          javascript-arrow-function
                                          javascript-class-declaration
                                          javascript-meta-property
                                          javascript-break-statement
                                          javascript-class-declaration
                                          javascript-continue-statement
                                          javascript-labeled-statement
                                          javascript-import-specifier
                                          javascript-export-statement
                                          javascript-variable-declarator))))
                       (list (cons :name (source-text ast))))
                     (mapcar {get-unbound-vals-helper obj ast}
                             (children ast)))
              :test #'equal)))
    (get-unbound-vals-helper obj (get-parent-ast obj ast) ast)))

(defmethod get-unbound-funs ((obj javascript) (ast javascript-ast)
                             &aux (children (remove nil (children ast)))
                               (callee (first children)))
  "Return all functions used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound functions within"
  (remove-duplicates
   (apply #'append
          (when (typep ast 'javascript-call-expression)
            (cond ((typep callee 'javascript-identifier)
                   ;; Free function call
                   (list (list (source-text callee)
                               nil nil (length (cdr children)))))
                  ((typep callee 'javascript-member-expression)
                   ;; Member function call
                   (list (list (nest (source-text)
                                     (second)
                                     (children callee))
                               nil nil (length (cdr children)))))
                  (t nil)))
          (mapcar {get-unbound-funs obj} children))
   :test #'equal))

(defmethod get-parent-full-stmt ((obj javascript) (ast javascript-ast))
  (find-if #'is-stmt-p (get-parent-asts obj ast)))

(defmethod get-function-from-function-call
    ((obj javascript) (callexpr javascript-ast))
  ;; NOTE: this currently only handles
  ;;       named functions declared with 'function'.
  (match callexpr
    ;; TODO: when needed, add support for member expression
    ;;       function calls.
    ((javascript-call-expression
      :javascript-function
      (javascript-identifier
       :text name))
     (enclosing-find-function obj callexpr name))))

(defmethod function-name ((node javascript-function-declaration))
  (match node
    ((javascript-function-declaration :javascript-name name)
     (source-text name))
    ((javascript-function :javascript-name name)
     (source-text name))))

(defmethod end-of-parameter-list
    ((software javascript) (function-node function-ast))
  (ematch function-node
    ((or (javascript-function-declaration :javascript-parameters params)
         (javascript-arrow-function
          :javascript-parameters (and params (type node))))
     (ast-end software params))
    ((javascript-arrow-function :javascript-parameter (and param (type node)))
     (ast-end software param))))

(defmethod parameter-type ((ast javascript-ast)) nil)

(defmethod lhs ((decl javascript-variable-declarator)) (javascript-name decl))

(defmethod rhs ((decl javascript-variable-declarator)) (javascript-value decl))


;;; Helper Functions.

(-> enclosing-find-function (javascript javascript-ast string)
  (values (or null javascript-ast) &optional))
(defun enclosing-find-function (obj start-ast function-name)
  "Find the function with the name FUNCTION-NAME in OBJ that is in
scope of START-AST."
  ;; NOTE: this currently only handles
  ;;       named functions declared with 'function'.
  (flet ((target-function (ast)
           (match ast
             ((javascript-function-declaration
               :javascript-name
               (javascript-identifier
                :text name))
              (equal name function-name)))))
    (find-if-in-scope #'target-function obj start-ast)))

;; Implement the generic format-genome method for Javascript objects.
(defmethod format-genome ((obj javascript) &key)
  (prettier obj))


;;; Whitespace rules

(define-empty-whitespace-methods ()
  javascript-ast javascript---
  javascript--- javascript-ast
  javascript-++ javascript-ast
  javascript-ast javascript-++
  javascript-ast javascript-\;
  ;; No whitespace before the arguments of an IIFE.
  javascript-parenthesized-expression javascript-arguments)

) ; #+:TREE-SITTER-JAVASCRIPT
