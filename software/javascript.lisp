;;; javascript.lisp --- Javascript software representation.
;;;
;;; Implements the core functionality of the software-evolution-library
;;; for files written in JavaScript.  The core functionality is supported
;;; by the command line tool
;;; @uref{https://github.com/acornjs/acorn.git, acorn}, which parses
;;; the code into ASTs.
;;;
;;; Additionally, the @url{https://github.com/prettier/prettier,
;;; prettier} command line JavaScript code formatter is required to
;;; support code formatting (see @pxref{Generic-Function format-genome}).
;;;
;;; As with @code{clang} software objects, @code{javascript} software
;;; objects are a parsed, AST representation.  All of the generic
;;; methods for @code{parseable} software objects have been implemented,
;;; allowing for a rich variety of manipulations and tests.
;;;
;;; Every AST in a @code{javascript} software object contains an AST class
;;; field with the AST type (e.g. BlockStatement, ForStatement).  Additional
;;; fields given by the @code{acorn} parser can be found in an association
;;; list on the AST's aux-data field.  For more information on the ASTs,
;;; see @uref{https://media.readthedocs.org/pdf/esprima/4.0/esprima.pdf,
;;; esprima}.
;;;
;;; Note: certain methods in this class are over-loaded to provide
;;; consistency with other software objects, but are not used in this
;;; implementation.
;;;
;;; @texi{javascript}
(defpackage :software-evolution-library/software/javascript
  (:nicknames :sel/software/javascript :sel/sw/javascript)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/components/formatting)
  (:shadowing-import-from :cl-json :decode-json-from-string)
  (:export :javascript
           :javascript-mutation
           :javascript-ast
           :javascript-ast-node
           :make-javascript-ast
           :copy-javascript-ast
           :make-javascript-ast-node
           :copy-javascript-ast-node))
(in-package :software-evolution-library/software/javascript)
(in-readtable :curry-compose-reader-macros)

(define-software javascript (parseable) ()
  (:documentation "Javascript software representation."))


;;; Javascript object creation
(defmethod from-file ((obj javascript) path)
  "Populate OBJ with the Javascript source at PATH.
* OBJ to be populated from the Javascript source at PATH
* PATH to source code to populate OBJ with"
  (setf (ext obj) (pathname-type (pathname path)))
  (from-string obj (file-to-string path))
  obj)

(defmethod from-string ((obj javascript) string)
  "Populate OBJ with the Javascript source code in STRING.
* OBJ to be ppopulated from Javascript source in STRING
* STRING source code to populate OBJ with"
  (setf (genome obj) string)
  obj)


;;; Javascript ast data structures
(define-ast (javascript-ast (:conc-name ast)))

(defmethod print-object ((obj javascript-ast-node) stream)
  "Print a representation of the javascript-ast-node OBJ to STREAM.
* OBJ clang-ast to print
* STREAM stream to print OBJ to"
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a" (ast-class obj)))))


;;; Javascript parsing
(defmethod parse-asts ((obj javascript))
  "Parse the genome of OBJ, return a list of ASTs.
* OBJ object to parse"
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (multiple-value-bind (stdout stderr exit)
        (shell "acorn --allow-hash-bang ~a" src-file)
        (if (zerop exit)
            (decode-json-from-string stdout)
            (error
              (make-instance 'mutate
                :text (format nil "acorn exit ~d~%stderr:~s"
                              exit
                              stderr)
                :obj obj :op :parse))))))

(define-constant +js-bound-id-parent-classes+
  (list "FunctionDeclaration"
        "FunctionExpression"
        "ArrowFunctionExpression"
        "ClassExpression"
        "MetaProperty"
        "BreakStatement"
        "ClassDeclaration"
        "ContinueStatement"
        "LabelledStatement"
        "ImportSpecifier"
        "ExportSpecifier"
        "ExportDefaultDeclaration"
        "VariableDeclarator")
  :test #'equalp
  :documentation
  "Parent AST classes of identifiers which should be interpreted as bound.")

(defmethod update-asts ((obj javascript))
  "Update the ast-root associated with OBJ.
* OBJ obj to update"
  (labels
      ((add-aux-data (ast alist)
         (copy ast :aux-data
                   (remove-if (lambda (pair)
                                (or (member (car pair)
                                            '(:type :start :end))
                                    (listp (cdr pair))))
                              alist)))
       (collect-children (ast-alist &aux (ast-class (aget :type ast-alist)))
         (remove-if #'null
                    (cond ((equal ast-class "MemberExpression")
                           (list (aget :object ast-alist)
                                 (aget :property ast-alist)))
                          ((equal ast-class "TaggedTemplateExpression")
                           (list (aget :readonly-tag ast-alist)
                                 (aget :readonly-quasi ast-alist)))
                          ((equal ast-class "TemplateLiteral")
                           (append (aget :quasis ast-alist)
                                   (aget :expressions ast-alist)))
                          ((equal ast-class "MetaProperty")
                           (list (aget :meta ast-alist)
                                 (aget :property ast-alist)))
                          ((equal ast-class "SequenceExpression")
                           (aget :expressions ast-alist))
                          ((equal ast-class "DoWhileStatement")
                           (list (aget :body ast-alist)
                                 (aget :test ast-alist)))
                          ((equal ast-class "WhileStatement")
                           (list (aget :test ast-alist)
                                 (aget :body ast-alist)))
                          ((equal ast-class "ExpressionStatement")
                           (list (aget :expression ast-alist)))
                          ((equal ast-class "ForStatement")
                           (list (aget :init ast-alist)
                                 (aget :test ast-alist)
                                 (aget :update ast-alist)
                                 (aget :body ast-alist)))
                          ((equal ast-class "LabeledStatement")
                           (list (aget :label ast-alist)
                                 (aget :body ast-alist)))
                          ((equal ast-class "SwitchStatement")
                           (append (list (aget :discriminant ast-alist))
                                   (aget :cases ast-alist)))
                          ((equal ast-class "SwitchCase")
                           (append (list (aget :test ast-alist))
                                   (aget :consequent ast-alist)))
                          ((equal ast-class "TryStatement")
                           (list (aget :block ast-alist)
                                 (aget :handler ast-alist)
                                 (aget :finalizer ast-alist)))
                          ((equal ast-class "CatchClause")
                           (list (aget :param ast-alist)
                                 (aget :body ast-alist)))
                          ((equal ast-class "VariableDeclaration")
                           (aget :declarations ast-alist))
                          ((equal ast-class "VariableDeclarator")
                           (list (aget :id ast-alist)
                                 (aget :init ast-alist)))
                          ((equal ast-class "WithStatement")
                           (list (aget :object ast-alist)
                                 (aget :body ast-alist)))
                          ((equal ast-class "Program")
                           (aget :body ast-alist))
                          ((equal ast-class "ImportDeclaration")
                           (append (aget :specifiers ast-alist)
                                   (list (aget :source ast-alist))))
                          ((equal ast-class "ExportAllDeclaration")
                           (list (aget :source ast-alist)))
                          ((equal ast-class "ExportDefaultDeclaration")
                           (list (aget :declaration ast-alist)))
                          ((equal ast-class "ExportNamedDeclaration")
                           (append (list (aget :declaration ast-alist))
                                   (aget :specifiers ast-alist)
                                   (list (aget :source ast-alist))))
                          ((equal ast-class "ExportSpecifier")
                           (list (aget :exported ast-alist)
                                 (aget :local ast-alist)))
                          ((member ast-class
                                   (list "ClassExpression"
                                         "ClassDeclaration")
                                   :test #'equal)
                           (list (aget :id ast-alist)
                                 (aget :superclass ast-alist)
                                 (aget :body ast-alist)))
                          ((member ast-class
                                   (list "Property"
                                         "MethodDefinition")
                                   :test #'equal)
                           (list (aget :key ast-alist)
                                 (aget :value ast-alist)))
                          ((member ast-class
                                   (list "ForInStatement"
                                         "ForOfStatement")
                                   :test #'equal)
                           (list (aget :left ast-alist)
                                 (aget :right ast-alist)
                                 (aget :body ast-alist)))
                          ((member ast-class
                                   (list "ArrayPattern"
                                         "ArrayExpression")
                                   :test #'equal)
                           (aget :elements ast-alist))
                          ((member ast-class
                                   (list "ObjectPattern"
                                         "ObjectExpression")
                                   :test #'equal)
                           (aget :properties ast-alist))
                          ((member ast-class
                                   (list "RestElement"
                                         "SpreadElement"
                                         "UpdateExpression"
                                         "AwaitExpression"
                                         "UnaryExpression"
                                         "YieldExpression"
                                         "ReturnStatement"
                                         "ThrowStatement")
                                   :test #'equal)
                           (list (aget :argument ast-alist)))
                          ((member ast-class
                                   (list "AssignmentPattern"
                                         "BinaryExpression"
                                         "LogicalExpression"
                                         "AssignmentExpression")
                                   :test #'equal)
                           (list (aget :left ast-alist)
                                 (aget :right ast-alist)))
                          ((member ast-class
                                   (list "FunctionExpression"
                                         "ArrowFunctionExpression"
                                         "FunctionDeclaration")
                                   :test #'equal)
                           (append (aget :params ast-alist)
                                   (list (aget :body ast-alist))))
                          ((member ast-class
                                   (list "ClassBody"
                                         "BlockStatement")
                                   :test #'equal)
                           (aget :body ast-alist))
                          ((member ast-class
                                   (list "CallExpression"
                                         "NewExpression")
                                   :test #'equal)
                           (append (list (aget :callee ast-alist))
                                   (aget :arguments ast-alist)))
                          ((member ast-class
                                   (list "BreakStatement"
                                         "ContinueStatement")
                                   :test #'equal)
                           (list (aget :label ast-alist)))
                          ((member ast-class
                                   (list "ConditionalExpression"
                                         "IfStatement")
                                   :test #'equal)
                           (list (aget :test ast-alist)
                                 (aget :consequent ast-alist)
                                 (aget :alternate ast-alist)))
                          ((member ast-class
                                   (list "ImportSpecifier"
                                         "ImportDefaultSpecifier"
                                         "ImportNamespaceSpecifier")
                                   :test #'equal)
                           (list (aget :local ast-alist)
                                 (aget :imported ast-alist)))
                          (t nil))))
       (make-children (genome parent-alist alist child-alists child-asts)
         (if child-alists
             (iter (with start = (aget :start alist))
                   (for child-alist in child-alists)
                   (for child-ast in child-asts)
                   (collect (subseq genome start (aget :start child-alist))
                            into result)
                   (collect child-ast into result)
                   (setf start (aget :end child-alist))
                   (finally
                     (return (append result
                                     (list (subseq genome
                                                   start
                                                   (aget :end alist)))))))
             (let ((text (subseq genome (aget :start alist) (aget :end alist))))
               (if (and (equal "Identifier" (aget :type alist))
                        (not (member
                               (aget :type parent-alist)
                               +js-bound-id-parent-classes+
                               :test #'equal)))
                   (list (unpeel-bananas text))
                   (list text)))))
       (make-tree (genome parent-ast-alist ast-alist
                   &aux (children (remove-if #'null
                                             (collect-children ast-alist)))
                        (new-ast-node (-> (make-javascript-ast-node
                                            :class (make-keyword
                                                     (string-upcase
                                                       (aget :type ast-alist))))
                                           (add-aux-data ast-alist))))
         (make-javascript-ast
           :node new-ast-node
           :children (make-children
                       genome
                       parent-ast-alist
                       ast-alist
                       children
                       (iter (for child in children)
                             (collect (make-tree genome
                                                 ast-alist
                                                 child)))))))
    (setf (ast-root obj)
          (make-tree (genome obj) nil (parse-asts obj)))
    (setf (slot-value obj 'genome)
          nil)
    obj))


;;; Javascript mutation
(define-mutation javascript-mutation (parseable-mutation)
  ()
  (:documentation
   "Specialization of the mutation interface for JavaScript software objects."))

(defmethod fixup-mutation (operation (current javascript-ast)
                           before ast after)
  (cond ((eq operation :remove)
         (list before after))
        (t (list before ast after))))


;;; Methods common to all software objects
(defmethod phenome ((obj javascript) &key (bin (temp-file-name)))
  "Create a phenotype of the javascript software OBJ.  In this case, override
the phenome method to output the genome of OBJ to BIN as JavaScript
is not a compiled language.
OBJ object to create a phenome for
BIN location where the phenome will be created on the filesystem"
  (to-file obj bin)
  (values bin 0 nil nil nil))

(defmethod rebind-vars ((ast javascript-ast)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (copy ast :children (mapcar {rebind-vars _ var-replacements fun-replacements}
                              (ast-children ast))))

(defmethod enclosing-scope ((obj javascript) (ast javascript-ast))
  "Return the enclosing scope of AST in OBJ.
OBJ javascript software object
AST ast to return the enclosing scope for"
  (or (find-if (lambda (ast)
                 (member (ast-class ast)
                         (list :BlockStatement
                               :FunctionDeclaration
                               :FunctionExpression
                               :ArrowFunctionExpression
                               :ForStatement
                               :ForInStatement
                               :ForOfStatement)))
               (cdr (get-parent-asts obj ast)))
      (ast-root obj)))

(defmethod scopes ((obj javascript) (ast javascript-ast))
  "Return lists of variables in each enclosing scope of AST.
Each variable is represented by an alist containing :NAME, :DECL, and :SCOPE.
OBJ javascript software object
AST ast to return the scopes for"
  (labels ((filter-identifiers (obj scope children)
             (cond ((and (member (ast-class scope)
                                 (list :ForInStatement
                                       :ForOfStatement))
                         (eq (ast-class (first children)) :Identifier))
                    ;; for (obj in objs) => '(obj)
                    (list (first children)))
                   ((and (eq (ast-class scope) :ForStatement)
                         (eq (ast-class (first children)) :VariableDeclaration))
                    ;; for (i = 0; i < 10; i++) {...} => '(i)
                    (->> (get-immediate-children obj (first children))
                         (mapcar [#'first {get-immediate-children obj}])
                         (remove-if-not [{eq :Identifier} #'ast-class])))
                   ((member (ast-class scope)
                            (list :FunctionDeclaration
                                  :FunctionExpression
                                  :ArrowFunctionExpression))
                    ;; function foo(bar, ...baz) {...} => '(bar baz)
                    (mappend
                      (lambda (ast)
                        (cond ((eq (ast-class ast) :Identifier)
                               (list ast))
                              ((eq (ast-class ast) :RestElement)
                               (list (first (get-immediate-children obj ast))))
                              (t nil)))
                      children))
                   (t
                    ;; The scope is a block, return the variable declarations
                    (->> (remove-if-not [{eq :VariableDeclaration} #'ast-class]
                                        children)
                         (mappend {get-immediate-children obj})
                         (mapcar [#'first {get-immediate-children obj}])
                         (remove-if-not [{eq :Identifier} #'ast-class]))))))
    (when (not (eq :Program (ast-class ast)))
      (let ((scope (enclosing-scope obj ast)))
        (cons (->> (iter (for c in
                              (get-immediate-children obj scope))
                         (while (ast-later-p ast c))
                         (collect c))
                   ; remove ASTs which are not variable identifiers
                   (filter-identifiers obj scope)
                   ; build result
                   (mappend
                     (lambda (ast)
                       `(((:name . ,(peel-bananas (source-text ast)))
                          (:decl . ,ast)
                          (:scope . ,scope)))))
                   (reverse))
              (scopes obj scope))))))

(defmethod get-unbound-vals ((obj javascript) (ast javascript-ast))
  "Return all variables used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound variables within"
  (labels ((get-unbound-vals-helper (obj parent ast)
             (remove-duplicates
               (apply #'append
                      (when (and (eq (ast-class ast) :Identifier)
                                 (not (member (ast-class parent)
                                              (append
                                                +js-bound-id-parent-classes+
                                                (list :CallExpression
                                                      :MemberExpression)))))
                        (list (cons :name (peel-bananas (source-text ast)))))
                      (mapcar {get-unbound-vals-helper obj ast}
                              (get-immediate-children obj ast)))
               :test #'equal)))
    (get-unbound-vals-helper obj (get-parent-ast obj ast) ast)))

(defmethod get-unbound-funs ((obj javascript) (ast javascript-ast)
                             &aux (children (get-immediate-children obj ast))
                                  (callee (first children)))
  "Return all functions used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound functions within"
  (remove-duplicates
    (apply #'append
           (when (eq (ast-class ast) :CallExpression)
             (cond ((eq (ast-class callee) :Identifier)
                    ;; Free function call
                    (list (-> (source-text callee)
                              (peel-bananas)
                              (list nil nil (length (cdr children))))))
                   ((eq (ast-class callee) :MemberExpression)
                    ;; Member function call
                    (list (-> (get-immediate-children obj callee)
                              (second)
                              (source-text)
                              (peel-bananas)
                              (list nil nil (length (cdr children))))))
                   (t nil)))
           (mapcar {get-unbound-funs obj}
                   (get-immediate-children obj ast)))
    :test #'equal))

(defmethod traceable-stmt-p ((obj javascript) (ast javascript-ast))
  "Return TRUE if AST is a traceable statement in the javascript software OBJ."
  (and (get-parent-ast obj ast)
       (eq :BlockStatement (ast-class (get-parent-ast obj ast)))))


;;; Implement the generic format-genome method for Javascript objects.
(defmethod format-genome ((obj javascript) &key)
  (prettier obj))


;;; Support for parsing a string directly into free-floating ASTs.
(defmethod parse-source-snippet ((type (eql :javascript))
                                 (snippet string)
                                 &key)
  "Parse the SNIPPET into a list of free-floating JavaScript ASTs."
  (handler-case
      (remove-if [{< 1} {length} {ast-path}]
                 (asts (from-string (make-instance 'javascript) snippet)))
    (mutate (e) (declare (ignorable e)) nil)))
