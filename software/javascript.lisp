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
;;; list on the AST's annotations field.  For more information on the ASTs,
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
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/parseable
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting)
  (:import-from :jsown)
  (:import-from :cffi :translate-camelcase-name)
  (:export :javascript
           :javascript-mutation
           :javascript-ast
           :acorn))
(in-package :software-evolution-library/software/javascript)
(in-readtable :curry-compose-reader-macros)

(define-software javascript (parseable file-w-attributes) ()
  (:documentation "Javascript software representation."))


;;; Javascript ast data structures
(defclass javascript-ast (functional-tree-ast)
  ((children :type list
             :initarg :children
             :initform nil
             :documentation "The list of children of the node,
which may be more nodes, or other values.")
   (child-slots :initform '(children) :allocation :class))
  (:documentation "Class of JavaScript ASTs."))


;;; Javascript parsing
(defgeneric acorn (obj)
  (:documentation "Invoke the acorn parser on the genome of OBJ returning a
raw list of ASTs in OBJ for use in `parse-asts`."))

(defmethod acorn ((obj javascript))
  (labels ((invoke-acorn (parsing-mode)
             "Invoke acorn with the given PARSING-MODE (:script or :module)."
             (with-temporary-file-of (:pathname src-file :type (ext obj))
               (genome-string obj)
               (multiple-value-bind (stdout stderr exit)
                   (shell "acorn --compact --allow-hash-bang ~a ~a"
                          (if (eq parsing-mode :module)
                              "--module"
                              "")
                          src-file)
                 (if (zerop exit)
                     (values stdout stderr exit)
                     (values nil stderr exit))))))
    (multiple-value-bind (stdout stderr exit)
        (multiple-value-or (invoke-acorn :module) (invoke-acorn :script))
      (if (zerop exit)
          (convert-acorn-jsown-tree (jsown:parse stdout))
          (error
            (make-instance 'mutate
              :text (format nil "acorn exit ~d~%stderr:~s"
                            exit
                            stderr)
              :obj obj :operation :parse))))))

(defun convert-acorn-jsown-tree (jt)
  (convert-jsown-tree jt #'jsown-str-to-acorn-keyword))

(defun jsown-str-to-acorn-keyword (str)
  (string-case-to-keywords ("delegate" "update" "await" "properties"
                                       "method" "shorthand" "key" "elements"
                                       "object" "property" "computed"
                                       "generator" "async" "params"
                                       "declarations" "id" "init " "kind"
                                       "test" "body" "js-left" "right"
                                       "operator" "prefix" "argument"
                                       "expression" "callee " "name" "arguments"
                                       "value" "raw" "sourcetype"
                                       "type" "start" "end")
                           str))

(defmethod parse-asts ((obj javascript) &optional (source (genome-string obj)))
  (labels
      ((annotations (alist)
         (remove-if (lambda (pair)
                      (or (member (car pair)
                                  '(:type :start :end))
                          (listp (cdr pair))))
                    alist))
       (collect-children (ast-alist &aux (ast-class (nest (make-keyword)
                                                          (string-upcase)
                                                          (aget :type ast-alist))))
         (remove-if #'null
                    (cond ((eq ast-class :MemberExpression)
                           (list (aget :object ast-alist)
                                 (aget :property ast-alist)))
                          ((eq ast-class :TaggedTemplateExpression)
                           (list (aget :tag ast-alist)
                                 (aget :quasi ast-alist)))
                          ((eq ast-class :TemplateLiteral)
                           (sort (append (aget :quasis ast-alist)
                                         (aget :expressions ast-alist))
                                 #'< :key {aget :end}))
                          ((eq ast-class :MetaProperty)
                           (list (aget :meta ast-alist)
                                 (aget :property ast-alist)))
                          ((eq ast-class :Property)
                           (if (< (aget :start (aget :value ast-alist))
                                  (aget :end (aget :key ast-alist)))
                               (list (aget :value ast-alist))
                               (list (aget :key ast-alist)
                                     (aget :value ast-alist))))
                          ((eq ast-class :MethodDefinition)
                           (list (aget :key ast-alist)
                                 (aget :value ast-alist)))
                          ((eq ast-class :SequenceExpression)
                           (aget :expressions ast-alist))
                          ((eq ast-class :DoWhileStatement)
                           (list (aget :body ast-alist)
                                 (aget :test ast-alist)))
                          ((eq ast-class :WhileStatement)
                           (list (aget :test ast-alist)
                                 (aget :body ast-alist)))
                          ((eq ast-class :ExpressionStatement)
                           (list (aget :expression ast-alist)))
                          ((eq ast-class :ForStatement)
                           (list (aget :init ast-alist)
                                 (aget :test ast-alist)
                                 (aget :update ast-alist)
                                 (aget :body ast-alist)))
                          ((eq ast-class :LabeledStatement)
                           (list (aget :label ast-alist)
                                 (aget :body ast-alist)))
                          ((eq ast-class :SwitchStatement)
                           (append (list (aget :discriminant ast-alist))
                                   (aget :cases ast-alist)))
                          ((eq ast-class :SwitchCase)
                           (append (list (aget :test ast-alist))
                                   (aget :consequent ast-alist)))
                          ((eq ast-class :TryStatement)
                           (list (aget :block ast-alist)
                                 (aget :handler ast-alist)
                                 (aget :finalizer ast-alist)))
                          ((eq ast-class :CatchClause)
                           (list (aget :param ast-alist)
                                 (aget :body ast-alist)))
                          ((eq ast-class :VariableDeclaration)
                           (aget :declarations ast-alist))
                          ((eq ast-class :VariableDeclarator)
                           (list (aget :id ast-alist)
                                 (aget :init ast-alist)))
                          ((eq ast-class :WithStatement)
                           (list (aget :object ast-alist)
                                 (aget :body ast-alist)))
                          ((eq ast-class :Program)
                           (aget :body ast-alist))
                          ((eq ast-class :ImportDeclaration)
                           (append (aget :specifiers ast-alist)
                                   (list (aget :source ast-alist))))
                          ((eq ast-class :ExportAllDeclaration)
                           (list (aget :source ast-alist)))
                          ((eq ast-class :ExportDefaultDeclaration)
                           (list (aget :declaration ast-alist)))
                          ((eq ast-class :ExportNamedDeclaration)
                           (append (list (aget :declaration ast-alist))
                                   (aget :specifiers ast-alist)
                                   (list (aget :source ast-alist))))
                          ((eq ast-class :ExportSpecifier)
                           (remove-duplicates
                            (list (aget :local ast-alist)
                                  (aget :exported ast-alist))
                            :test #'equal))
                          ((member ast-class
                                   (list :ClassExpression
                                         :ClassDeclaration))
                           (list (aget :id ast-alist)
                                 (aget :superclass ast-alist)
                                 (aget :body ast-alist)))
                          ((member ast-class
                                   (list :ForInStatement
                                         :ForOfStatement))
                           (list (aget :left ast-alist)
                                 (aget :right ast-alist)
                                 (aget :body ast-alist)))
                          ((member ast-class
                                   (list :ArrayPattern
                                         :ArrayExpression))
                           (aget :elements ast-alist))
                          ((member ast-class
                                   (list :ObjectPattern
                                         :ObjectExpression))
                           (aget :properties ast-alist))
                          ((member ast-class
                                   (list :RestElement
                                         :SpreadElement
                                         :UpdateExpression
                                         :AwaitExpression
                                         :UnaryExpression
                                         :YieldExpression
                                         :ReturnStatement
                                         :ThrowStatement))
                           (list (aget :argument ast-alist)))
                          ((member ast-class
                                   (list :AssignmentPattern
                                         :BinaryExpression
                                         :LogicalExpression
                                         :AssignmentExpression))
                           (list (aget :left ast-alist)
                                 (aget :right ast-alist)))
                          ((member ast-class
                                   (list :FunctionExpression
                                         :ArrowFunctionExpression
                                         :FunctionDeclaration)
                                   :test #'equal)
                           (append (list (aget :id ast-alist))
                                   (aget :params ast-alist)
                                   (list (aget :body ast-alist))))
                          ((member ast-class
                                   (list :ClassBody
                                         :BlockStatement)
                                   :test #'equal)
                           (aget :body ast-alist))
                          ((member ast-class
                                   (list :CallExpression
                                         :NewExpression)
                                   :test #'equal)
                           (append (list (aget :callee ast-alist))
                                   (aget :arguments ast-alist)))
                          ((member ast-class
                                   (list :BreakStatement
                                         :ContinueStatement)
                                   :test #'equal)
                           (list (aget :label ast-alist)))
                          ((member ast-class
                                   (list :ConditionalExpression
                                         :IfStatement)
                                   :test #'equal)
                           (list (aget :test ast-alist)
                                 (aget :consequent ast-alist)
                                 (aget :alternate ast-alist)))
                          ((member ast-class
                                   (list :ImportSpecifier
                                         :ImportDefaultSpecifier
                                         :ImportNamespaceSpecifier))
                           (remove-duplicates
                            (list (aget :imported ast-alist)
                                  (aget :local ast-alist))
                            :test #'equal))
                          (t nil))))
       (make-children (source-text alist child-alists child-asts
                       &aux (start (aget :start alist)))
         (if child-alists
             (iter (for child-alist in child-alists)
                   (for child-ast in child-asts)
                   (collect (subseq source-text start (aget :start child-alist))
                            into result)
                   (collect child-ast into result)
                   (setf start (aget :end child-alist))
                   (finally
                     (return (append result
                                     (list (subseq source-text
                                                   start
                                                   (aget :end alist)))))))
             (list (subseq source-text (aget :start alist) (aget :end alist)))))
       (make-tree (source-text ast-alist)
         (let ((children (collect-children ast-alist)))
           (make-instance 'javascript-ast
             :class (make-keyword (string-upcase (aget :type ast-alist)))
             :children (make-children source-text
                                      ast-alist
                                      children
                                      (mapcar {make-tree source-text} children))
             :annotations (annotations ast-alist)))))
    (nest (fix-newlines) (make-tree source (acorn obj)))))


;;;; Fixup code for newlines.  These should be in the same AST as
;;;; a statement they terminate
(defun is-stmt (ast)
  (member (ast-class ast) '(:dowhilestatement :forstatement :labeledstatement
                            :switchstatement :trystatement :withstatement
                            :forinstatement :forofstatement :blockstatement
                            :breakstatement :continuestatement
                            :expressionstatement :whilestatement
                            :throwstatement :returnstatement
                            :ifstatement)))

(defun move-prefixes-down (children allowed-fn prefix-fn)
  "Give a list CHILDREN of strings and AST nodes, find children
that satisfy ALLOWED-FN, are followed by a string, and for for which
PREFIX-FN returns a non-null value, which must be a position
in the string.   Move the [0..pos) prefix of that string
down into the list of children of the preceding node, concatenating
it onto the end of the last string in that node's children.
Return a new list of children."
  (labels ((children-with-suffix (children suffix)
             "Return a new list of CHILDREN with SUFFIX added."
             (iter (for tail on children)
                   (if (endp (cdr tail))
                       (let ((last-child (car tail)))
                         (if (stringp last-child)
                             (collect (concatenate 'string last-child suffix))
                             (progn
                               (collect last-child)
                               (collect suffix))))
                       (collect (car tail))))))
    (iter (for p on children)
          (for node = (car p))
          (for next = (cadr p))
          (if-let ((pos (and (typep node 'ast)
                             (funcall allowed-fn node)
                             (stringp next)
                             (funcall prefix-fn next))))
            (prog1
                (collect (nest (copy node :children)
                               (children-with-suffix (ast-children node)
                                                     (subseq next 0 pos))))
              (setf (cadr p) (subseq next pos)))
            (collect node)))))

(defun position-after-leading-newline (str)
  "Returns 1+ the position of the first newline in STR,
assuming it can be reached only by skipping over whitespace
or comments.  NIL if no such newline exists."
  (let ((len (length str))
        (pos 0))
    (loop
       (when (>= pos len) (return nil))
       (let ((c (elt str pos)))
         (case c
           (#\Newline (return (1+ pos)))
           ((#\Space #\Tab)
            (incf pos))
           ;; Skip over comments
           (#\/
            (incf pos)
            (when (>= pos len) (return nil))
            (let ((c (elt str pos)))
              (unless (eql c #\/)
                (return nil))
              (return
                (loop (incf pos)
                   (when (>= pos len) (return nil))
                   (when (eql (elt str pos) #\Newline)
                     (return (1+ pos)))))))
           (t (return nil)))))))

(defgeneric fix-newlines (ast)
  (:documentation "Fix newlines in JavaScript ASTs by pushing newlines
down into child statements.  This allows for mutation operations
to insert and replacement statements with newlines already present
in the new ASTs, obviating the need for fixups to add missing
newlines.")
  (:method ((ast javascript-ast))
    (nest (copy ast :children)
          (mapcar #'fix-newlines)
          (move-prefixes-down (ast-children ast)
                              #'is-stmt
                              #'position-after-leading-newline)))
  (:method ((ast t)) ast))


;;; Javascript mutation
(define-mutation javascript-mutation (parseable-mutation) ()
  (:documentation
   "Specialization of the mutation interface for JavaScript software objects."))


;;; Methods common to all software objects
(defmethod phenome ((obj javascript) &key (bin (temp-file-name)))
  "Create a phenotype of the javascript software OBJ.  In this case, override
the phenome method to output the genome of OBJ to BIN as JavaScript
is not a compiled language.

* OBJ object to create a phenome for
* BIN location where the phenome will be created on the filesystem"
  (to-file obj bin)
  (values bin 0 nil nil nil))

(defmethod rebind-vars ((ast javascript-ast)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (if (eq (ast-class ast) :Identifier)
      (copy ast :children (mapcar {rebind-vars _
                                               var-replacements
                                               fun-replacements}
                                  (ast-children ast)))
      (let ((c (mapcar (lambda (c)
                         (cond ((stringp c) c)
                               (t (rebind-vars c var-replacements
                                               fun-replacements))))
                       (ast-children ast))))
        (if (every #'eql c (ast-children ast))
            ast
            (copy ast :children c)))))

(defmethod enclosing-scope ((obj javascript) (ast javascript-ast))
  "Return the enclosing scope of AST in OBJ.
OBJ javascript software object
AST ast to return the enclosing scope for"
  (or (find-if (lambda (ast)
                 (member (ast-class ast)
                         (list :BlockStatement
                               :FunctionDeclaration
                               :FunctionExpression
                               :Program
                               :ArrowFunctionExpression
                               :ForStatement
                               :ForInStatement
                               :ForOfStatement)))
               (cdr (get-parent-asts obj ast)))
      (genome obj)))

(defmethod scopes ((obj javascript) (ast javascript-ast))
  "Return lists of variables in each enclosing scope of AST.
Each variable is represented by an alist containing :NAME, :DECL, and :SCOPE.
OBJ javascript software object
AST ast to return the scopes for"
  (labels ((get-parent-decl (obj identifier)
             "For the given IDENTIFIER AST, return the parent declaration."
             (car (remove-if-not [{eq :VariableDeclaration} #'ast-class]
                                 (get-parent-asts obj identifier))))
           (get-first-child-ast (obj ast)
             "Return the first child of the AST."
             (first (get-immediate-children obj ast)))
           (filter-identifiers (obj scope children)
             "Return all variable identifiers found in the CHILDREN of SCOPE."
             (if (member (ast-class scope)
                         (list :FunctionDeclaration
                               :FunctionExpression
                               :ArrowFunctionExpression))
                 ;; Special case for functions.
                 ;; function foo(bar, ...baz) {...} => '(bar baz)
                 (mappend
                   (lambda (ast)
                     (cond ((eq (ast-class ast) :Identifier)
                            (list ast))
                           ((eq (ast-class ast) :RestElement)
                            (list (first (get-immediate-children obj ast))))
                           (t nil)))
                   (if (eq (ast-class scope) :FunctionDeclaration)
                       (cdr children) ; elide function name identifer
                       children))
                 ;; Return the variable declarations in the scope.
                 ;; Note: An variable declaration or assignment in
                 ;; JavaScript may be destructuring
                 ;; (e.g. [a, b, c] = [1, 2, 3]).  In this case, we need
                 ;; all of the variables on the left-hand side of the
                 ;; expression.
                 (nest (remove-if-not [{eq :Identifier} #'ast-class])
                       (mappend (lambda (ast)
                                  (let ((child (get-first-child-ast obj ast)))
                                    (if (eq :Identifier (ast-class child))
                                        ;; single var
                                        (list child)
                                        ;; destructuring
                                        (get-children obj child)))))
                       (remove-if-not [{eq :VariableDeclarator} #'ast-class])
                       (mappend {get-immediate-children obj})
                       (remove-if-not [{eq :VariableDeclaration} #'ast-class]
                                      children)))))
    (when (not (eq :Program (ast-class ast)))
      (let ((scope (enclosing-scope obj ast)))
        (cons (nest (reverse)
                    ; build result
                    (mappend
                      (lambda (ast)
                        `(((:name . ,(source-text ast))
                           (:decl . ,(get-parent-decl obj ast))
                           (:scope . ,scope)))))
                    ; remove ASTs which are not variable identifiers
                    (filter-identifiers obj scope)
                    (iter (for c in
                               (get-immediate-children obj scope))
                          (while (path-later-p (ast-path obj ast)
                                               (ast-path obj c)))
                          (collect c)))
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
                                              (list :CallExpression
                                                    :MemberExpression
                                                    :FunctionDeclaration
                                                    :FunctionExpression
                                                    :ArrowFunctionExpression
                                                    :ClassExpression
                                                    :MetaProperty
                                                    :BreakStatement
                                                    :ClassDeclaration
                                                    :ContinueStatement
                                                    :LabelledStatement
                                                    :ImportSpecifier
                                                    :ExportSpecifier
                                                    :ExportDefaultDeclaration
                                                    :VariableDeclarator))))
                        (list (cons :name (source-text ast))))
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
                    (list (list (source-text callee)
                                nil nil (length (cdr children)))))
                   ((eq (ast-class callee) :MemberExpression)
                    ;; Member function call
                    (list (list (nest (source-text)
                                      (second)
                                      (get-immediate-children obj callee))
                                nil nil (length (cdr children)))))
                   (t nil)))
           (mapcar {get-unbound-funs obj}
                   (get-immediate-children obj ast)))
    :test #'equal))

(defmethod get-parent-full-stmt ((obj javascript) (ast javascript-ast))
  (find-if #'is-stmt (get-parent-asts obj ast)))


;;; Implement the generic format-genome method for Javascript objects.
(defmethod format-genome ((obj javascript) &key)
  (prettier obj))
