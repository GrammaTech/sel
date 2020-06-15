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
  (:nicknames :sel/software/javascript :sel/sw/javascript :sel/sw/js)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/parseable
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting)
  (:import-from :jsown)
  (:import-from :cffi :translate-camelcase-name)
  (:import-from :functional-trees :path-later-p)
  (:export :javascript
           :javascript-mutation
           :javascript-ast
           :skipped-before
           :skipped-after
           :acorn))
(in-package :software-evolution-library/software/javascript)
(in-readtable :curry-compose-reader-macros)

(define-software javascript (parseable file-w-attributes) ()
  (:documentation "Javascript software representation."))


;;; Javascript ast data structures
(defvar *string*)

(defclass javascript-ast (functional-tree-ast)
  ((start :initarg :start :initform (when *string* 0)
          :reader start :type (or null (integer 0 *)))
   (end :initarg :end :initform (when *string* (length *string*))
        :reader end :type (or null (integer 0 *)))
   (skipped-before
    :initarg :skipped-before :initform nil
    :reader skipped-before :type (or null javascript-ast-skipped))
   (skipped-after
    :initarg :skipped-after :initform nil
    :reader skipped-after :type (or null javascript-ast-skipped))
   (string-pointer :initarg :string-pointer :initform *string*
                   :reader string-pointer :type (or null string)))
  (:documentation "Class of JavaScript ASTs."))

(defclass javascript-ast-skipped (javascript-ast) ()
  (:documentation "Skipped region of source code text."))

(defvar js-children
  ;; TODO: Should we use `translate-camelcase-name'?
  '(((:Program) (:body . 0))
    ((:MemberExpression) (:object . 1) (:property . 1))
    ((:TaggedTemplateExpression) (:tag . 1) (:quasi . 1))
    ((:TemplateLiteral) (:quasis . 0) (:expressions . 0))
    ((:MetaProperty) (:meta . 1) (:property . 1))
    ((:Property)
     ;; if (< (start value) (end key)) just value
     (:key . 1) (:value . 1))
    ((:MethodDefinition) (:key . 1) (:value . 1))
    ((:SequenceExpression) (:expressions . 0))
    ((:DoWhileStatement) (:body . 1) (:test . 1))
    ((:WhileStatement) (:test . 1) (:body . 1))
    ((:ExpressionStatement) (:expression . 1))
    ((:ForStatement) (:init . 1) (:test . 1) (:update . 1) (:body . 1))
    ((:LabeledStatement) (:label . 1) (:body . 1))
    ((:SwitchStatement) (:discriminant . 1) (:cases . 0))
    ((:SwitchCase) (:test . 1) (:consequent . 0))
    ((:TryStatement) (:block . 1) (:handler . 1) (:finalizer . 1))
    ((:CatchClause) (:param . 1) (:body . 1))
    ((:VariableDeclaration) (:declarations . 0))
    ((:VariableDeclarator) (:id . 1) (:init . 1))
    ((:WithStatement) (:object . 1) (:body . 1))
    ((:Program) (:body . 0))
    ((:ImportDeclaration) (:specifiers . 0) (:source . 1))
    ((:ExportAllDeclaration) (:source . 1))
    ((:ExportDefaultDeclaration) (:declaration . 1))
    ((:ExportNamedDeclaration)
     (:declaration . 1) (:specifiers . 0) (:source . 1))
    ((:ExportSpecifier)
     ;; remove-duplicates
     (:local . 1) (:exported . 1))
    ((:ClassExpression :ClassDeclaration)
     (:id . 1) (:superclass . 1) (:body . 1))
    ((:ForInStatement :ForOfStatement) (:left . 1) (:right . 1) (:body . 1))
    ((:ArrayPattern :ArrayExpression) (:elements . 0))
    ((:ObjectPattern :ObjectExpression) (:properties . 0))
    ((:RestElement
      :SpreadElement
      :UpdateExpression
      :AwaitExpression
      :UnaryExpression
      :YieldExpression
      :ReturnStatement
      :ThrowStatement)
     (:argument . 1))
    ((:AssignmentPattern
      :BinaryExpression
      :LogicalExpression
      :AssignmentExpression)
     (:left . 1) (:right . 1))
    ((:FunctionExpression :ArrowFunctionExpression :FunctionDeclaration)
     (:params . 0) (:body . 1))
    ((:ClassBody :BlockStatement) (:body . 0))
    ((:CallExpression :NewExpression) (:callee . 1) (:arguments . 0))
    ((:BreakStatement :ContinueStatement) (:label . 1))
    ((:ConditionalExpression :IfStatement)
     (:test . 1) (:consequent . 1) (:alternate . 1))
    ((:ImportSpecifier :ImportDefaultSpecifier :ImportNamespaceSpecifier)
     ;; remove-duplicates
     (:imported . 1) (:local . 1))
    ;; Classes with no children.
    ((:Identifier :Literal))))

(defun expand-js-class (spec)
  (nest
   (destructuring-bind (ast-class-list . field-specifiers) spec)
   (mapcar
    (lambda (class)
      `(defclass ,(symbol-cat 'js class) (javascript-ast)
         ((acorn-slot-name :initform ,class :allocation :class)
          ,@(when field-specifiers
              `((child-slots
                 :initform (quote ,(mapcar «cons [{symbol-cat 'js} #'car] #'cdr»
                                           field-specifiers))
                 :allocation :class)))
          ,@(mapcar (lambda (field)
                      (destructuring-bind (field . arity) field
                        (let ((js-field (symbol-cat 'js field)))
                          (list* js-field :reader js-field
                                          :initform nil
                                          :initarg (make-keyword field)
                                          (when (zerop arity)
                                            (list :type 'list))))))
                    field-specifiers))
         (:documentation
          ,(format nil "Javascript AST node class for ~a acorn ASTs." class)))))
   ast-class-list))

(eval `(progn ,@(mappend #'expand-js-class js-children)))
(export (mapcar {symbol-cat 'js} (mappend #'first js-children)))

(defclass js-top (javascript-ast)
  ((top-level :initarg :top-level :reader top-level :type 'javascript-ast)
   (child-slots :initform '((top-level . 1)) :accessor child-slots
                :allocation :class))
  (:documentation "Top-level AST for JavaScript objects."))


;;; Javascript parsing
(defgeneric acorn (obj)
  (:documentation "Invoke the acorn parser on the genome of OBJ returning a
raw list of ASTs in OBJ for use in `parse-asts`."))

(defmethod acorn ((string string))
  (labels ((invoke-acorn (parsing-mode)
             "Invoke acorn with the given PARSING-MODE (:script or :module)."
             (with-temporary-file-of (:pathname src-file) string
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
              :operation :parse))))))

(defmethod acorn ((obj javascript)) (acorn (genome-string obj)))

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

(defmethod convert ((to-type (eql 'javascript-ast)) (string string)
                    &key &allow-other-keys)
  (nest
   (let ((*string* string)))
   (labels
       ((make-skipped (start end)
          (when (< start end)
            (make-instance 'javascript-ast-skipped :start start :end end)))
        (ranges (ast &aux ranges)
          (let (last)
            (mapc (lambda (child)
                    (when last (push (cons (start last) (start child)) ranges))
                    (setf last child))
                  (remove nil (children ast)))
            (push (cons (start last) (end ast)) ranges)
            (nreverse ranges)))
        (w/skipped (tree from to)
          (assert (= from (start tree)))
          (when (children tree)
            (setf (slot-value tree 'skipped-before)
                  (make-skipped from (start (first (children tree)))))
            (mapc (lambda (child range)
                    (destructuring-bind (from . to) range
                      (w/skipped child from to)))
                  (remove nil (children tree))
                  (ranges tree)))
          (setf (slot-value tree 'skipped-after)
                (make-skipped (end tree) to))
          tree)))
   ;; (fix-newlines)
   (make-instance 'js-top :top-level)
   (w/skipped (convert to-type (acorn string))
              0 (length *string*))))

(defmethod convert ((to-type (eql 'javascript-ast)) (spec null)
                    &key &allow-other-keys)
  nil)
(defmethod convert ((to-type (eql 'javascript-ast)) (spec list)
                    &key &allow-other-keys)
  "Create a JAVASCRIPT AST from the SPEC (specification) list."
  (assert (boundp '*string*) (*string*)
    "Can't create JS ASTs without `*string*'.")
  (let* ((raw-type (make-keyword (string-upcase (aget :type spec))))
         (type (symbol-cat 'js raw-type))
         (child-types (aget raw-type js-children :test #'member)))
    (apply #'make-instance type
           (mappend
            (lambda (field)
              (destructuring-bind (key . value) field
                (list key
                      (if-let ((spec (find key child-types :key #'car)))
                        (destructuring-bind (key . arity) spec
                          (declare (ignorable key))
                          (ecase arity
                            (1 (convert 'javascript-ast value))
                            (0 (mapcar {convert 'javascript-ast} value))))
                        value))))
            (cdr spec)))))

(defmethod parse-asts ((obj javascript) &optional (source (genome-string obj)))
  (convert 'javascript-ast source))

(defmethod source-text ((obj javascript-ast) &optional stream)
  (write-string (source-text (skipped-before obj)) stream)
  (if (children obj)
      (mapc [{write-string _ stream} #'source-text] (children obj))
      (write-string (subseq (string-pointer obj) (start obj) (end obj))
                    stream))
  (write-string (source-text (skipped-after obj)) stream))


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
                               (children-with-suffix (children node)
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
          (move-prefixes-down (children ast)
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
      (copy ast :name (rebind-vars (ast-annotation ast :name)
                                   var-replacements
                                   fun-replacements)
                :children (mapcar {rebind-vars _
                                               var-replacements
                                               fun-replacements}
                                  (children ast)))
      (let ((c (mapcar (lambda (c)
                         (cond ((stringp c) c)
                               (t (rebind-vars c var-replacements
                                               fun-replacements))))
                       (children ast))))
        (if (every #'eql c (children ast))
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
           (get-first-child-ast (ast)
             "Return the first child of the AST."
             (first (child-asts ast)))
           (filter-identifiers (scope children)
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
                            (list (first (child-asts ast))))
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
                                  (let ((child (get-first-child-ast ast)))
                                    (if (eq :Identifier (ast-class child))
                                        ;; single var
                                        (list child)
                                        ;; destructuring
                                        (child-asts child :recursive t)))))
                       (remove-if-not [{eq :VariableDeclarator} #'ast-class])
                       (mappend #'child-asts)
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
                    (filter-identifiers scope)
                    (iter (for c in (child-asts scope))
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
                              (child-asts ast)))
               :test #'equal)))
    (get-unbound-vals-helper obj (get-parent-ast obj ast) ast)))

(defmethod get-unbound-funs ((obj javascript) (ast javascript-ast)
                             &aux (child-asts (child-asts ast))
                                  (callee (first child-asts)))
  "Return all functions used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound functions within"
  (remove-duplicates
    (apply #'append
           (when (eq (ast-class ast) :CallExpression)
             (cond ((eq (ast-class callee) :Identifier)
                    ;; Free function call
                    (list (list (source-text callee)
                                nil nil (length (cdr child-asts)))))
                   ((eq (ast-class callee) :MemberExpression)
                    ;; Member function call
                    (list (list (nest (source-text)
                                      (second)
                                      (child-asts callee))
                                nil nil (length (cdr child-asts)))))
                   (t nil)))
           (mapcar {get-unbound-funs obj} child-asts))
    :test #'equal))

(defmethod get-parent-full-stmt ((obj javascript) (ast javascript-ast))
  (find-if #'is-stmt (get-parent-asts obj ast)))


;;; Implement the generic format-genome method for Javascript objects.
(defmethod format-genome ((obj javascript) &key)
  (prettier obj))
