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
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/components/formatting)
  #-windows (:shadowing-import-from :osicat :file-permissions)
  (:import-from :jsown)
  (:export :javascript
           :javascript-mutation
           :javascript-ast
           :javascript-ast-p
           :javascript-ast-node
           :make-javascript-ast
           :copy-javascript-ast
           :make-javascript-ast-node
           :copy-javascript-ast-node
           :parsing-mode))
(in-package :software-evolution-library/software/javascript)
(in-readtable :curry-compose-reader-macros)

(define-software javascript (parseable)
  ((parsing-mode
     :initarg :parsing-mode
     :reader parsing-mode
     :initform :script
     :documentation "Acorn parsing mode, either :script or :module."))
  (:documentation "Javascript software representation."))


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
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (multiple-value-bind (stdout stderr exit)
        (shell "acorn --compact --allow-hash-bang ~a ~a"
               (if (eq (parsing-mode obj) :module)
                   "--module"
                   "")
               src-file)
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
                                       "object" "property" "computed" "generator"
                                       "async" "params" "declarations"
                                       "id" "init " "kind" "test" "body" "left"
                                       "right" "operator" "prefix" "argument"
                                       "expression" "callee " "name" "arguments"
                                       "type" "start" "end" "value" "raw" "sourcetype")
                           str))

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
                           (list (aget :tag ast-alist)
                                 (aget :quasi ast-alist)))
                          ((equal ast-class "TemplateLiteral")
                           (sort (append (aget :quasis ast-alist)
                                         (aget :expressions ast-alist))
                                 #'< :key {aget :end}))
                          ((equal ast-class "MetaProperty")
                           (list (aget :meta ast-alist)
                                 (aget :property ast-alist)))
                          ((equal ast-class "Property")
                           (if (< (aget :start (aget :value ast-alist))
                                  (aget :end (aget :key ast-alist)))
                             (list (aget :value ast-alist))
                             (list (aget :key ast-alist)
                                   (aget :value ast-alist))))
                          ((equal ast-class "MethodDefinition")
                           (list (aget :key ast-alist)
                                 (aget :value ast-alist)))
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
                           (remove-duplicates
                             (list (aget :local ast-alist)
                                   (aget :exported ast-alist))
                             :test #'equal))
                          ((member ast-class
                                   (list "ClassExpression"
                                         "ClassDeclaration")
                                   :test #'equal)
                           (list (aget :id ast-alist)
                                 (aget :superclass ast-alist)
                                 (aget :body ast-alist)))
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
                           (remove-duplicates
                             (list (aget :imported ast-alist)
                                   (aget :local ast-alist))
                             :test #'equal))
                          (t nil))))
       (make-children (genome alist child-alists child-asts
                       &aux (start (aget :start alist)))
         (if child-alists
             (iter (for child-alist in child-alists)
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
             (list (subseq genome (aget :start alist) (aget :end alist)))))
       (make-tree (genome ast-alist)
         (let ((children (collect-children ast-alist))
               (new-ast-node (add-aux-data
                              (make-javascript-ast-node
                               :class (make-keyword
                                       (string-upcase
                                        (aget :type ast-alist))))
                              ast-alist)))
           (make-javascript-ast
             :node new-ast-node
             :children (make-children
                         genome
                         ast-alist
                         children
                         (iter (for child in children)
                               (collect (make-tree genome
                                                   child))))))))
    (setf (ast-root obj)
          (fix-newlines (make-tree (genome obj) (parse-asts obj))))
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
(defmethod to-file :after ((obj javascript) file
                           &aux (preamble (car (ast-children (ast-root obj)))))
  "Ensure JavaScript scripts are marked executable."
  (when (string= (subseq preamble 0 (min (length preamble) 2)) "#!")
    (push :user-exec (file-permissions file))))

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
      (ast-root obj)))

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
                   children)
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
                          (while (ast-later-p ast c))
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
                                              (append
                                                +js-bound-id-parent-classes+
                                                (list :CallExpression
                                                      :MemberExpression)))))
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

(defmethod traceable-stmt-p ((obj javascript) (ast javascript-ast))
  "Return TRUE if AST is a traceable statement in the javascript software OBJ."
  (or (null (get-parent-ast obj ast))
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

;;;; Fixup code for newlines.  These should be in the same AST as
;;;; a statement they terminate

(defun is-stmt (ast)
  (member (ast-class ast) '(:dowhilestatement :forstatement :labeledstatement
                            :switchstatement :trystatement :withstatement
                            :forinstatement :forofstatement :blockstatement
                            :breakstatement :continuestatement
                            :expressionstatement
                            :throwstatement :returnstatement
                            :ifstatement)))

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

(defun fix-newlines (ast)
  (map-ast ast #'fix-newlines-ast)
  ast)

(defun fix-newlines-ast (ast)
  (move-prefixes-down
   (ast-children ast)
   #'is-stmt #'position-after-leading-newline))
