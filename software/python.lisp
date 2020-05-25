;;; python.lisp --- Python software representation.
;;;
;;; Implements the core functionality of the software-evolution-library
;;; for files written in python.  The core functionality is supported
;;; by the command line script @code{python-astdump}, a thin wrapper
;;; around the python
;;; @url{https://docs.python.org/3/library/ast.html, AST library},
;;; which dumps the ASTs as JSON.  The @code{python-astdump} script
;;; requires python 3.8 or higher; please note this is not the default
;;; installed by the package manager for ubuntu-18.04 and lower.
;;;
;;; Beyond this, the @url{https://github.com/google/yapf, yapf} command
;;; line python code formatter is required to support code formatting
;;; (see @pxref{Generic-Function format-genome}).  This tool may be
;;; installed with pip.
;;;
;;; As with @code{javascript} software objects, @code{python} software
;;; objects are a parsed AST representation.  All of the generic
;;; methods for @code{parseable} software objects have been implemented,
;;; allowing for a rich variety of manipulations and tests.
;;;
;;; Every AST in a @code{python} software object contains an AST
;;; class field with the AST type (e.g. FunctionDef, Assign).  Additional
;;; fields in the python AST representation can be found in an association
;;; list on the AST's annotations field.  Unfortunately, these additional
;;; fields do not appear in the official documentation, although an external
;;; @url{https://greentreesnakes.readthedocs.io/en/latest/, AST reference}
;;; contains some details.
;;;
;;; @texi{python}
(defpackage :software-evolution-library/software/python
  (:nicknames :sel/software/python :sel/sw/python)
  (:use :gt/full
        :babel
        :cl-json
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/parseable
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting)
  (:export :python
           :python-mutation
           :python-ast))
(in-package :software-evolution-library/software/python)
(in-readtable :curry-compose-reader-macros)

(define-software python (parseable file-w-attributes) ()
  (:documentation "Python software representation."))


;;; Python ast data structures
(defclass python-ast (functional-tree-ast)
  ((children :type list
             :initarg :children
             :initform nil
             :documentation "The list of children of the node,
which may be more nodes, or other values.")
   (child-slots :initform '(children) :allocation :class))
  (:documentation "Class of Python ASTs."))


;;; Python parsing
(define-constant +asdl+
  (alist-hash-table
   '((:Module . ((:body . *)))
     (:Interactive . ((:body . *)))
     (:Expression . ((:body . 1)))
     (:FunctionDef . ((:decorator-list . *)
                      (:args . 1)
                      (:body . *)
                      (:returns . ?)))
     (:AsyncFunctionDef . ((:decorator-list . *)
                           (:args . 1)
                           (:body . *)
                           (:returns . ?)))
     (:ClassDef . ((:decorator-list . *)
                   (:bases . *)
                   (:keywords . *)
                   (:body . *)))
     (:Return . ((:value . ?)))
     (:Delete . ((:targets . *)))
     (:Assign . ((:targets . *)
                 (:value . 1)))
     (:AugAssign . ((:target . 1)
                    (:value . 1)))
     (:AnnAssign . ((:target . 1)
                    (:annotation . 1)
                    (:value . ?)))
     (:For . ((:target . 1)
              (:iter . 1)
              (:body . *)
              (:orelse . *)))
     (:AsyncFor . ((:target . 1)
                   (:iter . 1)
                   (:body . *)
                   (:orelse . *)))
     (:While . ((:test . 1)
                (:body . *)
                (:orelse . *)))
     (:If . ((:test . 1)
             (:body . *)
             (:orelse . *)))
     (:With . ((:items . *)
               (:body . *)))
     (:AsyncWith . ((:items . *)
                    (:body . *)))
     (:Raise . ((:exc . ?)
                (:cause . ?)))
     (:Try . ((:body . *)
              (:handlers . *)
              (:orelse . *)
              (:finalbody . *)))
     (:Assert . ((:test . 1)
                 (:msg . ?)))
     (:Expr . ((:value . 1)))
     (:BoolOp . ((:values . *)))
     (:BinOp . ((:left . 1)
                (:right . 1)))
     (:UnaryOp . ((:operand . 1)))
     (:Lambda . ((:args . 1)
                 (:body . 1)))
     (:IfExp . ((:body . 1)
                (:test . 1)
                (:orelse . 1)))
     (:Dict . ((:keys . *)
               (:values . *)))
     (:Set . ((:elts . *)))
     (:ListComp . ((:elt . 1)
                   (:generators . *)))
     (:SetComp . ((:elt . 1)
                  (:generators . *)))
     (:DictComp . ((:key . 1)
                   (:value . 1)
                   (:generators . *)))
     (:GeneratorExp . ((:elt . 1)
                       (:generators . *)))
     (:Await . ((:value . 1)))
     (:Yield . ((:value . ?)))
     (:YieldFrom . ((:value . 1)))
     (:Compare . ((:left . 1)
                  (:comparators . *)))
     (:Call . ((:func . 1)
               (:args . *)
               (:keywords . *)))
     (:Attribute . ((:value . 1)))
     (:Subscript . ((:value . 1)
                    (:slice . 1)))
     (:Starred . ((:value . 1)))
     (:List . ((:elts . *)))
     (:Tuple . ((:elts . *)))
     (:Slice . ((:lower . ?) (:upper . ?) (:step . ?)))
     (:ExtSlice . ((:dims . *)))
     (:Index . ((:value . 1)))
     (:Comprehension . ((:target . 1)
                        (:iter . 1)
                        (:ifs . *)))
     (:ExceptHandler . ((:type . ?)
                        (:body . *)))
     (:Arguments . ((:posonlyargs . *)
                    (:args . *)
                    (:defaults . *)
                    (:vararg . ?)
                    (:kwonlyargs . *)
                    (:kw-defaults . *)
                    (:kwarg . ?)))
     (:Arg . ((:annotation . ?)))
     (:Keyword . ((:value . 1)))
     (:WithItem . ((:context-expr . 1)
                   (:optional-vars . ?)))))
  :test #'equalp
  :documentation "Abstract Syntax Description Language for python.")

(define-constant +stmt-ast-classes+
  '(:Module :FunctionDef :AsyncFunctionDef :ClassDef :Return :Delete
    :Assign :AugAssign :AnnAssign :For :AsyncFor :While :If
    :With :AsyncWith :Raise :Try :ExceptHandler :Assert :Import :ImportFrom
    :Global :NonLocal :Expr :Pass :Break :Continue)
  :test #'equal
  :documentation "Stmt AST subclasses for python.")

(define-constant +ast-classes-with-larger-child-spans+
  '(:Arg :GeneratorExp :Starred :ClassDef :AsyncFunctionDef :FunctionDef)
  :test #'equal
  :documentation "AST classes which may have child spans larger than span
given by python in the line, col attributes.")

(define-constant +ast-classes-with-unsorted-children+
  '(:Dict :Call :AsyncFunctionDef :FunctionDef :Arguments)
  :test #'equal
  :documentation "AST classes which may have children which are not
in textual (sorted) order.")

(defgeneric python-astdump (obj source-text)
  (:documentation "Invoke the python-astdump script to dump ASTs in
SOURCE-TEXT in JSON format.")
  (:method ((obj python) (source-text string))
    (with-temporary-file-of (:pathname src) source-text
      (multiple-value-bind (stdout stderr exit)
          (shell "~a ~a"
                 (make-pathname
                  :directory (append +software-evolution-library-dir+
                                     (list "bin"))
                  :name "python-astdump")
                 src)
        (if (zerop exit)
            (handler-bind ((no-char-for-code
                            (lambda (e)
                              (declare (ignorable e))
                              (invoke-restart 'substitute-char #\?)))
                           (floating-point-overflow
                            (lambda (e)
                              (declare (ignorable e))
                              (invoke-restart 'bignumber-string))))
              (decode-json-from-string stdout))
            (error (make-instance 'mutate
                     :text (format nil "python-astdump exit ~d~%~
                                        stderr:~s"
                                   exit stderr)
                     :operation :parse
                     :obj obj)))))))

(defmethod parse-asts ((obj python) &optional (source-text (genome-string obj))
                       &aux (source-octets (string-to-octets source-text))
                         (line-offsets (line-offsets source-text)))
  (labels ((ast-class (json &aux (pair (assoc :class json)))
             "Return the AST class of the JSON AST representation."
             ;; Update JSON if the class is a string to ensure
             ;; subsequent calls to `ast-class` do not need to
             ;; invoke `make-keyword`.  This halves the amount
             ;; of time spent creating ASTs after dumping
             ;; JSON and parsing.
             (when (stringp (cdr pair))
               (setf (cdr pair)
                     (make-keyword (string-upcase (aget :class json)))))
             (cdr pair))
           (offset (line col)
             "Return the offset into SOURCE-OCTETS for the given LINE and COL."
             (+ col (aget :start (gethash line line-offsets))))
           (last-parsed-char-in-line-p (line col)
             "Return T if LINE and COL represent the last parsed
             (non-whitespace, non-comment) byte in a line of
             SOURCE-OCTETS."
             (= (offset line col)
                (aget :end-parsed (gethash line line-offsets))))
           (offset-w-corrections (ast-class line col)
             "Return the offset into SOURCE-TEXT for the given LINE and COL
             with corrections to include the trailing newline for stmt AST
             classes."
             (if (and (member ast-class +stmt-ast-classes+)
                      (last-parsed-char-in-line-p line col)
                      (gethash (1+ line) line-offsets))
                 (offset (1+ line) 0)
                 (offset line col)))
           (start (json)
             "Return the start offset into SOURCE-OCTETS from the JSON AST
             representation."
             (cond ((null json) nil) ;; no json, no offset
                   ((eq :Module (ast-class json))
                    ;; special case for top-level AST
                    0)
                   ((and (null (aget :lineno json))
                         (null (aget :col-offset json)))
                    ;; no line info, return the start of the first child
                    (start (car (collect-children json))))
                   ((member (ast-class json)
                            +ast-classes-with-larger-child-spans+)
                    ;; special case where the parent AST may have less
                    ;; span than the children.  If so, use the span of
                    ;; the children.
                    (min (or (start (car (collect-children json)))
                             most-positive-fixnum)
                         (offset-w-corrections (ast-class json)
                                               (aget :lineno json)
                                               (aget :col-offset json))))
                    ;; offset given in JSON
                   (t (offset-w-corrections (ast-class json)
                                            (aget :lineno json)
                                            (aget :col-offset json)))))
           (end (json)
             "Return the end offset into SOURCE-OCTETS from the JSON AST
             representation."
             (cond ((null json) nil)
                   ((eq :Module (ast-class json))
                    ;; special case for top-level AST
                    (length source-octets))
                   ((and (null (aget :lineno json))
                         (null (aget :col-offset json)))
                    ;; no line info, return the end of the last child
                    (end (lastcar (collect-children json))))
                   ((member (ast-class json)
                            +ast-classes-with-larger-child-spans+)
                    ;; special case where the parent AST may have less
                    ;; span than the children.  If so, use the span of
                    ;; the children.
                    (max (or (end (lastcar (collect-children json)))
                             most-negative-fixnum)
                         (offset-w-corrections (ast-class json)
                                               (aget :end-lineno json)
                                               (aget :end-col-offset json))))
                    ;; offset given in JSON
                   (t (offset-w-corrections (ast-class json)
                                            (aget :end-lineno json)
                                            (aget :end-col-offset json)))))
           (start-and-end-p (json)
             "Return T if JSON AST representation contains both a start and
             end offset."
             (and (start json) (end json)))
           (child-keys (ast-class)
             "Return the AST classes of the children of AST-CLASS."
             (mapcar #'car (gethash ast-class +asdl+)))
           (remove-children (json &aux (keys (child-keys (ast-class json))))
             "Remove all of the children from the JSON AST representation."
             (remove-if {member _ keys} json :key #'car))
           (sort-children (json children-json)
             "Sort CHILDREN-JSON if the JSON AST representation is one where
             children may not be in ascending textual order."
             (if (member (ast-class json) +ast-classes-with-unsorted-children+)
                 (sort children-json #'< :key #'start)
                 children-json))
           (collect-children (json)
             "Collect the CHILD ASTs of the JSON AST representation in a list."
             (iter (for (key . arity) in (gethash (ast-class json) +asdl+))
                   (case arity
                     (1 (collecting (aget key json) into result))
                     (* (appending (aget key json) into result))
                     (? (when-let ((child (aget key json)))
                          (collecting child into result))))
                   (finally
                    (return (nest (sort-children json)
                                  (remove-if-not #'start-and-end-p result))))))
           (annotation-p (key value)
             "Return T if KEY and VALUE represent an AST annotation."
             (and (not (member key (list :class :lineno :col-offset
                                         :end-lineno :end-col-offset)))
                  (not (null value))))
           (annotations (json)
             "Return a list of the AST annotations (non-child key, value pairs)
             in the JSON AST representation."
             (iter (for (key . value) in (remove-children json))
                   (when (annotation-p key value)
                     (cond ((eq key :ctx)
                            (collect (cons key (nest (make-keyword)
                                                     (string-upcase)
                                                     (aget :class value)))))
                           ((eq key :op)
                            (collect (cons key (aget :class value))))
                           ((eq key :ops)
                            (collect (cons key (mapcar {aget :class} value))))
                           (t (collect (cons key value)))))))
           (interleave (json children-json
                        &key (start (start json)) (end (end json)))
             "Interleave CHILDREN-JSON with snippets of SOURCE-TEXT based on
             the start and end offsets of the ASTs."
             (if children-json
                 (iter (for child-json in children-json)
                       (for previous-json previous child-json)
                       (collect (nest (octets-to-string)
                                      (subseq source-octets
                                              (if previous-json
                                                  (end previous-json)
                                                  start)
                                              (start child-json)))
                                into result)
                       (collect child-json into result)
                       (finally (return (nest (append result)
                                              (list)
                                              (octets-to-string)
                                              (subseq source-octets
                                                      (end child-json) end)))))
                 (list (octets-to-string (subseq source-octets start end)))))
           (make-tree (json)
             "Create a python AST from the JSON AST representation."
             (make-instance 'python-ast
               :class (ast-class json)
               :annotations (annotations json)
               :children (nest (mapcar (lambda (child)
                                         (if (listp child)
                                             (make-tree child)
                                             child)))
                               (interleave json)
                               (collect-children json)))))
    (make-tree (python-astdump obj source-text))))

(defun line-offsets (source-text)
  "Return a hash table mapping line numbers (1 indexed) to an association
list of :start, :end-parsed where :start is the beginning byte offset of a
line in SOURCE-TEXT and :end-parsed is the byte offset of the last parsed
(non-whitespace and non-comment) character in the line."
  (let ((ht (make-hash-table))
        (trailing-ws-or-comment (create-scanner "(\\s+|\\s*#.*)$")))
    (iter (for (i line) in (indexed (split-sequence #\Newline source-text)))
          (for prev previous line)
          (let* ((start (if (aget :start (gethash i ht))
                            (+ (aget :start (gethash i ht))
                               (length (string-to-octets prev))
                               1)
                            0))
                 (end-parsed (nest (+ start)
                                   (length)
                                   (string-to-octets)
                                   (regex-replace trailing-ws-or-comment
                                                  line
                                                  ""))))
            (setf (gethash (1+ i) ht)
                  (list (cons :start start)
                        (cons :end-parsed end-parsed))))
          (finally (return ht)))))


;;; Python mutation
(define-mutation python-mutation (parseable-mutation) ()
  (:documentation
   "Specialization of the mutation interface for Python software objects."))


;;; Methods common to all software objects
(defmethod phenome ((obj python) &key (bin (temp-file-name)))
  "Create a phenotype of the python software OBJ.  In this case, override
the phenome method to output the genome of OBJ to BIN as python
is not a compiled language.

* OBJ object to create a phenome for
* BIN location where the phenome will be created on the filesystem"
  (to-file obj bin)
  (values bin 0 nil nil nil))

(defmethod get-parent-full-stmt ((obj python) (ast python-ast))
  (cond ((member (ast-class ast) +stmt-ast-classes+) ast)
        (t (get-parent-full-stmt obj (get-parent-ast obj ast)))))

(defmethod rebind-vars ((ast python-ast)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (if (eq (ast-class ast) :Name)
      (copy ast :id (rebind-vars (ast-annotation ast :id)
                                 var-replacements
                                 fun-replacements)
                :children (mapcar {rebind-vars _
                                               var-replacements
                                               fun-replacements}
                                  (ast-children ast)))
      (let ((rebound-children
             (mapcar (lambda (c)
                       (cond ((stringp c) c)
                             (t (rebind-vars c var-replacements
                                             fun-replacements))))
                     (ast-children ast))))
        (if (equalp rebound-children (ast-children ast))
            ast
            (copy ast :children rebound-children)))))

(defmethod enclosing-scope ((obj python) (ast python-ast))
  "Return the enclosing scope of AST in OBJ.
OBJ python software object
AST ast to return the enclosing scope for"
  (or (find-if (lambda (parent)
                 (or ;; Normal case: AST is a member of a class
                     ;; of ASTs defining a new scope.
                     (member (ast-class parent)
                             (list :FunctionDef
                                   :AsyncFunctionDef
                                   :ClassDef
                                   :For
                                   :AsyncFor
                                   :While
                                   :With
                                   :AsyncWith
                                   :Try
                                   :Lambda
                                   :ExceptHandler))
                     ;; Special case: For if statements, we do not
                     ;; want to create a new scope for elif clauses.
                     ;; The (or ...) below returns nil for elif clauses.
                     (and (eq (ast-class parent) :If)
                          (or (not (eq (ast-class ast) :If))
                              (starts-with-subseq "if" (source-text ast))))))
               (cdr (get-parent-asts obj ast)))
      (genome obj)))

(defmethod scopes ((obj python) (ast python-ast))
  "Return lists of variables in each enclosing scope of AST.
Each variable is represented by an alist containing :NAME, :DECL, and :SCOPE.
OBJ javascript software object
AST ast to return the scopes for"
  (labels ((else-clause-p (ast)
             "Return T if AST represents the start of an else clause."
             (nest (starts-with-subseq "else")
                   (trim-whitespace)
                   (source-text ast)))
           (in-same-if-clause (ast1 ast2
                               &aux (parent1 (get-parent-ast obj ast1))
                                 (parent2 (get-parent-ast obj ast2)))
             "Return T if AST1 and AST2 appear in the same if statement
             clause (the body or the else)."
             (if (and parent1 parent2 (eq parent1 parent2)
                      (eq (ast-class parent1) :If))
                 (let ((else-path (nest (remove-if #'null)
                                        (append (ast-path obj parent1))
                                        (list)
                                        (position-if #'else-clause-p)
                                        (ast-children parent1))))
                   (equal (path-later-p (ast-path obj ast1) else-path)
                          (path-later-p (ast-path obj ast2) else-path)))
                 t))
           (contains-scope-var-p (ast)
             "Return T if AST contains a variable declaration or assignment."
             (member (ast-class ast) (list :Arguments :Global :NonLocal
                                           :Assign :AnnAssign)))
           (get-lhs-names (obj assignment)
             "Return all NAME ASTs on the left-hand-side of ASSIGNMENT."
             (nest (remove-if-not (lambda (ast)
                                    (and (eq (ast-class ast) :Name)
                                         (eq (ast-annotation ast :ctx) :store))))
                   (get-children obj assignment)))
           (build-scope-alist (obj scope ast)
             "Return an alist containing :name, :decl, and :scope for the
             variable in AST."
             (mapcar (lambda (name)
                       `((:name . ,name)
                         (:decl . ,ast)
                         (:scope . ,scope)))
                     (cond ((member (ast-class ast) (list :Assign :AnnAssign))
                            (mapcar (lambda (name)
                                      (ast-annotation name :id))
                                    (get-lhs-names obj ast)))
                           ((eq (ast-class ast) :Arguments)
                            (mapcar (lambda (arg)
                                      (ast-annotation arg :arg))
                                    (get-immediate-children obj ast)))
                           ((member (ast-class ast) (list :Global :NonLocal))
                            (ast-annotation ast :names)))))
           (remove-duplicate-names (scope)
             "Remove vars with duplicate names in SCOPE."
             (remove-duplicates scope
                                :key {aget :name}
                                :test #'equal
                                :from-end t))
           (scopes-helper (obj ast)
             "Helper function performing the recursive step of searching
             enclosing scopes for variables."
             (when (not (eq :Module (ast-class ast)))
               (let ((scope (enclosing-scope obj ast)))
                 (cons (nest (reverse)
                             ; remove duplicate names, taking the
                             ; first assignment
                             (remove-duplicate-names)
                             ; build result
                             (mappend {build-scope-alist obj scope})
                             ; remove ASTs where no variable
                             ; declaration/assignment found
                             (remove-if-not #'contains-scope-var-p)
                             ; collect ASTs prior to AST
                             (iter (for c in (get-immediate-children obj scope))
                                   (when (and (in-same-if-clause ast c)
                                              (path-later-p (ast-path obj ast)
                                                            (ast-path obj c)))
                                     (collect c))))
                       (scopes-helper obj scope)))))
           (remove-vars-in-enclosing-scopes (scope enclosing-scopes)
             "Remove vars in SCOPE which appear in ENCLOSING-SCOPES."
             (remove-if (lambda (var &aux (name (aget :name var)))
                          (some {find-if [{equal name}{aget :name}]}
                                enclosing-scopes))
                        scope)))
    ;; After using scopes-helper to retrieve the variables in each scope,
    ;; remove variables in the current scope which appear in an enclosing
    ;; scope as a final post-processing step.
    (iter (for (current-scope . enclosing) on (scopes-helper obj ast))
          (collect (remove-vars-in-enclosing-scopes current-scope enclosing)))))

(defmethod get-unbound-vals ((obj python) (ast python-ast))
  "Return all variables used (but not defined) within AST.
* OBJ python software object containing AST
* AST ast to retrieve unbound variables within"
  (labels ((call-name-p (parents name)
             (find-if (lambda (parent)
                        (match parent
                               ;; free function
                               ((ast
                                 (ast-class :Call)
                                 (ast-children (list* (type string)
                                                      (guard ast (eq ast name))
                                                      _)))
                                t)
                               ;; method
                               ((ast
                                 (ast-class :Call)
                                 (ast-children
                                   (list* (type string)
                                          (ast
                                           (ast-class :Attribute)
                                           (ast-children
                                             (list* (type string)
                                                    (guard ast (eq ast name))
                                                    _))))))
                                t)))
                      parents))
           (bound-name-p (parent)
             (member (ast-class parent)
                     (list :FunctionDef
                           :AsyncFunctionDef
                           :ClassDef)))
           (get-unbound-vals-helper (obj parents ast)
             (remove-duplicates
               (apply #'append
                      (when (and (eq (ast-class ast) :Name)
                                 (not (or (bound-name-p (car parents))
                                          (call-name-p parents ast))))
                        (list (cons :name (source-text ast))))
                      (mapcar {get-unbound-vals-helper obj (cons ast parents)}
                              (get-immediate-children obj ast)))
               :test #'equal)))
    (get-unbound-vals-helper obj (get-parent-asts obj ast) ast)))

(defmethod get-unbound-funs ((obj python) (ast python-ast)
                             &aux (children (get-immediate-children obj ast))
                               (callee (first children)))
  "Return all functions used (but not defined) within AST.  The returned
value will be of the form (list FUNCTION-ATTRS) where FUNCTION-ATTRS is a
list of form (FUNCTION-NAME UNUSED UNUSED NUM-PARAMS).

* OBJ python software object containing AST
* AST ast to retrieve unbound functions within"
  (remove-duplicates
    (apply #'append
           (when (eq (ast-class ast) :Call)
             (cond ((eq (ast-class callee) :Name)
                    ;; Free function call
                    (list (list (source-text callee)
                                nil nil (length (cdr children)))))
                  ((eq (ast-class callee) :Attribute)
                   ;; Member Function call
                   (list (list (ast-annotation callee :attr)
                               nil nil (length (cdr children)))))
                  (t nil)))
          (mapcar {get-unbound-funs obj}
                  (get-immediate-children obj ast)))
    :test #'equal))


;;; Implement the generic format-genome method for python objects.
(defmethod format-genome ((obj python) &key)
  "Format the genome of OBJ using YAPF (Yet Another Python Formatter)."
  (yapf obj))
