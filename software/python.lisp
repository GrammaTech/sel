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
;;; Every AST in a @code{python} software object is of a subclass
;;; representing its AST type (e.g. FunctionDef, Assign).  Additional
;;; fields in the python AST representation can be found in an association
;;; list on the AST's annotations field.  Unfortunately, these additional
;;; fields do not appear in the official documentation, although an external
;;; @url{https://greentreesnakes.readthedocs.io/en/latest/, AST reference}
;;; contains some details.
;;;
;;; @texi{python}
(uiop:define-package :software-evolution-library/software/python
  (:nicknames :sel/software/python :sel/sw/python)
  (:use :gt/full
        :babel
        :cl-json
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/parseable
        :software-evolution-library/software/non-homologous-parseable
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting)
  (:import-from :cffi :translate-camelcase-name)
  (:import-from :functional-trees :path-later-p)
  (:export :python
           :python-mutation
           :python-ast
           :collect-var-uses))
(in-package :software-evolution-library/software/python)
(in-readtable :curry-compose-reader-macros)

(define-software python (non-homologous-parseable file-w-attributes) ()
  (:documentation "Python software representation."))


;;; Javascript ast data structures
(defconst +python-children+
  '(((:module :interactive) (:body . 0))
    ((:expression) (:body . 1))
    ((:async-function-def :function-def) (:decorator-list . 0)
                                         (:args . 1)
                                         (:returns . 1)
                                         (:body . 0))
    ((:class-def) (:decorator-list . 0)
                  (:bases . 0)
                  (:keywords . 0)
                  (:body . 0))
    ((:return) (:value . 1))
    ((:delete) (:targets . 0))
    ((:assign) (:targets . 0)
               (:value . 1))
    ((:aug-assign) (:target . 1)
                   (:value . 1))
    ((:ann-assign) (:target . 1)
                   (:annotation . 1)
                   (:value . 1))
    ((:async-for :for) (:target . 1)
                       (:iter . 1)
                       (:body . 0)
                       (:orelse . 0))
    ((:if :while) (:test . 1)
                  (:body . 0)
                  (:orelse . 0))
    ((:async-with :with) (:items . 0)
                         (:body . 0))
    ((:raise) (:exc . 1)
              (:cause . 1))
    ((:try) (:body . 0)
            (:handlers . 0)
            (:orelse . 0)
            (:finalbody . 0))
    ((:assert) (:test . 1)
               (:msg . 1))
    ((:bool-op) (:values . 0))
    ((:bin-op) (:left . 1)
               (:right . 1))
    ((:unary-op) (:operand . 1))
    ((:lambda) (:args . 1)
               (:body . 1))
    ((:if-exp) (:body . 1)
               (:test . 1)
               (:orelse . 1))
    ((:dict) (:keys . 0)
             (:values . 0))
    ((:list-comp :set-comp :generator-exp) (:elt . 1)
                                           (:generators . 0))
    ((:dict-comp) (:key . 1)
                  (:value . 1)
                  (:generators . 0))
    ((:await :yield :yield-from :attribute :expr :starred :index)
     (:value . 1))
    ((:compare) (:left . 1)
                (:comparators . 0))
    ((:call) (:func . 1)
             (:args . 0)
             (:keywords . 0))
    ((:subscript) (:value . 1)
                  (:slice . 1))
    ((:list :set :tuple) (:elts . 0))
    ((:slice) (:lower . 1) (:upper . 1) (:step . 1))
    ((:ext-slice) (:dims . 0))
    ((:comprehension) (:target . 1)
                      (:iter . 1)
                      (:ifs . 0))
    ((:except-handler) (:type . 1)
                       (:body . 0))
    ((:arguments) (:posonlyargs . 0)
                  (:args . 0)
                  (:defaults . 0)
                  (:vararg . 1)
                  (:kwonlyargs . 0)
                  (:kw-defaults . 0)
                  (:kwarg . 1))
    ((:arg) (:annotation . 1))
    ((:keyword) (:value . 1))
    ((:withitem) (:context-expr . 1)
                 (:optional-vars . 1))
    ((:import
      :import-from
      :global
      :nonlocal
      :pass
      :break
      :continue
      :constant
      :name
      :named-expr
      :joined-str)))
  "Definition of Python classes and child slots.")

(eval-always
 (defclass python-ast (non-homologous-ast) ()
   (:documentation "Class of Python ASTs.")))

(progn
  #.`(progn ,@(mappend {expand-ast-classes 'python-ast 'py}
                       +python-children+)))
(export-always
 (mapcar {symbol-cat 'py}
         (mappend «append #'first [{mapcar #'car} #'cdr]»
                  +python-children+)))

(define-constant +stmt-ast-types+
  '(py-module py-function-def py-async-function-def py-class-def py-return
    py-delete py-assign py-aug-assign py-ann-assign py-for py-async-for
    py-while py-if py-with py-async-with py-raise py-try
    py-except-handler py-assert py-import py-import-from py-global
    py-nonlocal py-expr py-pass py-break py-continue)
  :test #'equal
  :documentation "Stmt AST types for python.")

(define-constant +ast-types-with-larger-child-spans+
  '(py-arg py-generator-exp py-starred py-class-def py-async-function-def
    py-function-def)
  :test #'equal
  :documentation "AST types which may have child spans larger than span
given by python in the line, col attributes.")

(define-constant +ast-types-with-unsorted-children+
  '(py-dict py-call py-async-function-def py-function-def py-arguments)
  :test #'equal
  :documentation "AST types which may have children which are not
in textual (sorted) order.")

(defmethod ast-type-to-rebind-p ((ast python-ast)) nil)
(defmethod ast-type-to-rebind-p ((ast py-name)) t)
(defmethod ast-annotation-to-rebind ((ast py-name)) :id)


;;; Python parsing
(defun python-astdump (source-text)
  "Invoke the python-astdump script to dump ASTs in SOURCE-TEXT in JSON format."
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
                                :operation :parse))))))

(defmethod convert ((to-type (eql 'python-ast)) (string string)
                    &key &allow-other-keys
                    &aux (string-octets (string-to-octets string))
                      (line-offsets (line-offsets string)))
  (labels
      ((safe-subseq (start end)
         "Return STRING-OCTETS in the range [START, END) as a string
         or an empty string if the offsets are invalid."
         (if (< start end)
             (octets-to-string (subseq string-octets start end))
             ""))
       (offset (line col)
         "Return the offset into SOURCE-OCTETS for the given LINE and COL."
         (+ col (aget :start (gethash line line-offsets))))
       (last-parsed-char-in-line-p (line col)
         "Return T if LINE and COL represent the last parsed (non-whitespace,
         non-comment) byte in a line of STRING-OCTETS."
         (= (offset line col)
            (aget :end-parsed (gethash line line-offsets))))
       (normalized-offset (ast line col)
         "Return the offset into STRING-OCTETS for the given LINE and COL
         with corrections to include the trailing newline for stmt ASTs."
         (if (and (member (type-of ast) +stmt-ast-types+)
                  (last-parsed-char-in-line-p line col)
                  (gethash (1+ line) line-offsets))
             (offset (1+ line) 0)
             (offset line col)))
       (start (ast)
         "Return the start offset into SOURCE-OCTETS from the AST
         representation."
         (cond ((null ast) nil)
               ((typep ast 'py-module)
                0)
               ((and (null (ast-annotation ast :lineno))
                     (null (ast-annotation ast :col-offset)))
                (start (car (remove nil (children ast)))))
               ((member (type-of ast) +ast-types-with-larger-child-spans+)
                (min (or (start (car (remove nil (children ast))))
                         most-positive-fixnum)
                     (normalized-offset ast
                                       (ast-annotation ast :lineno)
                                       (ast-annotation ast :col-offset))))
               (t (normalized-offset ast
                                     (ast-annotation ast :lineno)
                                     (ast-annotation ast :col-offset)))))
       (end (ast)
         "Return the end offset into SOURCE-OCTETS from the AST
         representation."
         (cond ((null ast) nil)
               ((typep ast 'py-module)
                (length string-octets))
               ((and (null (ast-annotation ast :lineno))
                     (null (ast-annotation ast :col-offset)))
                (end (lastcar (remove nil (children ast)))))
               ((member (type-of ast) +ast-types-with-larger-child-spans+)
                (max (or (end (lastcar (remove nil (children ast))))
                         most-negative-fixnum)
                     (normalized-offset ast
                                        (ast-annotation ast :end-lineno)
                                        (ast-annotation ast :end-col-offset))))
               (t (normalized-offset ast
                                     (ast-annotation ast :end-lineno)
                                     (ast-annotation ast :end-col-offset)))))
       (start-and-end-p (ast)
         "Return T if AST contains both a start and end offset."
         (and (start ast) (end ast)))
       (remove-children-without-start-end-offsets (ast)
         "Destructively remove children of AST which do not have
         start and end offsets populated."
         (mapc (lambda (slot)
                 (destructuring-bind (name . arity) slot
                   (let ((value (slot-value ast name)))
                     (if (= 1 arity)
                         (setf (slot-value ast name)
                               (if (start-and-end-p value) value nil))
                         (setf (slot-value ast name)
                               (remove-if-not #'start-and-end-p value))))))
               (child-slots ast)))
       (add-child-order-annotation (ast)
         "Destructively modify AST, adding a :child-order annotation
         defining the textual order of the children for those AST
         types whose children are not in textual order by default."
         (when (member (type-of ast) +ast-types-with-unsorted-children+)
           (let* ((children (remove nil (children ast)))
                  (sorted-children (sort children #'< :key #'start)))
             (setf (slot-value ast 'annotations)
                   (if (equal children sorted-children)
                       (ast-annotations ast)
                       (cons (cons :child-order
                                   (mapcar {position _ ast} sorted-children))
                             (ast-annotations ast)))))))
       (normalized-children (ast)
         "Return the sorted, non-nil children of AST after destructively
         modifying AST to remove children without start and end offsets
         and adding a :child-order annotation to those AST types whose
         children are not in textual order by default."
         (remove-children-without-start-end-offsets ast)
         (add-child-order-annotation ast)
         (sorted-children ast))
       (ranges (children from to)
         "Return the offsets of the source text ranges between CHILDREN."
         (iter (for child in children)
               (for prev previous child)
               (if prev
                   (collect (cons (end prev) (start child))
                            into ranges)
                   (collect (cons from (start child))
                            into ranges))
               (finally (return (append ranges
                                        (list (cons (end child) to)))))))
       (w/interleaved-text (ast from to
                            &aux (children (normalized-children ast)))
         "Destructively modify AST to populate the INTERLEAVED-TEXT
         field with the source text to be interleaved between the
         children of AST."
         (if children
             (progn
               (setf (slot-value ast 'interleaved-text)
                     (mapcar (lambda (range)
                               (destructuring-bind (from . to) range
                                 (safe-subseq from to)))
                             (ranges children from to)))
               (mapc (lambda (child)
                       (w/interleaved-text child (start child) (end child)))
                     children))
             (setf (slot-value ast 'interleaved-text)
                   (list (safe-subseq (start ast) (end ast)))))
         (setf (slot-value ast 'annotations)
               (adrop '(:lineno :col-offset :end-lineno :end-col-offset)
                      (slot-value ast 'annotations)))
         ast))
   (w/interleaved-text (convert to-type (python-astdump string))
                       0 (length string-octets))))

(defmethod convert ((to-type (eql 'python-ast)) (spec null)
                    &key &allow-other-keys)
  nil)

(defmethod convert :around ((to-type (eql 'python-ast)) (spec list)
                            &rest args &key &allow-other-keys)
  "Wrapper around convert to perform various fixups on the SPEC from python."
  ;; Represent :ctx, :op, and :ops fields as annotations instead
  ;; of AST children.  Additionally, drop nil annotations."
  (setf spec
        (iter (for (key . value) in spec)
              (cond ((eq key :ctx)
                     (collect (cons key (nest (make-keyword)
                                              (string-upcase)
                                              (aget :class value)))))
                    ((eq key :op)
                     (collect (cons key (aget :class value))))
                    ((eq key :ops)
                     (collect (cons key (mapcar {aget :class} value))))
                    ((not (null value))
                     (collect (cons key value))))))

  (apply #'call-next-method to-type spec args))

(defmethod convert ((to-type (eql 'python-ast)) (spec list)
                    &key &allow-other-keys)
  "Create a PYTHON AST from the SPEC (specification) list."
  (convert-helper spec 'py 'python-ast +python-children+))

(defmethod convert ((to-type (eql 'python-ast)) (spec python-ast)
                    &key &allow-other-keys)
  "Pass thru an existing PYTHON AST.  This is useful in manual AST creation."
  spec)

(defmethod parse-asts ((obj python) &optional (source (genome-string obj)))
  (convert 'python-ast source))

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
  (cond ((member (type-of ast) +stmt-ast-types+) ast)
        (t (get-parent-full-stmt obj (get-parent-ast obj ast)))))

(defmethod enclosing-scope ((obj python) (ast python-ast))
  "Return the enclosing scope of AST in OBJ.
OBJ python software object
AST ast to return the enclosing scope for"
  (or (find-if (lambda (parent)
                 (or ;; Normal case: AST is a member of a class
                     ;; of ASTs defining a new scope.
                     (member (type-of parent)
                             (list 'py-function-def
                                   'py-async-function-def
                                   'py-class-def
                                   'py-for
                                   'py-async-for
                                   'py-while
                                   'py-with
                                   'py-async-with
                                   'py-try
                                   'py-lambda
                                   'py-except-handler))
                     ;; Special case: For if statements, we do not
                     ;; want to create a new scope for elif clauses.
                     ;; The (or ...) below returns nil for elif clauses.
                     (and (typep parent 'py-if)
                          (or (not (typep ast 'py-if))
                              (starts-with-subseq "if" (source-text ast))))))
               (cdr (get-parent-asts obj ast)))
      (genome obj)))

(defmethod scopes ((obj python) (ast python-ast))
  "Return lists of variables in each enclosing scope of AST.
Each variable is represented by an alist containing :NAME, :DECL, and :SCOPE.
OBJ python software object
AST ast to return the scopes for"
  (labels ((in-same-if-clause (ast1 ast2 &aux (p (get-parent-ast obj ast1)))
             "Return T if AST1 and AST2 appear in the same if statement
             clause (the body or the else)."
             (if (and (typep p 'py-if) (eq p (get-parent-ast obj ast2)))
                 (or (= 2 (length (intersection (list ast1 ast2) (py-body p))))
                     (= 2 (length (intersection (list ast1 ast2) (py-orelse p)))))
                 t))
           (contains-scope-var-p (ast)
             "Return T if AST contains a variable declaration or assignment."
             (member (type-of ast) (list 'py-arguments 'py-global 'py-nonlocal
                                         'py-assign 'py-ann-assign)))
           (get-lhs-names (assignment)
             "Return all NAME ASTs on the left-hand-side of ASSIGNMENT."
             (nest (remove-if-not (lambda (ast)
                                    (and (typep ast 'py-name)
                                         (eq (ast-annotation ast :ctx) :store))))
                   (child-asts assignment :recursive t)))
           (build-scope-alist (scope ast)
             "Return an alist containing :name, :decl, and :scope for the
             variable in AST."
             (mapcar (lambda (name)
                       `((:name . ,name)
                         (:decl . ,ast)
                         (:scope . ,scope)))
                     (etypecase ast
                       ((or py-assign py-ann-assign)
                        (mapcar {ast-annotation _ :id}
                                (get-lhs-names ast)))
                       (py-arguments
                        (mapcar {ast-annotation _ :arg}
                                (sorted-children ast)))
                       ((or py-global py-nonlocal)
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
             (unless (typep ast 'py-module)
               (let ((scope (enclosing-scope obj ast)))
                 (cons (nest (reverse)
                             ; remove duplicate names, taking the
                             ; first assignment
                             (remove-duplicate-names)
                             ; build result
                             (mappend {build-scope-alist scope})
                             ; remove ASTs where no variable
                             ; declaration/assignment found
                             (remove-if-not #'contains-scope-var-p)
                             ; collect ASTs prior to AST
                             (iter (for c in (remove nil (children scope)))
                                   (when (and (in-same-if-clause ast c)
                                              (path-later-p obj ast c))
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
  (labels ((call-name-p (parent name)
             (when (typep parent 'py-call)
               (let ((func (py-func parent)))
                 (typecase func
                   ;; free function
                   (py-name (eq func name))
                   ;; method call
                   (py-attribute (eq (py-value func) name))))))
           (bound-name-p (parent)
             (member (type-of parent)
                     (list 'py-function-def
                           'py-async-function-def
                           'py-class-def)))
           (get-unbound-vals-helper (obj parent ast)
             (remove-duplicates
               (apply #'append
                      (when (and (typep ast 'py-name)
                                 (not (or (bound-name-p parent)
                                          (call-name-p parent ast))))
                        (list (cons :name (source-text ast))))
                      (mapcar {get-unbound-vals-helper obj ast}
                              (children ast)))
               :test #'equal)))
    (get-unbound-vals-helper obj (get-parent-ast obj ast) ast)))

(defmethod get-unbound-funs ((obj python) (ast python-ast)
                             &aux (children (remove nil (children ast)))
                               (callee (first children)))
  "Return all functions used (but not defined) within AST.  The returned
value will be of the form (list FUNCTION-ATTRS) where FUNCTION-ATTRS is a
list of form (FUNCTION-NAME UNUSED UNUSED NUM-PARAMS).

* OBJ python software object containing AST
* AST ast to retrieve unbound functions within"
  (remove-duplicates
    (apply #'append
           (when (typep ast 'py-call)
             (cond ((typep callee 'py-name)
                    ;; Free function call
                    (list (list (source-text callee)
                                nil nil (length (cdr children)))))
                  ((typep callee 'py-attribute)
                   ;; Member Function call
                   (list (list (ast-annotation callee :attr)
                               nil nil (length (cdr children)))))
                  (t nil)))
          (mapcar {get-unbound-funs obj} children))
    :test #'equal))

(-> collect-var-uses (python python-ast) list)
(defun collect-var-uses (obj ast)
  "Collect uses of AST in OBJ."
  ;;TODO: expand this to work inside classes.
  ;;      This may require significat modifications to acount
  ;;      for 'self' variables.
  (labels ((same-name-p (ast name)
             "Return T if AST represents an AST that contains the same
              name as NAME."
             (typecase ast
               (py-arg (equal (ast-annotation ast :arg) name))
               (py-name (equal (ast-annotation ast :id) name))
               (py-global (find-if {equal name} (ast-annotation ast :names)))
               (py-nonlocal
                (find-if {equal name} (ast-annotation ast :names)))))
           (find-name-in-scopes (name scopes)
             "Search SCOPES for a variable named NAME."
             (find-if
              (lambda (scope)
                (find-if
                 (lambda (var-info)
                   (equal name (aget :name var-info)))
                 scope))
              scopes))
           (get-analysis-set (scope first-occurrence name binding-class)
             "Collect all relevant asts with NAME in SCOPE that occur at
              or before FIRST-OCCURRENCE. BINDING-CLASS determines whether
              'global' or 'nonlocal' should be used to determine if NAME
              is in-scope for assignments."
             ;; Currently, py-name, py-arg, and either py-nonlocal or py-global
             ;; are relevant.
             (remove-if-not
              (lambda (ast)
                (or (and (same-name-p ast name)
                         ;; TODO: this check will likely be superfluous
                         ;;       if #'scopes is changed.
                         (path-later-p obj ast first-occurrence))
                    (eq ast first-occurrence)))
              (collect-if
               (lambda (ast)
                 (member
                  (type-of ast)
                  `(py-name py-arg ,binding-class)))
               scope)))
           (find-var-uses (assorted-by-scope binding-class)
             "Search assorted-by-scope for usages of variables
              with the same name. BINDING-CLASS specifies whether
              the variable is global or local and provides the
              name of the class used for binding it to a scope."
             (iter
               (iter:with out-of-scope = nil)
               (iter:with local-var-p = (eq binding-class 'py-nonlocal))
               (for vars-in-scope in assorted-by-scope)
               (for scope = (enclosing-scope obj (car vars-in-scope)))
               ;; Prune any scope that occurs after the local binding
               ;; has been squashed.
               (when out-of-scope
                 (if (shares-path-of-p obj scope out-of-scope)
                     (next-iteration)
                     (setf out-of-scope nil)))
               (cond
                 ((find-if {typep _ 'py-arg} vars-in-scope)
                  ;; All nested scopes are out-of-scope.
                  (and local-var-p (setf out-of-scope scope)))
                 ((find-if {typep _ binding-class} vars-in-scope)
                  (collect vars-in-scope))
                 ((find-if [{typep _ 'py-assign} {get-parent-full-stmt obj}]
                           vars-in-scope)
                  ;; All nested scopes are out-of-scope.
                  (and local-var-p (setf out-of-scope scope)))))))
    ;; TODO: expand this to accept more than just ast's with an :id
    ;;       annotation. This is currently just py-name. py-arg
    ;;       should work as soon as the name can be retrieved.
    (let* ((name (ast-annotation ast :id))
           (var-info (find-name-in-scopes name (scopes obj ast)))
           (scope (or (aget :scope var-info) (enclosing-scope obj ast)))
           ;; The path will be nil when given the global scope.
           (binding-class (if (ast-path obj scope)
                              'py-nonlocal
                              'py-global))
           (assorted-by-scope
             (assort (get-analysis-set scope (or (aget :decl var-info)
                                                 (get-parent-full-stmt obj ast))
                                       name binding-class)
                     :key {enclosing-scope obj})))
      ;; NOTE: the following comment will become relevant when the prior
      ;;       TODO is addressed.
      ;; Don't pass in the first scope of assorted-by-scope as the first
      ;; one may include a py-arg which find-var-uses would misinterpret
      ;; as squashing the binding's scope.
      (flatten (cons (car assorted-by-scope)
                     (find-var-uses (cdr assorted-by-scope)
                                    binding-class))))))


;;; Implement the generic format-genome method for python objects.
(defmethod format-genome ((obj python) &key)
  "Format the genome of OBJ using YAPF (Yet Another Python Formatter)."
  (yapf obj))
