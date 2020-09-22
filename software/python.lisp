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
           :collect-var-uses
           :identical-name-p
           :get-asts-in-namespace))
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
(defmethod ast-type-to-rebind-p ((ast py-arg)) t)
(defmethod ast-annotation-to-rebind ((ast py-arg)) :arg)


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
    (process-indentation
     (w/interleaved-text (convert to-type (python-astdump string))
                         0 (length string-octets)))))

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
                                   'py-lambda))))
               (cdr (get-parent-asts obj ast)))
      (genome obj)))

(defmethod scopes ((obj python) (ast python-ast)
                   &aux (enclosing-scope (enclosing-scope obj ast)))
  "Return lists of variables in each enclosing scope of AST.
Each variable is represented by an alist containing :NAME, :DECL, and :SCOPE.
OBJ python software object
AST ast to return the scopes for"
  ;; TODO: confirm that this, more or less, covers what's needed
  ;;       with classes initially.
  (labels ((build-alist (ast name scope)
             "Return an alist containing :name, :decl, and :scope for the
             variable in AST."
             `((:decl . ,ast)
               (:name . ,name)
               (:scope . ,scope)))
           (find-nonlocal-binding (name enclosing-scope)
             "Find the nonlocal binding for NAME in ENCLOSING-SCOPE."
             (build-alist
              (find-if (lambda (ast)
                         (typecase ast
                           (py-nonlocal
                            (when (and
                                   (find-if
                                    {equalp name} (ast-annotation ast :names))
                                   (not
                                    (eq enclosing-scope
                                        (enclosing-scope obj enclosing-scope))))
                              (find-nonlocal-binding
                               name (enclosing-scope obj enclosing-scope))))
                           (py-assign
                            (find-if
                             (lambda (target)
                               (equalp name (ast-annotation target :id)))
                             (py-targets ast)))
                           (py-ann-assign
                            (equalp name (ast-annotation (py-target ast) :id)))
                           (py-arguments
                            (find-if
                             (lambda (arg)
                               (equalp name (ast-annotation arg :arg)))
                             (py-args (py-args ast))))))
                       (remove nil (children enclosing-scope)))
              name enclosing-scope))
           (find-global-binding (name &aux (genome (genome obj)))
             "Find the global binding for NAME in ENCLOSING-SCOPE."
             (build-alist
              (find-if (lambda (ast)
                         (typecase ast
                           (py-assign
                            (find-if
                             (lambda (target)
                               (equalp name (ast-annotation target :id)))
                             (py-targets ast)))
                           (py-ann-assign
                            (equalp name (ast-annotation (py-target ast) :id)))
                           (py-class-def
                            (equalp name (ast-annotation ast :name)))))
                       (remove nil (children genome)))
              name genome))
           (find-function-bindings (scope)
             "Find the function bindings that occur in scope."
             (mapcar
              (lambda (ast)
                (build-alist ast (ast-annotation ast :name) scope))
              (remove-if-not
               (lambda (ast)
                 (member
                  (type-of ast) '(py-function-def 'py-async-function-def)))
               (remove nil (children scope)))))
           (find-import-bindings (scope)
             "Find the import bindings that occur in scope."
             (mappend
              (lambda (ast)
                (mapcar
                 (lambda (alist)
                   (build-alist ast (or (aget :asname alist)
                                        (aget :name alist))
                                scope))
                 (ast-annotation ast :names)))
              (remove-if-not (lambda (child)
                               (member (type-of child)
                                       '(py-import py-import-from)))
                             (remove nil (children scope)))))
           (find-local-bindings ()
             "Find local bindings in scope. Returns the py-name
              objects associated with the bindings."
             ;; NOTE: this doesn't correctly return bindings
             ;;       that occur based on control flow like with if-else
             ;;       statements.
             (remove-duplicates
              (mappend
               (lambda (assignment)
                 (typecase assignment
                   (py-assign
                    (let ((target (car (py-targets assignment))))
                      (typecase target
                        (py-tuple (py-elts target))
                        (py-name (list target)))))
                   (py-ann-assign
                    (list (py-target assignment)))))
               ;;Remove bindings after.
               (remove-if-not
                {path-later-p obj ast}
                ;; Only consider the assignments
                (remove-if-not
                 (lambda (ast)
                   (member (type-of ast) '(py-assign py-ann-assign)))
                 (get-asts-in-namespace obj enclosing-scope))))
              :test (lambda (ast1 ast2)
                      (identical-name-p ast1 ast2))
              :from-end t))
           (get-global-bindings ()
             "Get the global bindings in scope."
             (mappend (lambda (ast)
                        (mapcar #'find-global-binding
                                (ast-annotation ast :names)))
                      (remove-if-not
                       {typep _ 'py-global}
                       (get-asts-in-namespace obj enclosing-scope))))
           (get-nonlocal-bindings ()
             "Get the nonlocal bindings in scope."
             (mappend (lambda (ast)
                        (mapcar
                         {find-nonlocal-binding
                          _ (enclosing-scope obj enclosing-scope)}
                         (ast-annotation ast :names)))
                      (remove-if-not
                       {typep _ 'py-nonlocal}
                       (get-asts-in-namespace obj enclosing-scope))))
           (get-function-bindings
               (scope &aux (enclosing-scope (enclosing-scope obj scope))
                        (function-bindings (find-function-bindings scope)))
             "Get the function bindings available in scope."
             (if (eq scope enclosing-scope)
                 function-bindings
                 (append function-bindings
                         (get-function-bindings enclosing-scope))))
           (get-import-bindings
               (scope &aux (enclosing-scope (enclosing-scope obj scope))
                        (import-bindings (find-import-bindings scope)))
             "Get the import bindings available in scope."
             (if (eq scope enclosing-scope)
                 import-bindings
                 (append import-bindings
                         (get-import-bindings enclosing-scope))))
           (get-function-arguments ()
             "Get the arguments to the current function."
             (when-let ((args (and (typep enclosing-scope 'py-function-def)
                                   (py-args enclosing-scope))))
               (mapcar (lambda (ast)
                         (build-alist
                          ast (ast-annotation ast :arg) enclosing-scope))
                       (py-args args))))
           (get-local-bindings ()
             "Get the local bindings available in scope."
             ;; Remove bindings after
             (remove-if-not
              (lambda (binding-alist)
                (path-later-p obj ast (aget :decl binding-alist)))
              ;; build-alist
              (mapcar
               (lambda (name)
                 (build-alist
                  (get-parent-full-stmt obj name)
                  (ast-annotation name :id) enclosing-scope))
               (find-local-bindings))))
           (get-except-binding ()
             "Get the variable bound by an except clause.
              Note that this is a special case."
             (when-let* ((except-handler
                          (find-if-in-parents
                           {typep _ 'py-except-handler} obj ast))
                         (name (ast-annotation except-handler :name)))
               ;; Return as a list for the #'append.
               (list (build-alist except-handler name enclosing-scope))))
           (group-by-scope (bindings)
             "Group BINDINGS by scope."
             (assort bindings :key (lambda (alist) (aget :scope alist))))
           (sort-top->down (scopes)
             "Sort SCOPES from the top-most to the bottom-most."
             (sort scopes
                   (lambda (ast1 ast2)
                     (path-later-p obj ast2 ast1))
                   :key (lambda (list)
                          (aget :scope (car list))))))
    (sort-top->down
     (group-by-scope
      (remove-duplicates
       (remove-if
        #'null
        ;; NOTE: order of the append matters here for get-except-binding and
        ;;       get-local-bindings.
        (append (get-except-binding)
                (get-global-bindings)
                (get-nonlocal-bindings)
                (get-function-bindings enclosing-scope)
                (get-import-bindings enclosing-scope)
                (get-function-arguments)
                (get-local-bindings)))
       :test (lambda (alist1 alist2)
               (equal (aget :name alist1) (aget :name alist2)))
       :from-end t)))))

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

;;; TODO: move into parseable?
(-> find-if-in-scopes (function list) list)
(defun find-if-in-scopes (predicate scopes)
  (mapc
   (lambda (scope)
     (when-let ((return-value (find-if predicate scope)))
       (return-from find-if-in-scopes return-value)))
   scopes))

(defmethod get-function-from-function-call ((obj python) (ast python-ast))
  (match ast
    ((py-call
      (py-func
       (py-name (ast-annotations (assoc :id name)))))
     (when-let ((function-alist
                 (find-if-in-scopes
                  (lambda (scope)
                    (and (equal name (aget :name scope))
                         (typep (aget :decl scope) 'py-function-def)))
                  (scopes obj ast))))
       (aget :decl function-alist)))))

(defmethod map-arguments-to-parameters
    ((obj python) (funcall python-ast)
     &aux (function (get-function-from-function-call obj funcall)))
  (unless function
    ;; Exit early if the function isn't found to prevent errors.
    (return-from map-arguments-to-parameters nil))
  (let* ((fn-all-args (py-args function))
         (fn-args (py-args fn-all-args))
         (fn-defaults (py-defaults fn-all-args))
         (fn-kwarg (py-kwarg fn-all-args))
         (fn-vararg (py-vararg fn-all-args))
         (fn-psonly+args (append (py-posonlyargs fn-all-args) fn-args))
         (call-args (py-args funcall)))
    (labels ((same-arg-p (arg1 arg2)
               "Return T if ARG1 and ARG2 represent the same id."
               (equalp (ast-annotation arg1 :arg)
                       (ast-annotation arg2 :arg)))
             (get-positional-args-with-defaults ()
               "Return a list of the position parameters that have default
                values."
               (when-let ((position
                           (position-if {find #\=}
                                        (interleaved-text fn-all-args))))
                 (drop (1- position) fn-psonly+args)))
             (get-used-defaults (defaults offset)
               "Return a list of positional arguments that have potentially
                been used with the default value. Note that the parameter
                could still be used as a keyword later in the call arguments."
               (mapcar #'cons (drop offset defaults) (drop offset fn-defaults)))
             (get-vararg ()
               "Return a list that maps the vararg to a tuple of its values."
               (when fn-vararg
                 (list (cons fn-vararg
                             (create-tuple
                              (drop (length fn-psonly+args) call-args))))))
             (get-positional-mappings ()
               "Map all positional arguments and defaults to their respective
                parameters."
               (let ((provided (mapcar #'cons fn-psonly+args call-args))
                     (defaults (get-positional-args-with-defaults)))
                 (append provided
                         (get-used-defaults
                          defaults
                          ;; number of defaults provided
                          (- (length provided) (- (length fn-psonly+args)
                                                  (length defaults))))
                         (get-vararg))))
             (create-kwarg-dict (keyword-args)
               "Return an AST that represents the keyword-arg dictionary."
               (create-dictionary
                (mapcar (lambda (keyword)
                          (format nil "\"~a\"" (ast-annotation keyword :arg)))
                        keyword-args)
                (mapcar #'py-value keyword-args)))
             (map-call-keys (&aux keyword-args)
               "Return an alist mapping provided keyword arguments to their
                parameters."
               (append
                (iter
                  (for key in (py-keywords funcall))
                  ;; NOTE: there shouldn't be any nils here unless
                  ;;       the function call is incorrect
                  (cond-let result
                    ((find-if {same-arg-p key}
                              (append (py-kwonlyargs fn-all-args) fn-args))
                     (collect (cons result (py-value key))))
                    (fn-kwarg (push key keyword-args))))
                (when fn-kwarg
                  (list (cons fn-kwarg (create-kwarg-dict keyword-args))))))
             (get-default-keys ()
               "Return an alist mapping keywords with their default
                parameters."
               (let ((keyword-offset (+ (length fn-psonly+args)
                                        (if fn-vararg 1 0)
                                        (length fn-defaults))))
                 (mapcon
                  (lambda (children interleaved)
                    (when (find #\= (car interleaved))
                      (list (cons (car children) (cadr children)))))
                  (drop keyword-offset (sorted-children fn-all-args))
                  (drop (1+ keyword-offset) (interleaved-text fn-all-args)))))
             (get-key-mappings ()
               "Map all keyword arguments to their respective parameters."
               (let ((mapped-keys (map-call-keys)))
                 (append
                  mapped-keys
                  (mappend (lambda (key-with-default)
                             (unless (aget (car key-with-default) mapped-keys)
                               (list key-with-default)))
                           (get-default-keys))))))
      ;; Switch the mapping around to be argument->parameter
      (mapcar
       (lambda (mapping)
         (cons (cdr mapping) (car mapping)))
       ;; Positional defaults may exist in the list even when the positional
       ;; was used as a keyword. Remove duplicates to fix this.
       (remove-duplicates
        (append (get-positional-mappings) (get-key-mappings))
        :test (lambda (cons1 cons2)
                (same-arg-p (car cons1) (car cons2))))))))


;;; Helper functions
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
             (mappend
              (lambda (scope)
                (find-if
                 (lambda (var-info)
                   (equal name (aget :name var-info)))
                 scope))
              scopes))
           (get-analysis-set (scope first-occurrence name binding-class)
             "Collect all relevant asts with NAME in SCOPE. BINDING-CLASS
              determines whether 'global' or 'nonlocal' should be used to
              determine if NAME is in-scope for assignments."
             ;; Currently, py-name, py-arg, and either py-nonlocal or py-global
             ;; are relevant.
             (remove-if-not
              (lambda (ast)
                (or (same-name-p ast name) (eq ast first-occurrence)))
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
             ;; Make sure the top-most scope comes first.
             (sort
              (assort (get-analysis-set scope (or (aget :decl var-info)
                                                  (get-parent-full-stmt obj ast))
                                        name binding-class)
                      :key {enclosing-scope obj})
              #'(lambda (ast1 ast2)
                  (< (length (ast-path obj ast1))
                     (length (ast-path obj ast2))))
              :key #'car)))
      ;; NOTE: the following comment will become relevant when the prior
      ;;       TODO is addressed.
      ;; Don't pass in the first scope of assorted-by-scope as the first
      ;; one may include a py-arg which find-var-uses would misinterpret
      ;; as squashing the binding's scope.
      (flatten (cons (car assorted-by-scope)
                     (find-var-uses (cdr assorted-by-scope)
                                    binding-class))))))

(-> identical-name-p (python-ast python-ast) boolean)
(defun identical-name-p (name1 name2)
  "Return T if the IDs of NAME1 and NAME2 are the same."
  (and (typep name1 'py-name)
       (typep name2 'py-name)
       (equalp (ast-annotation name1 :id)
               (ast-annotation name2 :id))))

(-> get-asts-in-namespace (python python-ast) list)
(defun get-asts-in-namespace (obj ast)
  "Get all of the ASTs in AST which are considered to be in the same namespace."
  ;; Note that with the first call to this function, AST should be the start of a
  ;; namespace.
  (labels ((new-namespace-p (ast)
             "Return T if AST starts a new namespace."
             ;; TODO: probably need to add some more types here.
             (member (type-of ast)
                     '(py-function-def py-async-function-def py-class-def)))
           (collect-asts (ast)
             (let ((children (remove nil (children ast))))
               (append children
                       (mappend (lambda (child)
                                  (unless (new-namespace-p child)
                                    (get-asts-in-namespace obj child)))
                                children)))))
    (sort (collect-asts ast)
          (lambda (ast1 ast2)
            (path-later-p obj ast2 ast1)))))

(-> create-tuple (list) python-ast)
(defun create-tuple (values &aux (length (length values)))
  "Create a new tuple AST that contains values."
  (convert 'python-ast
           `((:class . :tuple)
             (:interleaved-text
              ,@(cond
                  ((= 0 length) '("()"))
                  ((= 1 length) '("(" ",)"))
                  (t (append '("(")
                             (repeat-sequence '(", ") (1- length))
                             '(")")))))
             (:py-elts . ,values)
             (:annotations
              (:ctx . :load)))))

(-> create-dictionary (list list) (or python-ast null))
(defun create-dictionary (keys values &aux (length (length keys)))
  "Create a new dictionary AST that maps KEYS to VALUES."
  (when (= length (length values))
    (convert 'python-ast
             `((:class . :dict)
               (:interleaved-text
                ,@(cond
                    ((= 0 length) '("{}"))
                    (t (append '("{" " : ")
                               (repeat-sequence '(", " " : ") (1- length))
                               '("}")))))
               (:py-keys ,@keys)
               (:py-values ,@values)
               (:annotations
                (:child-order
                 ,@(iter
                     (for i from 0 below length)
                     (collect `((py-keys . ,i)))
                     (collect `((py-values . ,i))))))))))


;;; Implement the generic format-genome method for python objects.
(defmethod format-genome ((obj python) &key)
  "Format the genome of OBJ using YAPF (Yet Another Python Formatter)."
  (yapf obj))


;;; Indentation
(defmethod not-indentable-p ((ast py-constant)) t)
