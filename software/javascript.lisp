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
;;; Every AST in a @code{javascript} software object is of a subclass
;;; representing its AST type (e.g. BlockStatement, ForStatement).  Additional
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
(uiop:define-package :software-evolution-library/software/javascript
  (:nicknames :sel/software/javascript :sel/sw/javascript)
  (:use :gt/full
        :babel
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/parseable
        :software-evolution-library/software/non-homologous-parseable
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting)
  (:import-from :jsown :parse)
  (:import-from :cffi :translate-camelcase-name)
  (:import-from :functional-trees :path-later-p)
  (:export :javascript
           :javascript-mutation
           :javascript-ast
           :acorn
           :js-string-length))
(in-package :software-evolution-library/software/javascript)
(in-readtable :curry-compose-reader-macros)

(define-software javascript (non-homologous-parseable file-w-attributes) ()
  (:documentation "Javascript software representation."))


;;; Javascript ast data structures
(defconst +js-children+
  '(((:program) (:body . 0))
    ((:member-expression) (:object . 1) (:property . 1))
    ((:tagged-template-expression) (:tag . 1) (:quasi . 1))
    ((:template-literal) (:quasis . 0) (:expressions . 0))
    ((:meta-property) (:meta . 1) (:property . 1))
    ((:property)
     ;; See the comment about js-property in the `convert' method from
     ;; list to javascript-ast:
     ;; if (< (start value) (end key)) just value
     (:key . 1) (:value . 1))
    ((:method-definition) (:key . 1) (:value . 1))
    ((:sequence-expression) (:expressions . 0))
    ((:do-while-statement) (:body . 1) (:test . 1))
    ((:while-statement) (:test . 1) (:body . 1))
    ((:expression-statement) (:expression . 1))
    ((:super))
    ((:for-statement) (:init . 1) (:test . 1) (:update . 1) (:body . 1))
    ((:labeled-statement) (:label . 1) (:body . 1))
    ((:switch-statement) (:discriminant . 1) (:cases . 0))
    ((:switch-case) (:test . 1) (:consequent . 0))
    ((:try-statement) (:block . 1) (:handler . 1) (:finalizer . 1))
    ((:catch-clause) (:param . 1) (:body . 1))
    ((:variable-declaration) (:declarations . 0))
    ((:variable-declarator) (:id . 1) (:init . 1))
    ((:with-statement) (:object . 1) (:body . 1))
    ((:import-declaration) (:specifiers . 0) (:source . 1))
    ((:export-all-declaration) (:source . 1))
    ((:export-default-declaration) (:declaration . 1))
    ((:export-named-declaration)
     (:declaration . 1) (:specifiers . 0) (:source . 1))
    ((:export-specifier)
     ;; See the comment about export-specifier in the `convert' method
     ;; from list to javascript-ast.
     (:local . 1) (:exported . 1))
    ((:class-expression :class-declaration)
     (:id . 1) (:superclass . 1) (:body . 1))
    ((:for-in-statement :for-of-statement) (:left . 1) (:right . 1) (:body . 1))
    ((:array-pattern :array-expression) (:elements . 0))
    ((:object-pattern :object-expression) (:properties . 0))
    ((:rest-element
      :spread-element
      :update-expression
      :await-expression
      :unary-expression
      :yield-expression
      :return-statement
      :throw-statement)
     (:argument . 1))
    ((:assignment-pattern
      :binary-expression
      :logical-expression
      :assignment-expression)
     (:left . 1) (:right . 1))
    ((:function-expression :arrow-function-expression :function-declaration)
     (:id . 1) (:params . 0) (:body . 1))
    ((:class-body :block-statement) (:body . 0))
    ((:call-expression :new-expression) (:callee . 1) (:arguments . 0))
    ((:break-statement :continue-statement) (:label . 1))
    ((:conditional-expression :if-statement)
     (:test . 1) (:consequent . 1) (:alternate . 1))
    ((:import-specifier :import-default-specifier :import-namespace-specifier)
     ;; See the comment about import-specifier in the `convert' method
     ;; from list to javascript-ast.
     (:imported . 1) (:local . 1))
    ;; classes with no children.
    ((:identifier
      :this-expression
      :template-element
      :debugger-statement
      :empty-statement
      :literal)))
  "Definition of JavaScript classes and child slots.")

(eval-always
 (defclass javascript-ast (non-homologous-ast) ()
   (:documentation "Class of JavaScript ASTs.")))

(progn
  #.`(progn ,@(mappend {expand-ast-classes 'javascript-ast 'js}
                       +js-children+)))
(export-always
 (mapcar {symbol-cat 'js}
         (mappend «append #'first [{mapcar #'car} #'cdr]»
                  +js-children+)))

(define-constant +stmt-ast-types+
  '(js-do-while-statement js-for-statement js-labeled-statement
    js-switch-statement js-try-statement js-with-statement
    js-for-in-statement js-for-of-statement js-block-statement
    js-break-statement js-continue-statement js-expression-statement
    js-while-statement js-throw-statement js-return-statement
    js-if-statement)
  :test #'equal
  :documentation "Stmt AST types for JavaScript.")

(defmethod ast-type-to-rebind-p ((ast javascript-ast)) nil)
(defmethod ast-type-to-rebind-p ((ast js-identifier)) t)
(defmethod ast-annotation-to-rebind ((ast js-identifier)) :name)


;;; Javascript parsing

(defun js-string-length (string &key (start 0) end)
  "Compute the length of STRING according to the same rules as JavaScript's length property.

That is, the number of pairs in a UTF-16 encoding of the string, sans
BOM."
  (etypecase string
    #+sb-unicode (base-string (length string))
    (string
     (reduce #'+ string
             :key (lambda (char)
                    (let ((code (char-code char)))
                      (cond ((< code #x10000) 1)
                            ((< code #x10ffff) 2)
                            (t (error "Cannot be UTF-16: ~a"
                                      char)))))))))

(defun acorn (source-text)
  "Invoke the acorn parser on the genome of OBJ returning a
raw list of ASTs in OBJ for use in `parse-asts`."
  (labels ((invoke-acorn (parsing-mode)
             "Invoke acorn with the given PARSING-MODE (:script or :module)."
             (with-temporary-file-of (:pathname src-file) source-text
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
          (convert-acorn-jsown-tree (parse stdout))
          (error
            (make-instance 'mutate
              :text (format nil "acorn exit ~d~%stderr:~s"
                            exit
                            stderr)
              :operation :parse))))))

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

(defun translate-utf-16-offset (octets offset &key (from 0))
  "Translate OFFSET, an offset in characters, into an index into a
octet vector representing a UTF-16 encoded string."
  (declare (optimize speed)
           (octet-vector octets)
           (array-index offset))
  (flet ((surrogate? (code1 code2)
           (<= #xD800
               (logior (ash code1 16) code2)
               #xDBFF)))
    (declare (inline surrogate?))
    (nlet rec ((i from)
               (remaining offset))
      (declare (array-index remaining))
      (if (zerop remaining) i
          (rec (if (surrogate?
                    (aref octets i)
                    (aref octets (1+ i)))
                   (+ i 4)
                   (+ i 2))
               (1- remaining))))))

(defmethod convert ((to-type (eql 'javascript-ast)) (string string)
                    &key &allow-other-keys
                    &aux
                      (fast-path?
                       (= (js-string-length string)
                          (length string)))
                      (string-octets
                       (if fast-path?
                           (make-octet-vector 0)
                           (string-to-octets string
                                             :encoding :utf-16
                                             :use-bom nil))))
  (labels
      ((safe-subseq (start end)
         "Return STRING in the range [START, END) or an empty string if
         the offsets are invalid."
         (if (< start end)
             (if fast-path?
                 (subseq string start end)
                 (let* ((start-offset
                         (translate-utf-16-offset string-octets start))
                        (end-offset (translate-utf-16-offset
                                     string-octets
                                     (- end start)
                                     :from start-offset)))
                   (octets-to-string string-octets
                                     :start start-offset
                                     :end end-offset
                                     :encoding :utf-16le)))
             ""))
       (start (ast)
         "Return the start offset into STRING from the AST representation."
         (ast-annotation ast :start))
       (end (ast)
         "Return the end offset into STRING from the AST representation."
         (ast-annotation ast :end))
       (add-child-order-annotation (ast)
         "Destructively modify AST, adding a :child-order annotation
         defining the textual order of the children for those AST
         types whose children are not in textual order by default."
         (when (typep ast 'js-template-literal)
           (setf (slot-value ast 'annotations)
                 (cons (cons :child-order
                             (nest (mapcar {position _ ast})
                                   (sort (remove nil (children ast)) #'<
                                         :key #'end)))
                       (ast-annotations ast)))))
       (normalized-children (ast)
         "Return the sorted, non-nil children of AST after destructively
         modifying AST to add a :child-order annotation to those AST types
         whose children are not in a textual order by default."
         (add-child-order-annotation ast)
         (sorted-children ast))
       (ranges (children from to)
         "Return the offsets of the source text ranges between CHILDREN."
         (iter (for child in children)
               (for prev previous child)
               (if prev
                   (collect (cons (end prev) (start child)) into ranges)
                   (collect (cons from (start child)) into ranges))
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
               (adrop '(:start :end) (slot-value ast 'annotations)))
         ast))
    (nest (fix-newlines)
          (w/interleaved-text (convert to-type (acorn string))
                              0
                              (if fast-path?
                                  (length string)
                                  (ash (length string-octets) -1))))))

(defmethod convert ((to-type (eql 'javascript-ast)) (spec null)
                    &key &allow-other-keys)
  nil)

(defmethod convert :around ((to-type (eql 'javascript-ast)) (spec list)
                            &rest args &key &allow-other-keys
                            &aux (spec-type (nest (make-keyword)
                                                  (string-upcase)
                                                  (translate-camelcase-name)
                                                  (aget :type spec))))
  "Wrapper around convert to perform various fixups on the SPEC from acorn."
  ;; 1. For a property with no value (e.g., "p" in "{p, q}")
  ;; it sets the value to the key so we will explicitly drop
  ;; the value.
  ;;
  ;; 2. For import/export specifier the imported and local
  ;; children may reference the same text.
  ;;
  ;; 3. Replace the :type key with :class for interoperability.
  (case spec-type
    (:property
     (when (= (aget :start (aget :key spec))
              (aget :start (aget :value spec)))
       (setf spec (adrop '(:value) spec))))
    (:export-specifier
     (when (equal (aget :exported spec) (aget :local spec))
       (setf spec (adrop '(:local) spec))))
    ((or :import-specifier
         :import-default-specifier
         :import-namespace-specifier)
     (when (equal (aget :imported spec) (aget :local spec))
       (setf spec (adrop '(:local) spec)))))

  (when (aget :type spec)
    (setf spec (cons (cons :class (aget :type spec)) (adrop '(:type) spec))))

  (apply #'call-next-method to-type spec args))

(defmethod convert ((to-type (eql 'javascript-ast)) (spec list)
                    &key &allow-other-keys)
  "Create a JAVASCRIPT AST from the SPEC (specification) list."
  (convert-helper spec 'js 'javascript-ast +js-children+))

(defmethod convert ((to-type (eql 'javascript-ast)) (spec javascript-ast)
                    &key &allow-other-keys)
  "Pass thru an existing JAVASCRIPT AST.  This useful in manual AST creation."
  spec)

(defmethod parse-asts ((obj javascript) &optional (source (genome-string obj)))
  (convert 'javascript-ast source))

(defun fix-newlines (ast)
  "Fix newlines in JavaScript ASTs by pushing newlines down into child
statements.  This allows for mutation operations to insert and
replace statements with newlines already present in the new
ASTs, obviating the need for fixups to add missing newlines.
This method is destructive and, therefore, can only be utilized
during AST creation to respect functional trees invariants."
  (mapcar #'move-newlines-down ast))

(defun move-newlines-down (ast)
  "Destructively modify AST, pushing newlines down into child ASTs for
statement AST types."
  (iter (for child in (children ast))
        (for after-text in (cdr (interleaved-text ast)))
        (for i upfrom 1)
        (when-let ((pos (and (member (type-of child) +stmt-ast-types+)
                             (position-after-leading-newline after-text))))
          ;; Move the [0, pos) prefix of after-text containing the newline
          ;; down into the child node.
          (setf (slot-value child 'interleaved-text)
                (append (butlast (interleaved-text child))
                        (list (concatenate 'string
                                           (lastcar (interleaved-text child))
                                           (subseq after-text 0 pos)))))
          (setf (nth i (slot-value ast 'interleaved-text))
                (subseq after-text pos)))
        (finally (return ast))))

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

(defmethod enclosing-scope ((obj javascript) (ast javascript-ast))
  "Return the enclosing scope of AST in OBJ.
OBJ javascript software object
AST ast to return the enclosing scope for"
  (or (find-if (lambda (ast)
                 (member (type-of ast)
                         (list 'js-block-statement
                               'js-function-declaration
                               'js-function-expression
                               'js-program
                               'js-arrow-function-expression
                               'js-for-statement
                               'js-for-in-statement
                               'js-for-of-statement)))
               (cdr (get-parent-asts obj ast)))
      (genome obj)))

(defgeneric statements-in-scope (obj scope ast)
  (:documentation "Return all child statements of SCOPE prior to AST.")
  (:method (obj (scope javascript-ast) (ast javascript-ast))
    (iter (for c in (remove nil (children scope)))
          (while (path-later-p obj ast c))
          (collect c))))

(defgeneric identifiers (ast)
  (:documentation "Return all js-identifier nodes in AST and its children.")
  (:method ((ast javascript-ast))
    (remove-if-not {typep _ 'js-identifier}
                   (cons ast (child-asts ast :recursive t)))))

(defgeneric inner-declarations (ast)
  (:documentation
   "Return a list of variable declarations affecting inner scopes.")
  (:method ((ast javascript-ast)) nil)
  (:method ((ast js-function-declaration))
    (mappend #'identifiers (js-params ast)))
  (:method ((ast js-function-expression))
    (mappend #'identifiers (js-params ast)))
  (:method ((ast js-arrow-function-expression))
    (mappend #'identifiers (js-params ast)))
  (:method ((ast js-arrow-function-expression))
    (mappend #'identifiers (js-params ast)))
  (:method ((ast js-for-of-statement))
    (mappend #'identifiers (list (js-left ast))))
  (:method ((ast js-for-in-statement))
    (identifiers (js-left ast)))
  (:method ((ast js-for-statement))
    (identifiers (js-init ast))))

(defgeneric outer-declarations (ast)
  (:documentation
   "Return a list of variable declarations affecting outer scopes.")
  (:method ((ast javascript-ast)) nil)
  (:method ((ast js-variable-declaration))
    (mappend #'outer-declarations (js-declarations ast)))
  (:method ((ast js-object-pattern))
    (mappend #'outer-declarations (js-properties ast)))
  (:method ((ast js-array-pattern))
    (mappend #'identifiers (js-elements ast)))
  (:method ((ast js-rest-element))
    (identifiers (js-argument ast)))
  (:method ((ast js-variable-declarator))
    (identifiers (js-id ast)))
  (:method ((ast js-property))
    (identifiers (js-key ast))))

(defmethod scopes ((obj javascript) (ast javascript-ast))
  (labels ((get-parent-decl (obj identifier)
             "For the given IDENTIFIER AST, return the parent declaration."
             (car (remove-if-not {typep _ 'js-variable-declaration}
                                 (get-parent-asts obj identifier))))
           (ast-to-scope (obj scope ast)
             `((:name . ,(source-text ast))
               (:decl . ,(or (get-parent-decl obj ast) ast))
               (:scope . ,scope))))
    (unless (null (ast-path obj ast))
      (let ((scope (enclosing-scope obj ast)))
        (cons (nest (reverse)
                    (mapcar {ast-to-scope obj scope})
                    (append (inner-declarations scope))
                    (mappend #'outer-declarations)
                    (statements-in-scope obj scope ast))
              (scopes obj scope))))))

(defmethod get-unbound-vals ((obj javascript) (ast javascript-ast))
  "Return all variables used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound variables within"
  (labels ((get-unbound-vals-helper (obj parent ast)
             (remove-duplicates
               (apply #'append
                      (when (and (typep ast 'js-identifier)
                                 (not (member (type-of parent)
                                              (list 'js-call-expression
                                                    'js-member-expression
                                                    'js-function-declaration
                                                    'js-function-expression
                                                    'js-arrow-function-expression
                                                    'js-class-expression
                                                    'js-meta-property
                                                    'js-break-statement
                                                    'js-class-declaration
                                                    'js-continue-statement
                                                    'js-labelled-statement
                                                    'js-import-specifier
                                                    'js-export-specifier
                                                    'js-export-default-declaration
                                                    'js-variable-declarator))))
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
           (when (typep ast 'js-call-expression)
             (cond ((typep callee 'js-identifier)
                    ;; Free function call
                    (list (list (source-text callee)
                                nil nil (length (cdr children)))))
                   ((typep callee 'js-member-expression)
                    ;; Member function call
                    (list (list (nest (source-text)
                                      (second)
                                      (children callee))
                                nil nil (length (cdr children)))))
                   (t nil)))
           (mapcar {get-unbound-funs obj} children))
    :test #'equal))

(defmethod get-parent-full-stmt ((obj javascript) (ast javascript-ast))
  (find-if (lambda (ast)
             (member (type-of ast) +stmt-ast-types+))
           (get-parent-asts obj ast)))

(defmethod is-stmt-p ((ast javascript-ast))
  (member (type-of ast) +stmt-ast-types+))

(defmethod get-function-from-function-call
    ((obj javascript) (callexpr javascript-ast))
  ;; NOTE: this currently only handles
  ;;       named functions declared with 'function'.
  (match callexpr
    ((js-call-expression
      (js-callee
       (js-identifier (ast-annotations (assoc :name name)))))
     (enclosing-find-function obj callexpr name))))


;;; Helper Functions.
(-> enclosing-find-function (javascript javascript-ast string)
  (or null javascript-ast))
(defun enclosing-find-function (obj start-ast function-name)
  "Find the function with the name FUNCTION-NAME in OBJ that is in
scope of START-AST."
  ;; NOTE: this currently only handles
  ;;       named functions declared with 'function'.
  (flet ((target-function (ast)
           (match ast
             ((js-function-declaration
               (js-id
                (js-identifier
                 (ast-annotations (assoc :name name)))))
              (string= name function-name)))))
    (find-if-in-scope #'target-function obj start-ast)))


;;; Implement the generic format-genome method for Javascript objects.
(defmethod format-genome ((obj javascript) &key)
  (prettier obj))
