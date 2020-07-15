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
  (:import-from :functional-trees :path-later-p)
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
  ((interleaved-text :initarg :interleaved-text :initform nil :type list
                     :reader interleaved-text
                     :documentation "Interleaved text between children."))
  (:documentation "Class of JavaScript ASTs."))

(define-constant +js-children+
  '(((:program) (:body . 0))
    ((:member-expression) (:object . 1) (:property . 1))
    ((:tagged-template-expression) (:tag . 1) (:quasi . 1))
     ;; See the comment about js-template-literal in the `convert'
     ;; method from list to javascript-ast.
    ((:template-literal) (:body . 0))
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
    ((:for-statement) (:init . 1) (:test . 1) (:update . 1) (:body . 1))
    ((:labeled-statement) (:label . 1) (:body . 1))
    ((:switch-statement) (:discriminant . 1) (:cases . 0))
    ((:switch-case) (:test . 1) (:consequent . 0))
    ((:try-statement) (:block . 1) (:handler . 1) (:finalizer . 1))
    ((:catch-clause) (:param . 1) (:body . 1))
    ((:variable-declaration) (:declarations . 0))
    ((:variable-declarator) (:id . 1) (:init . 1))
    ((:with-statement) (:object . 1) (:body . 1))
    ((:program) (:body . 0))
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
  :test #'equalp
  :documentation "Definition of JavaScript classes and child slots.")

(defun expand-js-class (spec)
  (nest
   (destructuring-bind (ast-class-list . field-specifiers) spec)
   (mapcar
    (lambda (class)
      `(defclass ,(symbol-cat 'js class) (javascript-ast)
         (,@(when field-specifiers
              `((child-slots
                 :initform (quote ,(mapcar «cons [{symbol-cat 'js} #'car] #'cdr»
                                           field-specifiers))
                 :reader child-slots
                 :allocation :class)))
          ,@(mapcar (lambda (field)
                      (destructuring-bind (field . arity) field
                        (let ((js-field (symbol-cat 'js field)))
                          (list* js-field :reader js-field
                                          :initform nil
                                          :initarg (make-keyword
                                                    (symbol-cat 'js field))
                                          (when (zerop arity)
                                            (list :type 'list))))))
                    field-specifiers))
         (:documentation
          ,(format nil "Javascript AST node class for ~a acorn ASTs." class)))))
   ast-class-list))

(eval `(progn ,@(mappend #'expand-js-class +js-children+)))
(export (mapcar {symbol-cat 'js}
                (mappend «append #'first [{mapcar #'car} #'cdr]»
                         +js-children+)))

(defmethod source-text ((ast javascript-ast) &optional stream)
  (write-string (car (interleaved-text ast)) stream)
  (mapc (lambda (child text)
          (source-text child stream)
          (write-string text stream))
        (remove nil (children ast))
        (cdr (interleaved-text ast))))


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
  (labels
      ((safe-subseq (start end)
         (if (< start end) (subseq string start end) ""))
       (ranges (children from to &aux ranges)
         (let (last)
           (mapc (lambda (child)
                   (if last
                       (push (cons (ast-annotation last :end)
                                   (ast-annotation child :start))
                             ranges)
                       (push (cons from (ast-annotation child :start))
                             ranges))
                   (setf last child))
                 children)
           (push (cons (ast-annotation last :end) to) ranges)
           (nreverse ranges)))
       (w/interleaved-text (tree from to)
         (if-let* ((children (remove nil (children tree))))
           (progn
             (setf (slot-value tree 'interleaved-text)
                   (mapcar (lambda (range)
                             (destructuring-bind (from . to) range
                               (safe-subseq from to)))
                           (ranges children from to)))
             (mapc (lambda (child)
                     (w/interleaved-text child
                                         (ast-annotation child :start)
                                         (ast-annotation child :end)))
                   children))
           (setf (slot-value tree 'interleaved-text)
                 (list (safe-subseq (ast-annotation tree :start)
                                    (ast-annotation tree :end)))))
         (setf (slot-value tree 'annotations)
               (adrop '(:start :end) (slot-value tree 'annotations)))
         tree))
    (w/interleaved-text (convert to-type (acorn string))
                        0 (length string))))

(defmethod convert ((to-type (eql 'javascript-ast)) (spec null)
                    &key &allow-other-keys)
  nil)

(defmethod convert ((to-type (eql 'javascript-ast)) (spec list)
                    &key &allow-other-keys)
  "Create a JAVASCRIPT AST from the SPEC (specification) list."
  (let* ((raw-type (make-keyword (string-upcase (translate-camelcase-name
                                                 (aget :type spec)))))
         (type (symbol-cat 'js raw-type))
         (child-types (aget raw-type +js-children+ :test #'member)))
    ;; Fix inaccuracies in the parsed ASTs we get from acorn.
    ;;
    ;; 1. For a property with no value (e.g., "p" in "{p, q}")
    ;; it sets the value to the key so we will explicitly drop
    ;; the value.
    ;;
    ;; 2. For import/export specifier the imported and local
    ;; children may reference the same text.
    ;;
    ;; 3. For template literals, the children are not in textual
    ;; order from acorn (quasis may be intermingled with expressions).
    ;; As we require children to be in textual order for exact
    ;; reproduction with `source-text', we create an concatenate
    ;; the child fields together and sort the children in textual
    ;; order as the "body" of the template literal.
    (case type
      (js-property
       (when (= (aget :start (aget :key spec))
                (aget :start (aget :value spec)))
         (setf spec (adrop '(:value) spec))))
      (js-export-specifier
       (when (equal (aget :exported spec) (aget :local spec))
         (setf spec (adrop '(:local) spec))))
      ((or js-import-specifier
           js-import-default-specifier
           js-import-namespace-specifier)
       (when (equal (aget :imported spec) (aget :local spec))
         (setf spec (adrop '(:local) spec))))
      (js-template-literal
       (setf spec
             (nest (append spec)
                   (adrop '(:quasis :expressions))
                   (list (cons :body (sort (append (aget :quasis spec)
                                                   (aget :expressions spec))
                                           #'< :key {aget :end})))))))
    (apply #'make-instance type
           (mappend
            (lambda (field)
              (destructuring-bind (key . value) field
                (list (if (find key child-types :key #'car)
                          (make-keyword (symbol-cat 'js key))
                          key)
                      (if-let ((spec (find key child-types :key #'car)))
                        (destructuring-bind (key . arity) spec
                          (declare (ignorable key))
                          (ecase arity
                            (1 (convert 'javascript-ast value))
                            (0 (mapcar {convert 'javascript-ast} value))))
                        value))))
            (adrop '(:type) spec)))))

(defmethod parse-asts ((obj javascript) &optional (source (genome-string obj)))
  (convert 'javascript-ast source))


;;; Javascript mutation
(define-mutation javascript-mutation (parseable-mutation) ()
  (:documentation
   "Specialization of the mutation interface for JavaScript software objects."))


;;; Genome manipulations
(defmethod prepend-text-to-genome
    ((js javascript) (text string) &aux (root (genome js)))
  (setf (genome js)
        (nest (copy root :interleaved-text)
              (cons (concatenate 'string text (car (interleaved-text root)))
                    (cdr (interleaved-text root))))))

(defmethod append-text-to-genome
    ((js javascript) (text string) &aux (root (genome js)))
  (setf (genome js)
        (nest (copy root :interleaved-text)
              (append (butlast (interleaved-text root)))
              (list)
              (concatenate 'string text)
              (lastcar (interleaved-text root)))))


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
  (if (typep ast 'js-identifier)
      (copy ast :name (rebind-vars (ast-annotation ast :name)
                                   var-replacements
                                   fun-replacements)
                :interleaved-text (mapcar {rebind-vars _ var-replacements
                                                         fun-replacements}
                                          (interleaved-text ast)))
      (apply #'copy ast
             (mappend (lambda (child-slot)
                        (destructuring-bind (name . arity) child-slot
                          (list (make-keyword name)
                                (if (= arity 0)
                                    (mapcar {rebind-vars _ var-replacements
                                                           fun-replacements}
                                            (slot-value ast name))
                                    (rebind-vars (slot-value ast name)
                                                 var-replacements
                                                 fun-replacements)))))
                      (child-slots ast)))))

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
          (while (path-later-p (ast-path obj ast)
                               (ast-path obj c)))
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
           (when (typep ast 'js-call-expression)
             (cond ((typep callee 'js-identifier)
                    ;; Free function call
                    (list (list (source-text callee)
                                nil nil (length (cdr child-asts)))))
                   ((typep callee 'js-member-expression)
                    ;; Member function call
                    (list (list (nest (source-text)
                                      (second)
                                      (child-asts callee))
                                nil nil (length (cdr child-asts)))))
                   (t nil)))
           (mapcar {get-unbound-funs obj} child-asts))
    :test #'equal))

(defmethod get-parent-full-stmt ((obj javascript) (ast javascript-ast))
  (find-if (lambda (ast)
             (member (type-of ast)
                     (list 'js-do-while-statement
                           'js-for-statement
                           'js-labeled-statement
                           'js-switch-statement
                           'js-try-statement
                           'js-with-statement
                           'js-for-in-statement
                           'js-for-of-statement
                           'js-block-statement
                           'js-break-statement
                           'js-continue-statement
                           'js-expression-statement
                           'js-while-statement
                           'js-throw-statement
                           'js-return-statement
                           'js-if-statement)))
           (get-parent-asts obj ast)))


;;; Implement the generic format-genome method for Javascript objects.
(defmethod format-genome ((obj javascript) &key)
  (prettier obj))
