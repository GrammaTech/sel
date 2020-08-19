;;; lisp.lisp --- software representation of lisp code
;;;
;;; Eclector, see @url{https://github.com/robert-strandh/Eclector},
;;; is used to parse lisp source into concrete ASTs.
;;;
;;; @texi{lisp}
(defpackage :software-evolution-library/software/lisp
  (:nicknames :sel/software/lisp :sel/sw/lisp)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/parseable
        :eclector.parse-result)
  (:import-from :eclector.reader
                :evaluate-expression
                :interpret-symbol)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :functional-trees :path-later-p)
  (:shadowing-import-from :eclector.readtable
                          :copy-readtable
                          :set-dispatch-macro-character)
  (:shadowing-import-from :eclector.parse-result
                          :read
                          :read-from-string
                          :read-preserving-whitespace)
  (:export :lisp :lisp-ast
           :expression :expression-result
           :reader-conditional
           :feature-expression
           :reader-quote
           :reader-quasiquote
           :reader-unquote
           :reader-unquote-splicing
           :sharpsign-quote
           :*string*
           :transform-reader-conditional
           :walk-feature-expressions
           :walk-reader-conditionals
           :map-reader-conditionals
           :map-feature-expressions
           :transform-feature-expression
           :featurep-with
           :remove-expression-features
           :remove-feature-support
           :compound-form-p
           :compound-form-p*
           :get-compound-form-args
           :quote-p
           :quasiquote-p
           :quoted-p
           :find-in-defining-form
           :find-local-function
           :*bindings-allows-macros-p*
           :*bindings-form-is-macro-p*
           :*bindings-allows-symbol-macros-p*
           :*bindings-allows-top-level-p*
           :bindings
           :get-vars-from-binding-form
           :get-functions-from-binding-form
           :define-function-binding-form-alias
           :define-get-vars-from-binding-form
           :handle-as
           :collect-function-info
           :collect-function-info*
           :collect-var-info
           :define-var-binding-form-alias
           :children-of-type
           :fun-body
           :define-fun-body-alias
           :lambda-list
           :define-lambda-list-alias
           :collect-symbols
           :literalp
           :scope-contains-function-p
           :scope-contains-variable-p
           :bindings-contains-function-p
           :bindings-contains-variable-p
           :car-of-enclosing-form-p
           :map-arguments-to-parameters))
(in-package :software-evolution-library/software/lisp)
(in-readtable :curry-compose-reader-macros)


(defvar *string* nil)

(define-node-class lisp-ast (functional-tree-ast)
  ((expression :initarg :expression :initform nil :reader expression)
   (children :type list
             :initarg :children
             :initform nil
             :documentation "The list of children of the node, which
                             may be more nodes, or other values.")
   (child-slots :initform '(children) :allocation :class))
  (:documentation "Class of Common Lisp ASTs."))

(defmethod equal? ((x lisp-ast) (y lisp-ast))
  (and (call-next-method)
       ;; We already know that x and y have the same number of
       ;; children, and that the children are `equal?`.
       (if (children x) t
           ;; Expressions aren't updated on mutations, and leaf nodes
           ;; tend to be the only ones with accurate data.
           (equal? (expression x)
                   (expression y)))))


(defmacro define-matchable-class (class-name super-classes slots &rest options)
  "Define a new class that is wrapped in an eval-when form. This is to work
around an issue in SBCL--https://bugs.launchpad.net/sbcl/+bug/310120--that
prevents trivia:match from working correctly when classes are defined in the
same file as the match form its being used in."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,class-name ,super-classes
       ,slots
       ,@options)))

(define-matchable-class result (lisp-ast)
  ((start :initarg :start :initform (when *string* 0)
          :reader start :type (or null (integer 0 *)))
   (end :initarg :end :initform (when *string* (length *string*))
        :reader end :type (or null (integer 0 *)))
   (string-pointer :initarg :string-pointer :initform *string*
                   :reader string-pointer :type (or null string))))

(define-matchable-class expression-result (result) ())

(defmethod print-object ((obj expression-result) stream)
  (with-slots (start end string-pointer expression children) obj
    (if *print-readably*
        (format stream "~S" `(make-instance ',(class-name (class-of obj))
                               :start ,start
                               :end ,end
                               :string-pointer *string*
                               :expression ,expression
                               :children (list ,@children)))
        (print-unreadable-object (obj stream :type t)
          (format stream ":EXPRESSION ~a" expression)))))

(define-matchable-class reader-conditional (expression-result)
  ((feature-expression :initarg :feature-expression
                       :reader feature-expression)))

(defmethod initialize-instance :after ((obj reader-conditional) &key)
  (with-slots (feature-expression) obj
    (when (typep feature-expression 'expression-result)
      (callf #'expression feature-expression))
    (assert (typep feature-expression '(or symbol list)))))

(defmethod copy ((obj reader-conditional) &rest args &key &allow-other-keys)
  (apply #'call-next-method
         obj
         :feature-expression (feature-expression obj)
         args))

(defmethod print-object ((obj reader-conditional) stream)
  (nest
   (with-slots (feature-expression expression) obj)
   (if *print-readably* (call-next-method))
   (print-unreadable-object (obj stream :type t))
   (format stream "#~a~a :EXPRESSION ~a"
           (reader-conditional-sign obj)
           feature-expression expression)))

(define-matchable-class skipped-input-result (result)
  ((reason :initarg :reason :reader  reason)))

(defmethod print-object ((obj skipped-input-result) stream &aux (max-length 8))
  (nest (with-slots (start end string-pointer reason) obj)
        (if *print-readably*
            (format stream "~S" `(make-instance ',(class-name (class-of obj))
                                   :start ,start
                                   :end ,end
                                   :string-pointer *string*
                                   :reason ,reason)))
        (print-unreadable-object (obj stream :type t))
        (format stream ":REASON ~a :TEXT ~S" reason)
        (if (> (- end start) (- max-length 3))
            (concatenate
             'string
             (subseq string-pointer start (+ start (- max-length 3)))
             "...")
            (subseq string-pointer start end))))

(define-matchable-class reader-token (skipped-input-result)
  ())

(defmethod source-text ((obj reader-token) &optional stream)
  (write-string (string-pointer obj) stream))

(defmethod print-object ((obj reader-token) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t))))

(define-matchable-class sharpsign-dot (skipped-input-result)
  ((reason :initform :read-eval)))

(define-matchable-class reader-conditional-token (reader-token)
  ((reason :initform :reader-conditional)
   (start :initform 0)
   (end :initform 2)))

(define-matchable-class sharpsign-plus (reader-conditional-token)
  ((string-pointer :initform "#+")))

(define-matchable-class sharpsign-minus (reader-conditional-token)
  ((string-pointer :initform "#-")))

(define-matchable-class reader-quote (expression-result) ())

(define-matchable-class reader-quasiquote (expression-result) ())

(define-matchable-class reader-unquote (expression-result) ())

(define-matchable-class reader-unquote-splicing (expression-result) ())

(define-matchable-class sharpsign-quote (expression-result) ())

(defmethod convert ((to-type (eql 'lisp-ast)) (sequence list)
                    &key (spaces nil) (expression sequence)
                      (keyword-prefix ":")
                      &allow-other-keys)
  (labels ((m/space (&optional string)
             (or (and (not string) spaces (pop spaces))
                 (let ((*string* (or string " ")))
                   (make-instance 'skipped-input-result :reason :whitespace))))
           (m/keyword (symbol)
             (let ((*string* (concatenate 'string
                                          keyword-prefix
                                          (string-downcase
                                           (symbol-name symbol)))))
               (make-instance 'expression-result :expression symbol)))
           (m/symbol (symbol)
             (let ((*string* (string-downcase (format nil "~s" symbol))))
               (make-instance 'expression-result :expression symbol)))
           (m/other (other)
             (let ((*string* (format nil "~S" other)))
               (make-instance 'expression-result :expression other)))
           (intersperse-spaces (list)
             (let ((ult (length list))
                   (last nil))
               (iter (for el in list)
                     (for i upfrom 0)
                     (if (and (< i ult)
                              (> i 0)
                              (not (string= "(" (source-text last)))
                              (not (string= ")" (source-text el))))
                         (appending (list (m/space) el))
                         (collecting el))
                     (setf last el))))
           (convert (node)
             (when node
               (typecase node
                 (lisp-ast node)
                 (keyword (m/keyword node))
                 (symbol (m/symbol node))
                 (list
                  (let ((*string* ""))
                    (make-instance 'expression-result :expression expression
                                   :children
                                   (intersperse-spaces
                                    (append (list (m/space "("))
                                            (mapcar #'convert node)
                                            (list (m/space ")")))))))
                 (t (m/other node))))))
    (populate-fingers (convert sequence))))

;;; Trivial Eclector client used to customize parsing for SEL.
(define-matchable-class client (parse-result-client) ())

(defun sharpsign-sign-reader (stream char n)
  ;; TODO: back up according to digits in n
  (assert (not n))
  (nest
   (let* ((client (make-instance 'client))
          (start (- (file-position stream) 2))
          (feature-expression
           (let ((*package* (find-package :keyword)))
             (read client stream)))
          (expression (read client stream))
          (end (file-position stream))))
   (make 'reader-conditional
         :start start
         :end end
         :feature-expression feature-expression
         :expression expression
         :children (append
                    (list
                     (make (ecase char
                             (#\+ 'sharpsign-plus)
                             (#\- 'sharpsign-minus))
                           ;; These are for the benefit of read+, so
                           ;; it doesn't insert needless whitespace.
                           :start start
                           :end (+ start 2)))
                    (list feature-expression)
                    (list expression)))))

(defgeneric transform-reader-conditional (reader-conditional fn)
  (:documentation "Build a new reader condition by calling FN on READER-CONDITIONAL.

FN is called with three arguments: the sign, as a character \(+ or -);
the feature expresion \(as a list); and the guarded expression.

FN should return three values - a new sign, a new test, and a new
expression - which are used to build a new reader conditional.

If the sign, the test, and the expression are unchanged,
READER-CONDITIONAL is returned unchanged and a second value of t is
returned."))

(defmethod transform-reader-conditional ((result reader-conditional) fn)
  (mvlet* ((children (children result))
           (token (find-if (of-type 'reader-token) children))
           (sign
            (etypecase token
              (sharpsign-plus #\+)
              (sharpsign-minus #\-)))
           (test ex
                 (nest
                  (values-list)
                  (remove-if-not (of-type 'expression-result))
                  children))
           (new-sign new-test new-ex
                     (funcall fn sign (expression test) ex))
           (*string* nil))
    (assert (typep new-test '(or symbol list)))
    (if (and (eql new-sign sign)
             (equal new-test test)
             (eql ex new-ex))
        ;; Nothing has changed.
        (values result t)
        (make 'reader-conditional
              :start (start result)
              :end (end result)
              :feature-expression new-test
              :expression ex
              :children
              (mapcar (lambda (child)
                        (typecase child
                          (reader-token
                           (ecase new-sign
                             (#\+ (make 'sharpsign-plus))
                             (#\- (make 'sharpsign-minus))))
                          (expression-result
                           (econd ((eql child test)
                                   (if (equal new-test (expression test))
                                       test
                                       (convert 'lisp-ast new-test :keyword-prefix "")))
                                  ((eql child ex) new-ex)))
                          (t child)))
                      children)))))

(defmethod reader-conditional-sign ((ex reader-conditional))
  (let ((token (find-if (of-type 'reader-token) (children ex))))
    (etypecase token
      (sharpsign-plus #\+)
      (sharpsign-minus #\-))))

(defparameter *lisp-ast-readtable*
  (let ((readtable (copy-readtable eclector.readtable:*readtable*)))
    (set-dispatch-macro-character readtable #\# #\+ 'sharpsign-sign-reader)
    (set-dispatch-macro-character readtable #\# #\- 'sharpsign-sign-reader)
    readtable))

(defmethod make-expression-result
    ((client client) (result expression-result) (children t) (source t))
  result)

(defmethod make-expression-result
    ((client client) (result t) (children t) (source cons))
  (destructuring-bind (start . end) source
    (match result
           ((list '|#.| result)
            (make-instance 'expression-result
              :expression result
              :children (cons
                         (make 'sharpsign-dot :start start
                                              :end (+ 2 start)) children)
              :start start
              :end end))
           (otherwise
            (make-instance 'expression-result
              :expression result
              :children children
              :start start
              :end end)))))

(defmethod make-skipped-input-result
    ((client client) stream reason source)
  (declare (ignorable client stream))
  (make-instance 'skipped-input-result
    :reason reason :start (car source) :end (cdr source)))

(defmethod interpret-symbol
    ((client client) input-stream package-indicator symbol-name internp)
  (declare (ignorable input-stream))
  (let ((package (case package-indicator
                   ;; uninterned symbols have a nil package.
                   ((nil) nil)
                   (:current *package*)
                   (:keyword (find-package "KEYWORD"))
                   (t        (or (find-package package-indicator)
                                 ;; Return a fake package for missing packages.
                                 (find-package :missing)
                                 (make-package :missing))))))
    (cond
      ((null package) (make-symbol symbol-name))
      (internp (intern symbol-name package))
      (t (multiple-value-bind (symbol status)
             (find-symbol symbol-name package)
           (cond ((null status) ; Ignore `symbol-does-not-exist' errors.
                  ;; (eclector.base::%reader-error
                  ;;  input-stream 'eclector.reader::symbol-does-not-exist
                  ;;  :package package
                  ;;  :symbol-name symbol-name)
                  symbol)
                 ((eq status :internal) ; Ignore `symbol-is-not-external' errors.
                  ;; (eclector.base::%reader-error
                  ;;  input-stream 'eclector.reader::symbol-is-not-external
                  ;;  :package package
                  ;;  :symbol-name symbol-name)
                  symbol)
                 (t
                  symbol)))))))

;;; The next two forms are used to avoid throwing errors when a
;;; #. reader macro attempts to execute code during parsing.  We want
;;; to avoid this as we will typically not have the requisite
;;; variables defined.
(defgeneric wrap-in-sharpsign-dot (client material)
  (:method (client material)
    (declare (ignorable client))
    (list '|#.| material)))

(defmethod evaluate-expression ((client client) expression)
  (wrap-in-sharpsign-dot client expression))

(defun read-forms+ (string &key count)
  (check-type count (or null integer))
  (let ((*string* string)
        (client (make-instance 'client))
        (eclector.readtable:*readtable* *lisp-ast-readtable*))
    (labels
        ((process-skipped-input (start end)
           (when (< start end)
              (string-case (subseq string start end)
                ("'"
                 (list (make-instance 'reader-quote
                         :start start :end end)))
                ("`"
                 (list (make-instance 'reader-quasiquote
                         :start start :end end)))
                (","
                 (list (make-instance 'reader-unquote
                         :start start :end end)))
                (",@"
                 (list (make-instance 'reader-unquote-splicing
                                      :start start :end end)))
                ("#'"
                 (list (make-instance 'sharpsign-quote
                                      :start start :end end)))
                (t
                 (list (make-instance 'skipped-input-result
                         :start start :end end :reason 'whitespace))))))
         (w/space (tree from to)
           (let ((result
                   (etypecase tree
                     (list
                      (append
                       (iter (for subtree in tree)
                         (appending (process-skipped-input from (start subtree)))
                         (appending (w/space subtree
                                             (start subtree) (end subtree)))
                         (setf from (end subtree)))
                       (process-skipped-input from to)))
                     (result
                      (when (subtypep (type-of tree) 'expression-result)
                        (when (children tree)
                          ;; Use (sef slot-value) because this is now a
                          ;; functional tree node so the default setf would
                          ;; have no effect (it would create a copy).
                          (setf (slot-value tree 'children)
                                (w/space
                                 (children tree) (start tree) (end tree)))))
                      (append
                       (process-skipped-input from (start tree))
                       (list tree)
                       (process-skipped-input (end tree) to))))))
             result)))
      (let ((end (length string)))
        (w/space
         (with-input-from-string (input string)
           (loop :with eof = '#:eof
              :for n :from 0
              :for form = (if (and count (>= n count))
                              eof
                              (read client input nil eof))
              :until (eq form eof) :collect form
              :finally (when count
                         (setf end (file-position input)))))
         0 end)))))

(defun walk-skipped-forms (function forms)
  (mapcar
   (lambda (form)
     (etypecase form
       (skipped-input-result (funcall function form))
       (expression-result (walk-skipped-forms function (children form)))))
   forms))

(defun walk-forms (function forms)
  (mapcar (lambda (form)
            (etypecase form
              (skipped-input-result (funcall function form))
              (expression-result (funcall function form)
                                 (walk-forms function (children form)))))
          forms))

(defun write-stream-forms+ (forms stream)
  "Write the original source text of FORMS to STREAM."
  (walk-skipped-forms  {source-text _ stream} forms))

(defun write-string-forms+ (forms)
  "Write the original source text of FORMS to a string."
  (with-output-to-string (s) (write-stream-forms+ forms s)))


;;; Lisp software object
(define-software lisp (parseable)
  ()
  (:documentation "Common Lisp source represented naturally as lists of code."))

(defmethod convert ((to-type (eql 'lisp-ast)) (string string)
                    &key &allow-other-keys)
  (make-instance 'lisp-ast :children (read-forms+ string)))

(defmethod parse-asts ((lisp lisp) &optional (source (genome-string lisp)))
  (convert 'lisp-ast source))

(defmethod source-text ((obj result) &optional stream)
  (if (children obj)
      (mapc {source-text _ stream} (children obj))
      (write-string (string-pointer obj) stream
                    :start (start obj) :end (end obj))))

(defmethod convert ((to-type (eql 'expression-result)) (symbol symbol)
                    &key &allow-other-keys)
  (let ((*string*
         (string-invert-case
          (symbol-name symbol))))
    (make-instance 'expression-result
      :expression symbol
      :start 0
      :end (length *string*))))

(defmethod convert ((to-type (eql 'lisp-ast)) (symbol symbol)
                    &key &allow-other-keys)
  (make-instance 'lisp-ast
    :children (list (convert 'expression-result symbol))))

(defun walk-feature-expressions (fn ast)
  "Call FN, a function, on each feature expression in AST."
  (fbind (fn)
         (walk-reader-conditionals (lambda (sign featurex ex)
                                     (declare (ignore sign ex))
                                     (fn featurex))
                                   ast)))

(defun walk-reader-conditionals (fn ast)
  "Call FN, a function, on each reader conditional in AST.

FN is called with three arguments: the sign of the reader conditional
\(+ or -), the feature expression \(as a list), and the guarded
expression."
  (fbind (fn)
    (mapc (lambda (node)
            (when (typep node 'reader-conditional)
              (fn (reader-conditional-sign node)
                (feature-expression node)
                (expression node))))
          ast)
    (values)))

(defun featurex-empty? (featurex)
  (or (null featurex)
      (equal featurex '(:or))))

(defun map-feature-expressions (fn ast
                                &key remove-empty
                                  (remove-newly-empty remove-empty))
  "Build a new ast by calling FUN, a function, on each feature
expression in AST, substituting the old feature expression with the
return value of FN.

REMOVE-EMPTY and REMOVE-NOT-EMPTY have the same meaning as for
`map-reader-conditionals'."
  (fbind (fn)
         (map-reader-conditionals (lambda (sign featurex ex)
                                    (values sign (fn featurex) ex))
                                  ast
                                  :remove-empty remove-empty
                                  :remove-newly-empty remove-newly-empty)))

(defun map-reader-conditionals (fn ast
                                 &key remove-empty
                                 (remove-newly-empty remove-empty))
  "Build a new ast by calling FN, an function, on each reader
conditional in AST (as if by `transform-reader-conditional') and
substituting the old reader conditional with the new one.

If :REMOVE-EMPTY is true, remove any reader conditionals where the
feature expression is empty. If the sign is +, the entire reader
conditional is removed. If the sign is -, then only the guarded
expression is retained.

If :REMOVE-NEWLY-EMPTY is true, reader conditionals are removed if the
new feature expression is empty, but reader conditionals that were
already empty are retained."
  (assert (if remove-empty remove-newly-empty t))
  (nest
   (fbind (fn))
   (mapcar
    (lambda (node)
      (if (typep node 'reader-conditional)
          (block replace
            (flet ((remove (sign node)
                     (return-from replace
                       (ecase sign
                         (#\+ nil)
                         (#\- (expression node))))))
              (transform-reader-conditional
               node
               (lambda (sign featurex ex)
                 (if (and (featurex-empty? featurex) remove-empty)
                     (remove sign node)
                     (receive (sign featurex ex)
                         (fn sign featurex ex)
                       (if (and (featurex-empty? featurex)
                                remove-newly-empty)
                           (remove sign node)
                           (values sign featurex ex))))))))
          node))
    ast)))

(defun transform-feature-expression (feature-expression fn)
  "Call FN, a function, on each feature in FEATURE-EXPRESSION.
Substitute the return value of FN for the existing feature.

If FN returns nil, the feature is removed.

FN may return any feature expression, not just a symbol."
  (match feature-expression
         ((or nil (list :or)) nil)
         ((and symbol (type symbol))
          (funcall fn symbol))
         ((list (or :and :or :not))
          nil)
         ((list :and feature)
          (transform-feature-expression feature fn))
         ((list :or feature)
          (transform-feature-expression feature fn))
         ((list* (and prefix (or :and :or :not)) features)
          (let ((new
                 (cons prefix
                       (remove nil
                               (remove-duplicates
                                (mappend (lambda (feature-expression)
                                           (match (transform-feature-expression feature-expression fn)
                                                  ((list* (and subprefix (or :and :or))
                                                          features)
                                                   (if (eql subprefix prefix)
                                                       features
                                                       (list features)))
                                                  (x (list x))))
                                         features)
                                :test #'equal)))))
            (if (equal new feature-expression) new
                (transform-feature-expression new fn))))))

(defun featurep-with (feature-expression *features*)
  "Test FEATURE-EXPRESSION against the features in *FEATURES*.

The global value of `*features*` is ignored."
  (featurep feature-expression))

(defun remove-expression-features (feature-expression features)
  "Remove FEATURES from FEATURE-EXPRESSION.
If there are no features left, `nil' is returned."
  (transform-feature-expression feature-expression
                                (lambda (feature)
                                  (unless (member feature features)
                                    feature))))

(defun remove-feature-support (ast features)
  "Remove support for FEATURES from AST.
Each feature in FEATURES will be removed from all feature expressions,
and if any of the resulting expressions are empty their guards (and
possibly expressions) will be omitted according to the sign of the guard."
  (map-reader-conditionals (lambda (sign featurex ex)
                             (let ((featurex (remove-expression-features featurex features)))
                               (values sign featurex ex)))
                           ast
                           :remove-newly-empty t))


;;; Bindings
(defvar *bindings-allows-macros-p* nil
  "A special variable that modifies the functionality
of get-functions-from-binding-form. If set to a non-nil value,
macros will be returned as if they were functions.")

(defvar *bindings-form-is-macro-p* nil
  "A special variable that modifies the functionality of
functions that get bindings such that the returned information
is marked as a macro.")

(defvar *bindings-allows-symbol-macros-p* nil
  "A special variable that modifies the functionality
of get-vars-from-binding-form. If set to a non-nil value,
symbol macros will be returned as if they were variables.")

(defvar *bindings-allows-top-level-p* t
  "A special variable that modifies the functionality
of bindings. If set to a non-nil value,top level variable
definitions will be included.")


(defgeneric bindings (obj ast &key functions variables all)
  (:documentation "Return bindings that are local to OBJ and
in scope of AST. This method calls get-vars-from-binding-form
and get-functions-from-binding-form. All of the special variables
that modify the behavior of these methods can be utilized by
this method.")
  (:method ((obj lisp) (ast lisp-ast)
            &key functions variables all
            &aux (genome (genome obj)))
    ;; TODO: there are some issues with quoted symbols.
    ;; NOTE: this method is an approximation of what is in scope
    ;;       and local to obj. If forms like defparameter or defvar
    ;;       are not top-level, their variables will not be found.
    ;;       The way top-level variables are collected may also
    ;;       be slightly incorrect in some corner cases.
    (labels ((get-enclosing-scopes ()
               "Get the paths of all enclosing scopes."
               (mapcar #'reverse
                       (maplist #'identity
                                (cdr (reverse (ast-path obj ast))))))
             (get-vars (form)
               "Get the variables bound by FORM."
               (when (or all variables)
                 (get-vars-from-binding-form
                  obj (compound-form-p form) form
                  :reference-ast ast)))
             (get-functions (form)
               "Get the functions bound by FORM."
               (when (or all functions)
                 (get-functions-from-binding-form
                  obj (compound-form-p form) form
                  :reference-ast ast)))
             (get-bindings (form)
               "Get the variables and functions bound by FORM."
               (reduce (lambda (bindings binding)
                         (cons binding bindings))
                       (get-functions form)
                       :initial-value (get-vars form)))
             (get-top-level-bindings ()
               "Get all the top-level variables that
              are defined in obj."
               (nest
                (mappend #'get-bindings)
                (remove-if {shares-path-of-p obj ast})
                (children-of-type genome 'expression-result)))
             (get-local-vars-in-scope ()
               "Get the local variables that are in scope
              of reference-ast."
               (mapcar [#'get-bindings {lookup genome}]
                       (get-enclosing-scopes))))
      (if *bindings-allows-top-level-p*
          (cons (get-top-level-bindings) (get-local-vars-in-scope))
          (get-local-vars-in-scope)))))


(defgeneric get-functions-from-binding-form
    (obj car-of-form binding-form &key reference-ast)
  (:documentation "Retrieves the functions defined by BINDING-FORM.
CAR-OF-FORM can be used as an 'eql specializer to dispatch to a specific
method for the form. REFERENCE-AST is a reference point for the method
to filter out variables that aren't in scope of it. This is useful for
forms like 'let* or for forms at the top-level which REFERENCE-AST may
not be in the body of, such as 'defun.

Methods for this generic shouldn't check if CAR-OF-FORM is
what is expected as there are different forms that
can have their variables retrieved in the exact same way,
such as let and when-let.")
  (:method (obj car-of-form binding-form &key reference-ast)
    (declare (ignorable obj car-of-form binding-form reference-ast))
    ;; If it hasn't dispatched on car-of-form before here,
    ;; it likely isn't a binding form.
    nil))

(defmacro define-get-functions-from-binding-form
    (binding-form-name (&key (software 'obj)
                          (binding-form 'binding-form)
                          (reference-ast 'reference-ast))
     &body body)
  "Defines a specialization of get-functions-from-binding-form that specializes
on BINDING-FORM-NAME. The keyword arguments allow for explicit naming of the
parameters of the method.

A convenience local function, handle-as, is defined
for the method body which allows for easy transferring to another
specialization to handle the form. For exmaple, if a 'labels form has a
reference ast that is inside it but after its definitions, it can
transfer control to 'flet by calling handle-as: (handle-as 'flet).
handle-as has a :reference-ast keyword which serves the same purpose
as in get-vars-from-binding-form. It also has a :macro-p keyword
which adds a cons to the returned list that shows that the symbol
is a macro.

A convenience local function, collect-function-info, is defined for the
method body which collects info on a form in a format that can be
returned."
  (let ((car-of-form (gensym)))
    `(defmethod get-functions-from-binding-form
         ((,software lisp) (,car-of-form (eql ',binding-form-name))
          (,binding-form lisp-ast) &key ,reference-ast)
       (declare (ignorable ,car-of-form))
       (labels ((handle-as (symbol
                            &key (macro-p nil) (reference-ast ,reference-ast)
                            &aux (*bindings-form-is-macro-p* macro-p))
                  "Convenience function that allows for
                   easy transfer to another method specialization
                   to HANDLE the current form as if it were a SYMBOL
                   form."
                  (get-functions-from-binding-form
                   ,software symbol ,binding-form :reference-ast ,reference-ast))
                (collect-function-info (form)
                  "Collects info about the function that is defined
                   in FORM."
                  (let ((name-ast (or (compound-form-p* form) form)))
                    `((:name . ,(expression name-ast))
                      (:name-ast . ,name-ast)
                      (:decl . ,form)
                      (:namespace . function)
                      (:macro . ,*bindings-form-is-macro-p*)
                      (:scope . ,,binding-form))))
                (collect-function-info* (form)
                  "Collects info about the function that is defined
                   in FORM. This is used for defun-style forms."
                  (let ((name-ast (car (get-compound-form-args form))))
                    `((:name . ,(expression name-ast))
                      (:name-ast . ,name-ast)
                      (:decl . ,form)
                      (:namespace . function)
                      (:macro . ,*bindings-form-is-macro-p*)
                      (:scope . ,,binding-form)))))
         (declare (ignorable (function handle-as)
                             (function collect-function-info)
                             (function collect-function-info*)))
         ,@body))))

(defmacro define-function-binding-form-alias (binding-form-alias binding-form)
  "Creates a get-function-from-binding-form method that specializes the
parameter car-of-form on BINDING-FORM-ALIAS. The method immediately calls
the specialization for BINDING-FORM."
  `(define-get-functions-from-binding-form ,binding-form-alias ()
     (handle-as ',binding-form)))

(define-get-functions-from-binding-form flet
    (:software obj :binding-form binding-form :reference-ast reference-ast)
  (match binding-form
    ((lisp-ast
      (children
       (list* (@@ 3 _) functions skipped-input _)))
     (when (or (not reference-ast)
               (and (later-than-p obj reference-ast skipped-input)
                    (ancestor-of-p obj reference-ast binding-form)))
       (mapcar #'collect-function-info
               (children-of-type functions 'expression-result))))))

(define-get-functions-from-binding-form labels
    (:software obj :binding-form binding-form :reference-ast reference-ast)
  (match binding-form
    ((lisp-ast
      (children
       (list* (@@ 3 _) definitions _)))
     (let* ((no-reference-p (not reference-ast))
            (ancestor-of-definitions-p
              (and (not no-reference-p)
                   (ancestor-of-p obj reference-ast definitions))))
       (cond
         ((or no-reference-p
              (and (not ancestor-of-definitions-p)
                   (later-than-p obj reference-ast definitions)))
          (handle-as 'flet))
         (ancestor-of-definitions-p
          (mapcar
           #'collect-function-info
           (remove-if
            (lambda (ast)
              (later-than-p obj ast reference-ast))
            (children-of-type definitions 'expression-result)))))))))

(define-get-functions-from-binding-form defun
    (:binding-form binding-form)
  (list (collect-function-info* binding-form)))

(define-function-binding-form-alias defmethod defun)

;;; TODO: add support for defgeneric.

(define-get-functions-from-binding-form defmacro ()
  (when *bindings-allows-macros-p*
    (handle-as 'defun :macro-p t)))

(define-get-functions-from-binding-form macrolet ()
  (when *bindings-allows-macros-p*
    (handle-as 'flet :macro-p t)))

(defgeneric get-vars-from-binding-form
    (obj car-of-form binding-form &key reference-ast)
  (:documentation "Retrieves the variables defined by BINDING-FORM.
CAR-OF-FORM can be used as an 'eql specializer to dispatch to a specific
method for the form. REFERENCE-AST is a reference point for the method
to filter out variables that aren't in scope of it. This is useful for
forms like 'let* or for forms at the top-level which REFERENCE-AST may
not be in the body of, such as 'defun.

Methods for this generic shouldn't check if CAR-OF-FORM is
what is expected as there are different forms that
can have their variables retrieved in the exact same way,
such as let and when-let.")
  (:method (obj car-of-form binding-form &key reference-ast)
    (declare (ignorable obj car-of-form binding-form reference-ast))
    ;; If it hasn't dispatched on car-of-form before here,
    ;; it likely isn't a binding form.
    nil))

(defmacro define-get-vars-from-binding-form
    (binding-form-name (&key (software 'obj)
                          (binding-form 'binding-form)
                          (reference-ast 'reference-ast))
     &body body)
  "Defines a specialization of get-vars-from-binding-form that specializes
on BINDING-FORM-NAME. The keyword arguments allow for explicit naming of the
parameters of the method.

A convenience local function, handle-as, is defined
for the method body which allows for easy transferring to another
specialization to handle the form. For exmaple, if a 'let* form has a
reference ast that is inside it but after its definitions,it can
transfer control to 'let by calling handle-as: (handle-as 'let).
handle-as has a :reference-ast keyword which serves the same purpose
as in get-vars-from-binding-form. It also has a :macro-p keyword
which adds a cons to the returned list that shows that the symbol
is a macro.

A convenience local function, collect-var-info, is defined for the
method body which collects info on a form in a format that can be
returned."
  (let ((car-of-form (gensym)))
    `(defmethod get-vars-from-binding-form
         ((,software lisp) (,car-of-form (eql ',binding-form-name))
          (,binding-form lisp-ast) &key ,reference-ast)
       (declare (ignorable ,car-of-form))
       (labels ((handle-as (symbol
                            &key (macro-p nil) (reference-ast ,reference-ast)
                            &aux (*bindings-form-is-macro-p* macro-p))
                  "Convenience function that allows for easy transfer to another
                   method specialization to HANDLE the current form as if it were
                   a SYMBOL form."
                  (get-vars-from-binding-form ,software symbol ,binding-form
                                              :reference-ast ,reference-ast))
                (collect-var-info (form)
                  "Collects info about the variable that is defined in FORM."
                  (let ((name-ast (or (compound-form-p* form) form)))
                    `((:name . ,(expression name-ast))
                      (:name-ast . ,name-ast)
                      (:decl . ,form)
                      (:namespace . variable)
                      (:macro . ,*bindings-form-is-macro-p*)
                      (:scope . ,,binding-form)))))
         (declare (ignorable (function handle-as) (function collect-var-info)))
         ,@body))))

(defmacro define-var-binding-form-alias (binding-form-alias binding-form)
  "Creates a get-vars-from-binding-form method that specializes the
parameter car-of-form on BINDING-FORM-ALIAS. The method immediately calls
the specialization for BINDING-FORM."
  `(define-get-vars-from-binding-form ,binding-form-alias ()
     (handle-as ',binding-form)))

(-> get-required-vars-from-lambda-list (lisp-ast) t)
(defun get-required-vars-from-lambda-list (lambda-list)
  "Return the required parameters of LAMBDA-LIST."
  (iter
    (for parameter in (children-of-type lambda-list 'expression-result))
    (until (member (expression parameter) lambda-list-keywords))
    (collect parameter)))

(-> get-keyword-vars-from-lambda-list (lisp-ast) t)
(defun get-keyword-vars-from-lambda-list (lambda-list)
  "Return the keyword parameters and the position &key appears
in LAMBDA-list as values."
  (labels ((get-special-case (form)
             "Gets the variable name of a keyword argument
            when it has a keyword that varies from the
            variable name."
             (match form
               ;; keyword special case
               ((lisp-ast
                 (children
                  (list*
                   _
                   (lisp-ast
                    (children
                     (list* _ keyword _ name _)))
                   _)))
                (list name (expression keyword)))))
           (get-var-name-and-keyword (ast)
             "Get the variable name and the keyword associated with
              it as a list."
             (or (get-special-case ast)
                 (let ((var-name (or (compound-form-p* ast)
                                     ast)))
                   (list var-name (make-keyword (expression var-name)))))))
    (when-let* ((parameters (children-of-type lambda-list 'expression-result))
                (keyword-position (position-if [{eq '&key} #'expression]
                                               parameters)))
      (values
       (iter
         (for parameter in (subseq parameters (1+ keyword-position)))
         (until (member (expression parameter) lambda-list-keywords))
         (collect (get-var-name-and-keyword parameter)))
       keyword-position))))

(-> get-optional-vars-from-lambda-list (lisp-ast) t)
(defun get-optional-vars-from-lambda-list (lambda-list)
  "Return the optional parameters and the position &optional appears
in LAMBDA-list as values."
  (flet ((get-var-name (ast)
           (or (compound-form-p* ast)
               ast)))
    (when-let* ((parameters (children-of-type lambda-list 'expression-result))
                (optional-position (position-if [{eq '&optional} #'expression]
                                                parameters)))
      (values
       (iter
         (for parameter in (subseq parameters (1+ optional-position)))
         (until (member (expression parameter) lambda-list-keywords))
         (collect (get-var-name parameter)))
       optional-position))))

(-> get-rest-from-lambda-list (lisp-ast) t)
(defun get-rest-from-lambda-list (lambda-list)
  "Return the rest parameters and the position &rest appears
in LAMBDA-list as values."
  (when-let* ((parameters (children-of-type lambda-list 'expression-result))
              (rest-position (position-if [{eq '&rest} #'expression]
                                              parameters)))
    (values
     (list (nth (1+ rest-position) parameters))
     rest-position)))

(defun get-vars-from-ordinary-lambda-list
    (obj binding-form lambda-list &key reference-ast)
  "Get the vars from LAMBDA-LIST treating it as an ordinary lambda list.
Uses REFERENCE-AST to determine what's in scope of the default value forms."
  (labels ((get-key-special-case (form)
             "Gets the variable name of a keyword argument
              when it has a keyword that varies from the
              variable name."
             (match form
               ;; keyword special case
               ((lisp-ast
                 (children
                  (list*
                   _
                   (lisp-ast
                    (children
                     (list* (@@ 3 _) name _)))
                   _)))
                name)))
           (collect-var-info* (form)
             "An extended version of collect-var-info
              that handles the case where a keyword argument
              has a variable name that differs from the keyword."
             (let ((name-ast (or (get-key-special-case form)
                                 (compound-form-p* form)
                                 form)))
               `((:name . ,(expression name-ast))
                 (:name-ast . ,name-ast)
                 (:decl . ,form)
                 (:namespace . variable)
                 (:macro . ,*bindings-form-is-macro-p*)
                 (:scope . ,binding-form))))
           (get-parameter-asts ()
             "Returns all asts in the children of lambda list
              that aren't symbols starting with '&'."
             (remove-if [{member _ lambda-list-keywords} #'expression]
                        (children-of-type lambda-list 'expression-result))))
    (let* ((no-reference-p (not reference-ast))
           (ancestor-of-lambda-list-p
             (and (not no-reference-p)
                  (ancestor-of-p obj reference-ast lambda-list))))
      (cond
        ((or no-reference-p
             (and (not ancestor-of-lambda-list-p)
                  (later-than-p obj reference-ast lambda-list)))
         (mapcar #'collect-var-info*
                 (get-parameter-asts)))
        (ancestor-of-lambda-list-p
         ;; NOTE: in a lambda list, such as (a b c &optional d),
         ;;       a reference ast of 'c' will return 'a' and 'b'
         ;;       as being in scope. This shouldn't be a problem,
         ;;       but this note is here in case it becomes one.
         ;;
         ;; Collect the definitions that occur before it.
         (mapcar
          #'collect-var-info*
          ;; Remove the form reference-ast is in.
          (butlast
           (remove-if
            (lambda (ast)
              (later-than-p obj ast reference-ast))
            (get-parameter-asts)))))))))

(define-get-vars-from-binding-form let
    (:software obj :binding-form binding-form :reference-ast reference-ast)
  (match binding-form
    ((lisp-ast
      (children
       (list* (@@ 3 _) definitions _)))
     (when (or (not reference-ast)
               (and (not (ancestor-of-p obj reference-ast definitions))
                    (later-than-p obj reference-ast definitions)
                    (ancestor-of-p obj reference-ast binding-form)))
       (mapcar #'collect-var-info
               (children-of-type definitions 'expression-result))))))

(define-get-vars-from-binding-form let*
    (:software obj :binding-form binding-form :reference-ast reference-ast)
  (match binding-form
    ((lisp-ast
      (children
       (list* (@@ 3 _) definitions _)))
     (let* ((no-reference-p (not reference-ast))
            (ancestor-of-definitions-p
              (and (not no-reference-p)
                   (ancestor-of-p obj reference-ast definitions))))
       (cond
         ((or no-reference-p
              (and (not ancestor-of-definitions-p)
                   (later-than-p obj reference-ast definitions)))
          (handle-as 'let))
         (ancestor-of-definitions-p
          ;; Collect the definitions that occur before it.
          (mapcar
           #'collect-var-info
           ;; Remove the form reference-ast is in.
           (butlast
            (remove-if
             (lambda (ast)
               (later-than-p obj ast reference-ast))
             (children-of-type definitions 'expression-result))))))))))

(define-var-binding-form-alias when-let let)
(define-var-binding-form-alias when-let* let*)

(define-get-vars-from-binding-form if-let
    (:software obj :binding-form binding-form :reference-ast reference-ast)
  (match binding-form
    ((lisp-ast
      (children
       (list* (@@ 7 _) else-clause _)))
     (unless (and reference-ast (shares-path-of-p obj reference-ast else-clause))
       (handle-as 'let)))))

(define-get-vars-from-binding-form if-let*
    (:software obj :binding-form binding-form :reference-ast reference-ast)
  (match binding-form
    ((lisp-ast
      (children
       (list* (@@ 7 _) else-clause _)))
     (unless (and reference-ast (shares-path-of-p obj reference-ast else-clause))
       (handle-as 'let*)))))

(define-get-vars-from-binding-form defun
    (:software obj :binding-form binding-form :reference-ast reference-ast)
  (match binding-form
    ((lisp-ast
      (children
       (list* (@@ 5 _) lambda-list _)))
     (when (or (not reference-ast)
               (later-than-p obj reference-ast lambda-list))
       (get-vars-from-ordinary-lambda-list
        obj binding-form lambda-list :reference-ast reference-ast)))))

(define-get-vars-from-binding-form flet
    (:software obj :binding-form binding-form :reference-ast reference-ast)
  (labels ((in-local-function-p (local-functions)
             "If REFERENCE-AST is in a local function defintion,
              return the local function definition."
             (find-if {shares-path-of-p obj reference-ast}
                      (children-of-type local-functions 'expression-result)))
           (get-local-fun-params (local-function)
             "Get the parameters from LOCAL-FUNCTION that are
              in scope of reference-ast."
             (match local-function
               ((lisp-ast
                 (children
                  (list* (@@ 3 _) lambda-list _)))
                (when (later-than-p obj reference-ast lambda-list)
                  (get-vars-from-ordinary-lambda-list
                   obj binding-form lambda-list
                   :reference-ast reference-ast))))))
    ;; Difficult to determine what should be returned without
    ;; a reference-ast, so only return something when it is provided.
    (when reference-ast
      (match binding-form
        ((lisp-ast
          (children
           (list* (@@ 3 _) functions _)))
         (get-local-fun-params (in-local-function-p functions)))))))

(define-var-binding-form-alias labels flet)

(define-get-vars-from-binding-form symbol-macrolet ()
  (when *bindings-allows-symbol-macros-p*
    (handle-as 'let :macro-p t)))

(define-get-vars-from-binding-form defvar (:binding-form binding-form)
  (match binding-form
    ((lisp-ast
      (children
       (list* (@@ 3 _) var _)))
     (list (collect-var-info var)))))

(define-var-binding-form-alias defparameter defvar)

(define-get-vars-from-binding-form define-symbol-macro ()
  (when *bindings-allows-symbol-macros-p*
    (handle-as 'defvar :macro-p t)))

(defmethod scopes ((obj lisp) (ast lisp-ast))
  ;; TODO: add removing variables with duplicate names
  ;;       at the very end.
  (bindings obj ast :variables t))


;;; Function body
(defgeneric fun-body (car-of-form function-declaration)
  (:documentation "Retrieve the function body of FUNCTION-DECLARATION as
though it were defined in a CAR-OF-FORM form. The trailing parenthesis
should be removed. *bindings-allows-macros-p* can be set to get the body
from macros.")
  (:method (car-of-form declaration)
    (declare (ignorable car-of-form declaration))
    nil))

(defmacro define-fun-body-alias (car-of-form-alias car-of-form)
  "Creates a fun-body method that specializes the
parameter car-of-form on CAR-OF-FORM-ALIAS. The method immediately calls
the specialization for CAR-OF-FORM."
  `(defmethod fun-body
       ((car-of-form (eql ',car-of-form-alias)) (function-declaration lisp-ast))
     (fun-body ',car-of-form function-declaration)))

(-> get-top-level-fun-body (lisp-ast) (or null list))
(defun get-top-level-fun-body (ast)
  "Return the body of the top-level function represented by AST. "
  (match ast
    ((lisp-ast
      (children (list* (@@ 7 _) body-ast)))
     ;; Remove the trailing paren.
     (butlast body-ast))))

(-> get-local-fun-body (lisp-ast) (or null list))
(defun get-local-fun-body (ast)
  "Return the body of the local function represented by AST. "
  (match ast
    ((lisp-ast
      (children (list* (@@ 5 _) body-ast)))
     ;; Remove the trailing paren.
     (butlast body-ast))))

(defmethod fun-body
    ((car-of-form (eql 'defun)) (function-declaration lisp-ast))
  (get-top-level-fun-body function-declaration))

(defmethod fun-body
    ((car-of-form (eql 'defmethod)) (function-declaration lisp-ast))
  (match function-declaration
    ((lisp-ast
      (children
       (or
        (list* (@@ 5 _) (satisfies not-a-list-p) (@@ 3 _) body-ast)
        (list* (@@ 7 _) body-ast))))
     (butlast body-ast))))

(defmethod fun-body
    ((car-of-form (eql 'flet)) (function-declaration lisp-ast))
  (get-local-fun-body function-declaration))

(define-fun-body-alias labels flet)

(defmethod fun-body
    ((car-of-form (eql 'defmacro)) (function-declaration lisp-ast))
  (when *bindings-allows-macros-p*
    (fun-body 'defun function-declaration)))

(defmethod fun-body
    ((car-of-form (eql 'macrolet)) (function-declaration lisp-ast))
  (when *bindings-allows-macros-p*
    (fun-body 'flet function-declaration)))


;;; Function lambda list
(defgeneric lambda-list (car-of-form function-declaration)
  (:documentation "Retrieve the lambda list of FUNCTION-DECLARATION as
though it were defined in a CAR-OF-FORM form. *bindings-allows-macros-p*
can be set to retrieve lambda lists from macros.")
  (:method (car-of-form function-declaration)
    (declare (ignorable car-of-form function-declaration))
    nil))

(defmacro define-lambda-list-alias (car-of-form-alias car-of-form)
  "Creates a lambda-list method that specializes the
parameter car-of-form on CAR-OF-FORM-ALIAS. The method immediately calls
the specialization for CAR-OF-FORM."
  `(defmethod lambda-list
       ((car-of-form (eql ',car-of-form-alias)) (function-declaration lisp-ast))
     (lambda-list ',car-of-form function-declaration)))

(-> get-top-level-lambda-list (lisp-ast) (or null lisp-ast))
(defun get-top-level-lambda-list (ast)
  "Return the lambda list of the function represented by AST."
  (match ast
    ((lisp-ast
      (children
       (list* (@@ 5 _) lambda-list _)))
     lambda-list)))

(-> get-local-lambda-list (lisp-ast) (or null lisp-ast))
(defun get-local-lambda-list (ast)
  "Return the lambda list of the function represented by AST."
  (match ast
    ((lisp-ast
      (children
       (list* (@@ 3 _) lambda-list _)))
     lambda-list)))

(defmethod lambda-list
    ((car-of-form (eql 'defun)) (function-declaration lisp-ast))
  (get-top-level-lambda-list function-declaration))

(defmethod lambda-list
    ((car-of-form (eql 'defmethod)) (function-declaration lisp-ast))
  (match function-declaration
    ((lisp-ast
      (children
       (or
        (list* (@@ 5 _) (satisfies not-a-list-p) _ lambda-list _)
        (list* (@@ 5 _) lambda-list _))))
     lambda-list)))

(defmethod lambda-list
    ((car-of-form (eql 'flet)) (function-declaration lisp-ast))
  (get-local-lambda-list function-declaration))

(define-lambda-list-alias labels flet)

(defmethod lambda-list
    ((car-of-form (eql 'defmacro)) (function-declaration lisp-ast))
  (when *bindings-allows-macros-p*
    (get-top-level-fun-body function-declaration)))

(defmethod lambda-list
    ((car-of-form (eql 'macrolet)) (function-declaration lisp-ast))
  (when *bindings-allows-macros-p*
    (get-local-fun-body function-declaration)))


;;; Symbol Mappings
(defun map-symbol-information (obj leaf-ast hash-table)
  "In HASH-TABLE, map LEAF-AST to information on what it might be."
  ;; NOTE: there will be some false positives here, but
  ;;       this probably won't be a problem for the
  ;;       immediate use case.
  ;;       This currently falls apart with quotes.
  ;;       It will act incorrectly with special variables too.
  ;;       Various reader macros, such as #' or {}, can cause problems.
  (unless (children leaf-ast)
    (let ((bindings (bindings obj leaf-ast :all t))
          (symbol (expression leaf-ast)))
      (setf
       (gethash leaf-ast hash-table)
       (if (car-of-enclosing-form-p obj leaf-ast)
           ;; Function namespace
           (cond-let binding
             ;; Check for literal first to prevent following
             ;;  functions from throwing an error.
             ((literalp leaf-ast) '(:literal))
             ((special-operator-p symbol) '(:special-form))
             ((bindings-contains-function-p bindings symbol :allow-macros t)
              ;; Back-patch if the ast has been set to something
              ;;  as it's most likely incorrect.
              (symbol-macrolet ((binding-type (gethash (aget :name-ast binding)
                                                       hash-table)))
                (when binding-type
                  (setf binding-type '(:function-declaration))))
              ;; Return the full binding for now.
              (if (aget :macro binding)
                  `(:macro ,binding)
                  `(:function ,binding)))
             ((macro-function symbol) '(:macro))
             ((fboundp symbol) '(:function))
             ;; At this point, the symbol isn't bound as a function or macro.
             ;; Since the variable namespace makes a distinction between
             ;; keyword and symbol, make the same distinction here.
             ((keywordp symbol) '(:keyword))
             ;; default to symbol for now.
             (t '(:symbol)))
           ;; Variable namespace
           (cond-let binding
             ;; Check for literal first to prevent following
             ;;  functions from throwing an error.
             ((literalp leaf-ast) '(:literal))
             ((bindings-contains-variable-p bindings symbol :allow-macros t)
              ;; Back-patch if the ast has been set to something
              ;;  as it's most likely incorrect.
              (symbol-macrolet ((binding-type (gethash (aget :name-ast binding)
                                                       hash-table)))
                (when binding-type
                  (setf binding-type '(:variable-declaration))))
              ;; Return the full binding for now.
              (if (aget :macro binding)
                  `(:symbol-macro ,binding)
                  `(:variable ,binding)))
             ((not (eq symbol (macroexpand-1 symbol))) '(:symbol-macro))
             ;; Keywords are technically constant variables too, so
             ;; check before the #'boundp clause.
             ((keywordp symbol) '(:keyword))
             ((boundp symbol) '(:variable))
             ;; default to symbol for now.
             (t '(:symbol))))))))

(defun collect-symbols
    (obj ast
     &aux (table (make-hash-table))
       (*bindings-allows-macros-p* t)
       (*bindings-allows-symbol-macros-p* t))
  "Collect the symbols in AST into a hash table.
The hash table maps symbol asts to their possible usage.
This can be in the form of any of the following:

  (:special-form)
  (:function-declaration)
  (:macro)
  (:macro binding-information)
  (:function)
  (:function binding-information)
  (:variable-declaration)
  (:symbol-macro)
  (:symbol-macro binding-information)
  (:variable)
  (:variable binding-information)
  (:literal)
  (:keyword)
  (:symbol)

binding-information will be in the same form as
returned by #'bindings and will contain all information
gathered for that specific symbol."
  (mapc (lambda (leaf-ast)
          (map-symbol-information obj leaf-ast table))
        (collect-if (lambda (child)
                      (and (typep child 'expression-result)
                           (not (children child))))
                    ast))
  table)

(-> map-arguments-to-parameters (lisp lisp-ast) list)
(defun map-arguments-to-parameters (obj funcall-ast)
  "Map the arguments of FUNCALL-AST to its relevant function call in OBJ.
The mapping will be from argument ast to parameter ast in an alist."
  (when-let*
      ((bindings (bindings obj funcall-ast :functions t))
       (function-binding
        (bindings-contains-function-p bindings (compound-form-p funcall-ast)))
       (lambda-list-ast
        (when-let ((function-name (aget :scope function-binding)))
          (lambda-list (compound-form-p function-name)
                       (aget :decl function-binding))))
       (arg-list (get-compound-form-args funcall-ast)))
    (flet ((get-required-mapping ()
             "Return a mapping of required parameters to their
              arguments in funcall-ast."
             (cons
              :required
              (mapcar
               (lambda (ast)
                 (cons (pop arg-list) ast))
               (get-required-vars-from-lambda-list lambda-list-ast))))
           (get-optional-mapping
               (&aux (optional-vars
                      (get-optional-vars-from-lambda-list lambda-list-ast)))
             "Return a mapping of optional parameters to their
              arguments in funcall-ast."
             (cons
              :optional
              (mapcar
               (lambda (ast)
                 (cons (pop arg-list) ast))
               ;; TODO: is there a function to do this?
               ;; Trim optional-vars if it is longer than arg-list.
               (mapcar (lambda (parameter arg)
                         (declare (ignorable arg))
                         parameter)
                       optional-vars arg-list))))
           (get-rest-mapping ()
             "Return a mapping of the rest parameter to the
              arguments in funcall-ast that it applies to."
             (cons
              :rest
              (when-let ((rest-var (get-rest-from-lambda-list lambda-list-ast)))
                ;; Maintain the same form as the other mappings
                ;; by calling #'list.
                (list (cons (car rest-var) (copy-list arg-list))))))
           (get-keyword-mapping (&aux (keyword (get-keyword-vars-from-lambda-list
                                                lambda-list-ast)))
             "Return a mapping of keyword parameters to their
              arguments in funcall-ast."
             (cons
              :keyword
              (iter
                (while arg-list)
                (for arg = (pop arg-list))
                (for target-keyword = (is-keyword-p arg))
                (for key-pair = (when target-keyword
                                  (find-if {eq target-keyword}
                                           keyword :key #'cadr)))
                (when key-pair
                  (collect (cons (pop arg-list) (car key-pair))))))))
      (list (get-required-mapping)
            (get-optional-mapping)
            (get-rest-mapping)
            (get-keyword-mapping)))))


;;; Utility
(-> compound-form-p (lisp-ast &key (:name symbol)) t)
(defun compound-form-p (ast &key name)
  "If the AST is a compound form, return the car of the form. If NAME is
provided, return T if the car of the form is eq to NAME."
  (match ast
    ((lisp-ast
      (children (list* _ (expression-result (expression form-name)) _)))
     (cond
       (name (eq name form-name))
       ((symbolp form-name) form-name)))))

(-> compound-form-p* (lisp-ast) t)
(defun compound-form-p* (ast)
  "If the AST is a compound form, return the AST in the car of the form."
  (match ast
    ((lisp-ast
      (children (list* _ form-name-ast _)))
     form-name-ast)))

(-> get-compound-form-args (lisp-ast) list)
(defun get-compound-form-args (ast)
  "Return the args to the compound form represented by AST."
  (match ast
    ((lisp-ast
      (children (list* _ _ args)))
     (remove-if-not {typep _ 'expression-result} args))))

(-> literalp (lisp-ast) boolean)
(defun literalp (ast)
  "Return T if AST likely represents a literal."
  (typecase (expression ast)
    (cons nil)
    (symbol nil)
    (t t)))

(-> is-keyword-p (lisp-ast) t)
(defun is-keyword-p (ast)
  "If AST represents a keyword, return the keyword."
  (match ast
    ((lisp-ast
      (expression (type keyword)))
     (expression ast))))

(-> not-a-list-p (lisp-ast) t)
(defun not-a-list-p (ast &aux (first-child (car (children ast))))
  "Returns T if AST is probably not a list."
  (or (not (typep first-child 'skipped-input-result))
      (with-slots (string-pointer start end)
          first-child
        (not (scan "^\\s*\\(\\s*$" (subseq string-pointer start end))))))

(-> quote-p (lisp-ast) (or null lisp-ast))
(defun quote-p (ast)
  "Return the quoted form if AST represents a quote ast."
  (match ast
    ((lisp-ast
      (children
       (list (reader-quote) form)))
     form)
    ((lisp-ast
      (children
       (list _ (expression-result (expression 'quote)) _ form _)))
     form)))

(-> quasiquote-p (lisp-ast) (or null lisp-ast))
(defun quasiquote-p (ast)
  "Return the quoted form if AST represents a quasiquote ast."
  (match ast
    ((lisp-ast
      (children
       (list (reader-quasiquote) form)))
     form)
    ((lisp-ast
      (children
       (list _ #+sbcl
               (expression-result (expression 'sb-int:quasiquote))
               #-sbcl
               (guard (expression-result
                       (string-pointer str)
                       (start start)
                       (end end))
                      (string= "`" (subseq str start end)))
             _ form _)))
     form)))

(-> quoted-p (lisp lisp-ast) (or null lisp-ast))
(defun quoted-p (obj ast)
  "Return the quoted form if the AST is quoted."
  ;; TODO: This does not currently handle unquotes;
  ;;       it only checks if there is a quote or
  ;;       quasiquote somewhere above.
  (find-if-in-scope «or #'quote-p #'quasiquote-p» obj ast))

(->  find-in-defining-form (lisp lisp-ast symbol
                                 &key (:referencing-ast lisp-ast))
     (or null lisp-ast))
(defun find-in-defining-form (obj defining-form name &key referencing-ast)
  "Returns the ast in DEFINING-FORM that defines NAME.
If REFERENCING-AST is supplied, the returned ast must
occur before it."
  (let ((targeter (if referencing-ast
                      (lambda (target)
                        (and (path-later-p obj referencing-ast target)
                             (compound-form-p target :name name)))
                      {compound-form-p _ :name name})))
    (match defining-form
      ((lisp-ast
        (children (list* (@@ 3 _) definition-list _)))
       (cl:find-if targeter
                   (reverse
                    (remove-if-not {typep _ 'expression-result}
                                   (children definition-list))))))))

(-> find-local-function (lisp lisp-ast symbol &key (:referencing-ast lisp-ast))
    (or null lisp-ast))
(defun find-local-function (obj enclosed-form function-name
                            &key (referencing-ast enclosed-form))
  "Return the ast of the local function named FUNCTION-NAME
which is in scope of ENCLOSED-FORM."
  (when-let ((defining-form (find-if-in-scope [«or {eq 'flet} {eq 'labels}»
                                                #'compound-form-p]
                                               obj enclosed-form)))
    (if-let (local-function
             (find-in-defining-form obj defining-form function-name
                                    :referencing-ast referencing-ast))
      local-function
      (find-local-function obj defining-form function-name
                           :referencing-ast referencing-ast))))

(defmethod find-if-in-scope (predicate (obj lisp) (ast lisp-ast)
                              &key reference-ast)
  (declare (ignorable reference-ast))
  (when-let* ((enclosing-form-path (enclosing-scope obj ast))
              (enclosing-ast (lookup (genome obj) enclosing-form-path)))
    (if (funcall predicate enclosing-ast)
        enclosing-ast
        (find-if-in-scope predicate obj enclosing-ast))))

(defmethod enclosing-scope ((obj lisp) (ast lisp-ast))
  ;; Returns nil if already at the top level.
  (butlast (ast-path obj ast)))

(-> children-of-type (lisp-ast symbol) list)
(defun children-of-type (ast type)
  "Returns a list of the children of AST that are of type TYPE."
  (remove-if-not {typep _ type} (children ast)))

(-> later-than-p (lisp lisp-ast lisp-ast) t)
(defun later-than-p (obj later-than earlier-than)
  "Returns T if LATER-THAN occurs later than EARLIER-THAN
in OBJ. Note that children of EARLIER-THAN are considered
later than."
  (path-later-p obj later-than earlier-than))

(-> car-of-enclosing-form-p (lisp lisp-ast) t)
(defun car-of-enclosing-form-p (obj ast)
  "Return T if AST is the car of its enclosing form in OBJ."
  (match (lookup (genome obj) (enclosing-scope obj ast))
    ((lisp-ast
      (children (list* _ car _)))
     (eq car ast))))

(-> scope-contains-function-p (list t) list)
(defun scope-contains-function-p (scope symbol)
  "Return the binding for SYMBOL in a list if it is in SCOPE."
  (find-if «and [{equal symbol} {aget :name}]
                [{equal 'function} {aget :namespace}]»
           scope))

(-> scope-contains-variable-p (list t) list)
(defun scope-contains-variable-p (scope symbol)
  "Return the binding for SYMBOL in a list if it is in SCOPE."
  (find-if «and [{equal symbol} {aget :name}]
                [{equal 'variable} {aget :namespace}]»
           scope))

(-> bindings-contains-function-p (list t &key (:allow-macros t)) list)
(defun bindings-contains-function-p (bindings symbol &key allow-macros)
  "Return the binding for SYMBOL if it is in BINDINGS."
  (iter (for scope in (reverse bindings))
        (when-let ((binding (scope-contains-function-p scope symbol)))
          (unless (and (not allow-macros) (aget :macro binding))
            (return binding)))))

(-> bindings-contains-variable-p (list t &key (:allow-macros t)) list)
(defun bindings-contains-variable-p (bindings symbol &key allow-macros)
  "Return the binding for SYMBOL if it is in BINDINGS."
  (iter (for scope in (reverse bindings))
        (when-let ((binding (scope-contains-variable-p scope symbol)))
          (unless (and (not allow-macros) (aget :macro binding))
            (return binding)))))


;;; Example
#+example
(progn

;;; Rewrite ->> to use nest instead.
  (defun fix-double-arrow (node)
    (flet ((children (node)
             (let* ((*string* "nest")
                    (nest (make-instance 'expression-result
                            :expression 'nest :start 0 :end (length *string*)))
                    (space (remove-if-not
                            {typep _ 'skipped-input-result} (children node)))
                    (exprs (cons nest (reverse (cdr (remove-if-not
                                                     {typep _ 'expression-result}
                                                     (children node)))))))
               (mapcar ‹etypecase (skipped-input-result (pop space))
                        (expression-result (pop exprs))›
                        (children node)))))
      (let* ((*string* nil)
             (expression (cons 'nest (reverse (cdr (expression node)))))
             (children (children node)))
        (make-instance 'expression-result
          :expression expression
          :children children))))

  (defun rewrite-double-arrow (software)
    (setf (genome software)
          (mapcar (lambda (node)
                    (if (and (typep node 'expression-result)
                             (listp (expression node))
                             (equal '->> (first (expression node))))
                        (fix-double-arrow node)
                        node))
                    (genome software))))

  (defun rewrite-double-arrow-in-place (file)
    (string-to-file (source-text (rewrite-double-arrow
                                  (from-file (make-instance 'lisp) file))) file)))
