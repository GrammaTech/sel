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
  (:shadowing-import-from :eclector.readtable
                          :copy-readtable
                          :set-dispatch-macro-character)
  (:shadowing-import-from :eclector.parse-result
                          :read
                          :read-from-string
                          :read-preserving-whitespace)
  (:export :lisp :lisp-ast :lisp-ast-p
           :expression :expression-result
           :feature-guard
           :feature-expression
           :*string*
           :transform-feature-guard
           :walk-feature-expressions
           :walk-feature-guards
           :map-feature-guards
           :map-feature-expressions
           :transform-feature-expression
           :featurep-with
           :remove-expression-features
           :remove-feature-support))
(in-package :software-evolution-library/software/lisp)
(in-readtable :curry-compose-reader-macros)


(defvar *string* nil)

(defclass lisp-ast (ast)
  ((expression :initarg :expression :initform nil :reader expression
               :type list)
   (children :reader children
             :type list
             :initarg :children
             :initform nil
             :documentation "The list of children of the node,
which may be more nodes, or other values.")
   (child-slots :initform '(children) :allocation :class)
   (data-slot :initform 'expression :allocation :class))
  (:documentation "Class of Common Lisp ASTs."))

(defmethod fset-default-node-accessor ((node-type (eql 'lisp-ast)))
  'expression)

(defclass result (lisp-ast)
  ((start :initarg :start :initform (when *string* 0)
          :reader start :type (or null (integer 0 *)))
   (end :initarg :end :initform (when *string* (length *string*))
        :reader end :type (or null (integer 0 *)))
   (string-pointer :initarg :string-pointer :initform *string*
                   :reader string-pointer :type (or null string))))

(defclass expression-result (result) ())

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

(defclass feature-guard (expression-result)
  ((feature-expression :initarg :feature-expression
                       :reader feature-expression)))

(defclass skipped-input-result (result)
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

(defclass reader-token (skipped-input-result)
  ())

(defmethod source-text ((obj reader-token) &optional stream)
  (with-slots (string-pointer) obj
    (write-string string-pointer stream)))

(defmethod print-object ((obj reader-token) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t))))

(defclass sharpsign-dot (reader-token)
  ((reason :initform :read-eval)
   (string-pointer :initform "#.")
   (start :initform 0)
   (end :initform 2)))

(defclass reader-conditional (reader-token)
  ((reason :initform :reader-conditional)
   (start :initform 0)
   (end :initform 2)))

(defclass sharpsign-plus (reader-conditional)
  ((string-pointer :initform "#+")))

(defclass sharpsign-minus (reader-conditional)
  ((string-pointer :initform "#-")))

(def sharpsign-dot (make-instance 'sharpsign-dot))
(def sharpsign-plus (make-instance 'sharpsign-plus))
(def sharpsign-minus (make-instance 'sharpsign-minus))

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
             (let ((*string* (string-downcase (symbol-name symbol))))
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
(defclass client (parse-result-client) ())

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
   (make 'feature-guard
         :start start
         :end end
         :feature-expression feature-expression
         :expression expression
         :children (append
                    (list
                     (make (ecase char
                             (#\+ 'sharpsign-plus)
                             (#\- 'sharpsign-minus))
                           ;; These are for the benefit of read+.
                           :start start
                           :end (+ start 2)))
                    (list feature-expression)
                    (list expression)))))

(defgeneric transform-feature-guard (result fn)
  (:documentation "If RESULT is a feature expression, call FN with three arguments: the sign, as a character (+ or -); the actual test, as a list; and the guarded expression. FN should return three values - a new sign, a new test, and a new expression - which are used to build a new feature expression that replaces the old one."))

(defmethod transform-feature-guard ((result feature-guard) fn)
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
              result
              (make 'feature-guard
                    :start (start result)
                    :end (end result)
                    :feature-expression new-test
                    :expression ex
                    :children
                    (mapcar (lambda (child)
                              (typecase child
                                (reader-token
                                 (ecase new-sign
                                   (#\+ sharpsign-plus)
                                   (#\- sharpsign-minus)))
                                (expression-result
                                 (econd ((eql child test)
                                         (if (equal new-test (expression test))
                                             test
                                             (convert 'lisp-ast new-test :keyword-prefix "")))
                                        ((eql child ex) new-ex)))
                                (t child)))
                            children)))))

(defmethod feature-expression-sign ((ex feature-guard))
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
    ((client client) (result expression-result) children source)
  result)

(defmethod make-expression-result
    ((client client) (result t) (children t) (source cons))
  (destructuring-bind (start . end) source
    (match result
           ((list '|#.| result)
            (make-instance 'expression-result
              :expression result
              :children (cons sharpsign-dot children)
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
  (let ((package (case package-indicator
                   (:current *package*)
                   (:keyword (find-package "KEYWORD"))
                   (t        (or (find-package package-indicator)
                                 ;; Return a fake package for missing packages.
                                 (find-package :missing)
                                 (make-package :missing))))))
    (if internp
        (intern symbol-name package)
        (multiple-value-bind (symbol status)
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
                 symbol))))))

;;; The next two forms are used to avoid throwing errors when a
;;; #. reader macro attempts to execute code during parsing.  We want
;;; to avoid this as we will typically not have the requisite
;;; variables defined.
(defgeneric wrap-in-sharpsign-dot (client material)
  (:method (client material)
    (list '|#.| material)))

(defmethod evaluate-expression ((client client) expression)
  (wrap-in-sharpsign-dot client expression))

(defun read-forms+ (string &key count)
  (check-type count (or null integer))
  (let ((*string* string)
        (client (make-instance 'client))
        (eclector.readtable:*readtable* *lisp-ast-readtable*))
    (labels
        ((make-space (start end)
           (when (< start end)
             (list (make-instance 'skipped-input-result
                     :start start :end end :reason 'whitespace))))
         (w/space (tree from to)
           (let ((result
                  (etypecase tree
                    (list
                     (append
                      (iter (for subtree in tree)
                            (appending (make-space from (start subtree)))
                            (appending (w/space subtree
                                                (start subtree) (end subtree)))
                            (setf from (end subtree)))
                      (make-space from to)))
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
                      (make-space from (start tree))
                      (list tree)
                      (make-space (end tree) to))))))
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
  (walk-skipped-forms  [{format stream "~a"} #'source-text] forms))

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

(defmethod from-string ((lisp lisp) string)
  (setf (genome lisp) (convert 'lisp-ast string))
  lisp)

(defmethod from-file ((lisp lisp) file)
  (from-string lisp (file-to-string file)))

(defmethod update-paths ((node node) &optional path)
  (declare (ignorable path))
  node)

(defmethod source-text ((obj result) &optional stream)
  (if (children obj)
      (mapc [{write-string _ stream} #'source-text] (children obj))
      (write-string (subseq (string-pointer obj) (start obj) (end obj)) stream)))

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
  (fbind (fn)
         (walk-feature-guards (lambda (sign featurex ex)
                                (declare (ignore sign ex))
                                (fn featurex))
                              ast)))

(defun walk-feature-guards (fn ast)
  (fbind (fn)
         (traverse-nodes ast
                         (lambda (node)
                           (when (typep node 'feature-guard)
                             (fn (feature-expression-sign node)
                                 (expression (feature-expression node))
                                 (expression node)))
                           :keep-going))
         (values)))

(defun featurex-empty? (featurex)
  (or (null featurex)
      (equal featurex '(:or))))

(defun map-feature-expressions (fn ast &key
                                         remove-empty
                                         (remove-newly-empty remove-empty))
  (fbind (fn)
         (map-feature-guards (lambda (sign featurex ex)
                               (values sign (fn featurex) ex))
                             ast
                             :remove-empty remove-empty
                             :remove-newly-empty remove-newly-empty)))

(defun map-feature-guards (fn ast &key
                                    remove-empty
                                    (remove-newly-empty remove-empty))
  (assert (if remove-empty remove-newly-empty t))
  ;; TOD can changes be batched with `encapsulate'?
  (nest
   (fbind (fn))
   (let* ((to-remove '())
          (ast
           (map-tree
            (lambda (node)
              (if (typep node 'feature-guard)
                  (values
                   (block replace
                     (flet ((mark-for-remove (sign node)
                              (return-from replace
                                (ecase sign
                                  (#\+
                                   (push node to-remove)
                                   node)
                                  (#\-
                                   (expression node))))))
                       (transform-feature-guard
                        node
                        (lambda (sign featurex ex)
                          (if (and (featurex-empty? featurex) remove-empty)
                              (mark-for-remove sign node)
                              (receive (sign featurex ex)
                                       (fn sign featurex ex)
                                       (if (and (featurex-empty? featurex)
                                                remove-newly-empty)
                                           (mark-for-remove sign node)
                                           (values sign featurex ex))))))))
                   t)
                  node))
            ast))))
   (reduce (lambda (ast node)
             (remove node ast :test #'node-equalp))
           to-remove
           :initial-value ast)))

(defun transform-feature-expression (feature-expression fn)
  "Call FN on each feature in FEATURE-EXPRESSION.
If FN returns nil, the feature is removed.

FN may return any feature expression, not just a symbol."
  (match feature-expression
         (nil nil)
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
  "Remove FEATURES from FEATURE-EXPRESSION."
  (transform-feature-expression feature-expression
                                (lambda (feature)
                                  (unless (member feature features)
                                    feature))))

(defun remove-feature-support (ast features)
  "Remove support for FEATURES from AST.
Each feature in FEATURES will be removed from all feature expressions,
and if any of the resulting expressions are empty their guards (and
possibly expressions) will be omitted according to the sign of the guard."
  (map-feature-guards (lambda (sign featurex ex)
                        (let ((featurex (remove-expression-features featurex features)))
                          (values sign featurex ex)))
                      ast
                      :remove-newly-empty t))

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
          (map-tree (lambda (node)
                      (if (and (typep node 'expression-result)
                               (listp (expression node))
                               (equal '->> (first (expression node))))
                          (values (fix-double-arrow node) t)
                          (values node nil)))
                    (genome software))))

  (defun rewrite-double-arrow-in-place (file)
    (string-to-file (source-text (rewrite-double-arrow
                                  (from-file (make-instance 'lisp) file))) file)))
