;;; non-homologous-parseable.lisp --- Classes, methods, and functions common
;;; to parseable representations with non-homologous ASTs.
;;;
;;; This package was created after identifying and extracting portions of
;;; commonality between implementations with non-homologous ASTs.
(defpackage :software-evolution-library/software/non-homologous-parseable
  (:nicknames :sel/software/non-homologous-parseable
              :sel/sw/non-homologous-parseable)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/parseable)
  (:import-from :cffi :translate-camelcase-name)
  (:export :non-homologous-parseable
           :non-homologous-ast
           :interleaved-text
           :expand-ast-classes
           :convert-helper
           :ast-type-to-rebind-p
           :ast-annotation-to-rebind
           :process-indentation))
(in-package :software-evolution-library/software/non-homologous-parseable)
(in-readtable :curry-compose-reader-macros)

(define-software non-homologous-parseable (software-indentation parseable) ()
  (:documentation "Parseable software with non-homologous ASTs."))


;;; ASTs with named children slots.
(defclass interleaved-text ()
  ((interleaved-text :initarg :interleaved-text :initform nil :type list
                     :reader interleaved-text
                     :documentation "Interleaved text between children."))
  (:documentation "Mixin for interleaved-text slot"))

(defclass non-homologous-ast (indentation functional-tree-ast interleaved-text)
  ()
  (:documentation "Base class for non-homologous ASTs."))

(defmethod initialize-instance :after ((ast non-homologous-ast)
                                       &key &allow-other-keys)
  "Wrapper around AST creation to populate the interleaved text field
with empty strings between each child if the field is not populated."
  (setf (slot-value ast 'interleaved-text)
        (or (interleaved-text ast)
            (nest (repeat-sequence '(""))
                  (1+)(length)
                  (remove nil)
                  (children ast)))))

(defmethod ast-hash ast-combine-hash-values ((ast interleaved-text))
  (ast-hash (interleaved-text ast)))

(defun expand-ast-classes (superclass prefix spec)
  "Returns a list of AST node definitions derived from SPEC with the given
SUPERCLASS and PREFIX."
  (nest
   (destructuring-bind (ast-class-list . field-specifiers) spec)
   (mapcar
    (lambda (class)
      `(define-node-class ,(symbol-cat prefix class) (,superclass)
         (,@(when field-specifiers
              `((child-slots
                 :initform (quote ,(mapcar «cons [{symbol-cat prefix} #'car]
                                                 #'cdr»
                                           field-specifiers))
                 :allocation :class)))
          ,@(mapcar (lambda (field)
                      (destructuring-bind (field . arity) field
                        (let ((py-field (symbol-cat prefix field)))
                          (list* py-field :reader py-field
                                          :initform nil
                                          :initarg (make-keyword py-field)
                                          :initarg py-field
                                          (when (zerop arity)
                                            (list :type 'list))))))
                    field-specifiers))
         (:documentation
          ,(format nil "AST node class for ~a ASTs." class)))))
   ast-class-list))

(defun convert-helper (spec prefix superclass children-definitions)
  "Common function for converting an AST SPECification list into an AST.

* SPEC AST specification association list
* PREFIX prefix for type name and slots
* SUPERCLASS superclass of the AST type to create
* CHILDREN-DEFINITIONS list mapping AST types to their child slots"
  (let* ((type (if (stringp (aget :class spec))
                   (nest (make-keyword)
                         (string-upcase)
                         (translate-camelcase-name)
                         (aget :class spec))
                   (aget :class spec)))
         (child-types (aget type children-definitions :test #'member)))
    (apply #'make-instance (symbol-cat-in-package (symbol-package superclass)
                                                  prefix type)
           (mappend
            (lambda (field)
              (destructuring-bind (key . value) field
                (list (if (find key child-types :key #'car)
                          (make-keyword (symbol-cat prefix key))
                          key)
                      (if-let ((spec (find key child-types :key #'car)))
                        (destructuring-bind (key . arity) spec
                          (declare (ignorable key))
                          (ecase arity
                            (1 (convert superclass value))
                            (0 (mapcar {convert superclass} value))))
                        value))))
            (adrop '(:class) spec)))))

(defmethod equal? ((ast-a non-homologous-ast) (ast-b non-homologous-ast))
  (let ((hash1 (slot-value ast-a 'stored-hash))
        (hash2 (slot-value ast-b 'stored-hash)))
    (if (and hash1 hash2 (not (eql hash1 hash2)))
        nil
        (and (eq (type-of ast-a) (type-of ast-b))
             (equal? (interleaved-text ast-a)
                     (interleaved-text ast-b))
             (length= (children ast-a)
                      (children ast-b))
             (every #'equal? (children ast-a) (children ast-b))))))

(defmethod copy :around ((ast non-homologous-ast) &key &allow-other-keys)
  "Wrapper around copy to perform various fixups to the interleaved-text field
and child-order annotations of the copied AST in response to child AST
insertions or removals.  These fixups exist to mimic the mimic behavior
of JavaScript and python ASTs when child ASTs were stored in a flat list
of children and not in named children slots."
  (let* ((copy (call-next-method))
         (old-children (remove nil (children ast)))
         (new-children (remove nil (children copy)))
         (old-children-length (length old-children))
         (new-children-length (length new-children)))
    (labels ((text-changed ()
               "Return T if the interleaved-text field changed between
               the old and new ASTs."
               (not (equal (interleaved-text copy) (interleaved-text ast))))
             (child-order-changed ()
               "Return T if the :child-order field is populated and has
               changed after the AST copy."
               (let ((old-child-order (ast-annotation ast :child-order))
                     (new-child-order (ast-annotation copy :child-order)))
                 (and old-child-order new-child-order
                      (not (equal old-child-order new-child-order)))))
             (ast-additions ()
               "Return a list of new AST children in COPY."
               (set-difference new-children old-children :key #'serial-number))
             (ast-removals ()
               "Return a list of AST children removed in COPY."
               (set-difference old-children new-children :key #'serial-number))
             (ast-in-difference-p (ast difference)
               "Return T if AST is a member of DIFFERENCE."
               (member (serial-number ast) difference :key #'serial-number))
             (fixup-child-order (difference)
               "Return a new child order annotation after adjusting for AST
               removals in DIFFERENCE."
               (iter (for child in (sorted-children ast))
                     (unless (ast-in-difference-p child difference)
                       (collect (position child copy)))))
             (ith-in-difference-p (i children difference)
               "Return T if the Ith AST in CHILDREN is a member of DIFFERENCE."
               (when-let ((ith-child (nth i children)))
                 (ast-in-difference-p ith-child difference)))
             (fixup-interleaved-text (difference)
               "Return new interleaved text after adjusting for AST insertions
               or removals in DIFFERENCE."
               ;; This mimics the behavior of JS and python ASTs when child
               ;; ASTs were stored in a flat list of children and not in
               ;; named child slots.  For AST deletions, the interleaved text
               ;; is concatenated into a single value; for AST insertions,
               ;; an empty string is added.  There are issues with this
               ;; approach which may be addressed in a different changeset;
               ;; this simply preserves existing behavior.
               ;;
               ;; TODO: This is C-style code; refactor to use LISP idioms.
               (let ((text (interleaved-text copy))
                     (children (if (< new-children-length old-children-length)
                                   (sorted-children ast)
                                   (sorted-children copy)))
                     (child-i 0)
                     (text-i 0))
                 (iter (while (< child-i (length children)))
                       (if (ith-in-difference-p child-i children difference)
                           (if (< new-children-length old-children-length)
                               ;; cut operation, concatenate the text in the cut
                               (collect
                                 (iter (while (ith-in-difference-p child-i
                                                                   children
                                                                   difference))
                                       (collect (nth text-i text) into rslt)
                                       (incf text-i)
                                       (unless (first-iteration-p)
                                         (incf child-i))
                                       (finally
                                         (return (apply #'concatenate 'string
                                                        (remove nil rslt))))))
                               ;; insert operation, pad "" for the insertions
                               (appending
                                 (iter (while (ith-in-difference-p child-i
                                                                   children
                                                                   difference))
                                       (when (first-iteration-p)
                                         (collect (nth text-i text))
                                         (incf text-i))
                                       (incf child-i)
                                       (collect ""))))
                           (prog1
                             (collect (nth text-i text))
                             (incf child-i)
                             (incf text-i)))))))

      ;; Determine which ASTs were added or removed, if any.
      (when-let ((difference (cond ((< old-children-length new-children-length)
                                    (ast-additions))
                                   ((> old-children-length new-children-length)
                                    (ast-removals))
                                   (t nil))))
        ;; Assertions to force the client to provide more information when
        ;; we cannot clearly ascertain intent within this method.
        ;; This occurs when ASTs are both added and removed or when
        ;; inserting into an AST with an explicitly defined child order.
        (assert (or (text-changed) (not (and (ast-additions) (ast-removals))))
                (ast)
                "When creating an AST copy with both child AST additions and ~
                removals, the interleaved-text field must also be explicity ~
                set.")
        (assert (or (null (ast-annotation copy :child-order))
                    (and (child-order-changed) (not (ast-additions))))
                (ast)
                "When creating an AST copy with an explicit child order ~
                annotation, child AST additions are not allowed without ~
                explicitly setting the :child-order annotation.")

        ;; Update the :child-order annotation and interleaved-text field to
        ;; reflect the AST changes.
        (setf (slot-value copy 'annotations)
              (if (or (null (ast-annotation copy :child-order))
                      (child-order-changed))
                  (ast-annotations copy)
                  (cons (cons :child-order (fixup-child-order difference))
                        (adrop '(:child-order) (ast-annotations copy)))))
        (setf (slot-value copy 'interleaved-text)
              (if (text-changed)
                  (interleaved-text copy)
                  (fixup-interleaved-text difference)))))
    copy))

(defmethod source-text :before ((ast non-homologous-ast)
                                &optional stream
                                &aux (children (sorted-children ast)))
  (declare (ignorable stream))
  (assert (= (1+ (length children)) (length (interleaved-text ast))) (ast)
          "The AST to be printed has ~d children and ~d element(s) of ~
          interleaved text.  The AST must have interleaved text between ~
          each child, ~d element(s) total."
          (length children) (length (interleaved-text ast))
          (1+ (length children))))

(defmethod source-text ((ast non-homologous-ast) &optional stream
                        &aux (root ast) indent-p indentation-ast)
  (labels ((ends-with-newline-p (string)
             "Return T if STRING ends with a newline."
             (unless (emptyp string)
               (eql #\newline (last-elt string))))
           (create-indentation (indentation)
             "Create the string representation of INDENTATION.
              This handles converting spaces to tabs."
             (if *indent-with-tabs-p*
                 (mvlet ((tabs spaces (floor indentation *spaces-per-tab*)))
                   (concatenate
                    'string
                    (repeat-sequence "	" tabs)
                    (repeat-sequence " " spaces)))
                 (repeat-sequence " " indentation)))
           (get-indentation (ast parent-list)
             "Get the indentation at AST with its parents provided
              in PARENT-LIST."
             ;; Patch the indent-children slots of AST if
             ;; the value is T. The value T could be provided
             ;; from a #'convert invocation.
             (when (eq t (indent-children ast))
               (setf (indent-children ast)
                     (get-default-indentation ast parent-list)))
             (get-indentation-at ast parent-list))
           (patch-inner-indentation (text ast parents
                                     &aux (indentation
                                           (get-indentation ast parents))
                                       (split-text (split "\\n" text)))
             "Patch the newlines that occur inside interleaved text.
              This assumes that the indentation should be the same
              as the parent."
             ;; NOTE: this will likely indent comments incorrectly.
             ;;       This behavior is expected.
             (cond
               ((not-indentable-p ast) text)
               ((< 1 (length split-text))
                ;; Add the newline back in if there was one at the end
                ;; of the string since it gets removed by #'split.
                (concatenate
                 'string
                 (reduce
                  (lambda (total subseq)
                    (concatenate
                     'string
                     total
                     (format nil "~%~a~a"
                             (create-indentation
                              (if (emptyp subseq)
                                  0
                                  indentation))
                             subseq)))
                  (cdr split-text)
                  :initial-value (car split-text))
                 (format nil "~a"
                         (if (ends-with-newline-p text) #\newline ""))))
                 (t text)))
           (source-text* (ast &optional parents)
             "Recursively retrieve the source  text of AST
              and write it to stream."
             ;; TODO: make this readable.
             (let* ((interleaved-text (interleaved-text ast))
                    (starting-text (car interleaved-text))
                    (indentable-p (not (not-indentable-p ast))))
               (when (and (not (emptyp starting-text))
                          (eql #\newline (first starting-text)))
                 (setf indent-p nil
                       indentation-ast nil))
               (unless (or (not indent-p)
                           (not indentable-p))
                 (setf indent-p nil
                       indentation-ast nil)
                 (write-string
                  (create-indentation (get-indentation ast parents))
                  stream))
               (when (and (ends-with-newline-p starting-text)
                          indentable-p)
                 (setf indent-p t
                       indentation-ast ast))
               (write-string
                (patch-inner-indentation starting-text ast parents)
                stream)
               (mapc (lambda (child text)
                       (source-text* child (cons ast parents))
                       (when (and (not (emptyp text))
                                  (eql #\newline (first text)))
                         (setf indent-p nil
                               indentation-ast nil))
                       (when (and indent-p
                                  indentable-p
                                  ;; Prevent indentation from being
                                  ;; wasted on empty strings before it
                                  ;; reaches a child.
                                  (or (not (emptyp text))
                                      (not (ancestor-of-p
                                            root indentation-ast ast))))
                         (setf indent-p nil
                               indentation-ast nil)
                         (write-string
                          (create-indentation (get-indentation ast parents))
                          stream))
                       (when (and (ends-with-newline-p text)
                                  indentable-p)
                         (setf indent-p t
                               indentation-ast ast))
                       (write-string (patch-inner-indentation text ast parents)
                                     stream))
                     (sorted-children ast)
                     (cdr interleaved-text)))))
    (source-text* ast)))

(defgeneric ast-type-to-rebind-p (ast)
  (:documentation "Return T if AST is of a type where its variables/functions
should be rebound."))

(defgeneric ast-annotation-to-rebind (ast)
  (:documentation "Return the AST annotation field to be rebound as part of
variable rebinding."))

(defmethod rebind-vars ((ast non-homologous-ast)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (if (ast-type-to-rebind-p ast)
      (copy ast :interleaved-text (mapcar {rebind-vars _ var-replacements
                                                         fun-replacements}
                                          (interleaved-text ast))
                (ast-annotation-to-rebind ast)
                (rebind-vars (ast-annotation ast (ast-annotation-to-rebind ast))
                             var-replacements
                             fun-replacements))
      (apply #'copy ast
             (mappend (lambda (child-slot)
                        (destructuring-bind (name . arity) child-slot
                          (list (make-keyword name)
                                (cond ((= arity 0)
                                       (mapcar {rebind-vars _ var-replacements
                                                              fun-replacements}
                                               (slot-value ast name)))
                                      ((slot-value ast name)
                                       (rebind-vars (slot-value ast name)
                                                    var-replacements
                                                    fun-replacements))))))
                      (child-slots ast)))))


;;; Overrides for non-homologous parseable representations.
(defmethod prepend-text-to-genome ((obj non-homologous-parseable) (text string)
                                   &aux (root (genome obj)))
  "Prepend non-AST TEXT to OBJ's genome."
  (setf (genome obj)
        (nest (copy root :interleaved-text)
              (cons (concatenate 'string text (car (interleaved-text root)))
                    (cdr (interleaved-text root))))))

(defmethod append-text-to-genome-preamble ((obj non-homologous-parseable)
                                           (text string)
                                          &aux (root (genome obj)))
  "Append non-AST TEXT to OBJ's genome preamble."
  (setf (genome obj)
        (nest (copy root :interleaved-text)
              (cons (concatenate 'string (car (interleaved-text root)) text)
                    (cdr (interleaved-text root))))))

(defmethod append-text-to-genome ((obj non-homologous-parseable) (text string)
                                  &aux (root (genome obj)))
  "Prepend non-AST TEXT to OBJ's genome."
  (setf (genome obj)
        (nest (copy root :interleaved-text)
              (append (butlast (interleaved-text root)))
              (list)
              (concatenate 'string text)
              (lastcar (interleaved-text root)))))

;;;  Indentation
(-> process-indentation (non-homologous-ast) non-homologous-ast)
(defun process-indentation (root &aux indentation-carryover indentation-ast)
  (labels ((adjusted-spaces-from-tabs
               (subseq &aux (tab-count (count #\tab subseq)))
             "Return the number of spaces that are used for tabs minus
              the number of tabs."
             (- (* tab-count *spaces-per-tab*)
                tab-count))
           (starts-with-indentation-p (string)
             "If STRING starts with indentation, return
              the first position without indentation."
             (when indentation-carryover
               (mvlet ((start end (scan "^[ \\t]*" string)))
                 (declare (ignorable start))
                 end)))
           (ends-with-newline-p (string)
             "If STRING ends with a newline and optionally indentation,
              return the position of the newline."
             (scan "\\n[ \\t]*$" string))
           (update-indentation-slots
               (ast parents indentation text
                &aux (parent (car parents))
                  (total-indentation (+ indentation indentation-carryover))
                  (inherited-indentation
                      (get-indentation-at ast parents)))
             "Patch either AST or PARENT to have INDENTATION for the
              relevant line or lines."
             (symbol-macrolet ((indent-children (indent-children parent))
                               (indent-adjustment (indent-adjustment ast)))
               (cond
                 ;; Avoid wasting the newline on empty text before
                 ;; reaching a child.
                 ((and (emptyp text) (ancestor-of-p root indentation-ast ast)))
                 ((and parent (not indent-children))
                  (setf indent-children (- total-indentation
                                           inherited-indentation)
                        indentation-carryover nil
                        indentation-ast nil))
                 (t (setf indent-adjustment (- total-indentation
                                               inherited-indentation)
                          indentation-carryover nil
                          indentation-ast nil)))))
           (patch-leading-indentation
               (text ast parents
                &aux (indentation (starts-with-indentation-p text))
                  (not-empty-string-p (not (emptyp text))))
             "Return TEXT with the leading indentation removed and
              the relevant indentation slot updated."
             (cond-let leading-indentation
               ((and indentation
                     (= indentation (length text))
                     not-empty-string-p)
                (setf indentation-carryover
                      (+ indentation-carryover
                         indentation
                         (adjusted-spaces-from-tabs
                          (subseq text 0 indentation))))
                "")
               ;; This prevents weird indentation caused by
               ;; missing the indentation that occurs after the newline.
               ((and not-empty-string-p
                     (eql #\newline (first text)))
                (setf indentation-carryover nil
                      indentation-ast nil)
                text)
               ((or indentation
                    ;; NOTE: check if text exists here so that
                    ;;       the inherited indentation can be
                    ;;       set to 0. This prevents back-propogation
                    ;;       of indentation to previous siblings.
                    (and indentation-carryover
                         (scan "[^ \\t\\n]" text)))
                (update-indentation-slots
                 ast parents (+ leading-indentation
                                (adjusted-spaces-from-tabs
                                 (subseq text 0 leading-indentation)))
                 text)
                (subseq text leading-indentation))
               (t text)))
           (patch-trailing-indentation (text ast)
             "Return TEXT with the trailing indentation removed and
              indentation-carryover updated."
             (cond-let trailing-indentation
               ((ends-with-newline-p text)
                (setf indentation-carryover
                      (+ (- (length text) (1+ trailing-indentation))
                         (adjusted-spaces-from-tabs
                          (subseq text trailing-indentation)))
                      indentation-ast ast)
                (subseq text 0 (1+ trailing-indentation)))
               (t text)))
           (patch-internal-indentation (text)
             "Return TEXT where all newlines proceeded by indentation
              are replaced with a newline."
             (cl-ppcre:regex-replace-all "\\n[ \\t]+" text (format nil "~%")))
           (patch-text (text ast parents)
             "Patch TEXT such that it useable for inherited indentation.
              Updates AST and PARENTS slots if necessary."
             (patch-internal-indentation
              (patch-trailing-indentation
               (patch-leading-indentation text ast parents)
               ast)))
           (process-indentation* (ast &optional parents
                           &aux (interleaved-text (interleaved-text ast)))
             "Process the text of AST such that its indentation
              is in the indentation slots."
             (setf (car interleaved-text)
                   (patch-text (car interleaved-text) ast parents))
             (mapl (lambda (child-list text-list &aux (child (car child-list)))
                     ;; this prevents patching literals that have
                     ;; multiple newlines.
                     (unless (not-indentable-p child)
                       (process-indentation* child (cons ast parents)))
                     (setf (car text-list)
                           (patch-text (car text-list) ast parents)))
                   (sorted-children ast)
                   (cdr interleaved-text))))
    (process-indentation* root)
    root))
