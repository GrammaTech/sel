;;; javascript-or-python.lisp --- Classes, methods, and functions common
;;; to both python and javascript.
;;;
;;; This package was created after identifying and extracting portions of
;;; commonality between the JavaScript and Python implementations.
(defpackage :software-evolution-library/software/javascript-or-python
  (:nicknames :sel/software/javascript-or-python
              :sel/sw/javascript-or-python)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/parseable)
  (:import-from :cffi :translate-camelcase-name)
  (:export :javascript-or-python
           :javascript-or-python-ast
           :interleaved-text
           :expand-js-or-py-ast-classes
           :convert-js-or-python
           :sorted-children
           :ast-type-to-rebind-p
           :ast-annotation-to-rebind))
(in-package :software-evolution-library/software/javascript-or-python)
(in-readtable :curry-compose-reader-macros)

(define-software javascript-or-python (parseable) ()
  (:documentation "JavaScript or Python software representation."))


;;; JavaScript or Python ASTs
(defclass javascript-or-python-ast (functional-tree-ast)
  ((interleaved-text :initarg :interleaved-text :initform nil :type list
                     :reader interleaved-text
                     :documentation "Interleaved text between children."))
  (:documentation "Base class for python and JavaScript ASTs."))

(defmethod initialize-instance :after ((ast javascript-or-python-ast)
                                       &key &allow-other-keys)
  "Wrapper around AST creation to populate the interleaved text field
with empty strings between each child if the field is not populated."
  (setf (slot-value ast 'interleaved-text)
        (or (interleaved-text ast)
            (nest (repeat-sequence '(""))
                  (1+)(length)
                  (remove nil)
                  (children ast)))))

(defun expand-js-or-py-ast-classes (superclass prefix spec)
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
                                          :initarg (make-keyword
                                                    (symbol-cat prefix field))
                                          (when (zerop arity)
                                            (list :type 'list))))))
                    field-specifiers))
         (:documentation
          ,(format nil "AST node class for ~a ASTs." class)))))
   ast-class-list))

(defun convert-js-or-python (superclass spec children-definitions)
  "Common function for converting an AST SPECification list from python or
acorn into an AST.

* SUPERCLASS superclass of the AST type to create (python|javascript-ast)
* SPEC AST specification list from python or acorn
* CHILDREN-DEFINITIONS list mapping AST types to their child slots"
  (let* ((superclass-key (make-keyword superclass))
         (prefix (if (eq superclass-key :python-ast) 'py 'js))
         (type-field (if (eq superclass-key :python-ast) :class :type))
         (type (nest (make-keyword)
                     (string-upcase)
                     (translate-camelcase-name)
                     (aget type-field spec)))
         (child-types (aget type children-definitions :test #'member)))
    (apply #'make-instance (symbol-cat prefix type)
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
            (adrop (list type-field) spec)))))

(defmethod copy :around ((ast javascript-or-python-ast) &key &allow-other-keys)
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
               (ast-in-difference-p (nth i children) difference))
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

(defgeneric sorted-children (ast)
  (:documentation "Return the children of AST sorted in textual order.")
  (:method :before ((ast javascript-or-python-ast)
                    &aux (children (remove nil (children ast))))
    (assert (or (null (ast-annotation ast :child-order))
                (= (length children)
                   (length (ast-annotation ast :child-order))))
            (ast)
            "The number of elements in the AST's :child-order annotation ~
            defining the order of the children does not match the number ~
            of children, ~d versus ~d."
            (length (ast-annotation ast :child-order)) (length children)))
  (:method ((ast javascript-or-python-ast))
    (if (ast-annotation ast :child-order)
        (mapcar {lookup ast} (ast-annotation ast :child-order))
        (remove nil (children ast)))))

(defmethod source-text :before ((ast javascript-or-python-ast)
                                &optional stream
                                &aux (children (sorted-children ast)))
  (declare (ignorable stream))
  (assert (= (1+ (length children)) (length (interleaved-text ast))) (ast)
          "The AST to be printed has ~d children and ~d element(s) of ~
          interleaved text.  The AST must have interleaved text between ~
          each child, ~d element(s) total."
          (length children) (length (interleaved-text ast))
          (1+ (length children))))

(defmethod source-text ((ast javascript-or-python-ast) &optional stream)
  (write-string (car (interleaved-text ast)) stream)
  (mapc (lambda (child text)
          (source-text child stream)
          (write-string text stream))
        (sorted-children ast)
        (cdr (interleaved-text ast))))

(defgeneric ast-type-to-rebind-p (ast)
  (:documentation "Return T if AST is of a type where its variables/functions
should be rebound."))

(defgeneric ast-annotation-to-rebind (ast)
  (:documentation "Return the AST annotation field to be rebound as part of
variable rebinding."))

(defmethod rebind-vars ((ast javascript-or-python-ast)
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
                                (if (= arity 0)
                                    (mapcar {rebind-vars _ var-replacements
                                                           fun-replacements}
                                            (slot-value ast name))
                                    (rebind-vars (slot-value ast name)
                                                 var-replacements
                                                 fun-replacements)))))
                      (child-slots ast)))))


;;; JavaScript or Python parseable overrides
(defmethod prepend-text-to-genome ((obj javascript-or-python) (text string)
                                   &aux (root (genome obj)))
  "Prepend non-AST TEXT to OBJ's genome."
  (setf (genome obj)
        (nest (copy root :interleaved-text)
              (cons (concatenate 'string text (car (interleaved-text root)))
                    (cdr (interleaved-text root))))))

(defmethod append-text-to-genome ((obj javascript-or-python) (text string)
                                  &aux (root (genome obj)))
  "Prepend non-AST TEXT to OBJ's genome."
  (setf (genome obj)
        (nest (copy root :interleaved-text)
              (append (butlast (interleaved-text root)))
              (list)
              (concatenate 'string text)
              (lastcar (interleaved-text root)))))
