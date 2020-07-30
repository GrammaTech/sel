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
  (:export :javascript-or-python
           :javascript-or-python-ast
           :interleaved-text
           :expand-js-or-py-ast-classes
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

(defmethod source-text ((ast javascript-or-python-ast) &optional stream)
  (when (interleaved-text ast)
    (write-string (car (interleaved-text ast)) stream)
    (mapc (lambda (child text)
            (source-text child stream)
            (write-string text stream))
          (remove nil (children ast))
          (cdr (interleaved-text ast)))))

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
