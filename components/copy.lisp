;;; copy.lisp ---Utilities for copying and generating copying methods

(defpackage :software-evolution-library/components/copy
  (:use :gt/full)
  (:export :define-default-copy)
  (:nicknames
   :sel/components/copy
   :sel/cp/copy))
(in-package :software-evolution-library/components/copy)


(defun copy-key-pair (obj initarg slot-name value suppliedp)
  (cond
    (suppliedp `(,initarg ,value))
    ((slot-boundp obj slot-name)
     `(,initarg ,(slot-value obj slot-name)))))

;;; TODO: refactor #define-software to use this somehow.
(defmacro define-default-copy (class-name (&key around-method)
                               &body body
                               &aux (obj-var (format-symbol t "OBJ"))
                                 (keywords-var (format-symbol t "KEYWORD-ARGS")))
  "Define a default copy method for CLASS-NAME which either sets or copies the
direct slots of an object of type CLASS-NAME. BODY is inserted after the slots
on the new object have been set. The variables OBJ, COPY, and KEYWORD-ARGS are
available in BODY .

:AROUND-METHOD -- generates an :around specialized method. This is useful for
                  classes which inherit from other classes that have copy
                  methods or are to be inherited from."
  (labels ((supplied-var (slot-name)
             (format-symbol t "~a-SUPPLIED-P" slot-name))
           (generate-parameter (slot-name)
             `(,slot-name nil ,(supplied-var slot-name)))
           (generate-setter (slot-name)
             `(cond
                (,(supplied-var slot-name)
                 (setf (slot-value copy ',slot-name) ,slot-name))
                ((slot-boundp ,obj-var ',slot-name)
                 (setf (slot-value copy ',slot-name)
                       (slot-value ,obj-var ',slot-name)))))
           (generate-base-method (class-name slot-names initargs)
             `(defmethod copy ((,obj-var ,class-name)
                               &rest ,keywords-var
                               &key ,@(mapcar #'generate-parameter slot-names)
                               &allow-other-keys)
                (declare (ignorable ,keywords-var))
                (let ((copy
                        (apply
                         #'make-instance ',class-name
                         (mappend
                          #'identity
                          (remove
                           nil
                           (mapcar
                           (op (sel/cp/copy::copy-key-pair ,obj-var _ _ _ _))
                            ',initargs
                            ',slot-names
                            ,(cons 'list slot-names)
                            ,(cons 'list (mapcar #'supplied-var
                                                 slot-names))))))))
                  ,@body
                  copy)))
           (generate-around-method (class-name slot-names)
             `(defmethod copy :around
                  ((,obj-var ,class-name)
                   &rest ,keywords-var
                   &key ,@(mapcar #'generate-parameter slot-names)
                   &allow-other-keys)
                (declare (ignorable ,keywords-var))
                (let ((copy (call-next-method)))
                  ,@(mapcar #'generate-setter slot-names)
                  ,@body
                  copy))))
    (let* ((class (find-class class-name))
           (slots (class-direct-slots class))
           (initargs (mapcar (op (car (slot-definition-initargs _))) slots))
           (slot-names
             (mapcar (lambda (slot initarg)
                       (when initarg
                         (slot-definition-name slot)))
                     slots initargs)))
      (if around-method
          (generate-around-method class-name (remove nil slot-names))
          (generate-base-method
           class-name (remove nil slot-names) (remove nil initargs))))))
