;;; lisp-fn.lisp --- software representation of individual Lisp functions
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; the class of lisp software objects
(defclass lisp-fn (lisp)
  ()
  (:documentation "DOCFIXME"))

(defvar *test-script*  nil "Script capable of executing external code.")

(defmacro externally (&body body)
  "Evaluate body in a protective external process."
  `(multiple-value-bind (output err-output exit)
       (shell "~a ~a" *test-script* (format nil "~S" `(format nil "~S" ,@body)))
     (if (= exit 0)
         (read-from-string output)
         (error "external execution failed"))))

(defmethod evaluate ((lisp-fn lisp-fn))
  "DOCFIXME"
  (externally
   (mapcar #'test-flopped
           (funcall #'collect-test-results `(lambda () ,(get lisp-fn 'tests))))))
