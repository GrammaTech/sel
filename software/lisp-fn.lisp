;;; lisp-fn.lisp --- software representation of individual Lisp functions
(defpackage :software-evolution-library/software/lisp-fn
  (:nicknames :sel/software/lisp-fn :sel/sw/lisp-fn)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility))
(in-package :software-evolution-library/software/lisp-fn)
(in-readtable :curry-compose-reader-macros)


;;; the class of lisp software objects
(defclass lisp-fn (lisp)
  ()
  (:documentation "DOCFIXME"))

(defvar *test-script*  nil "Script capable of executing external code.")

;;; TODO: Below is not implemented (or likely planned correctly).

#+(or )
(defmacro externally (&body body)
  "Evaluate body in a protective external process."
  `(multiple-value-bind (output err-output exit)
       (shell "~a ~a" *test-script* (format nil "~S" ,`(format nil "~S" ,@body)))
     (if (= exit 0)
         (read-from-string output)
         (error "external execution failed"))))

#+(or )
(defmethod evaluate ((lisp-fn lisp-fn))
  "DOCFIXME"
  (externally
   (mapcar #'test-flopped
           (funcall #'collect-test-results `(lambda () ,(get lisp-fn 'tests))))))
