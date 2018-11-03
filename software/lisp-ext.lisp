;;; lisp-ext.lisp --- software rep of Lisp code (external eval)
(defpackage :software-evolution-library/software/lisp-ext
  (:nicknames :sel/software/lisp-ext :sel/sw/lisp-ext)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility))
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; the class of lisp software objects
(defclass lisp-exe (software-exe)
  ()
  (:documentation "DOCFIXME"))

(defvar *test-script*  nil "Script capable of running tests.")
(defvar *pos-test-num* nil "Number of positive tests")
(defvar *neg-test-num* nil "Number of negative tests")

(defmethod from-file ((lisp-exe lisp-exe) file)
  "DOCFIXME

* LISP-EXE DOCFIXME
* FILE DOCFIXME
"
  (with-open-file (in file)
    (setf (genome lisp-exe)
          (loop :for form = (read in nil :eof)
             :until (eq form :eof)
             :collect form)))
  lisp-exe)

(defun lisp-exe-from-file (path)
  "DOCFIXME"
  (from-file (make-instance 'lisp-exe) path))

(defun lisp-exe-to-file (software path)
  "DOCFIXME

* SOFTWARE DOCFIXME
* PATH DOCFIXME
"
  (with-open-file (out path :direction :output :if-exists :supersede)
    (dolist (form (genome software))
      (format out "~&~S" form))))

(defmethod exe ((lisp-exe lisp-exe) &optional place)
  "DOCFIXME

* LISP-EXE DOCFIXME
* PLACE DOCFIXME
"
  (let ((exe (or place (temp-file-name))))
    (lisp-exe-to-file lisp-exe exe)
    exe))

(defmethod evaluate ((lisp-exe lisp-exe))
  "DOCFIXME"
  (evaluate-with-script lisp-exe *test-script* *pos-test-num* *neg-test-num*))
