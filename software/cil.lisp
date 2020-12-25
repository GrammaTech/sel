;;; cil.lisp --- cil software representation
(defpackage :software-evolution-library/software/cil
  (:nicknames :sel/software/cil :sel/sw/cil)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/components/file
        :software-evolution-library/software/ir)
  (:export :cil))
(in-package :software-evolution-library/software/cil)
(in-readtable :curry-compose-reader-macros)


;;; cil software objects
(define-software cil (ir) ()
  (:documentation "C abstract syntax trees using C Intermediate Language (CIL).
See http://kerneis.github.io/cil/."))

(defmethod apply-mutation ((cil cil) op)
  "DOCFIXME"
  (with-temporary-file-of (:pathname src :type (ext cil)) (genome cil)
    (multiple-value-bind (stdout stderr exit)
        (shell "cil-mutate ~a ~a ~a"
               (ecase (car op)
                 (:cut    "-cut")
                 (:insert "-insert")
                 (:swap   "-swap")
                 (:ids    "-ids")
                 (:trace  "-trace"))
               (if (eq (car op) :trace)
                   (if (second op)
                       (format nil "-trace-file ~a" (second op))
                       "")
                   (mapconcat (lambda (pair)
                                (format nil "-stmt~d ~d" (car pair) (cdr pair)))
                              (loop :for id :in (cdr op) :as i :from 1
                                 :collect (cons i id)) " "))
               src)
      (unless (zerop exit)
        (error (make-condition 'mutate
                 :text (format nil "cil-mutate:~a" stderr)
                 :obj cil
                 :operation op)))
      (setf (genome cil) stdout)))
  cil)
