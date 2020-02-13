;;; cil.lisp --- cil software representation
(defpackage :software-evolution-library/software/cil
  (:nicknames :sel/software/cil :sel/sw/cil)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/source)
  (:export :cil))
(in-package :software-evolution-library/software/cil)
(in-readtable :curry-compose-reader-macros)


;;; cil software objects
(define-software cil (source) ()
  (:documentation "C abstract syntax trees using C Intermediate Language (CIL).
See http://kerneis.github.io/cil/."))

(defmethod apply-mutation ((cil cil) op)
  "DOCFIXME"
  (with-temp-file-of (src (ext cil)) (genome cil)
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

(defmethod instrument ((cil cil) &key points functions functions-after
                                      trace-file trace-env
                                      instrument-exit filter num-threads)
  "Instrument CIL for traced execution.
Optionally specify the name of the file in which to save trace data."
  (unless (null trace-env)
    (warn "Tracing to env variable is not support for CIL software objects."))
  (unless (null points)
    (warn
     "Program point instrumentation not supported for CIL software objects."))
  (unless (and (null functions) (null functions-after))
    (warn
     "Custom function instrumentation not supported for CIL software objects."))
  (unless (null instrument-exit)
    (warn
     "Custom instrument-exit not supported for CIL software objects."))
  (unless (null filter)
    (warn
     "Custom filter not supported for CIL software objects."))
  (unless (null num-threads)
    (warn
     "Multi-threaded instrumented not supported for CIL software objects."))
  (apply-mutation cil (list :trace trace-file))
  cil)
