(defpackage :example
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility))
(in-package :example)

;;;; Create a list of 10 neutral variants

;;; Load ASM software object from file
(defvar *orig* (from-file (make-instance 'asm) "test/etc/gcd/gcd.s"))

;;; Create an empty list of variants
(defvar variants nil "List to hold accumulated neutral variants.")

;;; Initialize fitness of `*orig*'. See 002-evaluation.lisp for `test'
(setf (fitness *orig*) (test *orig*))

(handler-bind
    ;; Handle errors that might occur during mutation
    ((no-mutation-targets
      (lambda (e)
        (declare (ignorable e))
        (invoke-restart 'try-another-mutation))))
  ;; Create a variant by applying a random mutation to a copy of `*orig*'
  (do ((variant (mutate (copy *orig*))))
      ((>= (length variants) 10) variants)
    ;; Test the fitness of the variant
    (setf (fitness variant) (test variant))
    ;; When the fitness of the variant matches that of `*orig*', add it
    ;; to the list of neutral variants
    (when (= (fitness variant) (fitness *orig*))
      (push variant variants))))
