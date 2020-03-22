(defpackage :example
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/asm))
(in-package :example)

;;;; Create a list of 10 neutral variants

;;; Load ASM software object from file
(defvar *orig* (from-file (make-instance 'asm) "test/etc/gcd/gcd.s"))

;;; Create an empty list of variants
(defvar variants nil "List to hold accumulated neutral variants.")

;;; Run the GCD unit tests on ASM. Return the number of passing tests.
(defun test (asm)
  (ignore-errors
    (with-temporary-file (:pathname bin)
      ;; Build executable
      (phenome asm :bin bin)
      (count-if #'identity
                (loop :for i :below 12 :collect
                   (multiple-value-bind (stdout stderr errno)
                       (shell "test/etc/gcd/test.sh ~a ~d" bin i)
                     (declare (ignorable stdout stderr))
                     ;; Collect list of T/NIL indicating if the exit code was 0.
                     ;; Tests whose exit code is 0 are considered successful.
                     (zerop errno)))))))

;;; Initialize fitness of `*orig*'.
(setf (fitness *orig*) (test *orig*))

;; Create a variant by applying a random mutation to a copy of `*orig*'
(do ((variant (handler-bind
                  ;; Handle errors that might occur during mutation
                  ((no-mutation-targets
                    (lambda (e)
                      (declare (ignorable e))
                      (invoke-restart 'try-another-mutation))))
                (mutate (copy *orig*)))))
    ((>= (length variants) 10) variants)
  ;; Test the fitness of the variant
  (setf (fitness variant) (test variant))
  ;; When the fitness of the variant matches that of `*orig*', add it
  ;; to the list of neutral variants
  (when (= (fitness variant) (fitness *orig*))
    (push variant variants)))
