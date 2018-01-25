(defpackage :example
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility))
(in-package :example)

(defvar *orig* (from-file (make-instance 'asm) "test/etc/gcd/gcd.s"))

;;; Run the GCD unit tests on ASM. Return the number of passing tests.
(defun test (asm)
  (ignore-errors
    (with-temp-file (bin)
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

;;; Set the fitness of `*orig*' before creating the *population*.
;;; `evolve' assumes that all variants have an initialized, non-NIL fitness
(setf (fitness *orig*) (test *orig*))

;;; Initialize the population with 100 copies of `*orig*'
(setf *population*
      (loop :for i :below 100 :collect (copy *orig*)))

;;; Ensure the population doesn't grow above 100
(setf *max-population-size* 100)

;;; The target fitness is for a variant to pass all 12 unit tests.
;;; When *target-fitness-p* evaluates to T, evolution will stop.
(let ((*target-fitness-p*
       (lambda (obj)
         (or (= 12 (fitness obj))
             (funcall *fitness-predicate* (fitness obj) 12)))))
  ;; Limit the evolution to stop after 100 fitness evaluations, even if
  ;; `*target-fitness-p*' is not yet satisfied
  (evolve #'test :max-evals 100))
