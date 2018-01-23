(defpackage :example
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility))
(in-package :example)

(defvar *orig* (from-file (make-instance 'asm) "test/etc/gcd/gcd.s"))

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
