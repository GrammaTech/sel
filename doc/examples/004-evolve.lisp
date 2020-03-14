(defpackage :example
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/asm))
(in-package :example)

(defparameter *orig*
  (from-file (make-instance 'asm)
             (make-pathname :name "gcd"
                            :type "s"
                            :directory (append +software-evolution-library-dir+
                                               (list "test" "etc" "gcd")))))

;;; Run the GCD unit tests on ASM. Return the number of passing tests.
(defun test (asm)
  (ignore-errors
    (with-temp-file (bin)
      ;; Build executable
      (phenome asm :bin bin)
      (count-if #'identity
                (loop :for i :below 12 :collect
                   (multiple-value-bind (stdout stderr errno)
                       (shell "~atest/etc/gcd/test.sh ~a ~d"
                              (namestring
                               (make-pathname
                                :directory +software-evolution-library-dir+))
                              bin i)
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
  (handler-bind
      ((no-mutation-targets
        (lambda (e)
          (declare (ignorable e))
          (invoke-restart 'try-another-mutation)))
       (mutate
        (lambda (e)
          (declare (ignorable e))
          (invoke-restart 'try-another-mutation))))
    ;; Limit the evolution to stop after 100 fitness evaluations, even if
    ;; `*target-fitness-p*' is not yet satisfied
    (evolve #'test :max-evals 100)))
