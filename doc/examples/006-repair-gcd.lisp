(defpackage :example
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility))
(in-package :example)

(defvar *orig* (from-file (make-instance 'clang) "test/etc/gcd/gcd.c"))
(defvar *test-script* "test/etc/gcd/test.sh")
(defvar *num-tests* 12)
(defvar *test-suite* (make-instance 'test-suite
                      :test-cases
                      (loop :for i :below *num-tests* :collecting
                         (make-instance 'test-case
                                        :program-name *test-script*
                                        :program-args
                                        (list :bin (write-to-string i))))))

(defun test (obj)
  (with-temp-file (bin)
    (phenome obj :bin bin)
    ;; run the test-suite on bin, returning the number of passing tests
    (evaluate bin *test-suite*)))

(setf (fitness *orig*) (test *orig*))
(setf *max-population-size* (expt 2 10))
(setf *population* (loop :for i :below 100 :collect (copy *orig*)))

(let ((*target-fitness-p*
       (lambda (obj)
         (or (= *num-tests* (fitness obj))
             (funcall *fitness-predicate* (fitness obj) *num-tests*)))))
  ;; Errors occasionally crop up during mutations or crossover. For now, ignore
  ;; them by invoking 'try-another-mutation or 'ignore-failed-mutation.
  ;; All other errors are reported.
  (handler-bind
      ((no-mutation-targets
        (lambda (e)
          (declare (ignorable e))
          (invoke-restart 'try-another-mutation)))
       (t (lambda (e)
            (cond
              ((find-restart 'ignore-failed-mutation)
               (invoke-restart 'ignore-failed-mutation))
              ((find-restart 'try-another-mutation)
               (invoke-restart 'try-another-mutation))
              (t (error e))))))
    (evolve #'test)))
