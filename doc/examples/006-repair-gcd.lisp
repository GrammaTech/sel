(defpackage :example
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang
        :software-evolution-library/components/test-suite))
(in-package :example)

(defparameter *orig*
  (from-file (make-instance 'clang)
             (make-pathname :name "gcd"
                            :type "c"
                            :directory (append +software-evolution-library-dir+
                                               (list "test" "etc" "gcd")))))
(defvar *test-script* (make-pathname
                       :name "test"
                       :type "sh"
                       :directory (append +software-evolution-library-dir+
                                          (list "test" "etc" "gcd"))))
(defvar *num-tests* 12)
(defvar *test-suite* (make-instance 'test-suite
                      :test-cases
                      (loop :for i :below *num-tests* :collecting
                         (make-instance 'test-case
                                        :program-name *test-script*
                                        :program-args
                                        (list :bin (write-to-string i))))))

(defun test (obj)
  (with-temporary-file (:pathname bin)
    (phenome obj :bin bin)
    ;; run the test-suite on bin, returning the number of passing tests
    (evaluate bin *test-suite*)))

(setf (fitness *orig*) (test *orig*))
(setf *max-population-size* (expt 2 10))
;;; This repair is basically random search around the original, so a
;;; larger population is much better.
(setf *population* (repeatedly (expt 2 10) (copy *orig*)))

(setf *clang-mutation-types*  ; Set mutation probabilities for repair.
      (cumulative-distribution
       (normalize-probabilities
        '((clang-cut               .  1)
          (clang-cut-full          .  2)
          (clang-insert            .  1)
          (clang-insert-full       .  2)
          (clang-swap              .  1)
          (clang-swap-full         .  2)
          (clang-replace           .  1)
          (clang-replace-full      .  2)))))

(let ((*target-fitness-p* (lambda (obj) (= *num-tests* (fitness obj)))))
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
