(defpackage :example
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/asm))
(in-package :example)

(defvar *orig* (from-file (make-instance 'asm) "test/etc/gcd/gcd.s"))

(defun test (asm)
  "Run the GCD unit tests on ASM. Return the number of passing tests."
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

;; Apply 10 mutations to copies of `*orig*'
(loop :for i :below 10 :do
   (handler-bind
       ((no-mutation-targets
         (lambda (e)
           (declare (ignorable e))
           (invoke-restart 'try-another-mutation))))
     (multiple-value-bind (mutant edit) (mutate (copy *orig*))
       ;; Set fitness to be the number of passing unit tests
       (setf (fitness mutant) (test mutant))
       ;; Print the final fitness and the mutation applied.
       (format t "~2d fitness for edit ~S~%" (fitness mutant) edit))))
