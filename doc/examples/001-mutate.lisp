(defpackage :example
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/asm))
(in-package :example)

;;; gcd.s may be compiled from gcd.c in the test/ directory.
;; create an ASM software object using `from-file'
(defvar *orig* (from-file (make-instance 'asm) "test/etc/gcd/gcd.s"))

;; the results of `mutate' are a mutated object MUTANT and a `mutation' EDIT
(multiple-value-bind (mutant edit)
    (handler-bind
        ((no-mutation-targets
          (lambda (e)
            (declare (ignorable e))
            (invoke-restart 'try-another-mutation))))
      (mutate (copy *orig*)))
  (let ((temp (temp-file-name :type "s")))
    ;; save MUTANT to temp file TEMP
    (to-file mutant temp)
    (format t "Results of applying ~S to gcd written to ~S~%"
            edit temp)))
