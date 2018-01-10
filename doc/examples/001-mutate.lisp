(defpackage :example
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility))
(in-package :example)

;;; gcd.s may be compiled from gcd.c in the test/ directory.
(let ((orig (from-file (make-instance 'asm) "test/etc/gcd/gcd.s"))) ; (1)
  (multiple-value-bind (mutant edit) (mutate (copy orig)) ; (2)
    (let ((temp (temp-file-name "s")))
      (to-file mutant temp)                                      ; (3)
      (format t "Results of applying ~S to gcd written to ~S.~%" ; (4)
              edit temp))))
