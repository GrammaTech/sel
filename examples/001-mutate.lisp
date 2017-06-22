(in-package :software-evolution-example)
(enable-curry-compose-reader-macros :include-utf8)


;; gcd.s may be compiled from gcd.c in the test/ directory.
(let ((orig (from-file (make-instance 'asm) "../test/gcd/gcd.s"))) ; (1)
  (multiple-value-bind (mutant edit) (mutate (copy *original*)) ; (2)
    (let ((temp (temp-file-name "s")))
      (to-file mutant temp)                                      ; (3)
      (format t "Results of applying ~S to gcd written to ~S.~%" ; (4)
              edit temp))))
