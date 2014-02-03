(in-package :software-evolution-example)

(defvar *orig* (from-file (make-instance 'asm) "../test/gcd/gcd.s"))

(setf (fitness *orig*) (test *orig*))   ; (2)

(setf *population*
      (loop :for i :below 100 :collect (copy *orig*))) ; (1)

(evolve #'test :max-evals 100 :target 11) ; (3)
