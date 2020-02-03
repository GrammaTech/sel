(require :software-evolution-library/lisp)
(in-package :software-evolution-library/lisp)
(in-readtable :curry-compose-reader-macros)

(setq *gcd*
      (make-instance 'lisp
        :genome '((DEFUN EUCLIDS-GCD (A B)
                    (IF (= A 0)
                        B)
                    (DO ()
                        ((= B 0) A)
                      (IF (> A B)
                          (SETF A (- A B))
                          (SETF B (- B A))))))))





(setq *test-forms*
      (mapcar (lambda-bind ((a b gcd)) `(= (euclids-gcd ,a ,b) ,gcd))
              '((1071 1029 21)
                (555 666 111)
                (678 987 3)
                (8767 653 1)
                (16777216 512 512)
                (16 4 4)
                (315 831 3)
                (513332 91583315 1)
                (112 135 1)
                (310 55 5)
                (0 55 55))))

(assert (= 10 (evaluate *gcd*)))

(setq *population* (list *gcd*))
