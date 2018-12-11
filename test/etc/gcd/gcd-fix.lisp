;; Buggy implementation of Euclid's GCD Algorithm
(DEFUN EUCLIDS-GCD (A B)
  (IF (= A 0)
      B
      (DO ()
          ((= B 0) A)
        (IF (> A B)
            (SETF A (- A B))
            (SETF B (- B A))))))
