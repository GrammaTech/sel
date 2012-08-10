;; playing with lisp software objects
(require :software-evolution)

;; test this entirely within lisp (no external script)
(setq *lsp* (lisp-from-file "test/gcd/gcd.lisp"))

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

(setq *population* (list *lsp*))

#+run
(repair)

#+diagnostics
(progn
  (mapcar #'fitness *population*)
  (setq *running* nil)
  (genome *lsp*)
  )
