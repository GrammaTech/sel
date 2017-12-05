(require :software-evolution-library)
(in-package :software-evolution-library)
(enable-curry-compose-reader-macros :include-utf8)

(setq *even*
      (make-instance 'lisp
        :genome '((defun my-even-p (num)
                    (cond ((< num 0) nil)
                          ((= 0 num) t)
                          (t (my-even-p (- 2 num))))))))

(setq *test-forms*
      (mapcar (lambda-bind ((num res)) `(equal (my-even-p ,num) ,res))
              '((0 t)
                (1 f)
                (2 t)
                (3 f)
                (4 t))))

(assert (= 2 (evaluate *even*)))

(setq *population* (list *even*))
