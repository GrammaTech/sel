(require :software-evolution)
(in-package :software-evolution)

(setq *easy*
      (make-instance 'lisp
        :genome '(defun easy (it)
                  (if (evenp it) t t))))

(setq *test-forms*
      (mapcar (lambda-bind ((num res)) `(equal (my-even-p ,num) ,res))
              '((0 t)
                (1 f)
                (2 t)
                (3 f)
                (4 t))))

(assert (= 3 (evaluate *easy*)))

(setq *population* (list *easy*))
