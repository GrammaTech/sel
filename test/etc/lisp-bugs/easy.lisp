(require :software-evolution)
(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)

(setq *easy*
      (make-instance 'lisp
        :genome '(defun easy (it)
                  (if (evenp it) t t))))

(setq *test-forms*
      (mapcar (lambda-bind ((num res))
                (lambda (f) (equal (funcall f num) res)))
              '((0 t)
                (1 f)
                (2 t)
                (3 f)
                (4 t))))

(setf (fitness *easy*) (evaluate *easy*))

(assert (= 3 (fitness *easy*)))

(setq *population* (list *easy*))

(repair :max-fit 5)

(mapcar #'fitness *population*)

(func (car *population*))
