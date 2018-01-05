(defpackage :example
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility))

(in-package :example)

(defvar *orig* (from-file (make-instance 'asm) "test/etc/gcd/gcd.s"))

(setf (fitness *orig*) (test *orig*))   ; (2)

(setf *population*
      (loop :for i :below 100 :collect (copy *orig*))) ; (1)

(let ((*target-fitness-p*
       (lambda (obj)
         (or (= 11 (fitness obj))
             (funcall *fitness-predicate* (fitness obj) 11)))))
  (evolve #'test :max-evals 100))       ; (3)
