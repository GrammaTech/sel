(in-package :software-evolution-library/example)
(enable-curry-compose-reader-macros :include-utf8)


(defvar *orig* (from-file (make-instance 'asm) "../test/gcd/gcd.s"))

(setf (fitness *orig*) (test *orig*))   ; (2)

(setf *population*
      (loop :for i :below 100 :collect (copy *orig*))) ; (1)

(let ((*target-fitness-p*
       (lambda (obj)
         (or (= 11 (fitness obj))
             (funcall *fitness-predicate* (fitness obj) 11)))))
  (evolve #'test :max-evals 100))       ; (3)
