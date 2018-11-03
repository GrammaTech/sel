;; repair using software evolution
(require :software-evolution-library/lisp)
(in-package :software-evolution-library/lisp)
(in-readtable :curry-compose-reader-macros)

(load #P"repair.lisp")

(defvar *asm*
  (progn
    (unless (probe-file "gcd.s")
      (shell "gcc -S gcd.c"))
    (from-file (make-instance 'asm) "gcd.s")))

;; Add the good and bad path information to gcd
;; (apply-path original :neg (samples-from-oprofile-file "sample.neg"))
;; (apply-path original :pos (samples-from-oprofile-file "sample.pos"))

#+run
(sb-thread:make-thread
 (lambda ()
   (setf (fitness *asm*) (test-suite *asm*))
   (setf *population* (loop :for i :below 100 :collect (copy *asm*)))
   (let ((*max-population-size* 100)
         (*target-fitness-p* (lambda (obj)
                               (or (= 12 (fitness obj))
                                   (funcall *fitness-predicate* (fitness obj) 12)))))
     (store (evolve #'test-suite ) "results-asm.store"))))
