;; repair using software evolution
(require :software-evolution)
(in-package :software-evolution)

(setq *max-population-size* 100)
(setq original (lisp-from-file "gcd.lisp"))

;; tests for the lisp Euclids-GCD implementation
(defun spec-to-test (spec)
  (lambda ()
    (if (handler-case
            (= (with-timeout (1)
                 (euclids-gcd (first spec) (second spec)))
               (third spec))
            (timeout-error (e) (declare (ignorable e)) nil))
        1 0)))
(setf *pos-tests* (mapcar #'spec-to-test '((1071 1029 21)
                                           (555 666 111)
                                           (678 987 3)
                                           (8767 653 1)
                                           (16777216 512 512)
                                           (16 4 4)
                                           (315 831 3)
                                           (513332 91583315 1)
                                           (112 135 1)
                                           (310 55 5))))
(setf *neg-tests* (mapcar #'spec-to-test '((0 55 55))))

;; build up a starting population -- 8 copies of the original
(dotimes (_ 8) (push original *population*))

;; evolve a repair
(defvar *repair* (repair :max-evals 1000 :max-fit 10))

;; conditionally save the repair
(let ((fixed-program-path "fixed.lisp"))
  (if *repair*
      (progn (format t "~&Repair found, saving to ~a~%" fixed-program-path)
             (lisp-to-file *repair* fixed-program-path))
      (format t "~&No repair found.~%")))

;;; monitoring of an active repair
#+Example
(identity *fitness-evals*)
#+Example
(mapcar #'fitness *population*)
