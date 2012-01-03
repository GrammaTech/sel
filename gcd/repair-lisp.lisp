;; repair using software evolution
(require :soft-ev)
(in-package :soft-ev)

(setq *pos-test-num* 10)
(setq *neg-test-num* 1)
(setq *max-population-size* 100)
(setq original (lisp-from-file "gcd.lisp"))

;; add the good and bad path information to gcd
;; (apply-path original :neg (samples-from-oprofile-file "sample.neg"))
;; (apply-path original :pos (samples-from-oprofile-file "sample.pos"))

;; build up a starting population -- 8 copies of the original
(dotimes (_ 8) (push original *population*))

;; evolve a repair
(defvar *repair* (evolve :max-evals 1000 :max-fit 10))

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
