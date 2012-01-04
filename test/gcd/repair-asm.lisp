;; repair using software evolution
(require :software-evolution)
(in-package :software-evolution)

;; compile gcd.c to assembly
(shell "gcc -S gcd.c")

(setq *pos-test-num* 10)
(setq *neg-test-num* 1)
(setq *test-script* "test.sh")
(setq *max-population-size* 100)
(setq *genome-averaging-keys* '(:pos :neg))
(setq original (asm-from-file "gcd.s"))

;; add the good and bad path information to gcd
;; (apply-path original :neg (samples-from-oprofile-file "sample.neg"))
;; (apply-path original :pos (samples-from-oprofile-file "sample.pos"))

;; build up a starting population -- 8 copies of the original
(dotimes (_ 8) (push original *population*))

;; evolve a repair
(defvar *repair* (repair :max-evals 1000 :max-fit 11))

;; conditionally save the repair
(let ((fixed-program-path "fixed.s"))
  (if *repair*
      (progn (format t "~&Repair found, saving to ~a~%" fixed-program-path)
             (asm-to-file *repair* fixed-program-path))
      (format t "~&No repair found.~%")))

;;; monitoring of an active repair
#+Example
(identity *fitness-evals*)
#+Example
(mapcar #'fitness *population*)
