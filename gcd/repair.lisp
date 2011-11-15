;; repair using software evolution
(require :soft-ev)
(in-package :soft-ev)

;; compile gcd.c to assembly
(shell "gcc -S gcd.c")

(let ((*pos-test-num* 5)
      (*neg-test-num* 1)
      (*max-population-size* 100)
      (*genome-averaging-keys* '(:pos :neg))
      (gcd (asm-from-file "gcd.s"))
      (fixed-program-path "fixed.s")
      repair)
  ;; add the good and bad path information to gcd
  (apply-path gcd :neg (samples-from-oprofile-file "sample.neg"))
  (apply-path gcd :pos (samples-from-oprofile-file "sample.pos"))
  ;; build up a starting population -- 8 copies of the original
  (dotimes (_ 8) (push gcd *population*))
  ;; evolve a repair
  (setq repair (evolve :max-evals 1000 :max-fit 6))
  (if repair
      (progn (format t "~&Repair found, saving to ~a~%" fixed-program-path)
             (asm-to-file repair fixed-program-path))
      (format t "~&No repair found.~%")))
