;; repair using software evolution
(require :soft-ev)
(in-package :soft-ev)

;; compile gcd.c to assembly
(shell-command "gcc -S gcd.c")

(let ((*pos-test-num* 5)
      (*neg-test-num* 1)
      (*max-population-size* 100)
      (original-program-path "gcd.s")
      (fixed-program-path "fixed.s")
      repair)
  ;; build up a starting population -- 8 copies of the original
  (dotimes (_ 8) (push (asm-from-file original-program-path) *population*))
  ;; evolve a repair
  (setq repair (evolve :max-evals 1000 :max-fit 6))
  (if repair
      (progn (format t "~&Repair found, saving to ~a~%" fixed-program-path)
             (asm-to-file repair fixed-program-path))
      (format t "~&No repair found.~%")))
