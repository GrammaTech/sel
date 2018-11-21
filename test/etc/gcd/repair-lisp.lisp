;; repair using software evolution
(require :software-evolution-library/lisp)
(in-package :software-evolution-library/lisp)
(in-readtable :curry-compose-reader-macros)

;; ensure the runner is build and ready
(unless (probe-file "lisp-runner")
  (error "Need to build the lisp-runner (make test/gcd/lisp-runner)."))

;; set required global parameters
(setq *pos-test-num* 10)
(setq *neg-test-num* 1)
(setq *test-script* "test-lisp.sh")
(setq *max-population-size* 100)
(defvar *gcd* (lisp-from-file "gcd.lisp"))

;; build up a starting population
(dotimes (n 8) (push *gcd* *population*))

;; evolve a repair
(defvar *repair* (repair :max-evals 1000 :max-fit 11))

;; conditionally save the repair
(let ((fixed-program-path "fixed.lisp"))
  (if *repair*
      (progn (format t "~&Repair found, saving to ~a~%" fixed-program-path)
             (lisp-to-file *repair* fixed-program-path))
      (format t "~&No repair found.~%")))
