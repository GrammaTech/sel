;; repair using software evolution
(require :software-evolution)
(in-package :software-evolution)

;; ensure the runner is build and ready
(unless (probe-file "lisp-runner")
  (error "Need to build the lisp-runner (make test/gcd/lisp-runner)."))

;; set required global parameters
(setq *pos-test-num* 2)
(setq *neg-test-num* 8)
(setq *test-script* "./test-lisp.sh")
(setq *max-population-size* 100)
(defvar *qs* (lisp-from-file "quick-sort.lisp"))

;; build up a starting population
(dotimes (_ 8) (push *qs* *population*))

;; evolve a repair
(defvar *repair* (repair :max-evals 5000 :max-fit 10))

;; conditionally save the repair
(let ((fixed-program-path "fixed.lisp"))
  (if *repair*
      (progn (format t "~&Repair found, saving to ~a~%" fixed-program-path)
             (lisp-to-file *repair* fixed-program-path))
      (format t "~&No repair found.~%")))
