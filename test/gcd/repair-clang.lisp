;; repair using software evolution
(require :software-evolution)
(in-package :software-evolution)

;; reproducibility
(let ((seed-path "seed"))
  (if (probe-file seed-path)
      (setf *random-state* (with-open-file (in seed-path) (read in)))
      (progn
        (setf *random-state* (make-random-state t))
        (with-open-file (out seed-path :direction :output)
          (write *random-state* :stream out)))))

(defvar *test-script* "./test.sh")
(defvar *orig* (clang-from-file "gcd.c" :c-flags (list "2>/dev/null")))

(defun run-test (phenome num)
  (multiple-value-bind (output err-output exit)
      (shell "~a ~a ~a" *test-script* phenome num)
    (declare (ignorable output err-output))
    (zerop exit)))

(def-memoized-function test-suite (clang)
  (let ((phenome (phenome clang)))
    (count t (loop :for num :upto 11 :collect (run-test phenome num)))))

;; sanity check
(setf (fitness *orig*) (test-suite *orig*))
(assert (= 11 (fitness *orig*)) (*orig*) "failed sanity check")

;; run repair
(let ((*population* (list *orig*))
      (*max-population-size* 100))
  (store (evolve #'test-suite) "results.store"))
