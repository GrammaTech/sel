;; repair using software evolution
(require :software-evolution)
(in-package :software-evolution)
(require :memoize)
(use-package :memoize)
(require :cl-store)
(use-package :cl-store)

;; reproducibility
(let ((seed-path "seed"))
  (if (probe-file seed-path)
      (setf *random-state* (with-open-file (in seed-path) (read in)))
      (progn
        (setf *random-state* (make-random-state t))
        (with-open-file (out seed-path :direction :output)
          (write *random-state* :stream out)))))

(defvar *test*  "./test.sh")
(defvar *base*  (file-to-string "gcd.c"))

(defun run-test (phenome num)
  (multiple-value-bind (output err-output exit)
      (shell "~a ~a ~a" *test* phenome num)
    (declare (ignorable output err-output))
    (zerop exit)))

(defun test-suite (ast)
  (with-temp-file (bin)
    (if (phenome ast :bin bin)
        (count t (loop :for num :upto 11 :collect (run-test bin num)))
        0)))
(memoize test-suite)

;; sanity check
(defun sanity-check (ast)
  (setf (fitness ast) (test-suite ast))
  (assert (= 11 (fitness ast)) (ast) "failed sanity check"))
