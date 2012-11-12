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
  (with-temp-file (bin)
    (if (phenome clang :bin bin)
        (count t (loop :for num :upto 11 :collect (run-test bin num)))
        0)))

;; sanity check
(setf (fitness *orig*) (test-suite *orig*))
(assert (= 11 (fitness *orig*)) (*orig*) "failed sanity check")

;; run repair
(defun brute-force ()
  (let ((num (num-ids *orig*)))
    (store
     (block repair
       (flet ((mut (op)
                (let ((new (copy *orig*)))
                  (format t "~&testing ~S~%" op)
                  (push op (edits new))
                  (when (= 12 (test-suite new))
                    (return-from repair new)))))
         ;; delete
         (loop :for id :below num :do
            (mut (list :cut id)))
         ;; insert
         (loop :for left :below num :do
            (loop :for right :below num :do
               (mut (list :insert left right))))
         ;; swap
         (loop :for left :below num :do
            (loop :for right :below num :do
               (mut (list :swap left right))))))
     "results.store")))

(defun ga ()
  (let ((*population* (repeatedly 100 (copy *orig*)))
        (*max-population-size* 100))
    (store (evolve #'test-suite :max-fit 12) "results.store")))
