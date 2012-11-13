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

(defvar *test*  "./test.sh")
(defvar *base*  (file-to-string "gcd.c"))
(defvar *clang* (clang-from-file "gcd.c"))
(defvar *cil*   (cil-from-file "gcd.c"))

(defun run-test (phenome num)
  (multiple-value-bind (output err-output exit)
      (shell "~a ~a ~a" *test* phenome num)
    (declare (ignorable output err-output))
    (zerop exit)))

(def-memoized-function test-suite (ast)
  (with-temp-file (bin)
    (if (phenome ast :bin bin)
        (count t (loop :for num :upto 11 :collect (run-test bin num)))
        0)))

;; sanity check
(defun sanity-check (ast)
  (setf (fitness ast) (test-suite ast))
  (assert (= 11 (fitness ast)) (ast) "failed sanity check"))

;; run different types of repair
(defun ga ()
  (let ((*population* (repeatedly 100 (copy *orig*)))
        (*max-population-size* 100))
    (store (evolve #'test-suite :max-fit 12) "results-ga.store")))

(defun brute-force (type)
  (assert (or (eq type 'cil) (eq type 'clang)) (type)
          "TYPE should be either `cil' or `clang'")
  (let ((num (num-ids *orig*)))
    (store
     (block repair
       (flet ((mut (op)
                (let ((it (make-instance type
                            :base *base*
                            :edits (list op)
                            :c-flags (case type
                                       (cil nil)
                                       (clang (list "2>/dev/null"))))))
                  (when (= 12 (test-suite it))
                    (format t "repair found!~%")
                    (return-from repair it)))))
         ;; delete
         (loop :for id :below num :do
            (mut (list :cut id)))
         (when (eq type 'clang)
           (error 'brute-force "should have found it by now (:CUT 42)"))
         ;; insert
         (loop :for left :below num :do
            (loop :for right :below num :do
               (mut (list :insert left right))))
         (when (eq type 'cil)
           (error 'brute-force "should have found it by now (:INSERT 0 9)"))
         ;; swap
         (loop :for left :below num :do
            (loop :for right :below num :do
               (mut (list :swap left right))))))
     "results.store")))
