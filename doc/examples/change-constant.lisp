;;; change-constant.lisp --- Simple example to mutate constants in source.
(defpackage :software-evolution-library/doc/examples/change-constant
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/clang))
(in-package :software-evolution-library/doc/examples/change-constant)
(in-readtable :curry-compose-reader-macros)

(defparameter *sqrt*
  (from-file (make-instance 'clang)
             (make-pathname :name "sqrt"
                            :type "c"
                            :directory sel/test::+etc-dir+)))

;;; Show some mutations.
(genome (mutate (copy *sqrt*)))

;;; Show the integer literals ASTs.
(remove-if-not [{equal :INTEGERLITERAL} #'ast-class]
               (asts *sqrt*))

;;; Show the text of the integer literal ASTs.
(mapcar #'source-text
        (remove-if-not [{equal :INTEGERLITERAL} #'ast-class]
                       (asts *sqrt*)))

;;; Replace the interesting AST with a random value.
(defun second-integer-to-rand (obj &optional (rand (random 10000)))
  (with obj
        (second (remove-if-not [{equal :INTEGERLITERAL} #'ast-class]
                               (asts obj)))
        (make-literal rand)))

(defun rand-integer-to-rand (obj &optional (rand (random 10000)))
  (with obj
        (random-elt (remove-if-not [{equal :INTEGERLITERAL} #'ast-class]
                                   (asts obj)))
        (make-literal rand)))

;;; Evaluate fitness (looking at cycles to execute).
(defun test-sqrt (obj &aux (total 0))
  (with-temporary-file (:pathname bin)
    (phenome obj :bin bin)
    (or (ignore-errors
          (dotimes (n 20 total)
            (mapcar (lambda (base square)
                      (multiple-value-bind (stdout stderr)
                          (shell "perf stat -x, -e task-clock ~a ~d" bin square)
                        (assert (= base (parse-integer stdout)))
                        (incf total
                              (parse-number
                               (subseq stderr 0 (position #\, stderr))))))
                    '(0 100 1000 10000) '(0 10000 1000000 100000000))))
        infinity)))

(dotimes (n 20)            ; Probably shouldn't see any timing effect.
  (let ((starting-point (* n 1000)))
    (format t "~a ~a~%"
            starting-point
            (test-sqrt (second-integer-to-rand (copy *sqrt*) starting-point)))))
