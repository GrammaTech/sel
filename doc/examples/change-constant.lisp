(ql:quickload :software-evolution-library)
(ql:quickload :software-evolution-library/test)
(in-package :software-evolution-library)

(defparameter *sqrt*
  (setf *sqrt*
        (from-file (make-instance 'clang)
                   (make-pathname :name "sqrt"
                                  :type "c"
                                  :directory sel/test::+etc-dir+))))

;;; Show some mutations.
(genome (mutate (copy *sqrt*)))

;;; Show the integer literals ASTs.
(remove-if-not [{equal :INTEGERLITERAL} #'ast-class #'car #'ast-ref-ast]
               (asts *sqrt*))

;;; Show the text of the integer literal ASTs.
(mapcar [#'source-text #'ast-ref-ast]
        (remove-if-not [{equal :INTEGERLITERAL} #'ast-class #'car #'ast-ref-ast]
                       (asts *sqrt*)))

;;; Replace the interesting AST with a random value.
(defun second-integer-to-rand (obj &optional (rand (random 10000)))
  (setf (ast-ref (second
                  (remove-if-not
                   [{equal :INTEGERLITERAL} #'ast-class #'car #'ast-ref-ast]
                   (asts obj)))
                 obj)
        (make-literal rand))
  obj)

(defun rand-integer-to-rand (obj &optional (rand (random 10000)))
  (setf (ast-ref (random-elt
                  (remove-if-not
                   [{equal :INTEGERLITERAL} #'ast-class #'car #'ast-ref-ast]
                   (asts obj)))
                 obj)
        (make-literal rand))
  obj)

;;; Evaluate fitness (looking at cycles to execute).
(defun test-sqrt (obj &aux (total 0))
  (with-temp-file (bin)
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
