(defpackage :example
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility
        :stefil
        :cl-ppcre)
  (:import-from :alexandria :lastcar)
  (:import-from :split-sequence :split-sequence))
(in-package :example)

(defsuite* test-examples)

(deftest ex-001-mutate-does-mutation ()
  (with-temp-file (tmp)
    (with-open-file (out tmp :direction :output :if-does-not-exist :create)
      (let ((*standard-output* out))
        (load "doc/examples/001-mutate.lisp")))
    (with-open-file (in tmp :direction :input)
      (let ((line (read-line in)))
        (let* ((mut-file (read-from-string
                          (lastcar (split-sequence #\Space line))))
               (mutant (from-file (make-instance 'asm) mut-file)))
          (is (genome mutant))
          (is (genome example::*orig*))
          (is (not (equal (genome example::*orig*)
                          (genome mutant)))))))))

(deftest ex-002-eval-defines-test-prints-muts ()
  (with-temp-file (tmp)
    (with-open-file (out tmp :direction :output :if-does-not-exist :create)
      (let ((*standard-output* out))
        (load "doc/examples/002-evaluation.lisp")))
    (is *orig*)
    (is (genome *orig*))
    (is (eql 11 (test *orig*)))
    (with-open-file (in tmp :direction :input)
      (loop :for line = (read-line in nil)
         :while line :do
         (multiple-value-bind (mstart mend regstart regend)
             (scan "\\s*\\d+ fitness for edit #<.* #<ASM .*> .*>" line)
           (declare (ignorable regstart regend))
           (is (zerop mstart))
           (is (eql mend (length line))))))))

(deftest ex-003-neutral-defines-variants ()
  (load "doc/examples/003-neutral.lisp")
  (is *orig*)
  (is (eql 11 (fitness *orig*)))
  (is variants)
  (is (eql 10 (length variants)))
  (is (every (lambda (asm)
               (not (equal (genome asm)
                           (genome *orig*))))
             variants)))

(deftest ex-004-evolve-runs ()
  (load "doc/examples/004-evolve.lisp")
  (is *orig*)
  (is (eql 11 (fitness *orig*)))
  (is (<= (length *population*) 100))
  (is (<= 105 *fitness-evals*)))
