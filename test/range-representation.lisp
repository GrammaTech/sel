;;;; range-representation.lisp --- Range representation.
(defpackage :software-evolution-library/test/range-representation
  (:nicknames :sel/test/range-representation)
  (:shadow :range)                      ;Don't clobber fset:range.
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/simple)
  (:export :test-range-representation))
(in-package :software-evolution-library/test/range-representation)
(in-readtable :curry-compose-reader-macros)
(defsuite test-range-representation "Range representation.")

(defixture range
  (:setup (setf *soft* (make-instance 'sw-range
                         :genome '((0 . 2) (1 . 1) (1 . 2))
                         :reference #("one" "two" "three"))))
  (:teardown (setf *soft* nil)))

(defvar *range-ref* nil)
(defixture double-range
  (:setup
   (setf *soft* (make-instance 'sw-range
                  :genome '((0 . 2) (1 . 1) (1 . 2))
                  :reference *range-ref*)
         *tfos* (make-instance 'sw-range
                  :genome '((2 . 5) (4 . 4) (4 . 5))
                  :reference *range-ref*)))
  (:teardown (setf *soft* nil *tfos* nil)))

(deftest range-size ()
  (with-fixture range (is (= 6 (size *soft*)))))

(deftest range-lines ()
  (with-fixture range
    (is (tree-equal (lines *soft*)
                    '("one" "two" "three" "two" "two" "three")
                    :test #'string=))))

(deftest range-nth-test ()
  (with-fixture range
    (is (equal (mapcar {range-nth _ (genome *soft*)}
                       (loop :for i :below (size *soft*) :collect i))
               '(0 1 2 1 1 2)))))

(deftest range-subseq-test ()
  (with-fixture range
    ;; to
    (is (tree-equal (range-subseq (genome *soft*) 0 1)
                    '((0 . 0))))
    (is (tree-equal (range-subseq (genome *soft*) 0 2)
                    '((0 . 1))))
    (is (tree-equal (range-subseq (genome *soft*) 0 3)
                    '((0 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 0 4)
                    '((0 . 2) (1 . 1))))
    (is (tree-equal (range-subseq (genome *soft*) 0 5)
                    '((0 . 2) (1 . 1) (1 . 1))))
    (is (tree-equal (range-subseq (genome *soft*) 0 6)
                    '((0 . 2) (1 . 1) (1 . 2))))
    ;; from
    (is (tree-equal (range-subseq (genome *soft*) 1 7)
                    '((1 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 2 7)
                    '((2 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 3 7)
                    '((1 . 1) (1 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 4 7)
                    '((1 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 5 7)
                    '((2 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 6 7)
                    'NIL))
    ;; both
    (is (tree-equal (range-subseq (genome *soft*) 2 5)
                    '((2 . 2) (1 . 1) (1 . 1))))))

(deftest some-range-cut-mutations ()
  (with-fixture range
    (is (tree-equal (genome (apply-mutation
                             *soft*
                             (make-instance 'simple-cut :targets 2)))
                    '((0 . 1) (1 . 1) (1 . 2))))
    (is (tree-equal (genome (apply-mutation
                             *soft*
                             (make-instance 'simple-cut :targets 2)))
                    '((0 . 1) (2 . 2))))
    (is (tree-equal (genome (apply-mutation
                             *soft*
                             (make-instance 'simple-cut :targets 1)))
                    '((0 . 0) (2 . 2))))
    (is (tree-equal (genome (apply-mutation
                             *soft*
                             (make-instance 'simple-cut :targets 1)))
                    '((0 . 0))))
    (is (null (genome (apply-mutation
                       *soft*
                       (make-instance 'simple-cut :targets 0)))))))

(deftest some-range-insert-mutations ()
  (with-fixture range
    (is (tree-equal
         (genome (apply-mutation
                  *soft*
                  (make-instance 'simple-insert :targets (list 0 2))))
         '((2 . 2) (0 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal
         (genome (apply-mutation
                  *soft*
                  (make-instance 'simple-insert :targets (list 5 1))))
         '((2 . 2) (0 . 2) (1 . 1) (0 . 0) (1 . 2))))
    (is (tree-equal
         (genome (apply-mutation
                  *soft*
                  (make-instance 'simple-insert :targets (list 5 2))))
         '((2 . 2) (0 . 2) (1 . 1) (1 . 1) (0 . 0) (1 . 2))))
    (is (tree-equal
         (genome (apply-mutation
                  *soft*
                  (make-instance 'simple-insert :targets (list 2 1))))
         '((2 . 2) (0 . 0) (0 . 0) (1 . 2) (1 . 1) (1 . 1) (0 . 0) (1 . 2))))))

(deftest some-range-swap-mutations ()
  (with-fixture range
    (apply-mutation *soft* (make-instance 'simple-swap :targets (list 0 2)))
    (is (tree-equal (lines *soft*)
                    '("three" "two" "one" "two" "two" "three")
                    :test #'string=))))

(deftest range-copy ()
  (with-fixture range (is (typep (copy *soft*) 'sw-range))))

(deftest range-single-point-crossover ()
  (with-fixture double-range
    (is (eq (reference *soft*) (reference *tfos*)))
    (let ((child (one-point-crossover *soft* *tfos*)))
      (is (typep child 'sw-range))
      (is (listp (genome child))))))

(deftest range-crossover ()
  (with-fixture double-range
    (let ((before-a (copy-tree (genome *soft*)))
          (before-b (copy-tree (genome *tfos*)))
          (child (crossover *soft* *tfos*)))
      (is (typep child 'sw-range))
      (is (listp (genome child)))
      ;; (is (not (null (edits child))))
      (is (eq (reference *soft*) (reference child)))
      (is (tree-equal before-a (genome *soft*)))
      (is (tree-equal before-b (genome *tfos*))))))
