;;;; diff.lisp --- Diff tests.
(defpackage :software-evolution-library/test/diff
  (:nicknames :sel/test/diff)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :diff))
(in-package :software-evolution-library/test/diff)
(in-readtable :curry-compose-reader-macros)
(defsuite diff)

(defvar *tfos* nil "Another software used in tests.")
(defvar *soft* nil "Software used in tests.")

(defmacro with-static-reference (software &rest body)
  (let ((ref-sym (gensym)))
    `(let ((,ref-sym (copy-tree (reference ,software))))
       ,@body
       (is (tree-equal ,ref-sym (reference ,software))))))

(deftest diff-size ()
  (with-fixture diff (is (= 4 (size *soft*)))))

(deftest diff-protects-reference ()
  (with-fixture diff
    (with-static-reference *soft*
      (setf (genome *soft*) nil)
      (is (tree-equal (reference *soft*)
                      '(((:CODE 1)) ((:CODE 2)) ((:CODE 3)) ((:CODE 4))))))))

(deftest diff-lines ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal (lines *soft*) '((1) (2) (3) (4)))))))

(deftest some-diff-cut-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-cut :targets 2)))
           '(((:CODE 1)) ((:CODE 2)) ((:CODE 4)))))
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-cut :targets 1)))
           '(((:CODE 1)) ((:CODE 4)))))
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-cut :targets 1)))
           '(((:CODE 1))))))))

(deftest some-diff-insert-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-insert :targets (list 0 2))))
           '(((:CODE 3)) ((:CODE 1)) ((:CODE 2))
             ((:CODE 3)) ((:CODE 4))))))))

(deftest some-diff-swap-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-swap :targets (list 0 2))))
           '(((:CODE 3)) ((:CODE 2)) ((:CODE 1)) ((:CODE 4))))))))

(deftest diff-copy ()
  (with-fixture diff (is (typep (copy *soft*) 'diff))))

(deftest diff-single-point-crossover ()
  (with-fixture double-diff
    (with-static-reference *soft*
      (is (tree-equal (reference *soft*) (reference *tfos*)))
      (let ((child (one-point-crossover *soft* *tfos*)))
        (is (typep child 'diff))
        (is (tree-equal (genome child) (genome *soft*)))))))

(deftest diff-array-protects-reference ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (setf (genome *soft*) nil)
      (is (tree-equal (reference *soft*)
                      '(((:CODE 1)) ((:CODE 2)) ((:CODE 3)) ((:CODE 4))))))))

(deftest diff-array-lines ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (tree-equal (lines *soft*) '((1) (2) (3) (4)))))))

(deftest some-diff-array-cut-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-cut :targets 2)))
                  #(((:CODE 1)) ((:CODE 2)) ((:CODE 4)))))
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-cut :targets 1)))
                  #(((:CODE 1)) ((:CODE 4)))))
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-cut :targets 1)))
                  #(((:CODE 1))))))))

(deftest some-diff-array-insert-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-insert :targets (list 0 2))))
                  #(((:CODE 3)) ((:CODE 1)) ((:CODE 2))
                    ((:CODE 3)) ((:CODE 4))))))))

(deftest some-diff-array-swap-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-swap :targets (list 0 2))))
                  #(((:CODE 3)) ((:CODE 2)) ((:CODE 1)) ((:CODE 4))))))))
