;;; tests.lisp --- tests for the `software-evolution' package

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :software-evolution-test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defsuite test)
(in-suite test)

(defvar *genome* nil "Genome used in tests.")
(defvar *soft*   nil "Software used in tests.")
(defvar *tfos*   nil "Another software used in tests.")
(defvar *gcd*    nil "Holds the gcd software object.")
(defvar *range-ref* #("one" "two" "three" "four" "five" "six")
  "Example range software object.")
(defvar *gcd-dir*
  (let ((dir (pathname-directory #.(or *compile-file-truename*
                                       *load-truename*
                                       *default-pathname-defaults*))))
    (make-pathname :directory (append dir (list "gcd"))))
  "Location of the gcd example directory")
(defun gcd-dir (filename) (merge-pathnames filename *gcd-dir*))

(defclass soft (software)
  ((genome   :initarg :genome   :accessor genome   :initform nil)))

(defmethod copy ((soft soft))
  (make-instance (type-of soft)
    :genome  (genome soft)
    :fitness (fitness soft)))

(defixture soft
  (:setup (setf *soft* (make-instance 'soft
                         :genome (coerce (loop for i from 0 to 9 collect i)
                                         'vector))))
  (:teardown (setf *soft* nil)))

(defixture range
  (:setup (setf *soft* (make-instance 'sw-range
                         :genome '((0 . 2) (1 . 1) (1 . 2))
                         :reference #("one" "two" "three"))))
  (:teardown (setf *soft* nil)))

(defixture double-range
  (:setup
     (setf *soft* (make-instance 'sw-range
                    :genome '((0 . 2) (1 . 1) (1 . 2))
                    :reference *range-ref*)
           *tfos* (make-instance 'sw-range
                    :genome '((2 . 5) (4 . 4) (4 . 5))
                    :reference *range-ref*)))
  (:teardown (setf *soft* nil *tfos* nil)))

(defixture gcd-asm
  (:setup (setf *gcd* (from-file (make-instance 'asm) (gcd-dir "gcd.s"))))
  (:teardown (setf *gcd* nil)))

(defixture population
  (:setup (setf *population* (loop for i from 1 to 9
                                collect (make-instance 'soft
                                          :genome (loop for j from 0 to i
                                                     collect j)
                                          :fitness i))))
  (:teardown (setf *population* nil)))


;;; ASM representation
(deftest simple-read ()
  (with-fixture gcd-asm
    (is (equal 'asm (type-of *gcd*)))))

(deftest idempotent-read-write ()
  (let ((a (software-evolution::temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file *gcd* a)
           (multiple-value-bind (out err ret)
               (software-evolution::shell "diff ~s ~a"
                                          (namestring (gcd-dir "gcd.s")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest idempotent-copy ()
  (with-fixture gcd-asm
   (is (software-evolution::equal-it *gcd* (copy *gcd*)))))

(deftest idempotent-read-copy-write ()
  (let ((a (software-evolution::temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file (copy *gcd*) a)
           (multiple-value-bind (out err ret)
               (software-evolution::shell "diff ~s ~a"
                                          (namestring (gcd-dir "gcd.s")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest edit-of-copy-does-not-change-original ()
  (with-fixture gcd-asm
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (mutate ant)
      (is (not (software-evolution::equal-it (genome ant) (genome *gcd*))))
      (is (equal orig-hash (sxhash (genome *gcd*)))))))

(deftest asm-cut-actually-shortens ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 4))
      (is (< (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-insertion-actually-lengthens ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:insert 4 8))
      (is (> (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-swap-maintains-length ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:swap 4 8))
      (is (not (tree-equal (genome variant) (genome *gcd*))))
      (is (= (length (genome variant)) (length (genome *gcd*)))))))

(deftest simple-crossover-test ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 0))
      ;; (push '(:cut 0) (edits variant))
      (let ((new (crossover variant *gcd*)))
        (is (not (tree-equal (genome new) (genome *gcd*))))
        ;; (is (some [{equal :crossover} #'car] (edits new)))
        ;; (is (some [{equal :cut} #'caar] (second (edits new))))
        ))))


;;; Range representation
(deftest range-size ()
  (with-fixture range (is (= 6 (size *soft*)))))

(deftest range-lines ()
  (with-fixture range
    (is (tree-equal (lines *soft*)
                    '("one" "two" "three" "two" "two" "three")
                    :test #'string=))))

(deftest range-nth-test ()
  (with-fixture range
    (is (equal (mapcar {software-evolution::range-nth _ (genome *soft*)}
                       (loop :for i :below (size *soft*) :collect i))
               '(0 1 2 1 1 2)))))

(deftest range-subseq-test ()
  (with-fixture range
    ;; to
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 1)
                    '((0 . 0))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 2)
                    '((0 . 1))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 3)
                    '((0 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 4)
                    '((0 . 2) (1 . 1))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 5)
                    '((0 . 2) (1 . 1) (1 . 1))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 6)
                    '((0 . 2) (1 . 1) (1 . 2))))
    ;; from
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 1 7)
                    '((1 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 2 7)
                    '((2 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 3 7)
                    '((1 . 1) (1 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 4 7)
                    '((1 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 5 7)
                    '((2 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 6 7)
                    'NIL))
    ;; both
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 2 5)
                    '((2 . 2) (1 . 1) (1 . 1))))))

(deftest some-range-cut-mutations ()
  (with-fixture range
    (is (tree-equal (apply-mutation *soft* '(:cut 2))
                    '((0 . 1) (1 . 1) (1 . 2))))
    (is (tree-equal (apply-mutation *soft* '(:cut 2))
                    '((0 . 1) (2 . 2))))
    (is (tree-equal (apply-mutation *soft* '(:cut 1))
                    '((0 . 0) (2 . 2))))
    (is (tree-equal (apply-mutation *soft* '(:cut 1))
                    '((0 . 0))))
    (is (null (apply-mutation *soft* '(:cut 0))))))

(deftest some-range-insert-mutations ()
  (with-fixture range
    (is (tree-equal (apply-mutation *soft* '(:insert 0 2))
                    '((2 . 2) (0 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (apply-mutation *soft* '(:insert 5 1))
                    '((2 . 2) (0 . 2) (1 . 1) (0 . 0) (1 . 2))))
    (is (tree-equal (apply-mutation *soft* '(:insert 5 2))
                    '((2 . 2) (0 . 2) (1 . 1) (1 . 1) (0 . 0) (1 . 2))))
    (is (tree-equal
         (apply-mutation *soft* '(:insert 2 1))
         '((2 . 2) (0 . 0) (0 . 0) (1 . 2) (1 . 1) (1 . 1) (0 . 0) (1 . 2))))))

(deftest some-range-swap-mutations ()
  (with-fixture range
    (apply-mutation *soft* '(:swap 0 2))
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


;;; Population tests
(deftest evict-population ()
  (with-fixture population
    (let ((before (length *population*)))
      (is (> before (length (progn (evict) *population*)))))))

(deftest incorporate-population ()
  (with-fixture population
    (let* ((before (length *population*))
           (*max-population-size* (+ 1 before)))
      (is (< before (length (progn (incorporate (make-instance 'software))
                                   *population*)))))))
