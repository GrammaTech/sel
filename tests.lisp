(in-package :soft-ev)
(require :stefil)
(use-package :stefil)
(defsuite soft-ev)
(in-suite soft-ev)


;;; ASM representation
(defvar *gcd* nil)

(defixture gcd-soft
  (:setup (setf *gcd* (asm-from-file "gcd.s")))
  (:teardown))

(deftest simple-read ()
  (with-fixture gcd-soft
    (is (equal 'soft-asm (type-of *gcd*)))))

(deftest idempotent-read-write ()
  (let ((a (temp-file-name)))
    (with-fixture gcd-soft
      (asm-to-file *gcd* a)
      (multiple-value-bind (out err ret) (shell "diff gcd.s ~a" a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest idempotent-copy ()
  (with-fixture gcd-soft
   (is (equal-it *gcd* (copy *gcd*)))))

(deftest idempotent-read-copy-write ()
  (with-fixture gcd-soft
    (let ((a (temp-file-name)))
      (asm-to-file (copy *gcd*) a)
      (multiple-value-bind (out err ret) (shell "diff gcd.s ~a" a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest simple-fitness ()
  (let ((*pos-test-num* 5)
        (*neg-test-num* 1)
        (*test-script* "./test.sh"))
    (with-fixture gcd-soft
      (is (= 5 (fitness *gcd*)))
      (is (= 5 (fitness (copy *gcd*)))))))
