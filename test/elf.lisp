;;;; elf.lisp --- ELF representation.
(defpackage :software-evolution-library/test/elf
  (:nicknames :sel/test/elf)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/elf
   :software-evolution-library/software/elf-cisc
   :software-evolution-library/software/elf-risc)
  (:export :test-elf))
(in-package :software-evolution-library/test/elf)
(in-readtable :curry-compose-reader-macros)
(defsuite test-elf "ELF representation.")

(defun bytes (elf) (mappend [#'cdr {assoc :code}] (genome elf)))

(deftest elf-read ()
  (with-fixture gcd-elf
    (is (or (equal 'elf-x86 (type-of *gcd*))
            (equal 'elf-mips (type-of *gcd*))))))

(deftest elf-idempotent-read-write ()
  (with-temporary-file (:pathname a)
    (with-fixture gcd-elf
      (phenome *gcd* :bin a)
      (multiple-value-bind (out err ret)
          (shell "diff ~s ~a" (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-copy-same-genome ()
  (with-fixture gcd-elf
    (is (equal (genome-string *gcd*)
               (genome-string (copy *gcd*))))))

(deftest elf-idempotent-read-copy-write ()
  (with-temporary-file (:pathname a)
    (with-fixture gcd-elf
      (phenome (copy *gcd*) :bin a)
      (multiple-value-bind (out err ret)
          (shell "diff ~s ~a"
                 (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-edit-of-copy-does-not-change-original ()
  (with-fixture gcd-elf
    (let ((orig-hash (sxhash (genome-string *gcd*)))
          (ant (copy *gcd*)))
      (handler-case (mutate ant)
        (mutate (obj) (declare (ignorable obj)) nil))
      (is (not (equal (sxhash (genome-string ant))
                      (sxhash (genome-string *gcd*)))))
      (is (equal orig-hash (sxhash (genome-string *gcd*)))))))

(deftest elf-cut-changes-but-maintains-byte-length ()
  (with-fixture gcd-elf
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 4))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equalp (bytes *gcd*) (bytes variant)))))))

(deftest elf-insertion-changes-but-maintains-lengthens ()
  (with-fixture gcd-elf
    (let ((variant (copy *gcd*))
          ;; FIND-SMALL: Pick a single-byte instruction so that it is
          ;; more likely that there are sufficient no-ops to delete.
          ;; This works with the local compiled version of gcd, but
          ;; may fail in the future or on other systems.
          (to-copy
           (without-compiler-notes
            (position-if [{eql 1} #'length {aget :code}] (genome *gcd*)))))
      (apply-mutation variant (list :insert 0 to-copy))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equalp (bytes *gcd*) (bytes variant)))))))

(deftest elf-replace-changes-but-maintains-length ()
  (with-fixture gcd-elf
    (let* (;; See FIND-SMALL in `elf-insertion-changes-but-maintains-lengthens'
           (to-copy
            (without-compiler-notes
             (position-if [{eql 1} #'length {aget :code}] (genome *gcd*))))
           (variant (nest (copy *gcd* :genome)
                          (elf-replace
                           *gcd* 0 (copy-tree (nth to-copy (genome *gcd*)))))))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equalp (bytes *gcd*) (bytes variant)))))))

(deftest elf-swap-changes-but-maintains-length ()
  (with-fixture gcd-elf
    (let* ((variant (copy *gcd*))
           ;; Find two instructions of differing content so the genome
           ;; isn't the same after our swap.
           (prev nil)
           (place (position-if (lambda (el)
                                 (prog1 (and prev (not (tree-equal prev el)))
                                   (setf prev el)))
                               (genome *gcd*))))
      (apply-mutation variant (list :swap place (1- place)))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equalp (bytes *gcd*) (bytes variant)))))))

;; This test may fail stochastically depending on the place selected.
#+broken
(deftest elf-swap-touching-last-element ()
  (with-fixture gcd-elf
    (let* ((variant (copy *gcd*))
           (place1 (1- (length (genome *gcd*))))
           (value1 (nth place1 (genome *gcd*)))
           (place2 (random (1- (length (genome *gcd*)))))
           (value2 (nth place2 (genome *gcd*))))
      (apply-mutation variant (list :swap place1 place2))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (or (tree-equal value1 value2)
              (not (equalp (bytes *gcd*) (bytes variant))))))))

(deftest elf-swap-with-self ()
  (with-fixture gcd-elf
    (let* ((place (random (length (genome *gcd*))))
           (variant (copy *gcd*)))
      (apply-mutation variant (list :swap place place))
      (is (equal (genome-string variant) (genome-string *gcd*))))))

(deftest elf-crossover-test ()
  (with-fixture gcd-elf
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 0))
      (let ((new (crossover variant *gcd*)))
        (is (not (equal (genome-string new) (genome-string *gcd*))))
        (is (= (length (bytes *gcd*)) (length (bytes variant))))))))
