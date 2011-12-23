(in-package :soft-ev)
(require :stefil)
(use-package :stefil)
(defsuite soft-ev-test)
(in-suite soft-ev-test)


;;; list genome
(defixture list-genome
  (:setup (setf *genome* (loop for i from 0 to 9 collect i)))
  (:teardown))

(defixture soft
  (:setup (setf *soft* (make-instance 'soft
                         :genome (loop for i from 0 to 9 collect i))))
  (:teardown))

(deftest ind-list ()
  (with-fixture list-genome
    (is (= 1 (ind *genome* 1)))))

(deftest inds-list ()
  (with-fixture list-genome
    (is (equal-it (inds *genome*) *genome*))))

(deftest setf-ind-list ()
  (with-fixture lsit-genome
    (setf (ind *genome* 1) :foo)
    (is (equal-it *gnome* '(0 :FOO 2 3 4 5 6 7 8 9)))))

(deftest cut-list ()
  (with-fixture list-genome
    (is (= 9 (length (cut *genome*))))))

(deftest insert-list ()
  (with-fixture list-genome
    (is (= 11 (length (insert *genome*))))
    (is (= 10 (length (remove-duplicates (insert *genome*)))))))

(deftest swap-list ()
  (with-fixture list-genome
    (is (= 10 (length (swap *genome*))))))

(deftest copy-soft ()
  (with-fixture soft
    (let ((new (copy *soft*)))
      (is (equal-it (genome new) (genome *soft*)))
      (cut new)
      (is (< (length (genome new))
             (length (genome *soft*)))))))


;;; tree genomes
(defixture tree-genome
  (:setup (setf *genome*
                #S(TREE
                   :DATA 1
                   :BRANCHES (#S(TREE :DATA 2 :BRANCHES NIL)
                              #S(TREE :DATA 3 :BRANCHES NIL)
                              #S(TREE :DATA 4 :BRANCHES
                                      (#S(TREE :DATA 5 :BRANCHES NIL)))
                              #S(TREE :DATA 6 :BRANCHES NIL)))))
  (:teardown))

(deftest list-to-tree ()
  (with-fixture tree-genome
    (is (equal-it (to-tree '(1 2 3 (4 5) 6))
                  *genome*))))

(deftest tree-to-list-conversion ()
  (with-fixture *genome*
    (is (equal-it (to-list (to-tree *genome*))
                  *genome*))))

(deftest ind-tree ()
  (with-fixture tree-genome
    (is (equal-it (ind *genome* 3)
                  #S(TREE :DATA 4
                          :BRANCHES (#S(TREE :DATA 5 :BRANCHES NIL)))))))

(deftest inds-tree ()
  (with-fixture tree-genome
    (is (equal-it (inds *genome*) '(0 1 2 3 4 5)))))

(deftest setf-ind-tree ()
  (with-fixture tree-genome
    (is (equal (setf (ind *genome* 2))
               (ind *genome* 2)))))


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


;;; Population tests
(defixture population
  (:setup (setf *population* (loop for i from 1 to 9
                                collect (make-instance 'soft
                                          :genome (loop for j from 0 to i
                                                     collect j)
                                          :fitness i))))
  (:teardown))

(deftest evict-population ()
  (with-fixture population
    (let ((before (length *population*)))
      (is (> before (length (progn (evict) *population*)))))))

(deftest incorporate-population ()
  (with-fixture population
    (let* ((before (length *population*))
           (*max-population-size* (+ 1 before)))
      (is (< before (length (progn (incorporate (make-instance 'soft))
                                   *population*)))))))
