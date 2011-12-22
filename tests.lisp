(in-package :soft-ev)
(require :stefil)
(use-package :stefil)
(defsuite soft-ev-test)
(in-suite soft-ev-test)


;;; tree representation
(deftest test-to-tree ()
  (is
   (equal-it
    (to-tree '(1 2 3 (4 5) 6))
    #S(TREE
       :DATA 1
       :BRANCHES (#S(TREE :DATA 2 :BRANCHES NIL)
                  #S(TREE :DATA 3 :BRANCHES NIL)
                  #S(TREE :DATA 4 :BRANCHES (#S(TREE :DATA 5 :BRANCHES NIL)))
                  #S(TREE :DATA 6 :BRANCHES NIL))))))

(deftest map-back-to-tree ()
  (let ((list '(1 2 3 (4 5) 6)))
    (is (equal-it (map-tree 'list #'tree-data (to-tree '(1 2 3 (4 5) 6)))
                  list))))

(deftest test-subtrees ()
  (is (equal-it (mapcar (lambda (subtree) (length (tree-branches subtree)))
                        (subtrees (to-tree '(1 2 3 4))))
                '(3 0 0 0))))


;;; general soft operators
(defixture list-genome
  (:setup (setf *genome* (loop for i from 0 to 9 collect i)))
  (:teardown))

(defixture soft
  (:setup (setf *soft* (make-instance 'soft
                         :genome (loop for i from 0 to 9 collect i))))
  (:teardown))

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
