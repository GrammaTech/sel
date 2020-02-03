;;;; asm.lisp --- ASM representation.
(defpackage :software-evolution-library/test/asm
  (:nicknames :sel/test/asm)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/constants
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
  (:export :asm))
(in-package :software-evolution-library/test/asm)
(in-readtable :curry-compose-reader-macros)
(defsuite asm)

(defvar *gcd* nil "Holds the gcd software object.")

(define-constant +asm-test-dir+ (append +etc-dir+ (list "asm-test"))
  :test #'equalp
  :documentation "Path to asm-test examples.")

(deftest simple-read ()
  (with-fixture gcd-asm
    (is (equal 'asm (type-of *gcd*)))))

(deftest (idempotent-read-write :long-running) ()
  (let ((a (temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file *gcd* a)
           (multiple-value-bind (out err ret)
               (shell "diff ~s ~a"
                      (namestring (gcd-dir "gcd.s.intel")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest idempotent-copy ()
  (with-fixture gcd-asm
    (is (equal-it *gcd* (copy *gcd*) nil '(oid)))))

(deftest (idempotent-read-copy-write :long-running) ()
  (let ((a (temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file (copy *gcd*) a)
           (multiple-value-bind (out err ret)
               (shell "diff ~s ~a" (namestring (gcd-dir "gcd.s.intel")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest edit-of-copy-does-not-change-original ()
  (with-fixture gcd-asm
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (is (with-retries (100)
            ;; Multiple tries to apply a mutation creating a
            ;; difference.  Stochastically some might result in the
            ;; same genome, e.g. by swapping to identical
            ;; instructions.
            (handler-bind
                ((no-mutation-targets
                  (lambda (c)
                    (declare (ignorable c))
                    (invoke-restart 'try-another-mutation))))
              (mutate ant))
            (when (not (equal-it (genome ant) (genome *gcd*)))
              (return t)))
          "In 100 tries, a mutation results in a different mutated genome.")
      (is (equal orig-hash (sxhash (genome *gcd*)))))))

(deftest asm-cut-actually-shortens ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'simple-cut :targets 4))
      (is (< (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-insertion-actually-lengthens ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'simple-insert
                                :targets (list 4 8)))
      (is (> (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-swap-maintains-length ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*))
          (mutation
           (make-instance 'simple-swap
             :targets (list
                       (position-if [{scan "mov"} {aget :code}]
                                    (genome *gcd*))
                       (position-if [{scan "call"} {aget :code}]
                                    (genome *gcd*))))))
      (setf variant (apply-mutation variant mutation))
      (is (not (tree-equal (genome variant) (genome *gcd*) :test #'tree-equal)))
      (is (= (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-replace-operand-maintains-length ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'asm-replace-operand
                                :targets (list 13 18)))
      (is (not (tree-equal (genome variant) (genome *gcd*) :test #'tree-equal)))
      (is (= (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-replace-operand-changes-operand ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'asm-replace-operand
                                :targets (list 13 18)))
      (is (not (equal (aget :code (elt (genome variant) 13))
                      (aget :code (elt (genome *gcd*) 13))))))))

(deftest asm-split-instruction-has-correct-length ()
  (let ((test1 (asm-split-instruction  "        movq    %rsp, %rbp"))
        (test2 (asm-split-instruction " movl    $0  , -4(%rbp)   ")))
    (is (= 3 (length test1)))
    (is (equal (list "movq" "%rsp" "%rbp") test1))
    (is (= 3 (length test2)))
    (is (equal (list "movl" "$0" "-4(%rbp)") test2))))

(deftest simple-crossover-test ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'simple-cut :targets 0))
      ;; (push '(:cut 0) (edits variant))
      (let ((new (crossover variant *gcd*)))
        (is (not (tree-equal (genome new) (genome *gcd*) :test #'tree-equal)))
        ;; (is (some [{equal :crossover} #'car] (edits new)))
        ;; (is (some [{equal :cut} #'caar] (second (edits new))))
        ))))

(deftest (asm-can-form-a-phenome :long-running) ()
  (with-fixture gcd-asm
    (with-temp-file (bin)
      (ignore-phenome-errors
       (is (phenome *gcd* :bin bin)
           "Phenome works on an ASM software object.")
       (is (probe-file bin)
           "Phenome creates the binary file for an ASM software object.")
       (is (nth-value 2 (shell "~a 1 1" bin))
           "Phenome creates a runnable binary for an ASM software object.")))))

(deftest homologous-crossover-same-same ()
  (with-fixture gcd-asm
    (is (tree-equal (genome *gcd*)
                    (genome (homologous-crossover *gcd* (copy *gcd*)))))))

(deftest homologous-crossover-with-cut ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*))
          (target 40))
      ;; apply cut to variant
      (apply-mutation variant
                      (make-instance 'simple-cut
                        :object variant
                        :targets target))
      (multiple-value-bind (crossed cross-a cross-b)
          (homologous-crossover *gcd* variant)
        (declare (ignorable cross-a cross-b))
        ;; If copied before cut, size is 1 smaller. If after it's the
        ;; same.  If equal it could go either way
        (cond
          ((< cross-b target) (is (= (1- (size *gcd*)) (size crossed))))
          ((= cross-b target) (is (or (= (1- (size *gcd*)) (size crossed))
                                      (= (size *gcd*) (size crossed)))))
          ((> cross-b target) (is (= (size *gcd*) (size crossed)))))))))

(deftest homologous-crossover-with-insert ()
  ;; NOTE: If crossover changes, this test may fail sporadically: behavior
  ;; depends on whether crossover target is before or after insert-pt.
  ;; TODO: This test documents a shortcoming in the crossover impl that we
  ;; want to fix. If the cross-point in A refers to an index that was previouly
  ;; copied from another part of A, we will select cross-b as the source of the
  ;; copied statement (which might not be close to the ideal point for a
  ;; homologous crossover.
  (with-fixture gcd-asm
    (let ((insert-pt 40)
          (stmt-to-insert 60)
          (variant (copy *gcd*)))
      (apply-mutation variant
                      (make-instance 'simple-insert
                        :object variant
                        :targets (list insert-pt stmt-to-insert)))
      (multiple-value-bind (crossed cross-a cross-b)
          (homologous-crossover variant *gcd*)
        (declare (ignorable cross-b))
        (cond
          ((< cross-a insert-pt)
           (is (= (size crossed) (size *gcd*))))
          ((= cross-a insert-pt)
           (is (= (size crossed)
                  (- (size *gcd*) (- stmt-to-insert insert-pt)))))
          (t (is (= (size crossed) (1+ (size *gcd*))))))))))

(deftest homologous-crossover-with-swap ()
  ;; NOTE: If crossover changes, this test may fail sporadically: behavior
  ;; depends on whether crossover target is exactly on a swap point or not.
  ;; TODO: This test documents a shortcoming in the crossover impl that we
  ;; want to fix. If the cross-point in A refers to an index that was previouly
  ;; swapped in B, we will select cross-b as that new location.
  (with-fixture gcd-asm
    (let ((targets (list 20 60))
          (variant (copy *gcd*)))
      (apply-mutation
       variant
       (make-instance 'simple-swap :object variant :targets targets))
      (multiple-value-bind (crossed cross-a cross-b)
          (homologous-crossover *gcd* variant)
        (declare (ignorable cross-b))
        (cond
          ((= cross-a 20)
           (is (= (size crossed) (- (size *gcd*) 40))))
          ((= cross-a 60)
           (is (= (size crossed) (+ (size *gcd*) 40))))
          (t (is (= (size crossed) (size *gcd*)))))))))



;;; ASM-HEAP representation.

(deftest simple-asm-heap-read-intel ()
  (with-fixture gcd-asm-heap-intel
    (is (equal 'asm-heap (type-of *gcd*)))
    (is (equal (asm-syntax *gcd*) ':intel))))

(deftest simple-asm-heap-read-att ()
  (with-fixture gcd-asm-heap-att
    (is (equal 'asm-heap (type-of *gcd*)))
    (is (equal (asm-syntax *gcd*) ':att))))

(defun idempotent-asm-heap-read-write (filename)
  (let ((a (temp-file-name)))
    (unwind-protect
         (to-file *gcd* a)
      (multiple-value-bind (out err ret)
          (shell "diff ~s ~a"
                 (namestring (gcd-dir filename)) a)
        (declare (ignorable out err))
        (is (= 0 ret)))
      (delete-file a))))

(deftest (idempotent-asm-heap-read-write-intel :long-running) ()
  (with-fixture gcd-asm-heap-intel
    (idempotent-asm-heap-read-write "gcd.s.intel")))

(deftest (idempotent-asm-heap-read-write-att :long-running) ()
  (with-fixture gcd-asm-heap-att
    (idempotent-asm-heap-read-write "gcd.s.att")))

(deftest (idempotent-asm-heap-copy-intel :long-running) ()
  (with-fixture gcd-asm-heap-intel
    (is (equal-it *gcd* (copy *gcd*) nil '(oid)))))

(deftest (idempotent-asm-heap-copy-att :long-running) ()
  (with-fixture gcd-asm-heap-att
    (is (equal-it *gcd* (copy *gcd*) nil '(oid)))))

(defun idempotent-asm-heap-read-copy-write (filename)
  (let ((a (temp-file-name)))
    (unwind-protect
         (to-file (copy *gcd*) a)
      (multiple-value-bind (out err ret)
          (shell "diff ~s ~a" (namestring (gcd-dir filename)) a)
        (declare (ignorable out err))
        (is (= 0 ret)))
      (delete-file a))))

(deftest (idempotent-asm-heap-read-copy-write-intel :long-running) ()
  (with-fixture gcd-asm-heap-intel
    (idempotent-asm-heap-read-write "gcd.s.intel")))

(deftest (idempotent-asm-heap-read-copy-write-att :long-running) ()
  (with-fixture gcd-asm-heap-att
    (idempotent-asm-heap-read-write "gcd.s.att")))

(defun edit-of-asm-heap-copy-does-not-change-original ()
  (let ((orig-hash (sxhash (genome *gcd*)))
        (variant (copy *gcd*)))
    (is (with-retries (100)
          ;; Multiple tries to apply a mutation creating a difference.
          ;; Stochastically some might result in the same genome,
          ;; e.g. by swapping to identical instructions.
          (handler-bind
              ((no-mutation-targets
                (lambda (c)
                  (declare (ignorable c))
                  (invoke-restart 'try-another-mutation))))
            (mutate variant))
          (when (not (equal-it (genome variant) (genome *gcd*)))
            (return t)))
        "In 100 tries, a mutation results in a different mutated genome.")
    (is (equal orig-hash (sxhash (genome *gcd*))))))

(deftest edit-of-asm-heap-copy-does-not-change-original-intel ()
  (with-fixture gcd-asm-heap-intel
    (edit-of-asm-heap-copy-does-not-change-original)))

(deftest edit-of-asm-heap-copy-does-not-change-original-att ()
  (with-fixture gcd-asm-heap-intel
    (edit-of-asm-heap-copy-does-not-change-original)))

(defun asm-heap-cut-actually-shortens ()
  (let ((variant (copy *gcd*)))
    (apply-mutation variant (make-instance 'simple-cut :targets 4))
    (is (< (length (genome variant)) (length (genome *gcd*))))))

(deftest asm-heap-cut-actually-shortens-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-cut-actually-shortens)))

(deftest asm-heap-cut-actually-shortens-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-cut-actually-shortens)))

(defun asm-heap-double-cut-shortens-correctly ()
  (let ((variant (copy *gcd*)))
    (apply-mutation variant (make-instance 'double-cut :targets '(16 18)))
    (is (= (length (genome variant)) (- (length (genome *gcd*)) 2)))
    (setf variant (copy *gcd*))
    (apply-mutation variant (make-instance 'double-cut :targets '(18 16)))
    (is (= (length (genome variant)) (- (length (genome *gcd*)) 2)))
    (setf variant (copy *gcd*))
    (apply-mutation variant (make-instance 'double-cut :targets '(18 18)))
    (is (= (length (genome variant)) (- (length (genome *gcd*)) 1)))))

(deftest asm-heap-double-cut-shortens-correctly-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-double-cut-shortens-correctly)))

(deftest asm-heap-double-cut-shortens-correctly-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-double-cut-shortens-correctly)))

(defun asm-heap-insertion-actually-lengthens ()
  (let ((variant (copy *gcd*)))
    (apply-mutation variant (make-instance 'simple-insert
                              :targets (list 4 8)))
    (is (> (length (genome variant)) (length (genome *gcd*))))))

(deftest asm-heap-insertion-actually-lengthens-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-insertion-actually-lengthens)))

(deftest asm-heap-insertion-actually-lengthens-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-insertion-actually-lengthens)))

;;;
;;; Note that these operands chosen (movsd, call) are
;;; both in common with both intel and att syntax (so work for both).
;;;
(defun asm-heap-swap-maintains-length ()
  (let ((variant (copy *gcd*))
        (mutation
         (make-instance 'simple-swap
           :targets (list
                     (position-if
                      [{equalp "movsd"} #'asm-line-info-opcode]
                      (genome *gcd*))
                     (position-if
                      [{equalp "call"} #'asm-line-info-opcode]
                      (genome *gcd*))))))
    (setf variant (apply-mutation variant mutation))
    (is (not (equalp (genome variant) (genome *gcd*))))
    (is (= (length (genome variant)) (length (genome *gcd*))))))

(deftest asm-heap-swap-maintains-length-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-swap-maintains-length)))

(deftest asm-heap-swap-maintains-length-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-swap-maintains-length)))

(defun asm-heap-replace-operand-maintains-length ()
  (let ((variant (copy *gcd*)))
    (apply-mutation variant (make-instance 'asm-replace-operand
                              :targets (list 16 17)))
    (is (not (tree-equal (genome variant) (genome *gcd*) :test #'tree-equal)))
    (is (= (length (genome variant)) (length (genome *gcd*))))))

(deftest asm-heap-replace-operand-maintains-length-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-replace-operand-maintains-length)))

(deftest asm-heap-replace-operand-maintains-length-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-replace-operand-maintains-length)))

(defun asm-heap-replace-operand-changes-operand ()
  (let ((variant (copy *gcd*)))
    ;; need a deep copy of the heap
    (setf (lines variant)(lines *gcd*))
    (apply-mutation variant (make-instance 'asm-replace-operand
                              :targets (list 16 17)))
    (is (not (equalp (asm-line-info-operands (elt (genome variant) 16))
                     (asm-line-info-operands (elt (genome *gcd*) 16)))))))

(deftest asm-heap-replace-operand-changes-operand-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-replace-operand-changes-operand)))

(deftest asm-heap-replace-operand-changes-operand-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-replace-operand-changes-operand)))

(defun asm-heap-simple-crossover-test ()
  (let ((variant (copy *gcd*)))
    (apply-mutation variant (make-instance 'simple-cut :targets 0))
    (let ((new (crossover variant *gcd*)))
      (is (not (tree-equal (genome new) (genome *gcd*) :test #'tree-equal))))))

(deftest asm-heap-simple-crossover-test-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-simple-crossover-test)))

(deftest asm-heap-simple-crossover-test-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-simple-crossover-test)))

(defun asm-heap-can-form-a-phenome ()
  (with-temp-file (bin)
    (ignore-phenome-errors
     (is (phenome *gcd* :bin bin)
         "Phenome works on an ASM-HEAP software object.")
     (is (probe-file bin)
         "Phenome creates the binary file for an ASM-HEAP software object.")
     (is (nth-value 2 (shell "~a 1 1" bin))
         "Phenome creates a runnable binary for an ASM-HEAP software object."))))

(deftest (asm-heap-can-form-a-phenome-intel :long-running) ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-can-form-a-phenome)))

(deftest (asm-heap-can-form-a-phenome-att :long-running) ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-can-form-a-phenome)))

(defun asm-heap-homologous-crossover-same-same ()
  (is (tree-equal (genome *gcd*)
                  (genome (homologous-crossover *gcd* (copy *gcd*)))
                  :test 'equalp)))

(deftest asm-heap-homologous-crossover-same-same-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-homologous-crossover-same-same)))

(deftest asm-heap-homologous-crossover-same-same-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-homologous-crossover-same-same)))
