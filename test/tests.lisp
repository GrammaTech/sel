;;; tests.lisp --- tests for the `software-evolution' package

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :software-evolution-test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defmacro every-is (function &rest lists)
  (let ((args-sym (gensym "args")))
    `(mapc (lambda (&rest ,args-sym)
             (is (apply ,function ,args-sym)))
           ,@lists)))

;;; Run tests is "batch" mode printing results as a string.
(defun batch-test (&optional args)
  (declare (ignorable args))
  (let* ((*test-progress-print-right-margin* (expt 2 20))
         (failures (stefil::failure-descriptions-of
                    (without-debugging (test)))))
    (format *error-output* "FAILURES~%")
    (mapc [{format *error-output* "  ~a~%"}
           #'stefil::name-of
           #'stefil::test-of
           #'car #'stefil::test-context-backtrace-of]
          (coerce failures 'list))))

(defsuite test)
(in-suite test)

(defvar *genome*      nil "Genome used in tests.")
(defvar *soft*        nil "Software used in tests.")
(defvar *tfos*        nil "Another software used in tests.")
(defvar *gcd*         nil "Holds the gcd software object.")
(defvar *hello-world* nil "Holds the hello world software object.")
(defvar *huf*         nil "Holds the huf software object.")
(defvar *range-ref* #("one" "two" "three" "four" "five" "six")
  "Example range software object.")
(defvar *base-dir*
  (pathname-directory #.(or *compile-file-truename*
                            *load-truename*
                            *default-pathname-defaults*)))
(defvar *gcd-dir*
  (make-pathname :directory (append *base-dir* (list "gcd")))
  "Location of the gcd example directory")

(defvar *hello-world-dir*
  (make-pathname :directory (append *base-dir* (list "hello-world")))
  "Location of the hello world example directory")

(defvar *huf-dir*
  (make-pathname :directory (append *base-dir* (list "huf")))
  "Location of the huf example directory")

(defun gcd-dir (filename) (merge-pathnames filename *gcd-dir*))
(defun hello-world-dir (filename) (merge-pathnames filename *hello-world-dir*))
(defun huf-dir (filename) (merge-pathnames filename *huf-dir*))

(define-software soft (software)
  ((genome :initarg :genome :accessor genome :initform nil)))

(defvar *soft-mutate-errors* nil
  "Control when mutations on soft objects throw errors.")

(defmethod crossover ((a soft) (b soft))
  (values (copy a)(list :fake-a) (list :fake-b)))
(defmethod mutate ((a soft))
  (if *soft-mutate-errors*
      (error (make-condition 'mutate
               :text "FAKE"
               :obj a
               :op '(:fake)))
      (values (copy a) (list :fake))))

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

(defixture diff
  (:setup
   (setf *soft* (make-instance 'diff)
         (genome *soft*) '(((:code 1)) ((:code 2)) ((:code 3)) ((:code 4)))))
  (:teardown (setf *soft* nil)))

(defixture double-diff
  (:setup
   (setf *soft* (make-instance 'diff)
         (genome *soft*) '(((:code 1)) ((:code 2)) ((:code 3)) ((:code 4)))
         *tfos* (make-instance 'diff)
         (genome *tfos*) '(((:code 1)) ((:code 2)) ((:code 3)) ((:code 4)))))
  (:teardown (setf *soft* nil *tfos* nil)))

(defixture diff-array
  (:setup
   (setf *soft* (make-instance 'diff)
         (genome *soft*) #(((:code 1)) ((:code 2)) ((:code 3)) ((:code 4)))))
  (:teardown (setf *soft* nil)))

(defixture gcd-asm
  (:setup (setf *gcd* (from-file (make-instance 'asm) (gcd-dir "gcd.s"))))
  (:teardown (setf *gcd* nil)))

(defixture gcd-elf
  (:setup
   (let ((arch (intern (string-upcase (subseq (shell "uname -m") 0 3)))))
     (setf *gcd* (from-file (make-instance (case arch
                                             (x86 'elf-x86)
                                             (mips 'elf-mips)))
                            (gcd-dir "gcd")))))
  (:teardown (setf *gcd* nil)))

(defixture hello-world-clang
  (:setup
    (setf *hello-world*
      (from-file (make-instance 'clang :compiler "clang-3.7"
                                       :flags '("-g -m32 -O0"))
                 (hello-world-dir "hello_world.c"))))
  (:teardown
    (setf *hello-world* nil)))

(defixture hello-world-clang-w-fodder
  (:setup
   (clang-w-fodder-setup-db (hello-world-dir "hello_world_ast.json"))
   (setf *hello-world*
     (from-file (make-instance 'clang-w-fodder :compiler "clang-3.7"
                                               :flags '("-g -m32 -O0"))
                (hello-world-dir "hello_world.c"))))
  (:teardown
    (setf *hello-world* nil)))

(defixture hello-world-clang-w-binary
  (:setup
   (clang-w-fodder-setup-db (hello-world-dir "hello_world_ast.json"))
   (let ((src-path (hello-world-dir "hello_world.c"))
         (bin-path (hello-world-dir "hello_world")))
     (unless (probe-file bin-path)
       (phenome (from-file (make-instance 'clang
                             :compiler "clang-3.7"
                             :flags '("-g -m32 -O0"))
                           src-path)
                :bin bin-path))
     (setf *hello-world*
           (from-file (make-instance 'clang-w-binary
                        :compiler "clang-3.7"
                        :flags '("-g -m32 -O0")
                        :bytes (file-to-bytes bin-path))
                      src-path))))
  (:teardown
   (setf *hello-world* nil)))

(defixture huf-clang
  (:setup
    (setf *huf*
      (from-file (make-instance 'clang :compiler "gcc" :flags '("-g -m32 -O0"))
                 (huf-dir "huf.c"))))
  (:teardown
    (setf *huf* nil)))

(defixture population
  (:setup (setf *population* (loop :for i :from 1 :to 9
                                collect (make-instance 'soft
                                          :genome (loop for j from 0 to i
                                                     collect j)
                                          :fitness i))
                *fitness-evals* 0
                *mutation-stats* (make-hash-table)
                *crossover-stats* (make-hash-table)))
  (:teardown (setf *population* nil
                   *fitness-evals* 0
                   *mutation-stats* nil
                   *crossover-stats* nil)))


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


;;; ELF representation
#| ;; TODO: Currently failing because we're not populating the .text section.

(defun bytes (elf) (mappend [#'cdr {assoc :code}] (genome elf)))

(deftest elf-read ()
  (with-fixture gcd-elf
    (is (or (equal 'elf-x86 (type-of *gcd*))
            (equal 'elf-mips (type-of *gcd*))))))

(deftest elf-idempotent-read-write ()
  (with-temp-file (a)
    (with-fixture gcd-elf
      (phenome *gcd* :bin a)
      (multiple-value-bind (out err ret)
          (software-evolution::shell "diff ~s ~a"
                                     (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-copy-same-genome ()
  (with-fixture gcd-elf
   (is (software-evolution::equal-it (genome *gcd*)
                                     (genome (copy *gcd*))))))

(deftest elf-idempotent-read-copy-write ()
  (with-temp-file (a)
    (with-fixture gcd-elf
      (phenome (copy *gcd*) :bin a)
      (multiple-value-bind (out err ret)
          (software-evolution::shell "diff ~s ~a"
                                     (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-edit-of-copy-does-not-change-original ()
  (with-fixture gcd-elf
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (handler-case (mutate ant)
        (mutate (obj) (declare (ignorable obj)) nil))
      (is (not (software-evolution::equal-it (genome ant) (genome *gcd*))))
      (is (equal orig-hash (sxhash (genome *gcd*)))))))

(deftest elf-cut-changes-but-maintains-byte-length ()
  (with-fixture gcd-elf
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 4))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equal-it (bytes *gcd*) (bytes variant)))))))

(deftest elf-insertion-changes-but-maintains-lengthens ()
  (with-fixture gcd-elf
    (let ((variant (copy *gcd*))
          ;; FIND-SMALL: Pick a single-byte instruction so that it is
          ;; more likely that there are sufficient no-ops to delete.
          ;; This works with the local compiled version of gcd, but
          ;; may fail in the future or on other systems.
          (to-copy (position-if [{= 1} #'length {aget :code}] (genome *gcd*))))
      (apply-mutation variant (list :insert 0 to-copy))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equal-it (bytes *gcd*) (bytes variant)))))))

(deftest elf-replace-changes-but-maintains-length ()
  (with-fixture gcd-elf
    (let* ((variant (copy *gcd*))
           ;; See FIND-SMALL in `elf-insertion-changes-but-maintains-lengthens'
           (to-copy (position-if [{= 1} #'length {aget :code}] (genome *gcd*)))
           (new-genome (software-evolution::elf-replace
                        variant 0 (copy-tree (nth to-copy (genome *gcd*))))))
      (is (= (length (mappend {aget :code} (genome *gcd*)))
             (length (mappend {aget :code} new-genome))))
      (is (not (equal-it (mappend {aget :code} (genome *gcd*))
                         (mappend {aget :code} new-genome)))))))

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
      (is (not (equal-it (bytes *gcd*) (bytes variant)))))))

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
              (not (equal-it (bytes *gcd*) (bytes variant))))))))

(deftest elf-swap-with-self ()
  (with-fixture gcd-elf
    (let* ((place (random (length (genome *gcd*))))
           (variant (copy *gcd*)))
      (apply-mutation variant (list :swap place place))
      (is (equal-it (genome variant) (genome *gcd*))))))

(deftest elf-crossover-test ()
  (with-fixture gcd-elf
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 0))
      (let ((new (crossover variant *gcd*)))
        (is (not (equal-it (genome new) (genome *gcd*))))
        (is (= (length (bytes *gcd*)) (length (bytes variant))))))))
|#


;;; Clang representation
(deftest simply-able-to-load-a-clang-software-object()
  (with-fixture hello-world-clang
    (is (not (null *hello-world*)))))

(deftest cut-changes-clang-asts ()
  (with-fixture hello-world-clang
    (let* ((variant (copy *hello-world*))
           (stmt1 (stmt-with-text variant
                                  "printf(\"Hello, World!\\n\")")))
      (apply-mutation variant `(:cut (:stmt1 . ,stmt1)))
      (is  (equal (asts variant) (asts *hello-world*))))))

(deftest cut-shortens-a-clang-software-object()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*))
          (stmt1 (stmt-with-text variant
                                 "printf(\"Hello, World!\\n\")")))
      (apply-mutation variant `(:cut (:stmt1 . ,stmt1)))
      (is (< (size variant)
             (size *hello-world*))))))

(deftest insert-lengthens-a-clang-software-object()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*))
          (stmt1 (stmt-with-text *hello-world*
                                 "printf(\"Hello, World!\\n\")"))
          (stmt2 (stmt-with-text *hello-world*
                                 "return 0")))
      (apply-mutation variant
                      `(:insert (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (is (> (size variant)
             (size *hello-world*))))))

(deftest swap-changes-a-clang-software-object()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*))
          (stmt1 (stmt-with-text *hello-world*
                                 "printf(\"Hello, World!\\n\")"))
          (stmt2 (stmt-with-text *hello-world*
                                 "return 0")))
      (apply-mutation variant
                      `(:swap (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (is (= (size variant)
             (size *hello-world*))))))

(deftest crossover-clang-software-object-do-not-crash()
  (with-fixture hello-world-clang
    (let* ((variant (crossover (copy *hello-world*) (copy *hello-world*))))
      (is (string/= (genome variant)
                    "")))))


;;; Clang w/ mutation fodder representation
(deftest simply-able-to-load-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (is (not (null *hello-world*)))))

(deftest insert-value-lengthens-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant '(:insert-value (:stmt1 . 2)
                                              (:value1 . "int i = 0;")))
      (is (> (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest set-value-changes-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant
        `(:set
          (:stmt1 . ,(stmt-with-text variant "\"Hello, World!\\n\""))
          (:value1 . "\"Hello, mutate!\"")))
      (is (= (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

;;; Targetted mutation test
(deftest pick-bad-returns-appropriate-ast-of-clang-w-binary-software-object ()
  (with-fixture hello-world-clang-w-binary
    (let ((variant (from-file (make-instance 'clang-w-binary
                                :compiler "clang"
                                :flags '("-g -O0 -m32")
                                :diff-addresses (list (make-instance 'range
                                                        :begin #x8048433
                                                        :end #x804843d))
                                :bytes (file-to-bytes (hello-world-dir
                                                       "hello_world")))
                              (hello-world-dir "hello_world.c"))))
      (is (member (pick-bad-targetted variant) (list 2 8))))))


;;; Clang utility methods
(deftest asts-populated-on-creation ()
  (with-fixture hello-world-clang
    (is (= 9 (length (asts *hello-world*))))))

(deftest asts-have-binary-addresses ()
  (with-fixture hello-world-clang-w-binary
    (is (>= (count-if-not {aget :begin--addr} (asts *hello-world*))
            2))))

(deftest is-parent-ast?-true-test()
  (with-fixture hello-world-clang
    (is (is-parent-ast? *hello-world*
          (get-ast *hello-world* (stmt-with-text *hello-world*
                                                  "return 0"))
          (get-ast *hello-world* (stmt-with-text *hello-world*
                                                  "0"))))))

(deftest is-parent-ast?-false-test()
  (with-fixture hello-world-clang
    (is (not (is-parent-ast? *hello-world*
               (get-ast *hello-world* (stmt-with-text *hello-world*
                                                       "0"))
               (get-ast *hello-world* (stmt-with-text *hello-world*
                                                       "return 0")))))))

(deftest tidy-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (clang-tidy variant)
      (is (= (size variant)
             (size *hello-world*))))))


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


;;; Diff tests
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
      (is (tree-equal (apply-mutation *soft* '(:cut 2))
                      '(((:CODE 1)) ((:CODE 2)) ((:CODE 4)))))
      (is (tree-equal (apply-mutation *soft* '(:cut 1))
                      '(((:CODE 1)) ((:CODE 4)))))
      (is (tree-equal (apply-mutation *soft* '(:cut 1))
                      '(((:CODE 1))))))))

(deftest some-diff-insert-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal (apply-mutation *soft* '(:insert 0 2))
                      '(((:CODE 3)) ((:CODE 1)) ((:CODE 2))
                        ((:CODE 3)) ((:CODE 4))))))))

(deftest some-diff-swap-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal (apply-mutation *soft* '(:swap 0 2))
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
      (is (equalp (apply-mutation *soft* '(:cut 2))
                  #(((:CODE 1)) ((:CODE 2)) ((:CODE 4)))))
      (is (equalp (apply-mutation *soft* '(:cut 1))
                  #(((:CODE 1)) ((:CODE 4)))))
      (is (equalp (apply-mutation *soft* '(:cut 1))
                  #(((:CODE 1))))))))

(deftest some-diff-array-insert-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (apply-mutation *soft* '(:insert 0 2))
                  #(((:CODE 3)) ((:CODE 1)) ((:CODE 2))
                    ((:CODE 3)) ((:CODE 4))))))))

(deftest some-diff-array-swap-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (apply-mutation *soft* '(:swap 0 2))
                  #(((:CODE 3)) ((:CODE 2)) ((:CODE 1)) ((:CODE 4))))))))

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

(deftest evolution-collects-no-statistics-by-default ()
  (let ((counter 0)
        (*fitness-predicate* #'>))
    (flet ((test (candidate)
             (declare (ignorable candidate))
             (incf counter)
             (if (= counter 5) 2 1)))
      (with-fixture population
        (evolve #'test :max-evals 10)
        (is (zerop (length (hash-table-alist *mutation-stats*))))))))

(deftest evolution-collects-statistics-when-asked ()
  (let ((counter 0)
        (*fitness-predicate* #'>))
    (flet ((test (candidate)
             (declare (ignorable candidate))
             (incf counter)
             (let ((out (if (> counter 10)
                            (- 0 counter)
                            counter)))
               out)))
      (with-fixture population
        ;; Should still signal errors.
        (let ((*soft-mutate-errors* t))
          (signals mutate (evolve #'test :max-evals 20 :mutation-stats t)))
        (evolve #'test :max-evals 20 :mutation-stats t)
        (is (equal '(:fake) (hash-table-keys *mutation-stats*)))
        ;; Error, and 1 more than :max-evals.
        (is (= 22 (length (gethash :fake *mutation-stats*))))
        (let ((statuses (mapcar #'cadr (gethash :fake *mutation-stats*))))
          (is (member :better statuses))
          (is (member :worse statuses))
          (is (member :error statuses)))))))

(deftest terminate-evolution-on-success ()
  (let ((counter 0))
    (flet ((test (candidate)
             (declare (ignorable candidate))
             (incf counter)
             (if (= counter 5) 2 1)))
      (with-fixture population
        (evolve #'test :target 2)
        (is (= *fitness-evals* 5))))))


;;; Helper function to avoid hard-coded statement numbers.
(defun stmt-with-text (obj text)
  (let ((the-snippet
         (find-if (lambda (snippet)
                    (and snippet
                         (equal text
                                (apply-replacements '(("(|" . "")
                                                      ("|)" . ""))
                                  (json-string-unescape
                                   (aget :src--text snippet))))))
                  (asts obj))))
    (aget :counter the-snippet)))

(deftest swap-can-recontextualize ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons :swap
              (list (cons :stmt1 (stmt-with-text variant "n > 0"))
                    (cons :stmt2 (stmt-with-text variant "bc=0")))))
      (multiple-value-bind (result exit)
          (phenome variant)
        (declare (ignorable result))
        (is (= 0 exit))))))

(defun diff-strings (original modified diff-region)
  "Convert a diff-region to a list of contents in ORIGINAL and MODIFIED."
  (flet ((diff-subseq (seq start length)
           (subseq seq start (+ start length))))
    (list (diff-subseq original
                       (diff::original-start diff-region)
                       (diff::original-length diff-region))
          (diff-subseq modified
                       (diff::modified-start diff-region)
                       (diff::modified-length diff-region)))))

(defun show-diff (original modified &optional (stream t))
  "Return a string diff of two software objects.
Useful for printing or returning differences in the REPL."
  (diff:render-diff (diff::generate-seq-diff 'DIFF:UNIFIED-DIFF
                                             (lines original)
                                             (lines modified))
                    stream))

(deftest swap-makes-expected-change ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*))
          (text-1 "n > 0")
          (text-2 "bc=0"))
      ;; Apply the swap mutation.
      (apply-mutation variant
        (cons :swap (list (cons :stmt1 (stmt-with-text variant text-1))
                          (cons :stmt2 (stmt-with-text variant text-2)))))
      ;; Each element should contain the text of one of the swapped pieces.
      (every-is {scan (create-scanner (list :alternation text-1 text-2))}
                (mapcar [{apply #'concatenate 'string}
                         {mapcar {apply #'concatenate 'string}}]
                        ;; Collect the differences between the
                        ;; original and the variant.
                        (mapcar {diff-strings (lines *huf*) (lines variant)}
                                (remove-if-not
                                 [{equal 'diff:modified-diff-region} #'type-of]
                                 (diff::compute-raw-seq-diff
                                  (lines *huf*)
                                  (lines variant)))))))))

(deftest insert-can-recontextualize ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons :insert
              (list (cons :stmt1 (stmt-with-text variant "bc=0"))
                    (cons :stmt2 (stmt-with-text variant "n > 0")))))
      (multiple-value-bind (result exit)
          (phenome variant)
        (declare (ignorable result))
        (is (= 0 exit))))))

(deftest insert-makes-expected-change ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons :insert
              (list (cons :stmt1 (stmt-with-text variant "bc=0"))
                    (cons :stmt2 (stmt-with-text variant "n > 0")))))
      ;; Original and modified strings of the difference.
      (destructuring-bind (original modified)
          (mapcar {apply #'concatenate 'string}
                  (first (mapcar {diff-strings (lines *huf*) (lines variant)}
                                 (remove-if-not
                                  [{equal 'diff:modified-diff-region} #'type-of]
                                  (diff::compute-raw-seq-diff
                                   (lines *huf*)
                                   (lines variant))))))
        (let ((size-o (length original))
              (size-m (length modified))
              (non-whitespace-orig
               (multiple-value-bind (match-p matches)
                   (scan-to-strings "^(\\s*)(\\S.*)" original)
                 (declare (ignorable match-p))
                 (aref matches 1))))
          ;; Modified should be longer.
          (is (> size-m size-o))
          ;; End of modified should be the original.
          (is (string= non-whitespace-orig
                       (subseq modified
                               (- size-m (length non-whitespace-orig))))))))))
