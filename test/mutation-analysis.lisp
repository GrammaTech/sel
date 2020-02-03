;;;; mutation-analysis.lisp --- Mutation analysis and statistics collection tests.
(defpackage :software-evolution-library/test/mutation-analysis
  (:nicknames :sel/test/mutation-analysis)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
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
  (:export :mutation-analysis))
(in-package :software-evolution-library/test/mutation-analysis)
(in-readtable :curry-compose-reader-macros)
(defsuite mutation-analysis)

(defvar *project* nil "Software used in project fixtures.")
(defvar *test* nil "Variable to hold evaluation function for tests.")

(defixture hello-world-clang-w-fitness
  (:setup
   (setf *hello-world*
         (from-file (make-clang :compiler "clang"
                                :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))
         *test* [#'length #'genome]
         *fitness-predicate* #'>
         *mutation-stats* (make-hash-table)
         *population* (list *hello-world*)))
  (:teardown
   (setf *hello-world* nil *test* nil *mutation-stats* (make-hash-table))))

(deftest mutation-stats-notices-fitness-improvement ()
  (with-fixture hello-world-clang-w-fitness
    (evaluate *test* *hello-world*)
    (is (numberp (fitness *hello-world*)))
    (let* ((variant (copy *hello-world*))
           (op (make-instance 'clang-insert
                 :targets `((:stmt1 . ,(stmt-starting-with-text variant
                                                                "printf"))
                            (:literal1 . ,(make-literal 0))))))
      (apply-mutation variant op)
      (is (null (fitness variant))
          "Fitness is null after `apply-mutation'")
      (analyze-mutation variant (list op nil nil *hello-world* nil nil) *test*)
      (is (not (null (fitness variant)))
          "`analyze-mutation' calculates fitness when missing")
      (let ((stats-alist (hash-table-alist *mutation-stats*)))
        (is (= (length stats-alist) 1) "Single element in stats")
        (is (equal :better (first (second (first stats-alist))))
            "`analyze-mutation' notices fitness improvement")))))

(deftest (mutation-project-recorded-correctly :long-running) ()
  (with-fixture grep-project
    (let* ((variant (copy *project*))
           (op (cons "grep.c"
                     (make-instance 'clang-cut
                       :object variant
                       :targets `((:stmt1 . ,(stmt-starting-with-text variant
                                                                      "status =")))))))
      (apply-mutation variant op)
      (setf (fitness variant) 0) ; arbitrary, just needs to exist
      (let ((packed-op (append (list (object (cdr op))
                                     (type-of (cdr op))
                                     (targets (cdr op))))))
        (analyze-mutation variant (list packed-op nil nil *project* nil nil) *test*)
        (let ((stats-alist (car (hash-table-alist *mutation-stats*))))
          (is (= (length stats-alist) 2) "Single key/val in stats")
          (is (eq (caar stats-alist) 'clang-cut) "key is CLANG-CUT to match mutation")
          (is (eq (caadr stats-alist) :dead) "val starts with 'dead' for given mutation"))))))

(deftest mutation-stats-notices-worsening ()
  (with-fixture hello-world-clang-w-fitness
    (evaluate *test* *hello-world*)
    (is (numberp (fitness *hello-world*)))
    (let* ((variant (copy *hello-world*))
           (op (make-instance 'clang-cut
                 :targets `((:stmt1 . ,(stmt-starting-with-text variant
                                                                "printf"))))))
      (apply-mutation variant op)
      (analyze-mutation variant (list op nil nil *hello-world* nil nil) *test*)
      (is (equal :worse (first (second (first (hash-table-alist
                                               *mutation-stats*)))))
          "`analyze-mutation' notices worse improvement"))))

(deftest mutation-stats-notices-same ()
  (with-fixture hello-world-clang-w-fitness
    (evaluate *test* *hello-world*)
    (is (numberp (fitness *hello-world*)))
    (let* ((variant (copy *hello-world*))
           (target (stmt-starting-with-text variant "printf"))
           (op (make-instance 'clang-swap
                 :targets `((:stmt1 . ,target) (:stmt2 . ,target)))))
      (setf (fitness variant) nil)
      (analyze-mutation variant (list op nil nil *hello-world* nil nil) *test*)
      (is (equal :same (first (second (first (hash-table-alist
                                              *mutation-stats*)))))
          "`analyze-mutation' notices no change: ~S"
          (hash-table-alist *mutation-stats*)))))

(locally (declare
          #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note style-warning))
  ;; The above `sb-ext:muffle-conditions' helps us ignore a "Cannot
  ;; find a type specializer for cut-and-swap" style warning.  Longer
  ;; term it would be better to update the `compose-mutations' macro
  ;; so that this warning is not triggered.
  (deftest able-to-compose-simple-mutations ()
    (compose-mutations cut-and-swap (clang-cut clang-swap))
    (finalize-inheritance (find-class 'cut-and-swap))
    (is (find-class 'cut-and-swap)
        "`compose-mutations' successfully defines a class")
    (is (some [{eql 'targeter} #'slot-definition-name]
              (class-slots (find-class 'cut-and-swap)))
        "`compose-mutations' defines a class with a targeter")
    (is (some [{eql 'picker} #'slot-definition-name]
              (class-slots (find-class 'cut-and-swap)))
        "`compose-mutations' defines a class with a picker"))

  (deftest able-to-apply-composed-mutation ()
    (compose-mutations swap-and-cut (clang-swap clang-cut))
    (with-fixture hello-world-clang-w-fitness
      (let (variant op)
        ;; Multiple tries to get around stochastic failures.
        ;; The mutation may make random choices which fail the test.
        (is (with-retries (100)
              (setf variant (copy *hello-world*))
              (setf op (make-instance 'swap-and-cut :object variant))
              (apply-mutation variant op)
              (when (and (different-asts (asts variant)
                                         (asts *hello-world*))
                         (not (equal (genome variant)
                                     (genome *hello-world*)))
                         (< (size variant)
                            (size *hello-world*)))
                (return t))))))))
