;;;; population.lisp --- Population tests.
(defpackage :software-evolution-library/test/population
  (:nicknames :sel/test/population)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/ast
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-population))
(in-package :software-evolution-library/test/population)
(in-readtable :curry-compose-reader-macros)
(defsuite test-population "Population tests."
  (clang-mutate-available-p))

(defixture population
  (:setup (setf *population* (loop :for i :from 1 :to 9
                                collect (make-instance 'soft
                                          :genome (loop :for j :from 0 :to i
                                                     :collect j)
                                          :fitness i))
                *fitness-evals* 0
                *mutation-stats* (make-hash-table)
                *crossover-stats* (make-hash-table)))
  (:teardown (setf *population* nil
                   *fitness-evals* 0
                   *mutation-stats* nil
                   *crossover-stats* nil)))

(defmacro every-is (function &rest lists)
  (let ((args-sym (gensym "args")))
    `(mapc (lambda (&rest ,args-sym)
             (is (apply ,function ,args-sym)))
           ,@lists)))

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
        (*fitness-predicate* #'>)
        (*mutation-stats* (make-hash-table)))
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
          (signals mutate (evolve #'test
                                  :max-evals 20
                                  :analyze-mutation-fn #'analyze-mutation)))
        (evolve #'test :max-evals 20 :analyze-mutation-fn #'analyze-mutation)
        (is (equal '(:fake) (hash-table-keys *mutation-stats*)))
        (is (= 21 (length (gethash :fake *mutation-stats*))))
        (let ((statuses (mapcar #'car (gethash :fake *mutation-stats*))))
          (is (member :better statuses))
          (is (member :worse statuses)))))))

(deftest terminate-evolution-on-success ()
  (let ((counter 0))
    (flet ((test (candidate)
             (declare (ignorable candidate))
             (incf counter)
             (if (= counter 5) 2 1)))
      (with-fixture population
        (let ((*target-fitness-p*
               (lambda (obj)
                 (or (= 2 (fitness obj))
                     (funcall *fitness-predicate* (fitness obj) 2)))))
          (evolve #'test))
        (is (= *fitness-evals* 5))))))

(deftest (swap-can-recontextualize :long-running) ()
  (with-fixture huf-clang
    (let ((mut (make-instance 'clang-swap :targets
                              (list (cons :stmt1 (stmt-with-text *huf* "n > 0"))
                                    (cons :stmt2 (stmt-with-text *huf* "bc=0;"))))))
      (is (with-retries (100)
            ;; NOTE: Without the retries recontectualization can fail
            ;;       stochastically, not all possible rebinding
            ;;       variables work.
            ;;
            ;; TODO: Refine recontextualization with type information
            ;;       so that this *always* works.
            (for var = (apply-mutation (copy *huf*) mut))
            (when (phenome-p var) (return t)))
          "Is able to rebind successfully with 100 tries"))))

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

(deftest (swap-makes-expected-change :long-running) ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*))
          (text-1 "n > 0")
          (text-2 "bc=0;"))
      ;; Apply the swap mutation.
      (apply-mutation variant
                      (cons 'clang-swap
                            (list (cons :stmt1
                                        (stmt-with-text variant text-1))
                                  (cons :stmt2
                                        (stmt-with-text variant text-2)))))
      ;; Each element should contain the text of one of the swapped
      ;; pieces with possibly different variable names.
      (every-is {scan (create-scanner (list :alternation text-1 text-2))}
                (remove-if
                 {string= ""}
                 (mapcar [{apply #'concatenate 'string}
                          {mapcar {apply #'concatenate 'string}}]
                         ;; Collect the differences between the
                         ;; original and the variant.
                         (mapcar {diff-strings (lines *huf*) (lines variant)}
                                 (remove-if-not
                                  [{equal 'diff:modified-diff-region} #'type-of]
                                  (diff::compute-raw-seq-diff
                                   (lines *huf*)
                                   (lines variant))))))))))

(deftest (swap-at-different-levels-can-recontextualize :long-running) ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*))
          (text-1 "n > 0")
          (text-2 "bn++;"))
      ;; Apply the swap mutation.
      (apply-mutation variant
                      (cons 'clang-swap
                            (list (cons :stmt1
                                        (stmt-with-text variant text-1))
                                  (cons :stmt2
                                        (stmt-with-text variant text-2)))))
      (let ((string-diffs
             (remove-if
              {string= ""}
              (mapcar [{apply #'concatenate 'string}
                       {mapcar {apply #'concatenate 'string}}]
                      ;; Collect the differences between the
                      ;; original and the variant.
                      (mapcar {diff-strings (lines *huf*) (lines variant)}
                              (remove-if-not
                               [{equal 'diff:modified-diff-region} #'type-of]
                               (diff::compute-raw-seq-diff
                                (lines *huf*)
                                (lines variant))))))))
        ;; Each element should contain the text of one of the swapped
        ;; pieces with possibly different variable names.
        (every-is {scan (create-scanner (list :alternation text-1 text-2))}
                  string-diffs)
        ;; Variables should not fail to be rebound due to no bound vars in
        ;; scope
        (every-is [{not} {scan "\/\* no bound vars in scope \/\*"}]
                  string-diffs)))))

(deftest (insert-can-recontextualize :long-running) ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
                      (cons 'clang-insert
                            (list (cons :stmt1
                                        (stmt-with-text variant "bc=0;"))
                                  (cons :stmt2
                                        (stmt-with-text variant "n > 0")))))
      (is (phenome-p variant)))))

(deftest (insert-makes-expected-change :long-running) ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
                      (cons 'clang-insert
                            (list (cons :stmt1 (stmt-with-text variant "bc=0;"))
                                  (cons :stmt2 (stmt-with-text variant "n > 0")))))
      ;; Original and modified strings of the difference.
      (destructuring-bind (original modified)
          (mapcar {apply #'concatenate 'string}
                  (first (mapcar {diff-strings (lines *huf*) (lines variant)}
                                 (remove-if-not
                                  [{equal 'diff:modified-diff-region}
                                   #'type-of]
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

;; When recontextualizing, function should be considered defined even
;; if its body is not present.
(deftest bodyless-function-is-not-recontextualized ()
  (let* ((obj (make-clang
               :genome "void test(int x);
                          int main(int argc, char **argv) {
                            test(0); return 0;
                           }"))
         (stmt (stmt-with-text obj "test(0);"))
         (*matching-free-function-retains-name-bias* 1.0))
    (apply-mutation obj
                    `(clang-replace (:stmt1 . ,stmt) (:stmt2 . ,stmt)))
    (is (string= (source-text stmt)
                 (source-text (stmt-with-text obj "test(0);"))))))

;; huf.c only contains one user function with 3 parameters,
;; check that random-function-name can find it.
(deftest (finds-function-binding :long-running) ()
  (with-fixture huf-clang
    (is (string= "inttobits"
                 (random-function-name (functions *huf*)
                                       :original-name "foo"
                                       :arity 3)))))
