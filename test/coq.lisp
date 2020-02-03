;;;; coq.lisp --- Coq software object tests.
(defpackage :software-evolution-library/test/coq
  (:nicknames :sel/test/coq)
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
   :defmethod :defgeneric))
(in-package :software-evolution-library/test/coq)
(in-readtable :curry-compose-reader-macros)


(in-readtable :serapi-readtable)

(defixture ls-test
  (:setup (setf *coq* (make-instance
                          'coq
                        :genome (copy-tree '(a (b ((c d) a))
                                             (b (() (c d e) ())))))))
  (:teardown
   (setf (genome *coq*) nil)
   (setf *coq* nil)))

(defixture total-maps
  (:setup (sleep 0.1)
          (setf *serapi-process* (make-serapi))
          (handler-bind ((serapi-timeout-error
                          (lambda (c)
                            (declare (ignorable c))
                            (invoke-restart 'use-empty-response))))
            (sel/cp/serapi-io::read-with-timeout *serapi-process* 2 0.1)
            (sel/cp/serapi-io::read-with-timeout *serapi-process* 2 0.1))
          (setf *coq* (from-file (make-instance 'coq)
                                 (coq-test-dir "TotalMaps.v"))))
  (:teardown
   (setf *coq* nil)
   (kill-serapi *serapi-process*)
   (setf *serapi-process* nil)))

(defixture math
  (:setup (sleep 0.1)
          (setf *serapi-process* (make-serapi))
          (handler-bind ((serapi-timeout-error
                          (lambda (c)
                            (declare (ignorable c))
                            (invoke-restart 'use-empty-response))))
            (sel/cp/serapi-io::read-with-timeout *serapi-process* 2 0.1)
            (sel/cp/serapi-io::read-with-timeout *serapi-process* 2 0.1))
          (setf *coq* (from-file (make-instance 'coq)
                                 (coq-test-dir "NatBinop.v"))))
  (:teardown
   (setf *coq* nil)
   (kill-serapi *serapi-process*)
   (setf *serapi-process* nil)))

(deftest (coq-from-file-sets-fields :long-running) ()
  (with-fixture math
    (is *coq*)
    (is (typep *coq* 'coq))
    (is (= 3 (length (genome *coq*)) (length (coq-definitions *coq*))))
    (is (not (imports *coq*)))
    (is (not (coq-modules *coq*)))
    (is (not (coq-sections *coq*))))
  (with-fixture total-maps
    (is *coq*)
    (is (typep *coq* 'coq))
    (is (= 171 (length (genome *coq*))))
    (is (= 4 (length (imports *coq*))))
    (is (not (coq-modules *coq*)))
    (is (not (coq-sections *coq*)))
    (is (= 12 (length (coq-definitions *coq*))))))

(deftest (coq-can-lookup-pretty-printed-repr-2 :long-running) ()
  (with-fixture math
    (let ((resp1 (run-coq-vernacular "Print binop."))
          (resp2 (first (lookup-coq-pp "binop"))))
      ;; one of the messages in resp1 has the pretty-printed repr. we would
      ;; get by using lookup-coq-pp
      (let ((msgs (coq-message-contents resp1)))
        (is (= 1 (length msgs)))
        (is (equal resp2 (first msgs)))))))


(deftest coq-find-nearest-type-works ()
  (let* ((ls (copy-tree'(a (b ((c d) e)) f (() (g) ()))))
         (coq (make-instance 'coq :genome ls))
         (types (iter (for i below (tree-size ls))
                      (collecting (find-nearest-type coq i)))))
    (is (equal types '(a b b c c c d e f g g g g f)))))

(deftest can-pick-coq-subtree-matching-type ()
  (let* ((ls '(a (b ((c d) e)) f (() (g) ())))
         (coq (make-instance 'coq :genome ls)))
    (is (not (pick-subtree-matching-type coq 'a 0)))
    (is (= 2 (pick-subtree-matching-type coq 'b 1)))
    (is (= 1 (pick-subtree-matching-type coq 'b 2)))
    (is (member (pick-subtree-matching-type coq 'c 3) '(4 5)))
    (is (= 13 (pick-subtree-matching-type coq 'f 8)))
    (is (= 8 (pick-subtree-matching-type coq 'f 13)))))

(deftest coq-pick-typesafe-bad-good-respects-types ()
  (let* ((ls '(a (b ((c d) e)) f (() (g) ())))
         (coq (make-instance 'coq :genome ls)))
    (iter (for i below 25)
          (handler-case
              (let ((pair (pick-typesafe-bad-good coq)))
                (is (= 2 (length pair)))
                (is (equal (find-nearest-type coq (first pair))
                           (find-nearest-type coq (second pair)))))
            (no-mutation-targets () nil)))))


(deftest coq-apply-type-safe-swap-mutation-test-1 ()
  (with-fixture ls-test
    ;; genome: '(a (b ((c d) a)) (b (() (c d e) ())))
    ;; swap (c d) and (c d e)
    (apply-mutation *coq* (make-instance 'type-safe-swap
                            :object *coq*
                            :targets (list 13 5)))
    (is (equal '(a (b ((c d e) a)) (b (() (c d) ())))
               (genome *coq*)))))

(deftest coq-apply-type-safe-swap-mutation-test-2 ()
  (with-fixture ls-test
    ;; swap (b ((c d) a)) with (b (() (c d e) ()))
    (apply-mutation *coq* (make-instance 'type-safe-swap
                            :object *coq*
                            :targets (list 9 2)))
    (is (equal '(a (b (() (c d e) ())) (b ((c d) a)))
               (genome *coq*)))))

(deftest coq-apply-type-safe-swap-mutation-test-3 ()
  (with-fixture ls-test
    ;; strange but permissible
    ;; swap ((b ((c d) a)) (b (() (c d e) ()))) with (b (() (c d e) ()))
    (apply-mutation *coq* (make-instance 'type-safe-swap
                            :object *coq*
                            :targets (list 9 1)))
    (is (equal '(a (b (() (c d e) ())) ((b ((c d) a)) (b (() (c d e) ()))))
               (genome *coq*)))))

(deftest coq-apply-type-safe-swap-mutation-test-4 ()
  (with-fixture ls-test
    ;; verify no issues at edge
    (apply-mutation *coq* (make-instance 'type-safe-swap
                            :object *coq*
                            :targets (list 7 0)))
    (is (equal '(a) (genome *coq*)))))

(deftest (can-synthesize-coq-expressions :long-running) ()
  (with-fixture serapi
    (let* ((types (list "false : bool"
                        "true : bool"
                        "negb : bool -> bool"
                        "orb : bool -> bool -> bool"))
           (tokenized-types (mapcar #'tokenize-coq-type types))
           (env (mapcar (lambda (ty)
                          (append (take 1 ty)
                                  (list (make-coq-var-reference (first ty)))
                                  (cdr ty)))
                        tokenized-types))
           (result1-asts (synthesize-typed-coq-expression "bool" env 2))
           (result1-strs (mapcar {lookup-coq-string _ :input-format #!'CoqExpr }
                                 result1-asts))
           (result2-asts (synthesize-typed-coq-expression "bool -> bool" env 2))
           (result2-strs (mapcar {lookup-coq-string _ :input-format #!'CoqExpr }
                                 result2-asts)))
      ;; Verify number of results
      (is (<= 14 (length result1-asts)))
      (is (<=  5 (length result2-asts)))
      ;; Spot-check result1
      (iter (for expected in '("false" "true" "negb false" "negb true"
                               "(orb true) (negb false)" "(orb true) true"))
            (is (member expected result1-strs :test #'equal)))
      ;; Complete check result2
      (iter (for expected in '("negb" "orb false" "orb true"
                               "orb (negb false)" "orb (negb true)"))
            (is (member expected result2-strs :test #'equal))))))
