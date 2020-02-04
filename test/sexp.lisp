;;;; sexp.lisp --- Sexp representation.
(defpackage :software-evolution-library/test/sexp
  (:nicknames :sel/test/sexp)
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
  (:export :sexp))
(in-package :software-evolution-library/test/sexp)
(in-readtable :curry-compose-reader-macros)
(defsuite sexp)

(deftest sexp-cut-first ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-cut :targets 0))
    (is (equal (genome *clang-expr*) '(1 (:* 2 (:- 3 :y)))))))

(deftest sexp-cut-leaf ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-cut :targets 1))
    (is (equal (genome *clang-expr*) '(:+ (:* 2 (:- 3 :y)))))))

(deftest sexp-cut-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-cut :targets 2))
    (is (equal (genome *clang-expr*) '(:+ 1)))))

#+(or ) ; TODO: Fix this (unused) function before turning on this test.
(deftest sexp-cut-function ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-cut :targets 3))
    (is (equal (genome *clang-expr*) '(:+ 1 (2 (:- 3 :y)))))))

(deftest sexp-swap-leaves ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-swap :targets '(1 4)))
    (is (equal (genome *clang-expr*) '(:+ 2 (:* 1 (:- 3 :y)))))))

(deftest sexp-swap-leaf-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-swap :targets '(1 5)))
    (is (equal (genome *clang-expr*) '(:+ (:- 3 :y) (:* 2 1))))))

(deftest sexp-swap-functions ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-swap :targets '(3 6)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 2 (:* 3 :y)))))))

;; FIXME: what is the correct behavior here?
(deftest sexp-swap-parent-child ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-swap :targets '(2 5)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 3 :y))))))

(deftest sexp-replace-leaves ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-replace :targets '(1 4)))
    (is (equal (genome *clang-expr*) '(:+ 2 (:* 2 (:- 3 :y)))))))

(deftest sexp-replace-leaf-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-replace :targets '(1 5)))
    (is (equal (genome *clang-expr*) '(:+ (:- 3 :y) (:* 2 (:- 3 :y)))))))

(deftest sexp-replace-parent-child ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-replace :targets '(2 5)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 3 :y))))))

(deftest sexp-replace-function ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'sexp-replace :targets '(3 6)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 2 (:- 3 :y)))))))
