;;;; csurf-asm-ancestry.lisp --- Ancestry tests.
(defpackage :software-evolution-library/test/csurf-asm-ancestry
  (:nicknames :sel/test/csurf-asm-ancestry)
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
   :defmethod :defgeneric)
  (:export :csurf-asm-ancestry))
(in-package :software-evolution-library/test/csurf-asm-ancestry)
(in-readtable :curry-compose-reader-macros)
(defsuite csurf-asm-ancestry)

(defvar *soft* nil "Software used in tests.")

(defclass csurf-asm-w/ancestry (csurf-asm ancestral) ())

(defixture csurf-asm-w-ancestry
  (:setup
   (reset-ancestry-id)
   (setf *soft*
         (from-file
          (make-instance 'csurf-asm-w/ancestry
            :redirect-file (asm-test-dir "calc.elf_copy_redirect.asm"))
          (asm-test-dir "calc.s.intel"))
         *test* [#'length #'genome])
   (evaluate *test* *soft*))
  (:teardown
   (reset-ancestry-id)
   (setf *soft* nil *test* nil)))

(deftest apply-mutation-logs-ancestry-on-csurf-asm ()
  (with-fixture csurf-asm-w-ancestry
    (let ((op (make-instance 'simple-cut :targets 4)))
      (apply-mutation *soft* op)
      (evaluate *test* *soft*)

      (is (< 1 (length (ancestors *soft*))))

      (is (= 1 (plist-get :id (first (ancestors *soft*)))))
      (is (not (null (plist-get :fitness (first (ancestors *soft*))))))
      (is (equal (type-of op)
                 (plist-get :mutant (first (ancestors *soft*)))))

      (is (= 0 (plist-get :id (second (ancestors *soft*)))))
      (is (equal 'from-file
                 (plist-get :how (second (ancestors *soft*))))))))
