;;;; csurf-asm-ancestry.lisp --- Ancestry tests.
(defpackage :software-evolution-library/test/csurf-asm-ancestry
  (:nicknames :sel/test/csurf-asm-ancestry)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/simple
   :software-evolution-library/software/csurf-asm
   :software-evolution-library/software/ancestral
   :software-evolution-library/software/asm-super-mutant)
  (:export :test-csurf-asm-ancestry))
(in-package :software-evolution-library/test/csurf-asm-ancestry)
(in-readtable :curry-compose-reader-macros)
(defsuite test-csurf-asm-ancestry "CSURF-ASM representation.")

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
