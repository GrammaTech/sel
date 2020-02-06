;;;; condition-synthesis.lisp --- Condition synthesis tests.
(defpackage :software-evolution-library/test/condition-synthesis
  (:nicknames :sel/test/condition-synthesis)
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
   :software-evolution-library/software/clang
   :software-evolution-library/software/new-clang
   :software-evolution-library/components/condition-synthesis
   :software-evolution-library/components/test-suite)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-condition-synthesis))
(in-package :software-evolution-library/test/condition-synthesis)
(in-readtable :curry-compose-reader-macros)
(defsuite test-condition-synthesis "Condition synthesis tests."
  (clang-mutate-available-p))

(defvar *test-suite* nil "Holds condition synthesis test suite object.")

(define-constant +condition-synthesis-dir+
    (append +etc-dir+ (list "condition-synthesis"))
  :test #'equalp
  :documentation "Path to condition synthesis examples.")

(define-constant +cs-tiny-dir+ (append +condition-synthesis-dir+
                                       (list "tiny-test"))
  :test #'equalp
  :documentation "Path to condition synthesis tighten-condition example.")


(defun cs-tiny-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +cs-tiny-dir+))

(defixture cs-tiny-clang
  (:setup (setf *soft*
                (from-file (make-clang)
                           (cs-tiny-dir "tiny-test.c")))
          (setf *test-suite*
                (make-instance 'test-suite
                  :test-cases
                  (iter (for i below 6)
                        (collecting
                         (make-instance 'test-case
                           :program-name (namestring (cs-tiny-dir "fitness.sh"))
                           :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(define-constant +cs-tighten-dir+ (append +condition-synthesis-dir+
                                          (list "test-tighten"))
  :test #'equalp
  :documentation "Path to condition synthesis tighten-condition example.")

(defun cs-tighten-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +cs-tighten-dir+))

(defixture cs-tighten-clang
  (:setup (setf
           *soft*
           (from-file (make-clang)
                      (cs-tighten-dir "test-tighten.c"))
           *test-suite*
           (make-instance 'test-suite
             :test-cases
             (iter (for i below 6)
                   (collecting
                    (make-instance 'test-case
                      :program-name (namestring (cs-tighten-dir "fitness.sh"))
                      :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(define-constant +cs-add-guard-dir+ (append +condition-synthesis-dir+
                                            (list "test-add-guard"))
  :test #'equalp
  :documentation "Path to condition synthesis add guard example.")

(defun cs-add-guard-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +cs-add-guard-dir+))

(defixture cs-add-guard-clang
  (:setup (setf
           *soft*
           (from-file (make-clang)
                      (cs-add-guard-dir "test-add-guard.c"))
           *test-suite*
           (nest
            (make-instance 'test-suite :test-cases)
            (iter (for i below 8))
            (collecting
             (make-instance 'test-case
               :program-name (namestring (cs-add-guard-dir "fitness.sh"))
               :program-args (list :bin (write-to-string i)))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(define-constant +cs-divide-dir+ (append +condition-synthesis-dir+
                                         (list "divide"))
  :test #'equalp
  :documentation "Path to condition synthesis if to while repair example.")

(defun cs-divide-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +cs-divide-dir+))

(defixture cs-divide-clang
  (:setup (setf
           *soft*
           (from-file (make-clang)
                      (cs-divide-dir "divide.c"))
           *test-suite*
           (make-instance 'test-suite
             :test-cases
             (iter (for i below 5)
                   (collecting
                    (make-instance 'test-case
                      :program-name (namestring (cs-divide-dir "fitness.sh"))
                      :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(deftest flip-works ()
  (is (string= (flip "") ""))
  (is (string= (flip "0000") "0001"))
  (is (string= (flip "0001") "001")))

(deftest synthesize-all-conditions ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         (synths (synthesize-conditions substs)))
    (is (= 12 (length synths)))
    (is (member '(:eq "x" "int" "5") synths :test #'equal))
    (is (member '(:neq "x" "int" "5") synths :test #'equal))
    (is (member '(:eq "y" "int" "6") synths :test #'equal))
    (is (member '(:neq "y" "int" "6") synths :test #'equal))
    (is (member '(:eq "x" "int" "10") synths :test #'equal))
    (is (member '(:neq "x" "int" "10") synths :test #'equal))
    (is (member '(:eq "x" "int" "15") synths :test #'equal))
    (is (member '(:neq "x" "int" "15") synths :test #'equal))
    (is (member '(:eq "y" "int" "4") synths :test #'equal))
    (is (member '(:neq "y" "int" "4") synths :test #'equal))
    (is (member '(:eq "z" "int" "2") synths :test #'equal))
    (is (member '(:neq "z" "int" "2") synths :test #'equal))))

(deftest entails-eq-works ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         (eq-cond '(:eq "x" "int" "10")))
    (is (entails (first substs) eq-cond "0"))
    (is (not (entails (first substs) eq-cond "1")))
    (is (not (entails (second substs) eq-cond "0")))
    (is (entails (second substs) eq-cond "1"))
    (is (entails (third substs) eq-cond "0"))
    (is (not (entails (third substs) eq-cond "1")))))

(deftest entails-neq-works ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         (eq-cond '(:neq "x" "int" "10")))
    (is (entails (first substs) eq-cond "1"))
    (is (not (entails (first substs) eq-cond "0")))
    (is (not (entails (second substs) eq-cond "1")))
    (is (entails (second substs) eq-cond "0"))
    (is (entails (third substs) eq-cond "1"))
    (is (not (entails (third substs) eq-cond "0")))))

(deftest find-best-condition-finds-good-conditions ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         ;; shuffle the synthesized conditions so we don't always find the same
         ;; one first
         (synths (shuffle (synthesize-conditions substs))))
    (is (equal (find-best-condition '("1" "0" "1") substs synths)
               '(:neq "x" "int" "10")))
    (is (member (find-best-condition '("1" "1" "0") substs synths)
                (list '( :eq "y"  "int" "6")
                      '( :neq "x" "int" "15")
                      '( :neq "y"  "int" "4")
                      '( :neq "z"  "int" "2"))
                :test #'equal))))

(deftest (tiny-test-repair-works :long-running) ()
  (handler-bind
      ((mutate ; TODO: Instrumentation in synthesize-condition should be fixed.
        (lambda (e)
          (if (find-restart 'keep-partial-asts)
              (invoke-restart 'keep-partial-asts)
              (error e)))))
    (with-fixture cs-tiny-clang
      (let* ((repair-mut (make-instance 'loosen-condition
                           :object *soft*
                           :targets (stmt-with-text *soft* "x > 5")))
             (repaired-prog (synthesize-condition *soft*
                                                  *test-suite*
                                                  repair-mut)))
        (is repaired-prog)
        (is (stmt-with-text repaired-prog "(x > 5) || (x == 5)"))))))

(deftest (test-tighten-repair :long-running) ()
  (handler-bind
      ((mutate ; TODO: Instrumentation in synthesize-condition should be fixed.
        (lambda (e)
          (if (find-restart 'keep-partial-asts)
              (invoke-restart 'keep-partial-asts)
              (error e)))))
    (with-fixture cs-tighten-clang
      (let* ((repair-mut (make-instance
                             'tighten-condition
                           :object *soft*
                           :targets (stmt-with-text *soft* "x >= 5")))
             (repaired-prog (synthesize-condition *soft* *test-suite*
                                                  repair-mut)))
        (is repaired-prog)
        (is (stmt-with-text repaired-prog "(x >= 5) && !(x == 5)"))))))

(deftest (test-add-condition-repair-target-24 :long-running) ()
  (handler-bind
      ((mutate ; TODO: Instrumentation in synthesize-condition should be fixed.
        (lambda (e)
          (if (find-restart 'keep-partial-asts)
              (invoke-restart 'keep-partial-asts)
              (error e)))))
    (with-fixture cs-add-guard-clang
      (let* ((repair-mut
              (make-instance 'add-condition
                :object *soft*
                :targets (stmt-starting-with-text *soft*
                                                  "if (x >= 5)")))
             (repaired-prog (synthesize-condition *soft* *test-suite*
                                                  repair-mut)))
        (is repaired-prog)
        (let ((outer (stmt-starting-with-text repaired-prog "if (!(x == 5))"))
              (inner (stmt-starting-with-text repaired-prog "if (x >= 5)")))
          (is outer)
          (is inner)
          (is (equalp
               inner
               (first
                (get-immediate-children
                 repaired-prog
                 (second (get-immediate-children repaired-prog outer)))))))))))

(deftest (test-add-condition-repair-target-30 :long-running) ()
  (handler-bind
      ((mutate ; TODO: Instrumentation in synthesize-condition should be fixed.
        (lambda (e)
          (if (find-restart 'keep-partial-asts)
              (invoke-restart 'keep-partial-asts)
              (error e)))))
    (with-fixture cs-add-guard-clang
      (let* ((repair-mut (make-instance
                             'add-condition
                           :object *soft*
                           :targets (stmt-starting-with-text
                                     *soft*
                                     "printf(\"x is larger")))
             (repaired-prog (synthesize-condition *soft* *test-suite*
                                                  repair-mut)))
        (is repaired-prog)
        (let ((outer (stmt-starting-with-text repaired-prog "if (!(x == 5))"))
              (inner (stmt-starting-with-text repaired-prog
                                              "printf(\"x is larger")))
          (is outer)
          (is inner)
          (is (equalp
               inner
               (first
                (get-immediate-children
                 repaired-prog
                 (second (get-immediate-children repaired-prog outer)))))))))))

(deftest (test-if-to-while-repair :long-running) ()
  (handler-bind
      ((mutate ; TODO: Instrumentation in synthesize-condition should be fixed.
        (lambda (e)
          (if (find-restart 'keep-partial-asts)
              (invoke-restart 'keep-partial-asts)
              (error e)))))
    (with-fixture cs-divide-clang
      (let* ((repair-mut
              (make-instance 'if-to-while-tighten-condition
                :object *soft*
                :targets (stmt-starting-with-text *soft*
                                                  "if (x >= 2)")))
             (repaired-prog (synthesize-condition *soft* *test-suite*
                                                  repair-mut)))
        (is repaired-prog)
        (let ((stmt (stmt-starting-with-text repaired-prog "while")))
          (is stmt)
          (is (eq :WhileStmt (ast-class stmt)))
          (is (equal "(x >= 2) && !(x == 2)"
                     (nest (peel-bananas)
                           (source-text)
                           (first)
                           (get-immediate-children repaired-prog stmt)))))))))
