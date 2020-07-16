;;;; asm-super-mutant.lisp --- ASM-SUPER-MUTANT representation.
(defpackage :software-evolution-library/test/asm-super-mutant
  (:nicknames :sel/test/asm-super-mutant)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/simple
   :software-evolution-library/software/asm-heap
   :software-evolution-library/software/super-mutant
   :software-evolution-library/software/asm-super-mutant
   :software-evolution-library/components/lexicase)
  (:export :test-asm-super-mutant))
(in-package :software-evolution-library/test/asm-super-mutant)
(in-readtable :curry-compose-reader-macros)
(defsuite test-asm-super-mutant "ASM-SUPER-MUTANT representation." *lib-papi*)

(define-constant +software-dir+
    (append (butlast (butlast +etc-dir+)) (list "software"))
  :test #'equalp
  :documentation "Path to sel/software directory (software source components)")

(defun software-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +software-dir+))

(defun asm-super-mutant-dir (filename directory-name)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory (append +asm-test-dir+ (list directory-name))))

(defixture asm-super-dead-stack-test
  (:setup
   (setf *soft* (from-file (make-instance 'asm-super-mutant)
                           (asm-test-dir "re_set_syntax.s.att"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*)
         (asm-test-dir "re_set_syntax.io"))
   (target-function-name *soft* "re_set_syntax"))

  (:teardown (setf *soft* nil)))

(defixture odd-even-asm-super-intel
  (:setup
   (setf *soft* (from-file (make-instance 'asm-super-mutant)
                           (asm-test-dir "is-even.s.intel"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*) (asm-test-dir "is_even.io"))
   (target-function-name *soft* "is_even"))

  (:teardown (setf *soft* nil)))

(defixture odd-even-asm-super-att
  (:setup
   (setf *soft* (from-file (make-instance 'asm-super-mutant)
                           (asm-test-dir "is-even.s.att"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*) (asm-test-dir "is_even.io"))
   (target-function-name *soft* "is_even"))

  (:teardown (setf *soft* nil)))

(defixture odd-even-asm-super-reg-test-intel
  (:setup
   (setf *soft* (from-file (make-instance 'asm-super-mutant)
                           (asm-test-dir "is-even.s.intel"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*)
         (asm-test-dir "is_even-reg-test.io"))
   (target-function-name *soft* "is_even"))

  (:teardown (setf *soft* nil)))

(defixture odd-even-asm-super-reg-test-att
  (:setup
   (setf *soft* (from-file (make-instance 'asm-super-mutant)
                           (asm-test-dir "is-even.s.att"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*)
         (asm-test-dir "is_even-reg-test.io"))
   (target-function-name *soft* "is_even"))

  (:teardown (setf *soft* nil)))

(defixture asm-super-rip-test-att
  (:setup
   (setf *soft* (from-file (make-instance 'asm-super-mutant)
                           (asm-test-dir "re_set_syntax.s.att"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*)
         (asm-test-dir "re_set_syntax.io"))
   (target-function-name *soft* "re_set_syntax"))

  (:teardown (setf *soft* nil)))

(defixture static-symbol-test-att
  (:setup
   (setf *soft*
         (from-file
          (make-instance 'asm-super-mutant
                         :data-path
                         (asm-super-mutant-dir "data.s"
                                               "static_symbol_test_att"))
          (asm-super-mutant-dir "target.s" "static_symbol_test_att"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*)
         (asm-super-mutant-dir "testcases" "static_symbol_test_att"))
   (target-function-name *soft* "debloat__deregister_tm_clones"))
  (:teardown (setf *soft* nil)))

(defixture rip-relative-symbol-test-att
  (:setup
   (setf *soft*
         (from-file
          (make-instance 'asm-super-mutant
                         :data-path
                         (asm-super-mutant-dir "data.s"
                                               "rip_relative_symbol_test_att"))
          (asm-super-mutant-dir "target.s" "rip_relative_symbol_test_att"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*)
         (asm-super-mutant-dir "testcases" "rip_relative_symbol_test_att"))
   (target-function-name *soft* "debloat____do_global_dtors_aux"))
  (:teardown (setf *soft* nil)))

(defixture asm-super-inline-test-att
  (:setup
   (setf *soft* (from-file (make-instance 'asm-super-mutant)
                           (asm-test-dir "inline-test-target.s.att"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*)
         (asm-test-dir "inline-test-testcases.io"))
   (target-function-name *soft* "debloat__insert_op1")
   (setf (sel/sw/asm-super-mutant::include-lines *soft*)
         (let ((asm (from-file (make-instance 'sel/sw/asm-heap:asm-heap)
                               (asm-test-dir "inline-test-includes.s.att"))))
           (lines asm))))
  (:teardown (setf *soft* nil)))

(defun asm-super-mutant-finds-improved-version ()
  ;; Add target function, and all possible single-cut variants.
  (setf (mutants *soft*)
        (cons (create-target *soft*)
              (create-all-simple-cut-variants *soft*)))
  (evaluate nil *soft*)
  (let ((best
         (lexicase-select-best
          (mutants *soft*)
          :predicate (lambda (x y) (< x y))))) ; Lower number is better.
    ;; Is the first test result of the first best "better" (lower
    ;; number) than the first test result of the original version?
    (is (< (elt (fitness (first best)) 0)
           (elt (fitness (elt (mutants *soft*) 0)) 0)))))

(deftest asm-super-mutant-finds-improved-version-intel ()
  (with-fixture odd-even-asm-super-intel
    (asm-super-mutant-finds-improved-version)))

(deftest asm-super-mutant-finds-improved-version-att ()
  (with-fixture odd-even-asm-super-att
    (asm-super-mutant-finds-improved-version)))

(deftest asm-super-mutant-finds-improved-version-reg-test-intel ()
  (with-fixture odd-even-asm-super-reg-test-intel
    (asm-super-mutant-finds-improved-version)))

(deftest asm-super-mutant-finds-improved-version-reg-test-att ()
  (with-fixture odd-even-asm-super-reg-test-att
    (asm-super-mutant-finds-improved-version)))

(deftest asm-super-mutant-inline-test-att ()
  (with-fixture asm-super-inline-test-att
    (asm-super-mutant-finds-improved-version)))

(deftest asm-super-converts-rip-to-absolute-addresses ()
  "Ensure rip-relative conversion works.
 Also make sure the rip-relative addresses get restored
 properly afterward."
  (with-fixture asm-super-rip-test-att
    (let ((target (create-target *soft*)))
      ;; ensure any rip-relative addresses are converted to absolute
      (dotimes (i (length (genome target)))
        (sel/sw/asm-super-mutant::convert-rip-relative-to-absolute target
                                                                   i *soft*))
      (let ((info1 (elt (genome target) 3))
            (info2 (elt (genome target) 6)))
        (is (not (search "%rip" (asm-line-info-text info1) :test 'equalp)))
        (is (search "0x61A460" (asm-line-info-text info1) :test 'equalp))
        (is (not (search "%rip" (asm-line-info-text info2) :test 'equalp)))
        (is (search "0x61A460" (asm-line-info-text info2) :test 'equalp)))
      ;; restore rip-relative addresses
      (restore-original-addresses target)
      (let ((info1 (elt (genome target) 3))
            (info2 (elt (genome target) 6)))
        (is (search "%rip" (asm-line-info-text info1) :test 'equalp))
        (is (not (search "0x61A460" (asm-line-info-text info1) :test 'equalp)))
        (is (search "%rip" (asm-line-info-text info2) :test 'equalp))
        (is (not (search "0x61A460" (asm-line-info-text info2)
                         :test 'equalp)))))))

(deftest asm-super-static-symbol-test ()
  "Ensure static symbol to absolute conversion works.
 Also make sure the addresses get restored
 properly afterward."
  (with-fixture static-symbol-test-att
    (let ((target (create-target *soft*)))
      ;; ensure any symbolic addresses are converted to absolute
      (dotimes (i (length (genome target)))
        (sel/sw/asm-super-mutant::convert-symbolic-address-to-absolute
         target i *soft*))
      (let ((info1 (elt (genome target) 2))
            (info2 (elt (genome target) 12)))
        (is (not (search "__TMC_END__" (asm-line-info-text info1)
                         :test 'equalp)))
        (is (search "0x601038" (asm-line-info-text info1) :test 'equalp))
        (is (not (search "__TMC_END__" (asm-line-info-text info2)
                         :test 'equalp)))
        (is (search "0x601038" (asm-line-info-text info2) :test 'equalp)))
      ;; restore rip-relative addresses
      (restore-original-addresses target)
      (let ((info1 (elt (genome target) 2))
            (info2 (elt (genome target) 12)))
        (is (search "__TMC_END__" (asm-line-info-text info1) :test 'equalp))
        (is (not (search "0x601038" (asm-line-info-text info1) :test 'equalp)))
        (is (search "__TMC_END__" (asm-line-info-text info2) :test 'equalp))
        (is (not (search "0x601038" (asm-line-info-text info2)
                         :test 'equalp)))))))

(deftest asm-super-rip-relative-symbol-test ()
  "Ensure static symbol to absolute conversion works.
 Also make sure the addresses get restored
 properly afterward."
  (with-fixture rip-relative-symbol-test-att
    (let ((target (create-target *soft*)))
      ;; ensure any symbolic addresses are converted to absolute
      (dotimes (i (length (genome target)))
        (sel/sw/asm-super-mutant::convert-rip-relative-to-absolute
         target i *soft*))
      (let ((info1 (elt (genome target) 0))
            (info2 (elt (genome target) 8)))
        (is (not (search "__TMC_END__(%rip%)" (asm-line-info-text info1)
                         :test 'equalp)))
        (is (search "0x601038" (asm-line-info-text info1) :test 'equalp))
        (is (not (search "__TMC_END__(%rip)" (asm-line-info-text info2)
                         :test 'equalp)))
        (is (search "0x601038" (asm-line-info-text info2) :test 'equalp)))
      ;; restore rip-relative addresses
      (restore-original-addresses target)
      (let ((info1 (elt (genome target) 0))
            (info2 (elt (genome target) 8)))
        (is (search "__TMC_END__" (asm-line-info-text info1) :test 'equalp))
        (is (not (search "0x601038" (asm-line-info-text info1) :test 'equalp)))
        (is (search "__TMC_END__" (asm-line-info-text info2) :test 'equalp))
        (is (not (search "0x601038" (asm-line-info-text info2)
                         :test 'equalp)))))))

(deftest asm-super-filters-out-dead-stack-specs ()
  "Filter out i/o specs we don't want.
 Ensure i/o mem specs in unused stack area (below rsp) do not
 get applied in test."
  (with-fixture asm-super-dead-stack-test
    (let ((input-spec (sel/sw/asm-super-mutant::input-spec *soft*))
          (output-spec (sel/sw/asm-super-mutant::output-spec *soft*)))
      (is (=
           (length
            (sel/sw/asm-super-mutant::input-specification-mem
             (elt input-spec 0)))
           4))
      (is (=
           (length
            (sel/sw/asm-super-mutant::input-specification-mem
             (elt output-spec 0)))
           4))
      (sel/sw/asm-super-mutant::remove-mem-below-sp
       (sel/sw/asm-super-mutant::input-spec *soft*))
      (sel/sw/asm-super-mutant::remove-mem-below-sp
       (sel/sw/asm-super-mutant::output-spec *soft*))
      (is (=
           (length
            (sel/sw/asm-super-mutant::input-specification-mem
             (elt input-spec 0)))
           1))
      (is (=
           (length
            (sel/sw/asm-super-mutant::input-specification-mem
             (elt input-spec 0)))
           1)))))
