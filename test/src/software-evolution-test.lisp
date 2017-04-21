;;; tests.lisp --- tests for the `software-evolution' package

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :software-evolution-test)
(enable-curry-compose-reader-macros :include-utf8)

;; Disable clang-format and any other helpers
(defmacro every-is (function &rest lists)
  (let ((args-sym (gensym "args")))
    `(mapc (lambda (&rest ,args-sym)
             (is (apply ,function ,args-sym)))
           ,@lists)))

(defun run-testbot (&rest a)
  (declare (ignorable a))
  (testbot-test #'test "SEL" +software-evolution-branch+))
(defun run-batch (&rest a) (declare (ignorable a)) (batch-test #'test))

(defsuite test)
(in-suite test)

(defvar *genome*      nil "Genome used in tests.")
(defvar *soft*        nil "Software used in tests.")
(defvar *tfos*        nil "Another software used in tests.")
(defvar *gcd*         nil "Holds the gcd software object.")
(defvar *binary-search* nil "Holds the binary_search software object.")
(defvar *empty-while* nil "Holds the empty-while software object.")
(defvar *headers*     nil "Holds the headers software object.")
(defvar *hello-world* nil "Holds the hello world software object.")
(defvar *huf*         nil "Holds the huf software object.")
(defvar *nested*      nil "Holds the nested software object.")
(defvar *scopes*      nil "Holds the scopes software object.")
(defvar *range-ref* #("one" "two" "three" "four" "five" "six")
  "Example range software object.")
(defvar *collatz*     nil "Holds the collatz software object.")
(defvar *fib*         nil "Holds the fibonacci software object.")
(defvar *variety*     nil "Holds the variety software object.")
(defvar *contexts*    nil "Holds the syntactic-contexts software object.")

(define-constant +etc-dir+
    (append (butlast (pathname-directory
                      #.(or *compile-file-truename*
                            *load-truename*
                            *default-pathname-defaults*)))
            (list "etc"))
  :test #'equalp
  :documentation "Path to directory holding testing artifacts.")

(define-constant +gcd-dir+ (append +etc-dir+ (list "gcd"))
  :test #'equalp
  :documentation "Path to directory holding gcd.")

(define-constant +headers-dir+ (append +etc-dir+ (list "headers"))
  :test #'equalp
  :documentation "Path to directory holding headers.")

(define-constant +hello-world-dir+ (append +etc-dir+ (list "hello-world"))
  :test #'equalp
  :documentation "Location of the hello world example directory")

(define-constant +clang-format-dir+ (append +etc-dir+ (list "clang-format"))
  :test #'equalp
  :documentation "Location of the clang-format example directory")

(define-constant +huf-dir+ (append +etc-dir+ (list "huf"))
  :test #'equalp
  :documentation "Location of the huf example directory")

(define-constant +nested-dir+ (append +etc-dir+ (list "nested"))
  :test #'equalp
  :documentation "Location of the nested example directory")

(define-constant +scopes-dir+ (append +etc-dir+ (list "scopes"))
  :test #'equalp
  :documentation "Location of the scopes example directory")

(define-constant +clang-crossover-dir+
    (append +etc-dir+ (list "clang-crossover"))
  :test #'equalp
  :documentation "Location of clang crossover example directory")

(define-constant +lisp-bugs-dir+
    (append +etc-dir+ (list "lisp-bugs"))
  :test #'equalp
  :documentation "Location of the lisp bugs directory")

(define-constant +expand-arithmatic-op-dir+
    (append +etc-dir+ (list "expand-arithmatic-op"))
  :test #'equalp
  :documentation "Location of the expand arithmatic op example dir")

(define-constant +explode-for-loop-dir+
    (append +etc-dir+ (list "explode-for-loop"))
  :test #'equalp
  :documentation "Location of the explode for loop example dir")

(define-constant +coalesce-while-loop-dir+
    (append +etc-dir+ (list "coalesce-while-loop"))
  :test #'equalp
  :documentation "Location of the coalesce while loop example dir")

(define-constant +collatz-dir+
    (append +etc-dir+ (list "collatz"))
  :test #'equalp
  :documentation "Location of the collatz example dir")

(define-constant +fib-dir+
    (append +etc-dir+ (list "fib"))
  :test #'equalp
  :documentation "Location of the fib example dir")

(define-constant +clang-tidy-dir+
    (append +etc-dir+ (list "clang-tidy"))
  :test #'equalp
  :documentation "Location of the clang-tidy example dir")

(define-constant +type-of-var-dir+
    (append +etc-dir+ (list "type-of-var"))
  :test #'equalp
  :documentation "Location of the type-of-var example dir")

(define-constant +variety-dir+
    (append +etc-dir+ (list "variety"))
  :test #'equalp
  :documentation "Location of the variety example dir")

(define-constant +contexts-dir+ (append +etc-dir+ (list "syntactic-contexts"))
  :test #'equalp
  :documentation "Path to the syntactic-contexts example."))

(defun gcd-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +gcd-dir+))

(defun headers-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +headers-dir+))

(defun hello-world-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +hello-world-dir+))

(defun clang-format-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +clang-format-dir+))

(defun huf-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +huf-dir+))

(defun nested-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +nested-dir+))

(defun scopes-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +scopes-dir+))

(defun clang-crossover-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +clang-crossover-dir+))

(defun lisp-bugs-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +lisp-bugs-dir+))

(defun expand-arithmatic-op-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +expand-arithmatic-op-dir+))

(defun explode-for-loop-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +explode-for-loop-dir+))

(defun coalesce-while-loop-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +coalesce-while-loop-dir+))

(defun collatz-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +collatz-dir+))

(defun fib-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +fib-dir+))

(defun clang-tidy-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +clang-tidy-dir+))

(defun type-of-var-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +type-of-var-dir+))

(defun variety-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +variety-dir+))

(defun contexts-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +contexts-dir+))

(define-software soft (software)
  ((genome :initarg :genome :accessor genome :initform nil)))

(defvar *soft-mutate-errors* nil
  "Control when mutations on soft objects throw errors.")

(defmethod crossover ((a soft) (b soft))
  (values (copy a)(list :fake-a) (list :fake-b)))
(defmethod mutate ((a soft))
  (setf (fitness a) nil)
  (if *soft-mutate-errors*
      (error (make-condition 'mutate
               :text "FAKE"
               :obj a
               :op '(:fake)))
      (values (copy a) (list :fake))))

(defixture soft
  (:setup (setf *soft* (make-instance 'soft
                         :genome (coerce (loop :for i :from 0 :to 9 :collect i)
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

(defixture binary-search-clang
  (:setup
   (setf *binary-search*
         (from-file
          (make-instance 'clang
            :flags (list
                    "-I"
                    (namestring (make-pathname :directory +etc-dir+))))
          (make-pathname
           :name "binary_search"
           :type "c"
           :directory +etc-dir+))))
  (:teardown
   (setf *binary-search* nil)))

(defixture gcd-clang
  (:setup
   (setf *gcd*
         (from-file (make-instance 'clang :compiler "clang")
                    (gcd-dir "gcd.c"))))
  (:teardown
   (setf *gcd* nil)))

(defixture gcd-wo-curlies-clang
  (:setup
   (setf *gcd*
         (from-file (make-instance 'clang :compiler "clang")
                    (gcd-dir "gcd-wo-curlies.c"))))
  (:teardown
   (setf *gcd* nil)))

(defixture headers-clang
  (:setup
   (setf *headers*
         (from-file (make-instance 'clang
                      :compiler "clang"
                      :flags (list "-I" (namestring
                                         (make-pathname
                                          :directory +headers-dir+))))
                    (headers-dir "main.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defixture hello-world-clang
  (:setup
    (setf *hello-world*
      (from-file (make-instance 'clang :compiler "clang"
                                       :flags '("-g -m32 -O0"))
                 (hello-world-dir "hello_world.c"))))
  (:teardown
    (setf *hello-world* nil)))

(defvar *good-asts* nil "Control pick-good")
(defvar *bad-asts* nil "Control pick-bad")
(define-software clang-control-picks (clang) ())
(defmethod good-stmts ((obj clang-control-picks))
  (or *good-asts* (stmt-asts obj)))
(defmethod bad-stmts ((obj clang-control-picks))
  (or *bad-asts* (stmt-asts obj)))

(defixture hello-world-clang-control-picks
  (:setup
    (setf *hello-world*
      (from-file (make-instance 'clang-control-picks :compiler "clang-3.7"
                                :flags '("-g -m32 -O0"))
                 (hello-world-dir "hello_world.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defixture empty-function-body-crossover-bug-clang
  (:setup
    (setf *soft*
      (from-file (make-instance 'clang :compiler "clang"
                                       :flags '("-g -m32 -O0"))
                 (clang-crossover-dir
                   "empty-function-body-crossover-bug.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture select-intraprocedural-pair-non-null-clang
  (:setup
    (setf *soft*
      (from-file (make-instance 'clang :compiler "clang"
                                       :flags '("-g -m32 -O0"))
                 (clang-crossover-dir
                   "select-intraprocedural-pair-non-null.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture intraprocedural-2pt-crossover-bug-clang
  (:setup
    (setf *soft*
      (from-file (make-instance 'clang :compiler "clang"
                                       :flags '("-g -m32 -O0"))
                 (clang-crossover-dir
                  "intraprocedural-2pt-crossover-bug.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture no-mutation-targets-clang
  (:setup
   (setf *soft* (from-file (make-instance 'clang)
                (lisp-bugs-dir "no-mutation-targets.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture no-insert-fodder-decl-mutation-targets-clang
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example.json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in)))
   (setf *soft* (from-file (make-instance 'clang-w-fodder)
                (lisp-bugs-dir "no-insert-fodder-decl-mutation-targets.c"))))
  (:teardown
   (setf *database* nil)
   (setf *soft* nil)))

(defixture contexts
  (:setup
   (setf *contexts*
         (from-file (make-instance 'clang :compiler "clang-3.7")
                    (contexts-dir "contexts.c"))))
  (:teardown
   (setf *contexts* nil)))

(defun inject-missing-swap-macro (obj)
  ;; Inject a macro that clang-mutate currently misses, then force the ASTs to
  ;; be recalculated by setting the genome-string.
  (add-macro obj
             "swap_" "swap_(I,J) do { int t_; t_ = a[(I)]; a[(I)] = a[(J)]; a[(J)] = t_; } while (0)")
  (setf (genome-string obj) (genome-string obj)))

(defixture hello-world-clang-w-fodder
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example.json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in)))
   (setf *hello-world*
     (from-file (make-instance 'clang-w-fodder :compiler "clang"
                                               :flags '("-g -m32 -O0"))
                (hello-world-dir "hello_world.c"))))
  (:teardown
    (setf *database* nil)
    (setf *hello-world* nil)))

(defixture gcd-clang-w-fodder
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example.json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in)))
   (setf *gcd*
         (from-file
          (make-instance 'clang-w-fodder
            :flags (list
                    "-I"
                    (namestring (make-pathname :directory +etc-dir+))))
          (gcd-dir "gcd.c"))))
  (:teardown
   (setf *database* nil)
   (setf *gcd* nil)))

(defixture empty-while-clang
  (:setup
   (setf *empty-while*
         (from-file (make-instance 'clang)
                    (coalesce-while-loop-dir "empty-while.c"))))
  (:teardown
   (setf *empty-while* nil)))

(defixture while-with-no-precedent-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang)
                    (coalesce-while-loop-dir "no-precedent.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture huf-clang
  (:setup
    (setf *huf*
      (from-file (make-instance 'clang :compiler "gcc" :flags '("-g -m32 -O0"))
                 (huf-dir "huf.c")))
    (inject-missing-swap-macro *huf*))
  (:teardown
    (setf *huf* nil)))

(defixture nested-clang
  (:setup
   (setf *nested*
         (from-file (make-instance 'clang :compiler "gcc" :flags '("-g -m32 -O0"))
                    (nested-dir "nested.c")))
   (inject-missing-swap-macro *nested*))
  (:teardown
   (setf *nested* nil)))

(defixture scopes-clang
  (:setup
    (setf *scopes*
          (from-file (make-instance 'clang-control-picks
                                    :compiler "clang" :flags '("-g -m32 -O0"))
                 (scopes-dir "scopes.c"))))
  (:teardown
   (setf *scopes* nil)))

(defixture scopes2-clang
  (:setup
    (setf *scopes*
          (from-file (make-instance 'clang-control-picks
                                    :compiler "clang" :flags '("-g -m32 -O0"))
                 (scopes-dir "scopes2.c"))))
  (:teardown
    (setf *scopes* nil)))

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

(defixture collatz-clang
  (:setup
    (setf *collatz*
          (from-file (make-instance 'clang
                       :compiler "clang"
                       :flags '("-m32" "-O0" "-g" "-c"))
                     (collatz-dir "collatz.c"))))
  (:teardown
    (setf *collatz* nil)))

(defixture fib-clang
  (:setup
    (setf *fib*
          (from-file (make-instance 'clang
                       :compiler "clang"
                       :flags '("-m32" "-O0" "-g" "-c"))
                     (fib-dir "fib.c"))))
  (:teardown
    (setf *fib* nil)))

(defixture crossover-no-compound-stmt-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang
                       :compiler "clang"
                       :flags '("-m32" "-O0" "-g"))
                     (clang-crossover-dir
                      "crossover-no-compound-stmt.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture crossover-switch-stmt-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang
                       :compiler "clang"
                       :flags '("-m32" "-O0" "-g"))
                     (clang-crossover-dir
                      "crossover-switch-stmt.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture tidy-adds-braces-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang
                       :compiler "clang"
                       :flags '("-m32" "-O0" "-g"))
                     (clang-tidy-dir "tidy-adds-braces.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture type-of-var-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang
                       :compiler "clang"
                       :flags '("-m32" "-O0" "-g"))
                     (type-of-var-dir "type-of-var.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture type-of-var-missing-decl-type-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang
                      :compiler "clang"
                      :flags '("-m32" "-O0" "-g"))
                    (type-of-var-dir "missing-decl-type.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture variety-clang
  (:setup
    (setf *variety*
          (from-file (make-instance 'clang
                       :compiler "clang"
                       :flags '("-m32" "-O0" "-g"))
          (variety-dir "variety.c"))))
  (:teardown
    (setf *variety* nil)))


;;; ASM representation
(in-suite test)
(defsuite* test-asm)

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
    (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'simple-swap
                                             :targets (list 4 8)))
      (is (not (tree-equal (genome variant) (genome *gcd*))))
      (is (= (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-replace-operand-maintains-length ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'asm-replace-operand
                                             :targets (list 13 18)))
      (is (not (tree-equal (genome variant) (genome *gcd*))))
      (is (= (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-replace-operand-changes-operand ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'asm-replace-operand
                                             :targets (list 13 18)))
      (is (not (equal (aget :code (elt (genome variant) 13))
                      (aget :code (elt (genome *gcd*) 13))))))))

(deftest asm-split-instruction-has-correct-length ()
  (let ((test1 (asm-split-instruction  "	movq	%rsp, %rbp"))
        (test2 (asm-split-instruction "	movl	$0  , -4(%rbp)   ")))
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
        (is (not (tree-equal (genome new) (genome *gcd*))))
        ;; (is (some [{equal :crossover} #'car] (edits new)))
        ;; (is (some [{equal :cut} #'caar] (second (edits new))))
        ))))


;;; ELF representation
#| ;; TODO: Currently failing because we're not populating the .text section.
(in-suite test)
(defsuite* test-elf)

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
(in-suite test)
(defsuite* test-clang)

(deftest simply-able-to-load-a-clang-software-object()
  (with-fixture hello-world-clang
    (is (not (null *hello-world*)))))

(deftest genome-change-clears-clang-software-object-fields ()
  (with-fixture hello-world-clang
    (setf (genome *hello-world*) "")
    (is (null (asts *hello-world*)))
    (is (null (stmt-asts *hello-world*)))
    (is (null (non-stmt-asts *hello-world*)))
    (is (null (functions *hello-world*)))
    (is (null (prototypes *hello-world*)))
    (is (null (includes *hello-world*)))
    (is (null (types *hello-world*)))
    (is (null (macros *hello-world*)))
    (is (null (globals *hello-world*)))
    (is (null (fitness *hello-world*)))
    (is (equalp (make-hash-table :test #'equal)
                (declarations *hello-world*)))))

;; Check our temporary hack to split multi-variable declarations.
(deftest split-multi-variable-declarations ()
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a,b;
}")))))
      "Splits a simple single-line declaration.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a,
         b;
}")))))
      "Splits a multi-line declaration.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a = 4,
         b = 68;
}")))))
      "Splits a multi-line declaration with initialization.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a = 4,b = 68;
}")))))
      "Splits a single-line declaration with initialization.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a=4,b=68;
}")))))
      "Splits another single-line declaration with initialization.")
  (is (= 2 (count-if {scan "double\\*"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double* a=4,b=68;
}")))))
      "Handles pointer type.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double *a=4,b=68;
}")))))
      "Handles first variable is pointer.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a=4,*b=68;
}")))))
      "Handles second variable is pointer.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a=fopen(one, two), b;
}")))))
      "Handles initialization with commas.")
  (is (= 2 (count-if {scan "const double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  const double a,b;
}")))))
      "Retains a modifier declaration.")
  (is (= 2 (count-if {scan "register int"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  register int a,b;
}")))))
      "Retains another modifier declaration.")
  (is (= 2 (count-if {scan "static unsigned int"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  static unsigned int a,b;
}")))))
      "Retains two modifiers in declaration.")
  (is (= 2 (count-if {scan "struct dfamust"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  struct dfamust *dm, *ndm;
}")))))
      "Another with a modifier."))

(deftest splits-global-and-stmt-asts ()
  (with-fixture huf-clang
    (is (find-if [{string= "\"this is an example for huffman encoding\""}
                  #'source-text]
                 (non-stmt-asts *huf*))
        "Ensure known global is in `globals'.")
    (is (find-if [{string= "int i"} #'source-text]
                 (stmt-asts *huf*))
        "Ensure known local variable is in `stmts'.")
    (is (null (find "ParmVar" (stmt-asts *huf*)
                    :key #'ast-class :test #'string=))
        "Ensure no ParmVar statement ASTs")
    (is (null (find "Function" (stmt-asts *huf*)
                    :key #'ast-class :test #'string=))
        "Ensure no Function statement ASTs")))

;; Check if the two AST lists differ. Do a smoke test with
;; the list lengths; if they match, use the src-text
;; field as a proxy for equality. Strict equality isn't
;; useful because of nondeterministic fields like :src-file.
(defun different-asts (this that)
  (or (not (equal (length this) (length that)))
      (not (every (lambda (x y)
                    (string= (source-text x) (source-text y)))
                  this that))))

(deftest can-compile-clang-software-object ()
  (with-fixture hello-world-clang
    (with-temp-file (bin)
      (multiple-value-bind (bin errno stderr stdout src)
          (phenome *hello-world*)
        (declare (ignorable stderr stdout src))
        (is (probe-file bin))
        (is (= 0 errno))))))

(deftest can-timeout-compiling-clang-software-object ()
  (with-fixture hello-world-clang
    ;; NOTE: The very small decimal in the following is required for
    ;;       Clozure CL to timeout successfully.  On SBCL a simple "0"
    ;;       will suffice.
    (let ((se::*compilation-timeout* 0.0000001))
      (with-temp-file (bin)
        (multiple-value-bind (bin errno stderr stdout src)
            (phenome *hello-world*)
          (declare (ignorable bin stderr stdout src))
          (is (= 124 errno)))))))

(deftest can-apply-mutation-w-value1 ()
  (with-fixture hello-world-clang
    (let* ((variant (copy *hello-world*))
           (stmt1 (stmt-with-text variant
                                  "printf(\"Hello, World!\\n\")")))
      (apply-mutation variant
        `(clang-replace
          (:stmt1 . ,stmt1)
          (:value1 . ,(se::make-statement "IntegerLiteral"
                                          :fullstmt
                                          '("0")))))
      (is (different-asts (asts variant) (asts *hello-world*)))
      (is (not (equal (genome variant) (genome *hello-world*)))))))

(deftest cut-shortens-a-clang-software-object()
  (with-fixture hello-world-clang
    (let* ((variant (copy *hello-world*))
           (stmt1 (stmt-with-text variant
                                  "printf(\"Hello, World!\\n\")")))
      (apply-mutation variant `(clang-cut (:stmt1 . ,stmt1)))
      (is (different-asts (asts variant)
                          (asts *hello-world*)))
      (is (not (equal (genome variant)
                      (genome *hello-world*))))
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
        `(clang-insert (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (is (different-asts (asts variant)
                          (asts *hello-world*)))
      (is (not (equal (genome variant)
                      (genome *hello-world*))))
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
        `(clang-swap (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (is (different-asts (asts variant)
                          (asts *hello-world*)))
      (is (not (equal (genome variant)
                      (genome *hello-world*))))
      (is (= (size variant)
             (size *hello-world*))))))

(deftest crossover-clang-software-object-does-not-crash()
  (with-fixture hello-world-clang
    (let* ((variant (crossover (copy *hello-world*) (copy *hello-world*))))
      (is (string/= (genome variant)
                    "")))))

(deftest empty-function-body-crossover-does-not-crash ()
  (with-fixture empty-function-body-crossover-bug-clang
    (let ((crossed (crossover *soft* *soft*)))
      (is (string/= (genome crossed)
                    "")))))

(deftest select-intraprocedural-pair-does-not-return-null ()
  (with-fixture select-intraprocedural-pair-non-null-clang
    (loop :for i :from 0 :to 100
          :do (multiple-value-bind (stmt1 stmt2)
                  (select-intraprocedural-pair *soft*)
                (is (not (null stmt1)))
                (is (not (null stmt2)))))))


;;; Misc. clang tests

(deftest able-to-wrap-statements-in-blocks ()
  (with-fixture gcd-wo-curlies-clang
    (let ((var (copy *gcd*)))
      ;; Setup, ensure everything is what we thing it should be.
      (is (string= "BinaryOperator"     ; Guard
                   (ast-class (stmt-with-text var "a > b"))))
      (is (string= "BinaryOperator"     ; Then
                   (ast-class (stmt-with-text var "a = a - b"))))
      (is (string= "BinaryOperator"     ; Else
                   (ast-class (stmt-with-text var "b = b - a"))))
      ;; Wrap children and ensure changes are made.

      (setf var (wrap-child var (stmt-starting-with-text var "if (a > b)")
                            1))
      (setf var (wrap-child var (stmt-starting-with-text var "if (a > b)")
                            2))
      (is (string= "BinaryOperator"     ; Guard
                   (ast-class (stmt-with-text var "a > b"))))
      (is (string= "CompoundStmt"       ; Then
                   (ast-class (get-parent-ast var
                                              (stmt-with-text var "a = a - b")))))
      (is (string= "CompoundStmt"       ; Then
                   (ast-class (get-parent-ast var
                                              (stmt-with-text var "b = b - a")))))
      ;; Ensure gcd remains unchanged.
      (is (string= "BinaryOperator"     ; Guard
                   (ast-class (stmt-with-text *gcd* "a > b"))))
      (is (string= "BinaryOperator"     ; Then
                   (ast-class (stmt-with-text *gcd* "a = a - b"))))
      (is (string= "BinaryOperator"     ; Else
                   (ast-class (stmt-with-text *gcd* "b = b - a")))))))

(deftest split-multi-var-decls-on-clang ()
  (with-fixture gcd-clang
    (is (not (scan "double a,b,c;" (genome-string *gcd*))))))

(deftest clang-headers-parsed-in-order ()
  (with-fixture headers-clang
    ;; TODO: Include "first.c" before include "third.c".

    ;; TODO: Ensure "MAIN" is present.  Presently MAIN is not present
    ;; because it is not used in the immediate source.

    ;; TODO: Ensure "ANOTHER" is not present.  It is defined in
    ;; another file.
    ))

(deftest clang-includes-initialized ()
  (with-fixture headers-clang
    (let ((includes (includes *headers*)))
      (is (listp includes))
      (is (= 2 (length includes)))
      (is (member "\"first.c\"" includes :test #'equal))
      (is (member "\"third.c\"" includes :test #'equal)))))

(deftest clang-macros-initialized ()
  (with-fixture headers-clang
    (let ((macros (macros *headers*)))
      (is (listp macros))
      (is (= 1 (length macros)))
      (is (equal (aget "ANOTHER" macros :test #'equal)
                 "ANOTHER 2")))))

(deftest clang-types-initialized ()
  (with-fixture headers-clang
    (let ((types (types *headers*)))
      (is (listp types))
      (is (equal (list "bar" "char" "char*" "foo" "int")
                 (sort (mapcar #'type-name types) #'string<))))))

(deftest update-asts-doesnt-duplicate-includes ()
  (with-fixture headers-clang
    (iter (for incl in (includes *headers*))
          ;; each include in the includes list only appears once in the genome
          ;; (all-matches includes start/end so length is double the number of
          ;; occurrences)
          (is (= 2 (length
                    (all-matches
                     (format nil "#include\\w* ~a" incl)
                     (genome *headers*))))))))

(deftest add-bad-include-doesnt-change-number-of-asts ()
  (with-fixture hello-world-clang
    (let ((orig-num-asts (size *hello-world*)))
      (se::add-include *hello-world* "<garbage.h>")
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest add-bad-type-doesnt-change-number-of-asts ()
  (with-fixture hello-world-clang
    (let ((orig-num-asts (size *hello-world*)))
      (se::add-type *hello-world*
          (se::snippet->clang-type
                  `((:decl . "struct printf { chocolate cake; }"))))
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest add-new-type-changes-genome-and-types ()
  (with-fixture hello-world-clang
    (let ((orig-genome-length (length (genome *hello-world*)))
          (orig-num-types (length (types *hello-world*)))
          (struct-str "struct printf { chocolate cake; }"))
      (se::add-type *hello-world*
          (se::snippet->clang-type `((:decl . ,struct-str))))
      ;; new type gets added to genome
      (is (= (+ orig-genome-length (length struct-str)
                (length (genome *hello-world*)))))
      (is (starts-with-subseq struct-str (genome *hello-world*)))
      ;; new type is added to types
      (is (= (1+ orig-num-types) (length (types *hello-world*)))))))

(deftest add-bad-macro-doesnt-change-number-of-asts ()
  (with-fixture hello-world-clang
    (let ((orig-num-asts (size *hello-world*)))
      (se::add-macro *hello-world*
                     "GARBAGE" "#ifndef GARBAGE DO_SOMETHING_FORGET_ENDIF()")
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest clang-expression-test ()
  (flet ((test-conversion (obj pair)
           (destructuring-bind (text expected-expression) pair
             (let ((result (expression obj (stmt-with-text obj text))))
               (is (equalp result expected-expression)
                   "Statement ~S yields ~S not ~S."
                   text result expected-expression)))))
    (append
     (with-fixture gcd-clang
       (mapc {test-conversion *gcd*}
             '(("b = b - a" (:= :b (:- :b :a)))
               ("a = a - b" (:= :a (:- :a :b)))
               ("b != 0"    (:!= :b 0))
               ("a > b"     (:> :a :b))
               ("a == 0"    (:== :a 0)))))
     (with-fixture binary-search-clang
       (mapc {test-conversion *binary-search*}
             '(("mid = (start + end) / 2"
                (:= :mid (:/ (:+ :start :end) 2)))
               ("haystack[i] = malloc(256 * sizeof(*haystack[i]))"
                (:= (:|[]| :haystack :i)
                 (:malloc (:* 256
                              (:sizeof (:unary-* (:|[]| :haystack :i))))))))))
     (with-fixture huf-clang
       (mapc {test-conversion *huf*}
             '(("h->h = malloc(sizeof(int)*s)"
                (:= (:-> :h :h) (:malloc (:* (:sizeof :int) :s))))
               ("heap->h = realloc(heap->h, heap->s + heap->cs)"
                (:= (:-> :heap :h)
                 (:realloc (:-> :heap :h) (:+ (:-> :heap :s)
                                              (:-> :heap :cs)))))))))))

(deftest clang-mutation-targets-default-test ()
  "Ensure mutation-targets returns all stmt asts by default"
  (with-fixture hello-world-clang
    (is (equalp (stmt-asts *hello-world*)
                (mutation-targets *hello-world*)))))

(deftest clang-mutation-targets-filter-test ()
  "Ensure the filter parameter to mutation-targets works as anticipated"
  (with-fixture hello-world-clang
    (is (equalp (remove-if-not #'se::full-stmt-filter
                               (stmt-asts *hello-world*))
                (mutation-targets *hello-world*
                                  :filter #'se::full-stmt-filter)))))

(deftest clang-mutation-targets-stmt-pool-test ()
  "Ensure the stmt-pool parameter to mutation-targets works as anticipated"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (remove-if-not #'se::full-stmt-filter
                                     (stmt-asts *hello-world*))))
      (is (equalp (remove-if-not #'se::full-stmt-filter
                                 (stmt-asts *hello-world*))
                  (mutation-targets *hello-world*
                                    :stmt-pool #'bad-stmts))))))

(deftest clang-mutation-targets-expand-stmt-pool-restart-test ()
  "Ensure the expand-stmt-pool restart works as intended"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (remove-if-not #'se::full-stmt-filter
                                     (stmt-asts *hello-world*))))
      ;; Before invoking the 'expand-stmt-pool filter, the
      ;; stmt pool does not include any full statements.
      ;; After its invocation, all full statements are returned.
      (is (equalp (remove-if-not #'se::full-stmt-filter
                                 (stmt-asts *hello-world*))
                  (->> (handler-bind
                         ((no-mutation-targets
                           (lambda (c)
                             (declare (ignorable c))
                             (invoke-restart 'expand-stmt-pool))))
                       (mutation-targets *hello-world*
                                         :filter #'se::full-stmt-filter
                                         :stmt-pool #'bad-stmts))))))))

(deftest clang-pick-general-does-not-throw-test ()
  "Ensure calling pick-general does not throw an exception"
  (with-fixture hello-world-clang
    (is (not (null (se::pick-general *hello-world* #'stmt-asts))))))

(deftest clang-pick-general-full-stmt-no-matching-test ()
  "Ensure calling pick-general with a full-stmt filter
throws a no-mutation-targets error when there are no full stmts,
e.g. after a bad crossover"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (remove-if #'se::full-stmt-filter
                                 (stmt-asts *hello-world*))))
      (signals no-mutation-targets
        (se::pick-general *hello-world* #'bad-stmts
                          :filter #'se::full-stmt-filter)))))

(deftest clang-pick-general-full-stmt-test ()
  "Ensure calling pick-general with a full-stmt filter returns a full
statement pick"
  (with-fixture hello-world-clang-control-picks
    (let ((pick (se::pick-general *hello-world* #'stmt-asts
                                  :filter #'se::full-stmt-filter)))
      (is (->> (aget :stmt1 pick)
               (ast-full-stmt))))))

(deftest clang-pick-general-same-class-no-matching-test ()
  "Ensure calling pick-general with a same-class filter throws
a no-mutation-targets error when a second statement with the same AST class
is not to be found"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* `(((:ast-class . "Nothing")))))
      (signals no-mutation-targets
        (se::pick-general *hello-world* #'stmt-asts
                          :filter #'se::same-class-filter
                          :second-pool #'bad-stmts)))))

(deftest clang-pick-general-same-class-test ()
  "Ensure calling pick-general with a same-class filter returns
two statements with the same class."
  (with-fixture hello-world-clang
    (let ((picks (se::pick-general *hello-world* #'stmt-asts
                                   :filter #'se::same-class-filter
                                   :second-pool #'stmt-asts)))
      (is (equal (->> (aget :stmt1 picks)
                      (get-ast *hello-world*)
                      (aget :ast-class))
                 (->> (aget :stmt2 picks)
                      (get-ast *hello-world*)
                      (aget :ast-class)))))))

(deftest clang-promote-guarded-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (handler-case
      (progn
        (build-op (make-instance 'clang-promote-guarded :object *soft*) *soft*)
        (is nil "build-op should have thrown no-mutation-targets error"))
      (error (e)
        (is (equal (type-of e) 'no-mutation-targets)
            "build-op should have thrown no-mutation-targets error")))))



(deftest pick-cut-decl-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets (se::pick-cut-decl *soft*))))

(deftest pick-swap-decls-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets (se::pick-swap-decls *soft*))))

(deftest pick-rename-variable-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets (se::pick-rename-variable *soft*))))


;;; Detailed clang mutation tests
;;;
;;; These all run the entire mutate method, rather that just
;;; apply-mutation, adjusting the good and bad picks to get
;;; predictable results. And they check the results of each mutation
;;; in as much detail as possible.
(in-suite test)
(defsuite* test-clang-mutations)

(defixture gcd-clang-control-picks
  (:setup
   (setf *gcd*
         (from-file (make-instance 'clang-control-picks :compiler "clang-3.7")
                    (gcd-dir "gcd.c"))))
  (:teardown
   (setf *gcd* nil)))

(defun asts-with-text (obj &rest texts)
  (mapcar {stmt-with-text obj} texts))

(deftest cut-full-removes-full-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-cut-full . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (< (count-if #'ast-full-stmt (asts variant))
             (count-if #'ast-full-stmt (asts *hello-world*)))))))

(deftest cut-removes-any-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-cut . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (< (length (asts variant)) (length (asts *hello-world*)))))))

(deftest insert-full-adds-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*clang-mutation-types* '((clang-insert-full . 1)))
          (*bad-asts* (asts-with-text *hello-world* "return 0"))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if #'ast-full-stmt (asts variant))
             (count-if #'ast-full-stmt (asts *hello-world*)))))))

(deftest insert-adds-any-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf"))
          (*good-asts* (asts-with-text *hello-world* "printf"))
          (*clang-mutation-types* '((clang-insert . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-starting-with-text variant "printfprintf")))))

(deftest insert-same-adds-same-class ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "0"))
          (*clang-mutation-types* '((clang-insert-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "return 00")))))

(deftest insert-full-same-adds-same-class-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0"))
          (*clang-mutation-types* '((clang-insert-full-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if [{equal "ReturnStmt"} #'ast-class]
                       (asts variant))
             (count-if [{equal "ReturnStmt"} #'ast-class]
                       (asts *hello-world*)))))))

(deftest replace-changes-any-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "0"))
          (*good-asts* (asts-with-text *hello-world* "printf"))
          (*clang-mutation-types* '((clang-replace . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "return printf")))))

(deftest replace-full-changes-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0"))
          (*good-asts* (asts-with-text *hello-world*
                                       "0" "printf(\"Hello, World!\\n\")"))
          (*clang-mutation-types* '((clang-replace-full . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if [{equal "CallExpr"} #'ast-class]
                       (asts variant))
             (count-if [{equal "CallExpr"} #'ast-class]
                       (asts *hello-world*)))))))

(deftest replace-same-changes-same-class ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "\"Hello, World!\\n\""))
          (*good-asts* (asts-with-text *hello-world*
                                       "0" "printf"))
          (*clang-mutation-types* '((clang-replace-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "printf(printf)")))))

(deftest replace-full-same-changes-same-class-full-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-replace-full-same . 1)))
          (variant (copy *hello-world*)))
      (multiple-value-bind  (variant mutation) (mutate variant)
        (is (ast-full-stmt (aget :stmt1 (targets mutation))))
        (is (ast-full-stmt (aget :stmt2 (targets mutation))))

        ;; Not a very interesting test: this can only replace a
        ;; statement with itself, but sometimes there are whitespace
        ;; changes. Just compare AST classes to avoid spurious
        ;; failures.
        (is (equal (mapcar [#'ast-class] (asts variant))
                   (mapcar [#'ast-class] (asts *hello-world*))))))))

(deftest swap-changes-any-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world*
                                      "\"Hello, World!\\n\"" "0"))
          (*clang-mutation-types* '((clang-swap . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "\"Hello, World!\\n\""))
      (is (stmt-with-text variant "0")))))

(deftest move-changes-any-stmts ()
  (with-fixture hello-world-clang-control-picks
    (flet ((count-matches ())))
    (let* ((bad-1 "printf(\"Hello, World!\\n\")")
           (bad-2 "return 0")
           (*bad-asts* (asts-with-text *hello-world* bad-1 bad-2))
           (*clang-mutation-types* '((clang-move . 1)))
           (variant (copy *hello-world*)))
      (mutate variant)
      ;; Still exist (> 0).
      (is (stmt-with-text variant bad-1)
          "Move doesn't remove \"Hello, World!\\n\".")
      (is (stmt-with-text variant bad-2)
          "Move doesn't remove \"0\".")
      ;; No duplicates (< 2).
      (is
       (= 1 (length (all-matches-as-strings (quote-meta-chars bad-1)
                                            (genome variant))))
       "Move doesn't duplicate \"Hello, World!\\n\".")
      (is
       (= 1 (length (all-matches-as-strings (quote-meta-chars bad-2)
                                            (genome variant))))
       "Move doesn't duplicate \"0\"."))))

(deftest swap-full-changes-full-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let ((*clang-mutation-types* '((clang-swap-full . 1)))
          ;; Avoid swapping the function body
          (*bad-asts* (remove-if [{member _  '("CompoundStmt" "Function")
                                          :test #'string=}
                                  #'ast-class]
                                 (asts *hello-world*)))
          (variant (copy *hello-world*)))

      (multiple-value-bind  (variant mutation) (mutate variant)
        ;; We can't predict exactly what will be swapped. Just
        ;; sanity check.
        (is (ast-full-stmt (aget :stmt1 (targets mutation))))
        (is (ast-full-stmt (aget :stmt2 (targets mutation))))
        (is (stmt-with-text variant "printf"))
        (is (stmt-with-text variant "return 0"))))))

(deftest swap-full-same-changes-same-class-full-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-swap-full-same . 1)))
          (variant (copy *hello-world*)))
      (multiple-value-bind  (variant mutation) (mutate variant)
        (is (ast-full-stmt (aget :stmt1 (targets mutation))))
        (is (ast-full-stmt (aget :stmt2 (targets mutation))))

        ;; Not a very interesting test: this can only swap a
        ;; statement with itself, but sometimes there are whitespace
        ;; changes. Just compare AST classes to avoid spurious
        ;; failures.
        (is (equal (mapcar #'ast-class (asts variant))
                   (mapcar #'ast-class (asts *hello-world*))))))))

(deftest unguard-conditional-compound-statements ()
  (flet ((subsequent-lines-p (genome-string first second)
           (-<>> genome-string
                 (split-sequence #\Newline)
                 (mapcar {string-trim " "})
                 (member first <> :test #'string=)
                 (second)
                 (string= second))))
    (with-fixture nested-clang
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('WHILE')")
                                (get-parent-asts *nested*)
                                (third))))))
           "/* While loop. */"
           "puts('WHILE');")
          "Promotes single-line body from within while loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('DO')")
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Do loop. */"
           "puts('DO');")
          "Promotes single-line body from within do loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('FOR')")
                                (get-parent-asts *nested*)
                                (third))))))
           "/* For loop. */"
           "puts('FOR');")
          "Promotes single-line body from within for loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('IF-1')")
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Single child. */"
           "puts('IF-1');")
          "Promotes single-line sole branch of if.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('IF-2')")
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Empty then. */"
           "puts('IF-2');")
          "Promotes single-line else of if w/o then.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('IF-3')")
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Empty else. */"
           "puts('IF-3');")
          "Promotes single-line then of if w/o else.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('IF-3')")
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Empty else. */"
           "puts('IF-3');")
          "Promotes single-line then of if w/o else.")
      (let ((genome-string
             (let ((copy (copy *nested*)))
               (genome
                (apply-mutation copy
                  (make-instance 'clang-promote-guarded
                    :object copy
                    :targets (->> (stmt-with-text *nested* "puts('MULTILINE')")
                                  (get-parent-asts *nested*)
                                  (third))))))))
        (is (and (subsequent-lines-p genome-string
                                     "/* Multiline loop. */"
                                     "puts('MULTILINE');")
                 (subsequent-lines-p genome-string
                                     "puts('MULTILINE');"
                                     "puts('WHILE-1');")
                 (subsequent-lines-p genome-string ; Ensure peels bananas.
                                     "puts('WHILE-1');"
                                     "j++;"))
            "Promotes multi-line body from within while loop.")))))

(deftest if-to-while-test ()
  (with-fixture gcd-clang-control-picks
    (let ((*clang-mutation-types* '((if-to-while . 1)))
          (*bad-asts* (list (find-if [{string= "IfStmt"} #'ast-class]
                                     (stmt-asts *gcd*)))))
      (multiple-value-bind  (variant mutation) (mutate (copy *gcd*))
        (is (string= "IfStmt"
                     (ast-class (targets mutation))))
        (let ((stmt (stmt-starting-with-text variant "while")))
          (is stmt)
          (is (string= "WhileStmt" (ast-class stmt))))))))


;;; Clang w/ mutation fodder representation
(in-suite test)
(defsuite* test-clang-w-fodder)

(deftest simply-able-to-load-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (is (not (null *hello-world*)))))

(deftest insert-fodder-decl-mutation-throws-error-if-no-targets-test ()
  (with-fixture no-insert-fodder-decl-mutation-targets-clang
    (signals no-mutation-targets
      (apply-mutation *soft* (make-instance 'insert-fodder-decl
                               :object *soft*)))))

(deftest insert-decl-lengthens-a-clang-w-fodder-software-object ()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant (make-instance 'insert-fodder-decl
                                :object variant))
      (is (> (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest insert-decl-rename-lengthens-and-insinuates-a-clang-w-fodder ()
  (with-fixture gcd-clang-w-fodder
    (let ((var (copy *gcd*))
          (mut (make-instance 'insert-fodder-decl-rep :object *gcd*)))
      (apply-mutation var mut)
      (is (string/= (genome var) (genome *gcd*))
          "Genome of *gcd* changed by INSERT-FODDER-DECL-REP.")
      (is
       (> (length (split (aget :new-var (aget :rename-variable (targets mut)))
                         (genome var)))
          2)
       "New decl variable appears in more than just the declaring ast."))))

(deftest insert-value-lengthens-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant '(clang-insert (:stmt1 . 3)
                                (:literal1 . "int i = 0;")))
      (is (> (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest set-value-changes-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant
        `(clang-replace
          (:stmt1 . ,(stmt-with-text variant "\"Hello, World!\\n\""))
          (:literal1 . "\"Hello, mutate!\"")))
      (is (= (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest pick-bad-fodder-works ()
  (with-fixture hello-world-clang-w-fodder
    (let ((mut (make-instance 'insert-fodder :object *hello-world*)))
      (is (not (null (targets mut))))
      (is (not (null (aget :stmt1 (targets mut)))))
      (is (not (null (aget :value1 (targets mut))))))))

(deftest pick-decl-fodder-works ()
  (with-fixture hello-world-clang-w-fodder
    (let ((mut (make-instance 'insert-fodder-decl :object *hello-world*)))
      (is (not (null (targets mut))))
      (is (not (null (aget :stmt1 (targets mut)))))
      (is (not (null (aget :value1 (targets mut))))))))

;;; Clang utility methods
(in-suite test)
(defsuite* test-clang-utility)

(deftest asts-populated-on-creation ()
  (with-fixture hello-world-clang
    (is (= 10 (length (asts *hello-world*))))))

(deftest parent-ast-p-true-test()
  (with-fixture hello-world-clang
    (is (parent-ast-p *hello-world*
                      (stmt-with-text *hello-world* "return 0")
                      (stmt-with-text *hello-world* "0")))))

(deftest parent-ast-p-false-test()
  (with-fixture hello-world-clang
    (is (not (parent-ast-p *hello-world*
                           (stmt-with-text *hello-world* "0")
                           (stmt-with-text *hello-world* "return 0"))))))

(deftest tidy-a-clang-software-object()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*)))
      (clang-tidy variant)
      (is (= (size variant)
             (size *hello-world*))))))

(deftest tidy-adds-braces ()
  (with-fixture tidy-adds-braces-clang
    (let ((variant (copy *soft*)))
      (clang-tidy variant)
      (is (= 2 (->> (stmt-asts variant)
                    (remove-if-not [{string= "CompoundStmt"}
                                    #'ast-class])
                    (length)))))))

(deftest format-a-clang-software-object ()
  (flet ((run (obj)
           (with-temp-file (bin)
             (phenome obj :bin bin)
             (shell bin))))
    (with-fixture hello-world-clang
      (multiple-value-bind (obj errno) (clang-format (copy *hello-world*))
        (is (zerop errno))
        (is (string= (run *hello-world*) (run obj)))))))

(deftest type-of-var-returns-correct-type ()
  (with-fixture type-of-var-clang
    (let ((var-type1 (type-of-var *soft* "a"
                                  (stmt-with-text *soft* "return 0")))
          (var-type2 (type-of-var *soft* "a"
                                  (stmt-with-text *soft* "return 1")))
          (var-type3 (type-of-var *soft* "a"
                                  (stmt-with-text *soft* "return 2")))
          (var-type4 (type-of-var *soft* "a"
                                  (stmt-with-text *soft* "int a[N][N]")))
          (var-type5 (type-of-var *soft* "b"
                                  (stmt-with-text *soft* "int **b"))))
      (is (equal "[][]" (aget :array var-type1)))
      (is (equal ""     (aget :array var-type2)))
      (is (equal "[][]" (aget :array var-type3)))
      (is (equal "[][]" (aget :array var-type4)))
      (is (equal ""     (aget :array var-type5)))
      (is (equal nil    (aget :pointer var-type1)))
      (is (equal t      (aget :pointer var-type2)))
      (is (equal nil    (aget :pointer var-type3)))
      (is (equal nil    (aget :pointer var-type4)))
      (is (equal t      (aget :pointer var-type5)))
      (is (equal "int"  (aget :type var-type1)))
      (is (equal "int"  (aget :type var-type2)))
      (is (equal "int"  (aget :type var-type3)))
      (is (equal "int"  (aget :type var-type4)))
      (is (equal "int*" (aget :type var-type5))))))

(deftest type-of-var-handles-missing-declaration-type ()
  (with-fixture type-of-var-missing-decl-type-clang
    (is (null (type-of-var *soft* "dirs")))))

(deftest apply-replacements-test ()
  (is (string= "Hello, world!"
               (apply-replacements nil
                                   "Hello, world!")))
  (is (string= "Hello, world!"
               (apply-replacements `((nil . nil))
                                   "Hello, world!")))
  (is (string= "Hello, world!"
               (apply-replacements `((nil . "Goodbye"))
                                   "Hello, world!")))
  (is (string= "Hello, world!"
               (apply-replacements `(("Hello" . nil))
                                   "Hello, world!")))
  (is (string= "Goodbye, world!"
               (apply-replacements `(("Hello" . "Goodbye"))
                                   "Hello, world!")))
  (is (string= "Goodbye, earth!"
               (apply-replacements `(("Hello" . "Goodbye") ("world" . "earth"))
                                   "Hello, world!"))))


;;; Range representation
(in-suite test)
(defsuite* test-range)

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
    (is (tree-equal (genome (apply-mutation
                             *soft*
                             (make-instance 'simple-cut :targets 2)))
                    '((0 . 1) (1 . 1) (1 . 2))))
    (is (tree-equal (genome (apply-mutation
                             *soft*
                             (make-instance 'simple-cut :targets 2)))
                    '((0 . 1) (2 . 2))))
    (is (tree-equal (genome (apply-mutation
                             *soft*
                             (make-instance 'simple-cut :targets 1)))
                    '((0 . 0) (2 . 2))))
    (is (tree-equal (genome (apply-mutation
                             *soft*
                             (make-instance 'simple-cut :targets 1)))
                    '((0 . 0))))
    (is (null (genome (apply-mutation
                       *soft*
                       (make-instance 'simple-cut :targets 0)))))))

(deftest some-range-insert-mutations ()
  (with-fixture range
    (is (tree-equal
         (genome (apply-mutation
                  *soft*
                  (make-instance 'simple-insert :targets (list 0 2))))
         '((2 . 2) (0 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal
         (genome (apply-mutation
                  *soft*
                  (make-instance 'simple-insert :targets (list 5 1))))
         '((2 . 2) (0 . 2) (1 . 1) (0 . 0) (1 . 2))))
    (is (tree-equal
         (genome (apply-mutation
                  *soft*
                  (make-instance 'simple-insert :targets (list 5 2))))
         '((2 . 2) (0 . 2) (1 . 1) (1 . 1) (0 . 0) (1 . 2))))
    (is (tree-equal
         (genome (apply-mutation
                  *soft*
                  (make-instance 'simple-insert :targets (list 2 1))))
         '((2 . 2) (0 . 0) (0 . 0) (1 . 2) (1 . 1) (1 . 1) (0 . 0) (1 . 2))))))

(deftest some-range-swap-mutations ()
  (with-fixture range
    (apply-mutation *soft* (make-instance 'simple-swap :targets (list 0 2)))
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


;;; Mutation analysis and statistics collection tests
(in-suite test)
(defsuite* test-mutation-analysis)

(defvar *test* nil "Variable to hold evaluation function for tests.")

(defixture hello-world-clang-w-fitness
  (:setup
   (setf *hello-world*
         (from-file (make-instance 'clang :compiler "clang"
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
                            (:literal1 . ,(se::make-statement "IntegerLiteral"
                                                         :fullstmt '("0")))))))
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
    (let* ((variant (copy *hello-world*))
           (op (make-instance 'swap-and-cut :object variant)))
      (apply-mutation variant op)
      (is (different-asts (asts variant)
                          (asts *hello-world*)))
      (is (not (equal (genome variant)
                      (genome *hello-world*))))
      (is (< (size variant)
             (size *hello-world*))))))

;; Ancestry tests
(defclass clang-w-ancestry (clang ancestral) ())

(defixture hello-world-clang-w-ancestry
  (:setup
   (setf software-evolution::*next-ancestry-id* 0
         *hello-world*
         (from-file (make-instance 'clang-w-ancestry :compiler "clang"
                                   :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))
         *test* [#'length #'genome])
   (evaluate *test* *hello-world*))
  (:teardown
   (setf *hello-world* nil
         *test* nil
         software-evolution::*next-ancestry-id* 0)))

(deftest apply-mutation-logs-ancestry ()
  (with-fixture hello-world-clang-w-ancestry
    (let ((op (make-instance 'clang-cut
                :object *hello-world*
                :targets `((:stmt1 . ,(stmt-with-text *hello-world*
                                                      "return 0"))))))
      (apply-mutation *hello-world* op)
      (evaluate *test* *hello-world*)

      (is (< 1 (length (ancestors *hello-world*))))

      (is (= 1 (plist-get :id (first (ancestors *hello-world*)))))
      (is (not (null (plist-get :fitness (first (ancestors *hello-world*))))))
      (is (equal (type-of op)
                 (plist-get :mutant (first (ancestors *hello-world*)))))

      (is (= 0 (plist-get :id (second (ancestors *hello-world*)))))
      (is (equal 'from-file
                 (plist-get :how (second (ancestors *hello-world*))))))))

(deftest crossover-logs-ancestry ()
  (with-fixture hello-world-clang-w-ancestry
    (let ((crossed (crossover *hello-world* *hello-world*)))
      (is (< 1 (length (ancestors crossed))))

      (is (not (null (plist-get :crossover (first (ancestors crossed))))))
      (is (= 0 (plist-get :id
                          (first (plist-get :cross-with
                                            (first (ancestors crossed)))))))
      (is (equal
           'from-file
           (plist-get :how
                      (first (plist-get :cross-with
                                        (first (ancestors crossed))))))))))

(deftest graphing-ancestry ()
  (with-fixture hello-world-clang-w-ancestry
    (let ((crossed (crossover *hello-world* *hello-world*))
          (op (make-instance 'clang-cut
                :object *hello-world*
                :targets `((:stmt1 . ,(stmt-with-text *hello-world*
                                                      "return 0"))))))
      (apply-mutation crossed op)
      (with-temp-file (save-base)
        (multiple-value-bind (stdout stderr errno)
            (save-ancestry crossed
                           (pathname-directory save-base)
                           (pathname-name save-base))
          (declare (ignorable stdout stderr))
          (let ((svg (make-pathname :directory (pathname-directory save-base)
                                    :name (pathname-name save-base)
                                    :type "svg"))
                (dot (make-pathname :directory (pathname-directory save-base)
                                    :name (pathname-name save-base)
                                    :type "dot")))
            (when (probe-file svg) (delete-file svg))
            (when (probe-file dot) (delete-file dot)))
          (is (zerop errno)))))))


;;; Diff tests
(in-suite test)
(defsuite* test-diff)

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
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-cut :targets 2)))
           '(((:CODE 1)) ((:CODE 2)) ((:CODE 4)))))
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-cut :targets 1)))
           '(((:CODE 1)) ((:CODE 4)))))
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-cut :targets 1)))
           '(((:CODE 1))))))))

(deftest some-diff-insert-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-insert :targets (list 0 2))))
           '(((:CODE 3)) ((:CODE 1)) ((:CODE 2))
             ((:CODE 3)) ((:CODE 4))))))))

(deftest some-diff-swap-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal
           (genome (apply-mutation
                    *soft*
                    (make-instance 'simple-swap :targets (list 0 2))))
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
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-cut :targets 2)))
                  #(((:CODE 1)) ((:CODE 2)) ((:CODE 4)))))
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-cut :targets 1)))
                  #(((:CODE 1)) ((:CODE 4)))))
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-cut :targets 1)))
                  #(((:CODE 1))))))))

(deftest some-diff-array-insert-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-insert :targets (list 0 2))))
                  #(((:CODE 3)) ((:CODE 1)) ((:CODE 2))
                    ((:CODE 3)) ((:CODE 4))))))))

(deftest some-diff-array-swap-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (genome (apply-mutation
                           *soft*
                           (make-instance 'simple-swap :targets (list 0 2))))
                  #(((:CODE 3)) ((:CODE 2)) ((:CODE 1)) ((:CODE 4))))))))


;;; Population tests
(in-suite test)
(defsuite* test-population)

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



;;; Helper functions to avoid hard-coded statement numbers.
(defun stmt-with-text (obj text)
  (find-if [{string= text} #'peel-bananas #'source-text]
           (asts obj)))

(defun stmt-starting-with-text (obj text)
  (find-if (lambda (snippet)
             (and snippet
                  (equal 0
                         (search text
                                 (peel-bananas (source-text snippet))))))
           (asts obj)))

(defun ast-starting-with-text (obj text)
  (when-let ((id (stmt-starting-with-text obj text)))
    (get-ast obj id)))

(deftest swap-can-recontextualize ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons 'clang-swap
              (list (cons :stmt1 (stmt-with-text variant "n > 0"))
                    (cons :stmt2 (stmt-with-text variant "bc=0")))))
      (is (compile-p variant)))))

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

(deftest swap-at-different-levels-can-recontextualize ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*))
          (text-1 "n > 0")
          (text-2 "bn++"))
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

(deftest insert-can-recontextualize ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons 'clang-insert
              (list (cons :stmt1
                          (stmt-with-text variant "bc=0"))
                    (cons :stmt2
                          (stmt-with-text variant "n > 0")))))
      (is (compile-p variant)))))

(deftest insert-makes-expected-change ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons 'clang-insert
              (list (cons :stmt1 (stmt-with-text variant "bc=0"))
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
  (let* ((obj (make-instance 'clang
                  :genome "void test(int x);
                          int main(int argc, char **argv) {
                            test(0); return 0;
                           }"))
         (stmt (stmt-with-text obj "test(0)"))
        (*matching-free-function-retains-name-bias* 1.0))
    (apply-mutation obj
                    `(clang-replace (:stmt1 . ,stmt) (:stmt2 . ,stmt)))
    (is (string= (source-text stmt)
                 (source-text (stmt-with-text obj "test(0)"))))))

;; huf.c only contains one user function with 3 parameters,
;; check that random-function-name can find it.
(deftest finds-function-binding ()
  (with-fixture huf-clang
    (is (string= "inttobits"
                 (random-function-name (functions *huf*)
                                       :original-name "foo"
                                       :arity 3)))))


;;; Fix compilation tests.
(in-suite test)
(defsuite* test-fix-compilation)

(defvar *broken-clang* nil "")
(defvar *broken-gcc* nil "")

(defixture broken-compilation
  (:setup (setf *broken-clang*
                (make-instance 'clang-w-fodder
                  :genome "int main(int argc, char **argv) {
	printf(\"Hello, World!\\n\");
	return missing_variable;}"))
          (setf *database*
                (with-open-file (in (make-pathname :name "euler-example.json"
                                                   :directory +etc-dir+))
                  (make-instance 'json-database :json-stream in))))
  (:teardown (setf *database* nil)))

(defixture broken-compilation-gcc
  (:setup (setf *broken-gcc*
                (make-instance 'clang-w-fodder
                  :compiler "gcc"
                  :flags '("-m32" "-O0" "-g")
                  :genome "int main(int argc, char **argv) {
	printf(\"Hello, World!\\n\");
	return missing_variable;}"))
          (setf *database*
                (with-open-file (in (make-pathname :name "euler-example.json"
                                                   :directory +etc-dir+))
                  (make-instance 'json-database :json-stream in))))
  (:teardown (setf *database* nil)))

(deftest fix-compilation-inserts-missing-include ()
  (with-fixture broken-compilation
    (is (scan (format nil "\#include <~a>" "stdio.h")
              (genome-string (fix-compilation *broken-clang* 1)))))
  (with-fixture broken-compilation-gcc
    (is (scan (format nil "\#include <~a>" "stdio.h")
              (genome-string (fix-compilation *broken-gcc* 1))))))

(deftest fix-compilation-inserts-declaration-and-initializes ()
  (with-fixture broken-compilation
    (is (scan (quote-meta-chars "missing_variable =")
              (genome (fix-compilation *broken-clang* 4)))))
  (with-fixture broken-compilation-gcc
    (is (scan (quote-meta-chars "missing_variable =")
              ;; Without the retries this test can fail stochastically.
              (iter (for fixed = (fix-compilation (copy *broken-gcc*) 4))
                    (unless (zerop (length (genome fixed)))
                      (return (genome fixed))))))))

(deftest fix-compilation-declare-var-as-pointer ()
  (with-temp-file (genome ".c")
    (string-to-file "int main(int argc, char **argv) {
                      int y = 0;
                      return *y;
                    }"
                    genome)
    (let ((broken-clang (from-file (make-instance 'clang
                                     :compiler "clang"
                                     :flags '("-m32" "-O0" "-g"))
                                   genome))
          (broken-gcc   (from-file (make-instance 'clang
                                     :compiler "gcc"
                                     :flags '("-m32" "-O0" "-g"))
                                   genome)))
      (is (compile-p (fix-compilation broken-clang 1)))
      (is (compile-p (fix-compilation broken-gcc 1))))))

(in-suite test)
(defsuite* test-clang-crossover)

(defun select-intraprocedural-pair-with-adjustments-test (obj)
  (let ((function (first (functions obj))))
    (loop :for i :from 0 :to 25
          :do (progn (multiple-value-bind (pt1 pt2)
                         (select-intraprocedural-pair obj)
                       (multiple-value-bind (stmt1 stmt2)
                           (adjust-stmt-range obj pt1 pt2)
                         (is (<= (1+ (first (se::stmt-range obj function)))
                                 stmt1
                                 (second (se::stmt-range obj function))))
                         (is (<= (1+ (first (se::stmt-range obj function)))
                                 stmt2
                                 (second (se::stmt-range obj function))))
                         (is (full-stmt-p obj
                                          (se::ast-at-index obj stmt1)))
                         (is (full-stmt-p obj
                                          (se::ast-at-index obj stmt2)))))))))

(deftest select-intraprocedural-pair-with-adjustments-collatz-test ()
  (with-fixture collatz-clang
    (select-intraprocedural-pair-with-adjustments-test *collatz*)))

(deftest select-intraprocedural-pair-with-adjustments-fib-test ()
  (with-fixture fib-clang
    (select-intraprocedural-pair-with-adjustments-test *fib*)))

(deftest select-intraprocedural-pair-with-adjustments-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (select-intraprocedural-pair-with-adjustments-test *soft*)))

(deftest select-intraprocedural-pair-with-adjustments-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (select-intraprocedural-pair-with-adjustments-test *soft*)))

(deftest nesting-relation-same-scope-test ()
  (with-fixture fib-clang
    (is (equal '(0 . 0)
               (nesting-relation *fib*
                                 (stmt-with-text *fib* "int t = x")
                                 (stmt-with-text *fib* "x = x + y")))
        (equal '(0 . 0)
               (nesting-relation *fib*
                                 (stmt-with-text *fib* "int t = x")
                                 (stmt-with-text *fib* "int t = x")))
        (equal '(0 . 0)
               (nesting-relation *fib*
                                 (aget :counter (first (stmt-asts *fib*)))
                                 (aget :counter (first (stmt-asts *fib*))))))))

(deftest nesting-relation-increasing-scope-fib-test ()
  (with-fixture fib-clang
    (is (equal '(0 . 1)
               (nesting-relation *fib*
                                 (stmt-with-text *fib* "int x = 0")
                                 (stmt-with-text *fib* "int t = x"))))))

(deftest nesting-relation-decreasing-scope-fib-test ()
  (with-fixture fib-clang
    (is (equal '(1 . 0)
               (nesting-relation *fib*
                                 (stmt-with-text *fib* "int t = x")
                                 (stmt-with-text *fib* "return x"))))))

(deftest nesting-relation-increasing-scope-collatz-test ()
  (with-fixture collatz-clang
    (is (equal '(0 . 2)
               (nesting-relation *collatz*
                                 (stmt-with-text *collatz* "int k = 0")
                                 (stmt-with-text *collatz* "m /= 2"))))))

(deftest nesting-relation-decreasing-scope-collatz-test ()
  (with-fixture collatz-clang
    (is (equal '(2 . 0)
               (nesting-relation *collatz*
                                 (stmt-with-text *collatz* "m /= 2")
                                 (stmt-with-text *collatz* "return k"))))))

(deftest nesting-relation-increasing-scope-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal '(0 . 2)
               (nesting-relation *soft*
                                 (stmt-with-text *soft* "int i")
                                 (stmt-with-text *soft*
                                   "printf(\"%d\\n\", i+j)"))))))

(deftest nesting-relation-decreasing-scope-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal '(2 . 0)
               (nesting-relation *soft*
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j)")
                                 (stmt-with-text *soft*
                                                 "return 0"))))))

(deftest nesting-relation-increasing-scope-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal '(0 . 1)
               (nesting-relation *soft*
                                 (->> "printf(\"%d\\n\", argc)"
                                      (stmt-with-text *soft*))
                                 (->> "printf(\"%d\\n\", argc + argc)"
                                      (stmt-with-text *soft*)))))))

(deftest nesting-relation-decreasing-scope-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal '(1 . 0)
               (nesting-relation *soft*
                                 (->> "printf(\"%d\\n\", argc + argc)"
                                      (stmt-with-text *soft*))
                                 (->> "return 0"
                                      (stmt-with-text *soft*)))))))

(deftest common-ancestor-fib-test ()
  (with-fixture fib-clang
    (is (equalp (function-body *fib*
                               (stmt-starting-with-text *fib* "int fib"))
                (common-ancestor *fib*
                                 (stmt-with-text *fib* "int x = 0")
                                 (stmt-with-text *fib* "int y = 1"))))
    (is (equalp (function-body *fib*
                               (stmt-starting-with-text *fib* "int fib"))
                (common-ancestor *fib*
                                 (stmt-with-text *fib* "int x = 0")
                                 (stmt-with-text *fib* "int t = x"))))
    (is (equalp (->> (stmt-starting-with-text *fib* "while")
                     (get-immediate-children *fib*)
                     (second))
                (common-ancestor *fib*
                                 (stmt-with-text *fib* "int t = x")
                                 (stmt-with-text *fib* "x = x + y"))))))

(deftest common-ancestor-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (->> (stmt-starting-with-text *collatz* "int collatz")
                     (function-body *collatz*))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "int k = 0")
                                 (stmt-with-text *collatz* "return k"))))
    (is (equalp (->> (stmt-starting-with-text *collatz* "int collatz")
                     (function-body *collatz*))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "int k = 0")
                                 (stmt-with-text *collatz* "m /= 2"))))
    (is (equalp (->> (stmt-starting-with-text *collatz* "int collatz")
                     (function-body *collatz*))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "m /= 2")
                                 (stmt-with-text *collatz* "return k"))))
    (is (equalp (stmt-starting-with-text *collatz* "if")
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "m /= 2")
                                 (stmt-with-text *collatz* "m = 3*m + 1"))))
    (is (equalp (->> (stmt-starting-with-text *collatz* "while")
                     (get-immediate-children *collatz*)
                     (second))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "m /= 2")
                                 (stmt-with-text *collatz* "++k"))))
    (is (equalp (->> (stmt-starting-with-text *collatz* "while")
                     (get-immediate-children *collatz*)
                     (second))
                (common-ancestor *collatz*
                                 (->> (stmt-starting-with-text *collatz* "while")
                                      (get-immediate-children *collatz*)
                                      (second))
                                 (stmt-starting-with-text *collatz* "if"))))))

(deftest common-ancestor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (common-ancestor *soft*
                                 (stmt-with-text *soft* "int i")
                                 (stmt-with-text *soft* "return 0"))))
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (common-ancestor *soft*
                                 (stmt-with-text *soft*
                                                 "int i")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j)"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (common-ancestor *soft*
                                 (stmt-starting-with-text *soft* "for (i = 0")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j)"))))
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (common-ancestor *soft*
                                 (stmt-starting-with-text *soft* "for (j = 0")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j)"))))))

(deftest common-ancestor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (common-ancestor *soft*
                                 (->> "printf(\"%d\\n\", argc)"
                                      (stmt-with-text *soft*))
                                 (->> "printf(\"%d\\n\", argc + argc)"
                                      (stmt-with-text *soft*)))))
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (common-ancestor *soft*
                                 (->> "printf(\"%d\\n\", argc + argc)"
                                      (stmt-with-text *soft*))
                                 (->> "return 0"
                                      (stmt-with-text *soft*)))))
    (is (equalp (->> (stmt-starting-with-text *soft* "switch")
                     (get-immediate-children *soft*)
                     (second))
                (common-ancestor *soft*
                                 (->> "printf(\"%d\\n\", argc + argc)"
                                      (stmt-with-text *soft*))
                                 (->> "printf(\"%d\\n\", argc * argc)"
                                      (stmt-with-text *soft*)))))
    (is (equalp (stmt-starting-with-text *soft* "case 1")
                (common-ancestor *soft*
                                 (->> "printf(\"%d\\n\", argc + argc)"
                                      (stmt-with-text *soft*))
                                 (stmt-starting-with-text *soft* "case 1"))))))

(deftest ancestor-after-fib-test ()
  (with-fixture fib-clang
    (is (equalp (stmt-with-text *fib* "int x = 0")
                (se::ancestor-after *fib*
                                    (->> "int fib"
                                         (stmt-starting-with-text *fib*)
                                         (function-body *fib*))
                                    (stmt-with-text *fib* "int x = 0"))))
    (is (equalp (->> (stmt-asts *fib*)
                     (remove-if-not [{string= "WhileStmt"}
                                     #'ast-class])
                     (first))
                (se::ancestor-after *fib*
                                    (->> "int fib"
                                         (stmt-starting-with-text *fib*)
                                         (function-body *fib*))
                                    (stmt-with-text *fib* "int t = x"))))
    (is (equalp (stmt-with-text *fib* "x = x + y")
                (se::ancestor-after *fib*
                                    (->> "while "
                                         (stmt-starting-with-text *fib*)
                                         (get-immediate-children *fib*)
                                         (second))
                                    (stmt-with-text *fib* "x = x + y"))))))

(deftest ancestor-after-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (stmt-with-text *collatz* "int k = 0")
                (se::ancestor-after *collatz*
                                    (->> "int collatz"
                                         (stmt-starting-with-text *collatz*)
                                         (function-body *collatz*))
                                    (stmt-with-text *collatz* "int k = 0"))))
    (is (equalp (stmt-with-text *collatz* "return k")
                (se::ancestor-after *collatz*
                                    (->> "int collatz"
                                         (stmt-starting-with-text *collatz*)
                                         (function-body *collatz*))
                                    (stmt-with-text *collatz* "return k"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (remove-if-not [{string= "WhileStmt"}
                                     #'ast-class])
                     (first))
                (se::ancestor-after *collatz*
                                    (->> "int collatz"
                                         (stmt-starting-with-text *collatz*)
                                         (function-body *collatz*))
                                    (stmt-with-text *collatz* "m /= 2"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (remove-if-not [{string= "IfStmt"}
                                     #'ast-class])
                     (first))
                (se::ancestor-after *collatz*
                                    (->> "while"
                                         (stmt-starting-with-text *collatz*)
                                         (get-immediate-children *collatz*)
                                         (second))
                                    (stmt-with-text *collatz* "m /= 2"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (remove-if-not [{string= "IfStmt"}
                                     #'ast-class])
                     (first))
                (se::ancestor-after *collatz*
                                    (->> "while"
                                         (stmt-starting-with-text *collatz*)
                                         (get-immediate-children *collatz*)
                                         (second))
                                    (stmt-with-text *collatz* "m = 3*m + 1"))))
    (is (equalp (stmt-with-text *collatz* "++k")
                (se::ancestor-after *collatz*
                                    (->> "while"
                                         (stmt-starting-with-text *collatz*)
                                         (get-immediate-children *collatz*)
                                         (second))
                                    (stmt-with-text *collatz* "++k"))))))

(deftest ancestor-after-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (se::ancestor-after *soft*
                                    (stmt-starting-with-text *soft* "for (i = 0")
                                    (stmt-with-text *soft*
                                                    "printf(\"%d\\n\", i+j)"))))
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                (se::ancestor-after *soft*
                                    (stmt-starting-with-text *soft*
                                                             "for (j = 0")
                                    (stmt-with-text *soft*
                                                    "printf(\"%d\\n\", i+j)"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (se::ancestor-after *soft*
                                    (->> "int main"
                                         (stmt-starting-with-text *soft*)
                                         (function-body *soft*))
                                    (stmt-with-text *soft*
                                                    "printf(\"%d\\n\", i+j)"))))
    (is (equalp (stmt-with-text *soft* "return 0")
                (se::ancestor-after *soft*
                                    (->> "int main"
                                         (stmt-starting-with-text *soft*)
                                         (function-body *soft*))
                                    (stmt-with-text *soft* "return 0"))))))

(deftest ancestor-after-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "case 2")
                (se::ancestor-after *soft*
                                    (->> "switch"
                                         (stmt-starting-with-text *soft*)
                                         (get-immediate-children *soft*)
                                         (second))
                                    (->> "printf(\"%d\\n\", argc * argc)"
                                         (stmt-with-text *soft*)))))))

(deftest full-stmt-successors-fib-test ()
  (with-fixture fib-clang
    (is (equal '("DeclStmt" "WhileStmt" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *fib* "int x = 0")
                    (full-stmt-successors *fib*)
                    (apply #'append)
                    (mapcar #'ast-class))))
    (is (equal '("BinaryOperator" "BinaryOperator" "CompoundStmt"
                 "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *fib* "int t = x")
                    (full-stmt-successors *fib*)
                    (apply #'append)
                    (mapcar #'ast-class))))
    (is (equal '("CompoundStmt")
               (->> (stmt-with-text *fib* "return x")
                    (full-stmt-successors *fib*)
                    (apply #'append)
                    (mapcar #'ast-class))))))

(deftest full-stmt-successors-collatz-test ()
  (with-fixture collatz-clang
    (is (equal '("WhileStmt" "CallExpr" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *collatz* "int k = 0")
                    (full-stmt-successors *collatz*)
                    (apply #'append)
                    (mapcar #'ast-class))))
    (is (equal '("CompoundStmt" "UnaryOperator" "CompoundStmt"
                 "CallExpr" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *collatz* "m /= 2")
                    (full-stmt-successors *collatz*)
                    (apply #'append)
                    (mapcar #'ast-class))))
    (is (equal '("CompoundStmt")
               (->> (stmt-with-text *collatz* "return k")
                    (full-stmt-successors *collatz*)
                    (apply #'append)
                    (mapcar #'ast-class))))))

(deftest full-stmt-successors-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal '("DeclStmt" "ForStmt" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *soft* "int i")
                    (full-stmt-successors *soft*)
                    (apply #'append)
                    (mapcar #'ast-class))))
    (is (equal '("ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                    (full-stmt-successors *soft*)
                    (apply #'append)
                    (mapcar #'ast-class))))))

(deftest full-stmt-successors-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal '("SwitchStmt" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
                    (full-stmt-successors *soft*)
                    (apply #'append)
                    (mapcar #'ast-class))))
    (is (equal '("CaseStmt" "DefaultStmt" "CompoundStmt"
                 "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                    (full-stmt-successors *soft*)
                    (apply #'append)
                    (mapcar #'ast-class))))))

(deftest enclosing-full-stmt-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (stmt-with-text *collatz* "m = 3*m + 1")
                (enclosing-full-stmt *collatz*
                                     (stmt-with-text *collatz* "3"))))
    (is (equalp (stmt-with-text *collatz* "m = 3*m + 1")
                (enclosing-full-stmt *collatz*
                                     (stmt-with-text *collatz*
                                                     "m = 3*m + 1"))))
    (is (equalp (stmt-with-text *collatz* "int k = 0")
                (enclosing-full-stmt *collatz*
                                     (stmt-with-text *collatz* "int k = 0"))))))

(deftest enclosing-full-stmt-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                    (enclosing-full-stmt *soft*))))
    (is (equalp (->> (remove-if-not [{string= "ForStmt"}#'ast-class]
                                   (stmt-asts *soft*))
                    (first))
               (enclosing-full-stmt *soft*
                                    (->> (remove-if-not [{string= "ForStmt"}
                                                         #'ast-class]
                                                        (stmt-asts *soft*))
                                         (first)))))
    (is (equalp (->> (remove-if-not [{string= "ForStmt"}#'ast-class]
                                   (stmt-asts *soft*))
                    (second))
               (enclosing-full-stmt *soft*
                                    (->> (remove-if-not [{string= "ForStmt"}
                                                         #'ast-class]
                                                        (stmt-asts *soft*))
                                         (second)))))
    (is (equalp (stmt-with-text *soft* "int i")
               (enclosing-full-stmt *soft*
                                    (stmt-with-text *soft* "int i"))))))

(deftest enclosing-full-stmt-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
                     (enclosing-full-stmt *soft*))))
    (is (equalp (->> (remove-if-not [{string= "SwitchStmt"} #'ast-class]
                                    (stmt-asts *soft*))
                     (first))
                (enclosing-full-stmt *soft*
                                     (->> (remove-if-not [{string= "SwitchStmt"}
                                                          #'ast-class]
                                                         (stmt-asts *soft*))
                                          (first)))))
    (is (equalp (->> (remove-if-not [{string= "CaseStmt"} #'ast-class]
                                    (stmt-asts *soft*))
                     (first))
                (enclosing-full-stmt *soft*
                                     (->> (remove-if-not [{string= "CaseStmt"}
                                                          #'ast-class]
                                                         (stmt-asts *soft*))
                                          (first)))))))

(deftest enclosing-block-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (->> (stmt-starting-with-text *collatz* "int collatz")
                     (function-body *collatz*))
                (enclosing-block *collatz* (stmt-with-text *collatz*
                                                           "int k = 0"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (find-if [{string= "WhileStmt"} #'ast-class])
                     (get-immediate-children *collatz*)
                     (second))
                (enclosing-block *collatz* (stmt-with-text *collatz* "++k"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (find-if [{string= "IfStmt"} #'ast-class])
                     (get-immediate-children *collatz*)
                     (second))
                (enclosing-block *collatz* (stmt-with-text *collatz* "m /= 2"))))
    (is (null (enclosing-block *collatz*
                               (->> (stmt-starting-with-text *collatz* "int collatz")
                                    (function-body *collatz*)))))))

(deftest enclosing-block-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                     (enclosing-block *soft*))))
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (enclosing-block *soft*
                                 (stmt-starting-with-text *soft*
                                                          "for (i = 0"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (enclosing-block *soft*
                                 (stmt-starting-with-text *soft* "for (j = 0"))))
    (is (null (enclosing-block *soft*
                               (->> (stmt-starting-with-text *soft* "int main")
                                    (function-body *soft*)))))))

(deftest enclosing-block-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
                     (enclosing-block *soft*))))
    (is (equalp (->> (stmt-asts *soft*)
                     (find-if [{string= "SwitchStmt"} #'ast-class])
                     (get-immediate-children *soft*)
                     (second))
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                     (enclosing-block *soft*))))
    (is (equalp (->> (stmt-asts *soft*)
                     (find-if [{string= "SwitchStmt"} #'ast-class])
                     (get-immediate-children *soft*)
                     (second))
                (->> (stmt-asts *soft*)
                     (remove-if-not [{string= "CaseStmt"} #'ast-class])
                     (first)
                     (enclosing-block *soft*))))))

(deftest block-p-collatz-test ()
  (with-fixture collatz-clang
    (loop :for ast
          :in (stmt-asts *collatz*)
          :do (is (equal (string= "CompoundStmt" (ast-class ast))
                         (block-p *collatz* ast))))))

(deftest block-p-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (loop :for ast
          :in (stmt-asts *soft*)
          :do (is (equal (or (string= "CompoundStmt" (ast-class ast))
                             (string= "ForStmt" (ast-class ast)))
                         (block-p *soft* ast))))))

(deftest block-p-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (loop :for ast
          :in (stmt-asts *soft*)
          :do (is (equal (string= "CompoundStmt" (ast-class ast))
                         (block-p *soft* ast))))))

(deftest block-successor-collatz-test ()
  (with-fixture collatz-clang
    (is (equal "WhileStmt"
               (->> (block-successor *collatz* (stmt-with-text *collatz*
                                                               "int k = 0"))
                    (ast-class))))
    (is (equal "ReturnStmt"
               (->> (block-successor *collatz*
                                     (stmt-with-text *collatz*
                                                     "printf(\"%d\\n\", k)"))
                    (ast-class))))
    (is (equal nil
               (block-successor *collatz* (stmt-with-text *collatz*
                                                          "m /= 2"))))
    (is (equal nil
               (block-successor *collatz* (stmt-with-text *collatz*
                                                          "++k"))))
    (is (equal nil
               (block-successor *collatz* (stmt-with-text *collatz*
                                                          "return k"))))))

(deftest block-successor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                    (block-successor *soft*))))
    (is (equal nil
               (block-successor *soft*
                                (stmt-starting-with-text *soft* "for (j = 0"))))
    (is (equalp (stmt-with-text *soft* "return 0")
                (block-successor *soft*
                                 (stmt-starting-with-text *soft* "for (i = 0"))))))

(deftest block-successor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                    (block-successor *soft*))))
    (is (equalp (stmt-starting-with-text *soft* "default:")
                (->> (stmt-starting-with-text *soft* "case 2:")
                     (block-successor *soft*))))))

(deftest block-predeccessor-collatz-test ()
  (with-fixture collatz-clang
    (is (equal nil
               (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                             "int k = 0"))))
    (is (equal "WhileStmt"
               (->> (block-predeccessor *collatz*
                                     (stmt-with-text *collatz*
                                                     "printf(\"%d\\n\", k)"))
                    (ast-class))))
    (is (equal nil
               (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                             "m /= 2"))))
    (is (equal "IfStmt"
               (->> (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                                  "++k"))
                    (ast-class))))
    (is (equal "CallExpr"
               (->> (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                                  "return k"))
                    (ast-class))))))

(deftest block-predeccessor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                    (block-predeccessor *soft*))))
    (is (equal nil
               (block-successor *soft*
                                (stmt-starting-with-text *soft* "for (j = 0"))))
    (is (equalp (stmt-with-text *soft* "int j")
                (block-predeccessor *soft*
                                   (stmt-starting-with-text *soft* "for (i = 0"))))))

(deftest block-predeccessor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                    (block-predeccessor *soft*))))
    (is (equalp (stmt-starting-with-text *soft* "case 1:")
               (->> (stmt-starting-with-text *soft* "case 2:")
                    (block-predeccessor *soft*))))))

(deftest crossover-2pt-outward-fib-test ()
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *fib*)
                                         (copy *fib*)
                                         (stmt-with-text *fib* "x = x + y")
                                         (stmt-with-text *fib* "return x")
                                         (stmt-with-text *fib* "x = x + y")
                                         (stmt-with-text *fib* "return x"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "x = x + y")
                          (stmt-with-text *fib* "return x"))
                    effective-a-pts)))))
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *fib*)
            (copy *fib*)
            (stmt-with-text *fib* "int t = x")
            (->> (stmt-asts *fib*)
                 (remove-if-not [{string= "WhileStmt"}
                                 #'ast-class])
                 (first))
            (stmt-with-text *fib* "int t = x")
            (->> (stmt-asts *fib*)
                 (remove-if-not [{string= "WhileStmt"}
                                 #'ast-class])
                 (first)))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int t = x")
                          (->> (stmt-asts *fib*)
                               (remove-if-not [{string= "WhileStmt"}
                                               #'ast-class])
                               (first)))
                   effective-a-pts))))))

(deftest crossover-2pt-outward-collatz-test ()
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "m /= 2")
                                         (stmt-with-text *collatz* "++k")
                                         (stmt-with-text *collatz* "m /= 2")
                                         (stmt-with-text *collatz* "++k"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (equalp (cons (stmt-with-text *collatz* "m /= 2")
                          (stmt-with-text *collatz* "++k"))
                    effective-a-pts)))))
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "++k")
                                         (stmt-with-text *collatz* "return k")
                                         (stmt-with-text *collatz* "++k")
                                         (stmt-with-text *collatz* "return k"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "++k")
                          (stmt-with-text *collatz* "return k"))
                    effective-a-pts))))))

(deftest crossover-2pt-outward-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
            (stmt-with-text *soft* "return 0")
            (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
            (stmt-with-text *soft* "return 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                          (stmt-with-text *soft* "return 0"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-starting-with-text *soft* "for (j = 0")
            (stmt-with-text *soft* "return 0")
            (stmt-starting-with-text *soft* "for (j = 0")
            (stmt-with-text *soft* "return 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-starting-with-text *soft* "for (j = 0")
                          (stmt-with-text *soft* "return 0"))
                    effective-a-pts))))))

(deftest crossover-2pt-outward-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
            (stmt-with-text *soft* "return 0")
            (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
            (stmt-with-text *soft* "return 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                          (stmt-with-text *soft* "return 0"))
                    effective-a-pts))))))

(deftest crossover-2pt-inward-fib-test ()
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *fib*)
                                         (copy *fib*)
                                         (stmt-with-text *fib* "int x = 0")
                                         (stmt-with-text *fib* "return x")
                                         (stmt-with-text *fib* "int x = 0")
                                         (stmt-with-text *fib* "return x"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int x = 0")
                          (stmt-with-text *fib* "return x"))
                    effective-a-pts)))))
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *fib*)
                                         (copy *fib*)
                                         (stmt-with-text *fib* "int x = 0")
                                         (stmt-with-text *fib* "x = x + y")
                                         (stmt-with-text *fib* "int x = 0")
                                         (stmt-with-text *fib* "x = x + y"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int x = 0")
                          (stmt-with-text *fib* "x = x + y"))
                    effective-a-pts))))))

(deftest crossover-2pt-inward-collatz-test ()
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "int k = 0")
                                         (stmt-with-text *collatz* "return k")
                                         (stmt-with-text *collatz* "int k = 0")
                                         (stmt-with-text *collatz* "return k"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "int k = 0")
                          (stmt-with-text *collatz* "return k"))
                    effective-a-pts)))))
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "int k = 0")
                                         (stmt-with-text *collatz*
                                                         "m = 3*m + 1")
                                         (stmt-with-text *collatz* "int k = 0")
                                         (stmt-with-text *collatz*
                                                         "m = 3*m + 1"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "int k = 0")
                          (stmt-with-text *collatz* "m = 3*m + 1"))
                    effective-a-pts)))))
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "int k = 0")
                                         (stmt-with-text *collatz* "++k")
                                         (stmt-with-text *collatz* "int k = 0")
                                         (stmt-with-text *collatz* "++k"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "int k = 0")
                          (stmt-with-text *collatz* "++k"))
                    effective-a-pts))))))

(deftest crossover-2pt-inward-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "int i")
            (stmt-with-text *soft* "return 0")
            (stmt-with-text *soft* "int i")
            (stmt-with-text *soft* "return 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i")
                          (stmt-with-text *soft* "return 0"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "int i")
            (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
            (stmt-with-text *soft* "int i")
            (stmt-with-text *soft* "printf(\"%d\\n\", i+j)"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i")
                          (stmt-with-text *soft* "printf(\"%d\\n\", i+j)"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "int i")
            (stmt-starting-with-text *soft* "for (i = 0")
            (stmt-with-text *soft* "int i")
            (stmt-starting-with-text *soft* "for (i = 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i")
                          (stmt-starting-with-text *soft* "for (i = 0"))
                   effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "int i")
            (stmt-starting-with-text *soft* "for (j = 0")
            (stmt-with-text *soft* "int i")
            (stmt-starting-with-text *soft* "for (j = 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i")
                          (stmt-starting-with-text *soft* "for (j = 0"))
                    effective-a-pts))))))

(deftest crossover-2pt-inward-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
            (stmt-with-text *soft* "printf(\"%d\\n\", argc * argc)")
            (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
            (stmt-with-text *soft* "printf(\"%d\\n\", argc * argc)"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft*
                                          "printf(\"%d\\n\", argc + argc)")
                          (stmt-with-text *soft*
                                          "printf(\"%d\\n\", argc * argc)"))
                    effective-a-pts)))))
  (with-fixture crossover-switch-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
            (stmt-with-text *soft* "return 0")
            (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
            (stmt-with-text *soft* "return 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
                          (stmt-with-text *soft* "return 0"))
                    effective-a-pts))))))

(deftest basic-2pt-crossover-works ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int d"))
           (a-stmt2 (stmt-with-text *scopes* "d = 5"))
           (b-stmt1 (stmt-with-text *scopes* "int e"))
           (b-stmt2 (stmt-with-text *scopes* "c = 10"))
           (target-a-pts (cons a-stmt1 a-stmt2)))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (equalp effective-a-pts target-a-pts))))))

(deftest crossover-can-match-nesting ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "--d"))
           (a-stmt2 (stmt-with-text *scopes* "int e"))
           (b-stmt1 (stmt-with-text *scopes* "c = 6"))
           (b-stmt2 (stmt-with-text *scopes* "e = 8"))
           (target-a-pts
            (cons (stmt-starting-with-text *scopes* "while (d > 0)")
                  a-stmt2)))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable variant a-pts b-pts))
        (is ok)
        (is (equalp effective-a-pts target-a-pts)))))
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "--d"))
           (a-stmt2 (stmt-with-text *scopes* "int e"))
           (b-stmt1 (stmt-with-text *scopes* "a = 13"))
           (b-stmt2 (stmt-with-text *scopes* "c = 15"))
           (target-a-pts
            (cons (stmt-starting-with-text *scopes* "for (b = 2;")
                  (stmt-starting-with-text *scopes* "if (b == 7)"))))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable variant a-pts b-pts))
        (is ok)
        (is (equalp effective-a-pts target-a-pts))))))

(deftest crossover-can-rebind-text ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int b"))
           (a-stmt2 (stmt-with-text *scopes* "int c"))
           (b-stmt1 (stmt-with-text *scopes* "a = 13"))
           (b-stmt2 (stmt-with-text *scopes* "a = 13")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant)))))
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int b"))
           (a-stmt2 (stmt-with-text *scopes* "int c"))
           (b-stmt1 (stmt-with-text *scopes* "d = 5"))
           (b-stmt2 (stmt-with-text *scopes* "--d")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))))))

(deftest crossover-the-world ()
  ;; Entire text of a function
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int a"))
           (a-stmt2 (stmt-with-text *scopes* "return a + b + c"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A single statement (the first one)
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int a"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A single statement (the last one)
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "return a + b + c"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A single complex statement
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A statement and one of its descendants
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (a-stmt2 (stmt-with-text *scopes* "c = 4"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A statement and one of its ancestors
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "c = 4"))
           (a-stmt2 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; Replace a single statement with multiple statements
  (with-fixture scopes-clang
    (let ((a-stmt (stmt-with-text *scopes* "int b"))
          (b-stmt1 (stmt-with-text *scopes* "c = 10"))
          (b-stmt2 (stmt-with-text *scopes* "g = 12")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt a-stmt
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (> (length (asts variant))
               (length (asts *scopes*))))
        ;; a is the only var in scope so all these assignments should
        ;; be rebound.
        (is (stmt-with-text variant "a = 10"))
        (is (stmt-with-text variant "a = 11"))
        (is (stmt-with-text variant "a = 12"))
        (is (not (stmt-with-text variant "int b"))))))
  ;; Replace multiple statements with a single statement
  (with-fixture scopes-clang
    (let ((a-stmt1 (stmt-with-text *scopes* "int b"))
          (a-stmt2 (stmt-with-text *scopes* "b = 1"))
          (b-stmt (stmt-with-text *scopes* "e = 8")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt b-stmt)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (< (length (asts variant))
               (length (asts *scopes*))))
        ;; a is the only var in scope so this assignment should
        ;; be rebound.
        (is (stmt-with-text variant "a = 8"))
        (is (not (stmt-with-text variant "int b")))
        (is (not (stmt-with-text variant "b = 1")))))))

(deftest intraprocedural-2pt-crossover-does-not-crash ()
  (with-fixture intraprocedural-2pt-crossover-bug-clang
    (let ((variant (intraprocedural-2pt-crossover
                     (copy *soft*)
                     (copy *soft*)
                     (stmt-with-text *soft* "printf(\"%d\\n\", argc + 1 < NUM)")
                     (stmt-with-text *soft* "argc--")
                     (stmt-with-text *soft* "printf(\"%d\\n\", argc + 1 < NUM)")
                     (stmt-with-text *soft* "argc--"))))
      (is (string/= (genome variant)
                    "")))))


(in-suite test)

(deftest single-decl-works ()
  (with-fixture scopes-clang
    (is (= 1 (length (ast-declares (stmt-with-text *scopes* "int a")))))))

(deftest multiple-decl-works ()
  (with-fixture scopes-clang
    (when-let* ((ast (stmt-with-text *scopes* "int f, g")))
      (is (= 2 (length (ast-declares ast)))))))

(deftest pick-for-loop-works ()
  (with-fixture scopes-clang
    (is (string= "ForStmt" (->> (se::pick-for-loop *scopes*)
                                (aget :stmt1)
                                (ast-class)))
        "Simply able to pick a for loop.")
    (let ((var (copy *scopes*)))
      (apply-mutation var (make-instance 'explode-for-loop :object var))
      (is (not (string= (genome var) (genome *scopes*)))
          "Exploded for loop changes genome.")
      (is (not (scan (quote-meta-chars "for") (genome var)))
          "Exploded for loop contains no for loop.")
      (flet ((run-and-get-return (obj)
               (with-temp-file (bin)
                 (phenome obj :bin bin)
                 (multiple-value-bind (stdout stderr return)
                     (shell bin)
                   (declare (ignorable stdout stderr))
                   return))))
        (is (= (run-and-get-return var) (run-and-get-return *scopes*))
            "Exploded for loop doesn't change program behavior.")))))

(deftest explode-for-loop-mutation-works ()
  "Test conversion of for loop variants computing factorials to while loops"
  (let ((simple-loop       (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "simple-loop.c")))
        (no-initialization (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "loop-no-initialization.c")))
        (no-conditional    (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "loop-no-conditional.c")))
        (no-increment      (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "loop-no-increment.c")))
        (no-body           (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "loop-no-body.c"))))
    (apply-mutation simple-loop
                    (make-instance 'explode-for-loop :object simple-loop))
    (apply-mutation no-initialization
                    (make-instance 'explode-for-loop :object no-initialization))
    (apply-mutation no-conditional
                    (make-instance 'explode-for-loop :object no-conditional))
    (apply-mutation no-increment
                    (make-instance 'explode-for-loop :object no-increment))
    (apply-mutation no-body
                    (make-instance 'explode-for-loop :object no-body))

    (flet ((run-factorial (obj n)
             (with-temp-file (bin)
               (phenome obj :bin bin)
               (multiple-value-bind (stdout stderr exit)
                   (shell (format nil "~a ~d" bin n))
                 (declare (ignorable stdout stderr))
                 exit))))

      (is (= 120 (run-factorial simple-loop 5)))
      (is (= 120 (run-factorial no-initialization 5)))
      (is (= 120 (run-factorial no-conditional 5)))
      (is (= 120 (run-factorial no-increment 5)))
      (is (= 120 (run-factorial no-body 5)))
      (is (not (scan (quote-meta-chars "for") (genome simple-loop))))
      (is (not (scan (quote-meta-chars "for") (genome no-initialization))))
      (is (not (scan (quote-meta-chars "for") (genome no-conditional))))
      (is (not (scan (quote-meta-chars "for") (genome no-increment))))
      (is (not (scan (quote-meta-chars "for") (genome no-body)))))))

(deftest pick-while-loop-works ()
  (with-fixture scopes-clang
    (is (string= "WhileStmt" (->> (se::pick-while-loop *scopes*)
                                  (aget :stmt1)
                                  (ast-class)))
        "Simply able to pick a while loop.")
    (let ((var (copy *scopes*)))
      (apply-mutation var (make-instance 'coalesce-while-loop :object var))
      (is (not (string= (genome var) (genome *scopes*)))
          "Coalesced while loop changes genome.")
      (is (not (scan (quote-meta-chars "while") (genome var)))
          "Coalesced while loop contains no while loop.")
      (flet ((run-and-get-return (obj)
               (with-temp-file (bin)
                 (phenome obj :bin bin)
                 (multiple-value-bind (stdout stderr return)
                     (shell bin)
                   (declare (ignorable stdout stderr))
                   return))))
        (is (= (run-and-get-return var) (run-and-get-return *scopes*))
            "Coalesced while loop doesn't change program behavior.")))))

(deftest pick-while-loop-works-even-with-empty-body ()
  (with-fixture empty-while-clang
    (let ((var (copy *empty-while*)))
      (apply-mutation var (make-instance 'coalesce-while-loop :object var))
      (is (not (scan (quote-meta-chars "while") (genome var)))
          "Coalesced while loop contains no while loop."))))

(deftest pick-while-loop-works-even-with-no-precedent ()
  (with-fixture while-with-no-precedent-clang
    (let ((var (copy *soft*)))
      (apply-mutation var (make-instance 'coalesce-while-loop :object var))
      (is (not (scan (quote-meta-chars "while") (genome var)))
          "Coalesced while loop contains no while loop."))))

(deftest delete-decl-stmts-works ()
  (with-fixture scopes-clang
    (let ((variant (copy *scopes*)))
      (apply-mutation
          variant
        `(cut-decl (:stmt1 . ,(stmt-with-text *scopes* "int a"))))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))
    (let ((variant (copy *scopes*)))
      (apply-mutation
          variant
        `(cut-decl (:stmt1 . ,(stmt-with-text *scopes* "int d"))))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))
    (when-let* ((variant (copy *scopes*))
                (id (stmt-with-text *scopes* "int f, g")))
      (apply-mutation variant `(cut-decl (:stmt1 . ,id)))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest swap-decls-works ()
  (with-fixture scopes-clang
    ;; Check if the basic swap-decls mutation works.
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (stmt-with-text *scopes* "int a"))))
      (apply-mutation variant
                      (make-instance 'swap-decls :object variant))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest rename-variable-works ()
  (with-fixture scopes-clang
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (stmt-with-text *scopes* "b = 1"))))
      (apply-mutation variant
                      (make-instance 'rename-variable :object variant))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest expand-arithmatic-op-throws-error-if-no-arithmatic-ops ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "no-compound-assign.c"))))
    (signals no-mutation-targets
      (build-op (make-instance 'expand-arithmatic-op :object obj) obj))))

(deftest expand-arithmatic-op-works-simple-compound-assignment ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir
                         "simple-compound-assign.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc * 2"))))

(deftest expand-arithmatic-op-works-complex-compound-assignment ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir
                         "complex-compound-assign.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc + ((argc*4) / rand())"))))

(deftest expand-arithmatic-op-works-increment ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "increment.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "i = i + 1"))))

(deftest expand-arithmatic-op-works-decrement ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "decrement.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc - 1"))))

(deftest expand-arithmatic-op-works-field-increment ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "field-increment.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "t.x = t.x + 1"))))

(deftest expand-arithmatic-op-works-field-decrement ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "field-decrement.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "t.x = t.x - 1"))))

(deftest expand-arithmatic-op-works-class-member-increment ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "class-member-increment.cpp"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "x = x + 1"))))

(deftest expand-arithmatic-op-works-class-member-decrement ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "class-member-decrement.cpp"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "x = x - 1"))))


;;; Adaptive-mutation tests.
(in-suite test)
(defsuite* test-adaptive-mutation)

(deftest bad-cut-changes-mutation-probability ()
  (let* ((se::*mutation-results-queue* #((cut . :worse) (cut . :dead)))
         (muts-0 '((cut . 1/2) (swap . 1)))
         (muts-1 (update-mutation-types muts-0))
         (muts-2 (update-mutation-types muts-1)))
    (is (< (aget 'cut muts-1) (aget 'cut muts-0))
        "Bad mutations lose probability.")
    (is (< (aget 'cut muts-2) (aget 'cut muts-1))
        "Bad mutations continue to lose probability.")))

(deftest mutation-queue-wraps-as-expected ()
  (let ((se::*mutation-results-queue*
         (make-array 100
                     :element-type '(cons symbol symbol)
                     :initial-element (cons :nothing :nothing)))
        (se::*mutation-results-queue-next* 0))
    (dotimes (n 100)
      (se::queue-mutation 'cut :dead))
    (is (every [{equal 'cut} #'car] se::*mutation-results-queue*)
        "`queue-mutation' fills `*mutation-results-queue*' as expected.")
    (se::queue-mutation 'swap :better)
    (is (equalp (cons 'swap :better) (aref se::*mutation-results-queue* 0))
        "`queue-mutation' wraps `*mutation-results-queue*' as expected.")))

(deftest update-mutation-types-returns-list-when-mutation-queue-unpopulated ()
  "Ensure update-mutation-types returns its first argument when the
*mutation-results-queue* is unpopulated"
  (let ((se::*mutation-results-queue*
          (copy-seq se::+initial-mutation-results-queue+))
        (se::*mutation-results-queue-next* 0)
        (mutation-types (copy-seq *clang-mutation-types*)))
    (is (equalp mutation-types
                (update-mutation-types mutation-types)))))

(deftest update-mutation-types-returns-list-when-mutation-queue-populated ()
  "Ensure update-mutation-types returns a list when the
*mutation-results-queue* is populated"
  (let ((se::*mutation-results-queue*
          (copy-seq se::+initial-mutation-results-queue+))
        (se::*mutation-results-queue-next* 0)
        (mutation-types (copy-seq *clang-mutation-types*)))
    (dotimes (n (length se::+initial-mutation-results-queue+))
      (se::queue-mutation 'cut :dead))
    (is (listp (update-mutation-types mutation-types)))))

(deftest adaptive-analyze-mutation-updates-results-queue-properly ()
  (let ((*fitness-predicate* #'<)
        (se::*mutation-results-queue-next* 0)
        (se::*mutation-results-queue*
         (copy-seq se::+initial-mutation-results-queue+))
        (parent-a (make-instance 'clang :fitness 2))
        (parent-b (make-instance 'clang :fitness 2))
        (crossed  (make-instance 'clang :fitness 1))
        (mutant   (make-instance 'clang :fitness 0)))
      (adaptive-analyze-mutation mutant
                                 `(clang-cut ,parent-a 0
                                   ,crossed ,parent-b 0)
                                 {fitness})
      (is (equal :better (cdr (aref se::*mutation-results-queue* 0))))))


;;; Database tests
(in-suite test)
(defsuite* test-database)

(defixture json-database
  (:setup
    (setf *database*
          (with-open-file (in (make-pathname :name "euler-example.json"
                                             :directory +etc-dir+))
            (make-instance 'json-database :json-stream in))))
  (:teardown
    (setf *database* nil)))

(defixture mongo-database
  (:setup
   (setf *database*
         (let ((host "dog")
               (port 27017))
           (handler-case (make-instance 'mongo-database
                           :db "euler_test_clang_O0_no_pic"
                           :host host
                           :port port)
             (usocket:ns-host-not-found-error (e)
               (declare (ignorable e))
               (warn "Host of Mongo database, ~a:~a, not found." host port)
               nil)))))
  (:teardown
   (setf *database* nil)))

(deftest json-database-find-snippet-respects-class ()
  (with-fixture json-database
    (is (null (-<>> (find-snippets *database* :ast-class "CompoundStmt")
                    (remove "CompoundStmt" <> :test #'string=
                                              :key {aget :ast-class}))))))

(deftest json-database-find-snippet-respects-decl ()
  (with-fixture json-database
    (is (null (->> (find-snippets *database* :decls nil)
                   (remove-if-not {aget :is-decl}))))))

(deftest json-database-find-snippet-respects-full-stmt ()
  (with-fixture json-database
    (is (null (->> (find-snippets *database* :full-stmt t)
                   (remove-if {aget :full-stmt}))))))

(deftest json-database-find-snippet-is-random ()
  (with-fixture json-database
    (let ((picks (loop :for i :from 0 :to 5
                       :collect (aget :hash (find-snippets *database*
                                                           :limit 1)))))
      (equal picks (remove-duplicates picks)))))

(deftest mongo-database-find-snippet-respects-class ()
  (with-fixture mongo-database
    (when *database*
      (is (null (-<>> (find-snippets *database* :ast-class "CompoundStmt")
                      (remove "CompoundStmt" <> :test #'string=
                              :key {aget :ast-class})))))))

(deftest mongo-database-find-snippet-respects-decl ()
  (with-fixture mongo-database
    (when *database*
      (is (null (->> (find-snippets *database* :decls nil)
                     (remove-if-not {aget :is-decl})))))))

(deftest mongo-database-find-snippet-respects-full-stmt ()
  (with-fixture mongo-database
    (when *database*
      (is (null (->> (find-snippets *database* :full-stmt t)
                     (remove-if {aget :full-stmt})))))))

(deftest mongo-database-find-snippet-is-random ()
  (with-fixture mongo-database
    (when *database*
      (let ((picks (loop :for i :from 0 :to 5
                         :collect (aget :hash (find-snippets *database*
                                                             :limit 1)))))
        (equal picks (remove-duplicates picks))))))


(in-suite test)
(defsuite* test-clang-instrumentation)

;;; Instrumentation tests
(in-suite test)
(defsuite* test-instrumentation)

(defun count-traceable (obj)
  "Return a count of full statements parented by compound statements"
  (count-if {can-be-made-traceable-p obj} (asts obj)))

(defun read-trace (string)
  "Read a trace into a lisp objects."
  (let ((start 0))
    (iter (for (values piece end) =
               (read-from-string string nil :eof :start start))
          (until (eql piece :eof))
          (setf start end)
          (collect piece))))

(deftest instrumentation-insertion-test ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*))))
      ;; Do we insert the right number of printf statements?
      (is (<= (* 2 (count-traceable *gcd*))
              (count-traceable instrumented)))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (let ((trace (read-trace stderr)))
            (is (listp trace))
            (is (= (length trace)
                   (length (split-sequence
                               #\Newline stderr
                               :remove-empty-subseqs t))))))))))

(deftest instrumentation-insertion-w-filter-test ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*)
                                    :filter {remove-if-not
                                             [{eq 93} #'ast-counter]})))
      ;; Do we insert the right number of printf statements?
      (is (eq (+ 3 (count-traceable *gcd*))
              (count-traceable instrumented)))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (let ((trace (read-trace stderr)))
            (is (listp trace))
            (is (= (length trace)
                   (length (split-sequence
                               #\Newline stderr
                               :remove-empty-subseqs t))))))))))

(deftest instrumentation-insertion-w-function-exit-test ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*)
                                    :instrument-exit t)))
      ;; Do we insert the right number of printf statements?
      (is (<= (* 2 (count-traceable *gcd*))
              (count-traceable instrumented)))

      ;; Is function exit instrumented?
      (is (stmt-with-text instrumented
                          (format nil "fputs(\"((:C . ~a)) \", stderr)"
                                  (-<>> (first (functions *gcd*))
                                        (function-body *gcd*)
                                        (position <> (asts *gcd*)
                                                  :test #'equalp)))))

      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (let ((trace (read-trace stderr)))
            (is (listp trace))
            (is (= (length trace)
                   (length (split-sequence
                               #\Newline stderr
                               :remove-empty-subseqs t))))))))))

(deftest instrumentation-insertion-w-points-test ()
  (with-fixture gcd-clang
    (let ((instrumented
           (handler-bind ((warning #'muffle-warning))
             (instrument (copy *gcd*)
               :points
               (iter (for ast in (stmt-asts *gcd*))
                     (for i upfrom 0)
                     (collect (list ast (if (evenp i) :odd :even))))))))
      ;; Do we insert the right number of printf statements?
      (is (<= (* 3 (count-traceable *gcd*))
              (count-traceable instrumented)))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (let ((trace (read-trace stderr)))
            (is (listp trace))
            (is (= (length trace)
                   (length (split-sequence
                               #\Newline stderr
                               :remove-empty-subseqs t))))))))))

(deftest instrumentation-insertion-w-trace-file-test ()
  (with-fixture gcd-clang
    (with-temp-file (trace)
      (with-temp-file (bin)
        (let ((instrumented
               (instrument (copy *gcd*) :trace-file trace)))
          (is (scan (quote-meta-chars trace) (genome-string instrumented)))
          (is (zerop (second (multiple-value-list
                              (phenome instrumented :bin bin)))))
          (is (probe-file bin))
          (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
            (declare (ignorable stdout stderr))
            (is (zerop errno))
            (is (probe-file trace))))))))

(deftest instrumentation-handles-missing-curlies-test ()
  (with-fixture gcd-wo-curlies-clang
    (let ((instrumented (instrument (copy *gcd*))))
      ;; Ensure we were able to instrument an else branch w/o curlies.
      (let* ((else-counter (position (stmt-with-text *gcd* "b = b - a")
                                     (asts *gcd*) :test #'equalp))
             (matcher (quote-meta-chars (format nil "(:C . ~d)" else-counter))))
        (is (scan matcher (genome instrumented)))
        ;; The next line (after flushing) should be the else branch.
        (let ((location (position-if {scan matcher} (lines instrumented))))
          (is (scan (quote-meta-chars "b = b - a")
                    (nth (+ 3 location) (lines instrumented))))))
      ;; Finally, lets be sure we still compile.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (is (listp (read-trace stderr))))))))

(deftest instrumentation-insertion-w-points-and-added-blocks-test ()
  (with-fixture gcd-wo-curlies-clang
    (let* ((cookie :test-cookie)
           (instrumented
            (instrument (copy *gcd*)
              :points
              `((,(stmt-with-text *gcd* "b - a") ,cookie)))))
      ;; Instrumented program holds the TEST-COOKIE line.
      (is (scan (quote-meta-chars (string cookie))
                (genome-string instrumented))
          "The point trace string ~S appears in the instrumented program text."
          (string cookie))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (is (scan (quote-meta-chars (string cookie)) stderr)
              "The point trace string ~S appears in the program output."
              (string cookie)))))))

(defparameter unbound-vals-fn
  [{mapcar [#'peel-bananas #'car]} #'se::ast-unbound-vals]
  "Function to pull unbound variables from an AST for use in `var-instrument'.")

(deftest instrumentation-print-unbound-vars ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list {var-instrument *gcd* :unbound-vals unbound-vals-fn})))
    (is (scan (quote-meta-chars "fprintf(stderr, \"(:UNBOUND-VALS")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
        (declare (ignorable stdout))
        (is (zerop errno))
        (let ((trace (read-trace stderr)))
          (is (listp trace) "We got a trace.")
          (is (= (length trace) (count-if {assoc :c} trace))
              "Counter in every trace element.")
          (is (= (length trace) (count-if {assoc :UNBOUND-VALS} trace))
              "Variable list in every trace element.")
          (is (> (length trace) (count-if {aget :UNBOUND-VALS} trace))
              "Variable list not populated in every trace element."))))))

(deftest instrumentation-print-in-scope-vars ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list {var-instrument *gcd* :scopes
                                        {get-vars-in-scope *gcd*}})))
    (is (scan (quote-meta-chars "fprintf(stderr, \"(:SCOPES")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
        (declare (ignorable stdout))
        (is (zerop errno))
        (let ((trace (read-trace stderr)))
          (is (listp trace) "We got a trace.")
          (is (= (length trace) (count-if {assoc :c} trace))
              "Counter in every trace element.")
          (is (= (length trace) (count-if {assoc :SCOPES} trace))
              "Variable list in every trace element.")
          (is (= (length trace) (count-if {aget :SCOPES} trace))
              "Variable list populated in every trace element.")
          (is (not (null (mappend {aget :SCOPES} trace)))
              "Variable list not always empty."))))))

(deftest instrumentation-print-argv ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (genome (instrument *gcd* :print-argv t)))
    (is (scan (quote-meta-chars "fprintf(stderr, \"((:INPUT")
              (genome-string *gcd*))
        "We find code to print input in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
        (declare (ignorable stdout))
        (is (zerop errno))
        (let ((trace (read-trace stderr)))
          (is (listp trace) "We got a trace.")
          (is (= 1 (count-if {assoc :input} trace))
              "Input shown once in trace.")
          (is (car (mapcar [{= 3} {length} {aget :input}]
                           (remove-if-not {aget :input} trace)))
              "GCD has 3 inputs."))))))

(deftest instrumentation-handles-binary-search ()
  (with-fixture binary-search-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *binary-search*
        :functions
        (list
         {var-instrument *binary-search* :unbound-vals unbound-vals-fn})))))


;;; Tests of declaration and type databases on clang objects
(deftest binary-search-collects-types ()
  (with-fixture binary-search-clang
    (let ((collected-vars (mapcar #'car (hash-table-alist
                                         (declarations *binary-search*))))
          ;; TODO: Add "argv" to this list once the outstanding issue
          ;;       with char* types in clang-mutate is resolved.
          (variables (list "haystack" "i" "argc")))
      (mapc (lambda (var)
              (is (member var collected-vars :test #'equal)
                  "Includes ~s variable in the `declarations' hash." var))
            variables)
      (mapc (lambda (var)
              (is (type-of-var *binary-search* var)
                  "Includes ~s variable in the `declarations' hash." var))
            variables))))

(deftest huf-knows-types ()
  (with-fixture huf-clang
    (is (and (listp (types *huf*)) (not (null (types *huf*))))
        "Huf software objects has a type database.")
    (is (= 7 (count-if #'type-pointer (types *huf*)))
        "Huf has seven pointer types.")
    (is (= 3 (count-if [#'not #'emptyp #'type-array] (types *huf*)))
        "Huf has three array types.")
    (is (= 3 (count-if [{string= "int"} #'type-name] (types *huf*)))
        "Huf has three different \"int\" types (some are array and pointer).")))

(deftest huf-finds-type-info-for-variables ()
  (with-fixture huf-clang
    (let ((type (type-of-var *huf* "strbit")))
      (is type "Found type for \"strbit\" in huf.")
      (is (string= "[]" (type-array type))
          "Variable \"strbit\" in huf is a dynamically sized array.")
      (is (not (type-pointer type))
          "Variable \"strbit\" in huf is not a pointer."))))


;; Lisp representation
(in-suite test)
(defsuite* test-lisp)

(defvar *clang-expr*  nil "The clang expression (lisp) software object.")
(defixture clang-expr
  (:setup
    (setf *clang-expr*
          (make-instance 'clang-expression
            :genome (copy-tree '(:+ 1 (:* 2 (:- 3 :y)))))))
  (:teardown
   (setf *clang-expr* nil)))

(deftest lisp-cut-first ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-cut :targets 0))
    (is (equal (genome *clang-expr*) '(1 (:* 2 (:- 3 :y)))))))

(deftest lisp-cut-leaf ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-cut :targets 1))
    (is (equal (genome *clang-expr*) '(:+ (:* 2 (:- 3 :y)))))))

(deftest lisp-cut-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-cut :targets 2))
    (is (equal (genome *clang-expr*) '(:+ 1)))))

#+(or ) ; TODO: Fix this (unused) function before turning on this test.
(deftest lisp-cut-function ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-cut :targets 3))
    (is (equal (genome *clang-expr*) '(:+ 1 (2 (:- 3 :y)))))))

(deftest lisp-swap-leaves ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-swap :targets '(1 4)))
    (is (equal (genome *clang-expr*) '(:+ 2 (:* 1 (:- 3 :y)))))))

(deftest lisp-swap-leaf-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-swap :targets '(1 5)))
    (is (equal (genome *clang-expr*) '(:+ (:- 3 :y) (:* 2 1))))))

(deftest lisp-swap-functions ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-swap :targets '(3 6)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 2 (:* 3 :y)))))))

;; FIXME: what is the correct behavior here?
(deftest lisp-swap-parent-child ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-swap :targets '(2 5)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 3 :y))))))

(deftest lisp-replace-leaves ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-replace :targets '(1 4)))
    (is (equal (genome *clang-expr*) '(:+ 2 (:* 2 (:- 3 :y)))))))

(deftest lisp-replace-leaf-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-replace :targets '(1 5)))
    (is (equal (genome *clang-expr*) '(:+ (:- 3 :y) (:* 2 (:- 3 :y)))))))

(deftest lisp-replace-parent-child ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-replace :targets '(2 5)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 3 :y))))))

(deftest lisp-replace-function ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-replace :targets '(3 6)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 2 (:- 3 :y)))))))


;; Mutations of clang expressions in Lisp form
(in-suite test)
(defsuite* test-clang-expression)

(deftest change-operator-first ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr*
      (make-instance 'change-operator :targets '(0 :-)))
    (is (equal (genome *clang-expr*) '(:- 1 (:* 2 (:- 3 :y)))))))

(deftest change-operator-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'change-operator :targets '(3 :+)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:+ 2 (:- 3 :y)))))))

(deftest double-constant ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'change-constant :targets '(7 :double)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:* 2 (:- 6 :y)))))))

(deftest halve-constant ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'change-constant :targets '(7 :halve)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:* 2 (:- 1 :y)))))))

(deftest mult-divide-leaf ()
        (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'mult-divide :targets 4))
    (is (equal (genome *clang-expr*) '(:+ 1 (:* (:/ (:* 2 2) 2) (:- 3 :y)))))))

(deftest mult-divide-subtree ()
        (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'mult-divide :targets 2))
    (is (equal (genome *clang-expr*) '(:+ 1 (:/ (:* (:* 2 (:- 3 :y)) 2) 2))))))

(deftest add-subtract-subtree ()
        (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'add-subtract :targets 2))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- (:+ (:* 2 (:- 3 :y)) 1) 1))))))

(deftest subtract-add-subtree ()
        (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'subtract-add :targets 2))
    (is (equal (genome *clang-expr*) '(:+ 1 (:+ (:- (:* 2 (:- 3 :y)) 1) 1))))))


;; Evaluation of clang expressions in Lisp form
(deftest eval-number-clang ()
  (is (equal (evaluate-expression (make-instance 'clang-expression) nil 1)
             '(1 "int"))))

(deftest eval-var-clang ()
  (is (equal (evaluate-expression
              (make-instance 'clang-expression) '((:a 1 "int")) :a)
             '(1 "int"))))

(deftest eval-function-clang ()
  (is (equal (evaluate-expression
              (make-instance 'clang-expression) '((:a 2 "int")) '(:+ 1 :a))
             '(3 "int"))))

(deftest eval-division-truncates-clang ()
  (is (equal (evaluate-expression
              (make-instance 'clang-expression) nil '(:/ 3 2)) '(1 "int")))
  (is (equal (evaluate-expression
              (make-instance 'clang-expression) nil '(:/ -3 2)) '(-1 "int"))))

(deftest eval-interior-max-clang ()
  (multiple-value-bind (result interior-max)
      (evaluate-expression
       (make-instance 'clang-expression) '((:a 3 "int"))  '(:- (:* 2 :a) 2))
    (is (equal result '(4 "int")))
    (is (equal interior-max 6))))

(deftest eval-signals-on-undefined-variable-clang ()
  (signals eval-error
    (evaluate-expression (make-instance 'clang-expression) nil  :a)))

(deftest eval-signals-on-unknown-type-clang ()
  (signals eval-error
    (evaluate-expression (make-instance 'clang-expression) nil  "test")))

(deftest eval-signals-on-unknown-function-clang ()
  (signals eval-error
    (evaluate-expression (make-instance 'clang-expression) nil  '(:test 1 2))))

(deftest eval-signals-on-wrong-arity-clang ()
  (signals eval-error
    (evaluate-expression (make-instance 'clang-expression) nil  '(:+ 1 2 3))))

(deftest eval-signals-on-illegal-pointer-ops-clang ()
  (signals eval-error
    (evaluate-expression
     (make-instance 'clang-expression) '((:ptr 1234 "*char"))
     '(:* 2 (:+ :ptr 1))))
  (signals eval-error
    (evaluate-expression
     (make-instance 'clang-expression) '((:ptr 1234 "*char"))
     '(:/ 2 (:+ :ptr 1)))))


;; Utility tests
(in-suite test)
(defsuite* test-utility)

(deftest intersects-does-not-include-endpoints ()
  (is (not (intersects (make-instance 'range :begin 0 :end 1)
                       (make-instance 'range :begin 1 :end 2))))
  (is (not (intersects (make-instance 'source-range
                         :begin (make-instance 'source-location :line 1
                                               :column 0)
                         :end   (make-instance 'source-location :line 2
                                               :column 0))
                       (make-instance 'source-range
                         :begin  (make-instance 'source-location :line 2
                                                :column 0)
                         :end    (make-instance 'source-location :line 3
                                                :column 0))))))

(deftest pad-list-expand-to-requisite-length ()
  (is (equal '(1 2 3 3) (pad '(1 2) 4 3))))

(deftest pad-list-already-of-requisite-length ()
  (is (equal '(1 2 3) (pad '(1 2 3) 3))))

(deftest file-to-string-restart ()
  #-ccl       ; CCL silently reads w/o warning despite bad encoding...
  (let ((path (make-pathname :directory +etc-dir+ :defaults "latin-1.c")))
    (signals stream-error
      (file-to-string path))
    (is (string= (handler-bind ((stream-error
                                 (lambda (c)
                                   (declare (ignorable c))
                                   (invoke-restart 'use-encoding :latin-1))))
               (file-to-string path))
         "/* Here is a non-ASCII character: § */
"))))

(deftest in-directory-with-trailing-slash ()
  (is (equal (in-directory #P"/tmp/build/" #P"src/test.c")
             #P"/tmp/build/src/test.c")))

(deftest in-directory-no-trailing-slash ()
  (is (equal (in-directory #P"/tmp/build" #P"src/test.c")
             #P"/tmp/build/src/test.c")))



;; project tests
(in-suite test)
(defsuite* test-project)

(defvar *s1*)
(defvar *s2*)
(defvar *project*)
(defixture project
  (:setup
   (setf *s1* (make-instance 'simple :genome "s1-genome"))
   (setf *s2* (make-instance 'simple :genome "s2-genome"))
   (setf *project* (make-instance 'project
                                  :evolve-files `(("s1" . ,*s1*)
                                                  ("s2" . ,*s2*)))))
    (:teardown (setf *project* nil)))

(deftest with-current-file-by-name ()
  (with-fixture project
    (with-current-file (*project* "s1")
      (is (eq (current-file *project*) *s1*)))
    (with-current-file (*project* "s2")
      (is (eq (current-file *project*) *s2*)))))

(deftest with-current-file-by-object ()
  (with-fixture project
    (with-current-file (*project* *s1*)
      (is (eq (current-file *project*) *s1*)))
    (with-current-file (*project* *s2*)
      (is (eq (current-file *project*) *s2*)))))

(defmethod test-method ((obj simple) value)
  value)

(deftest current-file-forwards-methods ()
  (with-fixture project
    (with-current-file (*project* "s2")
      (is (eq (test-method *project* 1) 1)))))

(deftest current-file-survives-copy ()
  (with-fixture project
    (with-current-file (*project* "s2")
      (let ((copy (copy *project*)))
        (is (string= (genome (current-file copy))
                     "s2-genome"))))))

(deftest with-current-file-unsets-copy ()
  (with-fixture project
    (let (copy)
      (with-current-file (*project* "s2")
        (setf copy (copy *project*)))
      ;; Exiting with-current-file should clear the current file of
      ;; copies of the project.
      (is (null (current-file copy))))))


;;; Condition synthesis tests
(in-suite test)
(defsuite* test-condition-synthesis)

(deftest flip-works ()
  (is (string= (se::flip "") ""))
  (is (string= (se::flip "0000") "0001"))
  (is (string= (se::flip "0001") "001")))

(deftest synthesize-all-conditions ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         (synths (se::synthesize-conditions substs)))
    (is (= 12 (length synths)))
    (is (member '("x" "5" :eq) synths :test #'equal))
    (is (member '("x" "5" :neq) synths :test #'equal))
    (is (member '("y" "6" :eq) synths :test #'equal))
    (is (member '("y" "6" :neq) synths :test #'equal))
    (is (member '("x" "10" :eq) synths :test #'equal))
    (is (member '("x" "10" :neq) synths :test #'equal))
    (is (member '("x" "15" :eq) synths :test #'equal))
    (is (member '("x" "15" :neq) synths :test #'equal))
    (is (member '("y" "4" :eq) synths :test #'equal))
    (is (member '("y" "4" :neq) synths :test #'equal))
    (is (member '("z" "2" :eq) synths :test #'equal))
    (is (member '("z" "2" :neq) synths :test #'equal))))

(deftest entails-eq-works ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         (eq-cond '("x" "10" :eq)))
    (is (se::entails (first substs) eq-cond "0"))
    (is (not (se::entails (first substs) eq-cond "1")))
    (is (not (se::entails (second substs) eq-cond "0")))
    (is (se::entails (second substs) eq-cond "1"))
    (is (se::entails (third substs) eq-cond "0"))
    (is (not (se::entails (third substs) eq-cond "1")))))

(deftest entails-neq-works ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         (eq-cond '("x" "10" :neq)))
    (is (se::entails (first substs) eq-cond "1"))
    (is (not (se::entails (first substs) eq-cond "0")))
    (is (not (se::entails (second substs) eq-cond "1")))
    (is (se::entails (second substs) eq-cond "0"))
    (is (se::entails (third substs) eq-cond "1"))
    (is (not (se::entails (third substs) eq-cond "0")))))

(deftest find-best-condition-finds-good-conditions ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         ;; shuffle the synthesized conditions so we don't always find the same
         ;; one first
         (synths (se::shuffle (se::synthesize-conditions substs))))
    (is (equal (se::find-best-condition '("1" "0" "1") substs synths)
               '("x" "10" :neq)))
    (is (member (se::find-best-condition '("1" "1" "0") substs synths)
                (list '("y"  "6" :eq)
                      '("x" "15" :neq)
                      '("y"  "4" :neq)
                      '("z"  "2" :neq))
                :test #'equal))))


;;; Selection tests
(in-suite test)
(defsuite* test-selection)

(deftest select-best-single-winner ()
  (let ((group (list (make-instance 'simple :fitness 1.0)
                     (make-instance 'simple :fitness 0.5)
                     (make-instance 'simple :fitness 0.1))))
    (is (equal (list (car group))
               (default-select-best group)))))

(deftest select-best-multiple-winners ()
  (let* ((group (list (make-instance 'simple :fitness 0.8)
                      (make-instance 'simple :fitness 0.5)
                      (make-instance 'simple :fitness 0.8)))
         (winners (default-select-best group)))
    (is (= (length winners) 2))
    (is (every [{= 0.8} #'fitness] winners))))

(deftest select-best-respects-fitness-predicate ()
  (let ((group (list (make-instance 'simple :fitness 1.0)
                     (make-instance 'simple :fitness 0.1)))
        (*fitness-predicate* #'<))
    (is (equal (list (lastcar group))
               (default-select-best group)))))

(deftest lexicase-select-best-single-winner ()
  (let ((group (list (make-instance 'simple :fitness #(1 1 1))
                     (make-instance 'simple :fitness #(0 0 0))
                     (make-instance 'simple :fitness #(0.1 0.1 0.1)))))
    (is (equal (list (car group))
               (lexicase-select-best group)))))

(deftest lexicase-select-best-multiple-winners ()
  (let* ((group (list (make-instance 'simple :fitness #(1 1 1))
                      (make-instance 'simple :fitness #(1 1 1))
                      (make-instance 'simple :fitness #(0 0 0))))
         (winners (lexicase-select-best group)))
    (is (= (length winners) 2))
    (is (every [{equalp #(1 1 1)} #'fitness] winners))))

(deftest lexicase-select-best-respects-fitness-predicate ()
  (let ((group (list (make-instance 'simple :fitness #(1 1 1))
                     (make-instance 'simple :fitness #(0.1 0.1 0.1))))
        (*fitness-predicate* #'<))
    (is (equal (list (lastcar group))
               (lexicase-select-best group)))))

(deftest lexicase-select-works ()
  (is (eq 2
          (-<> (list (make-instance 'simple :fitness #(0 1 1))
                     (make-instance 'simple :fitness #(0 1 1))
                     (make-instance 'simple :fitness #(1 1 0))
                     (make-instance 'simple :fitness #(1 1 0)))
               (lexicase-select <> 2)
               (length)))))

(deftest tournament-works ()
  (let ((*population* (list (make-instance 'simple :fitness 0.1)
                            (make-instance 'simple :fitness 0.2)
                            (make-instance 'simple :fitness 0.3)
                            (make-instance 'simple :fitness 0.4))))
    (is (member (tournament) *population*))))


(deftest lexicase-better-p-order ()
  (let ((a #(0 1 1))
        (b #(1 1 0)))
    (is (se::lexicase-better-p '(1 2 0) a b))
    (is (not (se::lexicase-better-p '(1 2 0) b a)))
    (is (se::lexicase-better-p '(1 0 2) b a))
    (is (not (se::lexicase-better-p '(1 0 2) a b)))))

(deftest lexicase-better-p-tie ()
  (let ((a #(0 1 1))
        (b #(0 1 1)))
    (is (not (se::lexicase-better-p '(0 1 2) a b)))
    (is (not (se::lexicase-better-p '(0 1 2) b a)))))

(deftest dominates-all-trivial-test ()
  (is (se::dominates-all (list #'>)
                         (list (make-instance 'simple :fitness '(0))
                               (make-instance 'simple :fitness '(0)))
                         (make-instance 'simple :fitness '(1)))))

(deftest pareto-selector-no-winners ()
  (let ((*population* (list (make-instance 'simple :fitness '(1))
                            (make-instance 'simple :fitness '(1))
                            (make-instance 'simple :fitness '(1))))
        (*pareto-comparison-set-size* 2))
    ;; No candidates will dominate because all have equal fitness, so
    ;; all should be selected.
    (is (equal *population*
               (pareto-selector *population*)))))

(deftest pareto-selector-choose-winners-maximizing-fitness ()
  (let ((candidates (list (make-instance 'simple :fitness '(1 0 0))
                          (make-instance 'simple :fitness '(1 1 0))
                          (make-instance 'simple :fitness '(1 1 1)))))
    (is (equalp
         (last candidates)
         (pareto-selector candidates :predicate #'>
                          :comparison-set
                          (list (make-instance 'simple :fitness '(0 0 0))
                                (make-instance 'simple :fitness '(0 1 1))))))
    (is (equalp
         (last candidates 2)
         (pareto-selector candidates :predicate #'>
                          :comparison-set
                          (list (make-instance 'simple :fitness '(0 0 0))
                                (make-instance 'simple :fitness '(0 1 0))))))
    (is (equalp
         (last candidates 3)
         (pareto-selector candidates :predicate #'>
                          :comparison-set
                          (list (make-instance 'simple :fitness '(0 0 0))))))))

(deftest pareto-selector-choose-losers-maximizing-fitness ()
  (let ((candidates (list (make-instance 'simple :fitness '(1 1 1))
                          (make-instance 'simple :fitness '(1 1 0))
                          (make-instance 'simple :fitness '(1 0 0)))))
    ;; By complementing :predicate (but leaving *fitness-predicate*
    ;; unchanged) we choose non-dominating candidates.
    (is (equalp
         (last candidates)
         (pareto-selector candidates :predicate (complement #'>)
                          :comparison-set
                          (list (make-instance 'simple :fitness '(0 0 0))
                                (make-instance 'simple :fitness '(1 0 0))))))
    (is (equalp
         (last candidates 2)
         (pareto-selector candidates :predicate (complement #'>)
                          :comparison-set
                          (list (make-instance 'simple :fitness '(0 0 0))
                                (make-instance 'simple :fitness '(1 1 0))))))
    (is (equalp
         (last candidates 3)
         (pareto-selector candidates :predicate (complement #'>)
                          :comparison-set
                          (list (make-instance 'simple :fitness '(1 1 1))))))))

(deftest pareto-selector-choose-winners-minimizing-fitness ()
  (let ((*fitness-predicate* #'<)       ; lower fitness is better
        (candidates (list (make-instance 'simple :fitness '(0 1 1))
                          (make-instance 'simple :fitness '(0 0 1))
                          (make-instance 'simple :fitness '(0 0 0)))))
    (is (equalp
         (last candidates)
         (pareto-selector candidates :predicate #'<
                          :comparison-set
                          (list (make-instance 'simple :fitness '(1 1 1))
                                (make-instance 'simple :fitness '(1 0 0))))))
    (is (equalp
         (last candidates 2)
         (pareto-selector candidates :predicate #'<
                          :comparison-set
                          (list (make-instance 'simple :fitness '(1 1 1))
                                (make-instance 'simple :fitness '(1 0 1))))))
    (is (equalp
         (last candidates 3)
         (pareto-selector candidates :predicate #'<
                          :comparison-set
                          (list (make-instance 'simple :fitness '(1 1 1))))))))

(deftest pareto-selector-choose-losers-minimizing-fitness ()
  (let ((*fitness-predicate* #'<)       ; lower fitness is better
        (candidates (list (make-instance 'simple :fitness '(0 0 0))
                          (make-instance 'simple :fitness '(0 0 1))
                          (make-instance 'simple :fitness '(0 1 1)))))
    ;; By complementing :predicate (but leaving *fitness-predicate*
    ;; unchanged) we choose non-dominating candidates.
    (is (equalp
         (last candidates)
         (pareto-selector candidates :predicate (complement #'<)
                          :comparison-set
                          (list (make-instance 'simple :fitness '(1 1 1))
                                (make-instance 'simple :fitness '(0 1 1))))))
    (is (equalp
         (last candidates 2)
         (pareto-selector candidates :predicate (complement #'<)
                          :comparison-set
                          (list (make-instance 'simple :fitness '(1 1 1))
                                (make-instance 'simple :fitness '(0 0 1))))))
    (is (equalp
         (last candidates 3)
         (pareto-selector candidates :predicate (complement #'<)
                          :comparison-set
                          (list (make-instance 'simple :fitness '(0 0 0))))))))

(deftest pareto-selector-lexicase ()
  (let ((candidates (list (make-instance 'simple :fitness '(#(0.1 0.1 0.1)))
                          (make-instance 'simple :fitness '(#(1 1 1)))))
        (middle (make-instance 'simple :fitness '(#(0.5 0.5 0.5))))
        (worst (make-instance 'simple :fitness '(#(0 0 0)))))
    (is (equalp (last candidates)
                (pareto-selector candidates :predicate #'>
                                 :comparison-set (list middle))))
    (is (equalp candidates
                (pareto-selector candidates :predicate #'>
                                 :comparison-set (list worst))))))

(deftest multi-objective-scalar-works ()
  (is (eq 1.0 (multi-objective-scalar '(0.1 0.4 0.5))))
  (is (eq 1.0 (multi-objective-scalar '(0.1 0.4 #(1 0))))))

(deftest crowding-distance-works ()
  (let ((*population* (list (make-instance 'simple :fitness '(0.5 0.5 0.5))
                            (make-instance 'simple :fitness '(0 0 1))
                            (make-instance 'simple :fitness '(0.6 1 0.1)))))

    (is (equal (+ 0.6 1 0.9)
               (se::crowding-distance (first *population*))))
    (is (equal infinity
               (se::crowding-distance (second *population*))))
    (is (equal infinity
               (se::crowding-distance (third *population*))))))

(deftest crowding-distance-lexicase-works ()
  (let ((*population*
         (list (make-instance 'simple :fitness '(#(1 0) #(1 1 0 0)))
               (make-instance 'simple :fitness '(#(0.5 0.5) #(0.5 0.5 0.5 0.5)))
               (make-instance 'simple :fitness '(#(0 1) #(0 0 1 1))))))

    (is (equal infinity
               (se::crowding-distance (first *population*))))
    (is (equal 2
               (se::crowding-distance (second *population*))))
    (is (equal infinity
               (se::crowding-distance (third *population*))))))

(deftest pick-least-crowded-works ()
  (let ((*population* (list (make-instance 'simple :fitness '(0.5 0.5 0.5))
                            (make-instance 'simple :fitness '(0.1 0.1 0.2))
                            (make-instance 'simple :fitness '(0 0 1))
                            (make-instance 'simple :fitness '(0.6 1 0.1)))))
    (is (equal (first *population*)
               (pick-least-crowded (subseq *population* 0 2))))))

(deftest pareto-tournament-works ()
  (let* ((*population* (list (make-instance 'simple :fitness '(1 1 1))
                             (make-instance 'simple :fitness '(0 0 0))
                             (make-instance 'simple :fitness '(0 0 0))))
         (*pareto-comparison-set-size* 3))

    (is (equalp (first *population*)
                (first (pareto-selector *population*))))))

(deftest pareto-tie-breaker-works ()
  (let ((*population* (list (make-instance 'simple :fitness '(1))
                            (make-instance 'simple :fitness '(1))
                            (make-instance 'simple :fitness '(1))))
        (*pareto-comparison-set-size* 3)
        (*tournament-selector* #'pareto-selector)
        (*tournament-tie-breaker* #'pick-least-crowded))
    ;; All candidates have equal fitness. This should force the tie
    ;; breaker to be called.
    (is (tournament :size 3))))


;; Style features tests
(in-suite test)
(defsuite* test-style-features)

(deftest avg-depth-function-asts-is-0 ()
  (with-fixture variety-clang
    (let ((asts (functions *variety*)))
      (is (= 4 (length asts)))
      ;; depths of all function asts are 0 (all top-level)
      (is (every #'zerop (mapcar {se::ast-depth *variety*} asts)))
      ;; average depth of all function asts is 0
      (is (= 0 (se::avg-depth-asts *variety* asts))))))

(deftest avg-depth-deeper-asts-correct ()
  (with-fixture variety-clang
    (let* ((add3-fun (get-ast *variety* (stmt-starting-with-text
                                         *variety*
                                         "int add3")))
           (stmts (aget :stmt-range add3-fun))
           (ctrs (iota (1+ (- (second stmts)
                              (first stmts)))
                       :start (first stmts)))
           (asts (mapcar {get-ast *variety*} ctrs))
           (depths (mapcar {se::ast-depth *variety*} asts)))
      (is (= (/ (reduce #'+ depths) (length depths)))
          (se::avg-depth-asts *variety* asts)))))

(deftest avg-depth-ast-node-type-function-is-0 ()
  (with-fixture variety-clang
    (is (zerop (se::avg-depth-ast-node-type *variety* "Function")))))

(deftest avg-depth-ast-node-type-return-stmts ()
  (with-fixture variety-clang
    (is (= 2 (se::avg-depth-ast-node-type *variety* "ReturnStmt")))))

(deftest node-type-counts ()
  (with-fixture variety-clang
    ;; list of (ast-class, occurrences)
    (let ((ast-counts
           (mapcar #'cons
                   se::*clang-c-ast-classes*
                   (coerce (se::ast-node-type-tf-extractor *variety*) 'list))))
      ;; for each ast-class, verify occurrence count is correct
      (iter (for (type . count) in ast-counts)
            (is (= count
                   (count-if [{equal type} {aget :ast-class}]
                             (asts *variety*))))
            (finally (return t))))))

(deftest ast-keywords-auto-counts ()
  (with-fixture variety-clang
    (let ((auto-counts
           (iter (for keyword in se::*clang-c-keywords*)
                 (collect
                     (cons (reduce #'+ (mapcar {se::auto-count-keyword keyword}
                                               (asts *variety*)))
                           keyword)
                   into counts)
                 (finally (return counts)))))
      (is (equal
           auto-counts
           '((0 . "alignof") (0 . "auto") (2 . "break") (2 . "case")
             (0 . "char") (0 . "const") (1 . "continue") (1 . "default")
             (1 . "do") (0 . "double") (0 . "else") (1 . "enum")
             (0 . "extern") (0 . "float") (1 . "for") (3 . "goto") (1 . "if")
             (0 . "inline") (0 . "int") (0 . "long") (0 . "register")
             (0 . "restrict") (4 . "return") (0 . "short") (0 . "signed")
             (0 . "sizeof") (0 . "static") (0 . "struct") (1 . "switch")
             (2 . "typedef") (0 . "union") (0 . "unsigned") (0 . "void")
             (0 . "volatile") (2 . "while")))))))

(deftest ast-keywords-search-counts ()
  (with-fixture variety-clang
    (let ((search-counts
           (iter (for keyword in se::*clang-c-keywords*)
                 (collect
                     (cons (reduce #'+ (mapcar {se::search-keyword keyword}
                                               (asts *variety*)))
                           keyword)
                   into counts)
                 (finally (return counts)))))
      (is (equal
           search-counts
           '((0 . "alignof") (0 . "auto") (0 . "break") (0 . "case")
             (1 . "char") (1 . "const") (0 . "continue") (0 . "default")
             (0 . "do") (3 . "double") (1 . "else") (1 . "enum")
             (0 . "extern") (0 . "float") (0 . "for") (0 . "goto") (0 . "if")
             (0 . "inline") (10 . "int") (0 . "long") (0 . "register")
             (0 . "restrict") (0 . "return") (0 . "short") (0 . "signed")
             (1 . "sizeof") (0 . "static") (1 . "struct") (0 . "switch")
             (0 . "typedef") (1 . "union") (0 . "unsigned") (1 . "void")
             (0 . "volatile") (0 . "while")))))))

(deftest ast-keyword-tf-extractor-correct ()
  (with-fixture variety-clang
    (is (equal
         (mapcar #'cons
                 (coerce (se::ast-keyword-tf-extractor *variety*) 'list)
                 se::*clang-c-keywords*)
         '((0 . "alignof") (0 . "auto") (2 . "break") (2 . "case") (1 . "char")
           (1 . "const") (1 . "continue") (1 . "default") (1 . "do")
           (3 . "double") (1 . "else") (2 . "enum") (0 . "extern") (0 . "float")
           (1 . "for") (3 . "goto") (1 . "if") (0 . "inline") (10 . "int")
           (0 . "long") (0 . "register") (0 . "restrict") (4 . "return")
           (0 . "short") (0 . "signed") (1 . "sizeof") (0 . "static")
           (1 . "struct") (1 . "switch") (2 . "typedef") (1 . "union")
           (0 . "unsigned") (1 . "void") (0 . "volatile") (2 . "while"))))))

(deftest small-bi-grams-count-example ()
  (let* ((ls (list "the" "tortoise" "and" "the" "hare" "and" "the" "race"))
         (bi-grams (se::bi-grams ls :key #'identity :test #'equal))
         (keys (hash-table-keys bi-grams))
         (vals (hash-table-values bi-grams))
         (sorted-keys (list (cons "and" "the")
                            (cons "hare" "and")
                            (cons "the" "hare")
                            (cons "the" "race")
                            (cons "the" "tortoise")
                            (cons "tortoise" "and"))))
    ;; correct number of keys/values
    (is (= 6 (length keys) (length vals)))
    ;; correct set of keys
    (is (equal sorted-keys
               (sort keys (lambda (k1 k2)
                            (or (string< (car k1) (car k2))
                                (and (string= (car k1) (car k2))
                                     (string< (cdr k1) (cdr k2))))))))
    ;; correct set of values (all 1 except "and the" which is 2)
    (iter (for key in sorted-keys)
          (if (equal key (cons "and" "the"))
              (is (= 2 (gethash key bi-grams 0)))
              (is (= 1 (gethash key bi-grams 0)))))))

(deftest function-bi-grams-count ()
  "For a list containing N Function ASTs, there are N-1 bi-grams, all
(Function, Function)."
  (with-fixture variety-clang
    (let* ((functions (functions *variety*))
           (bi-grams (se::bi-grams functions :key {aget :ast-class}))
           (keys (hash-table-keys bi-grams))
           (vals (hash-table-values bi-grams)))
      (is (= 1 (length keys) (length vals)))
      (is (equal (car keys) (cons "Function" "Function")))
      (is (= (1- (length functions)) (car vals))))))

(deftest small-ast-bigrams-count-example ()
  (with-fixture variety-clang
    (flet ((asts-by-type (type)
             (remove-if-not [{equal type} {aget :ast-class}]
                            (asts *variety*))))
      (let* ((asts (list (first (asts-by-type "Function"))
                         (first (asts-by-type "IntegerLiteral"))
                         (first (asts-by-type "DeclStmt"))))
             (bi-grams (se::bi-grams asts :key {aget :ast-class}))
             (keys (hash-table-keys bi-grams))
             (vals (hash-table-values bi-grams))
             (sorted-keys (list (cons "Function" "IntegerLiteral")
                                (cons "IntegerLiteral" "DeclStmt"))))
        (is (= 2 (length keys) (length vals)))
        (is (equal sorted-keys
                   (sort keys (lambda (k1 k2)
                                (or (string< (car k1) (car k2))
                                    (and (string= (car k1) (car k2))
                                         (string< (cdr k1) (cdr k2))))))))
        (is (equal '(1 1) vals))))))


(in-suite test)
(defsuite* clang-syntactic-contexts)

;; Tests of basic clang mutation operators
(defun count-matching-chars-in-stmt (char stmt)
  (let ((ast (if (listp stmt) (car stmt) stmt)))
    (count-if {eq char} (source-text ast))))

(defun find-function (obj name)
  (find-if [{string= name} #'ast-name]
           (functions obj)))

(deftest cut-full-stmt-removes-semicolon ()
  (with-fixture contexts
    (se::apply-mutation-ops *contexts*
                            `((:cut (:stmt1 . ,(stmt-with-text
                                                *contexts* "int x = 0")))))
    (is (eq 0
            (count-matching-chars-in-stmt
             #\;
             (find-function *contexts* "full_stmt"))))))

(deftest insert-full-stmt-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0")))
      (se::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,target)
                                         (:value1 . ,target)))))
    (is (eq 2 (count-matching-chars-in-stmt
             #\;
             (find-function *contexts* "full_stmt"))))))

(deftest insert-braced-full-stmt-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0"))
          (inserted (function-body *contexts*
                                   (find-function *contexts*
                                                  "list"))))
      (se::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,target)
                                         (:value1 . ,inserted)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest replace-full-stmt-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0"))
          (replacement (stmt-with-text *contexts* "int x = 1")))
      (se::apply-mutation-ops *contexts*
                              `((:set (:stmt1 . ,target)
                                      (:value1 . ,replacement)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest replace-full-stmt-with-braced-removes-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0"))
          (replacement (function-body *contexts*
                                      (find-function *contexts*
                                                         "list"))))
      (se::apply-mutation-ops *contexts*
                              `((:set (:stmt1 . ,target)
                                      (:value1 . ,replacement)))))
    (is (eq 0 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest cut-list-elt-removes-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b")))
      (se::apply-mutation-ops *contexts*
                             `((:cut (:stmt1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a,  int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest insert-list-elt-adds-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b")))
      (se::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,target)
                                         (:value1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b,int b, int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-list-elt-keeps-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b"))
          (replacement (stmt-with-text *contexts* "int a")))
      (se::apply-mutation-ops *contexts*
                              `((:set (:stmt1 . ,target)
                                      (:value1 . ,replacement)))))
    (is (starts-with-subseq
         "void list(int a, int a, int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest cut-final-list-elt-removes-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c")))
      (se::apply-mutation-ops *contexts*
                              `((:cut (:stmt1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b)"
         (source-text (find-function *contexts* "list"))))))

(deftest insert-final-list-elt-adds-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c")))
      (se::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,target)
                                         (:value1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b, int c,int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-final-list-elt-keeps-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c"))
          (replacement (stmt-with-text *contexts* "int a")))
      (se::apply-mutation-ops *contexts*
                              `((:set (:stmt1 . ,target)
                                      (:value1 . ,replacement)))))
    (is (starts-with-subseq
         "void list(int a, int b, int a)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-braced-adds-braces-and-semicolon ()
  (with-fixture contexts
    (let ((target
           (second (get-immediate-children
                    *contexts*
                    (car (get-immediate-children
                          *contexts*
                          (function-body *contexts*
                                         (find-function *contexts*
                                                        "braced_body")))))))
          (replacement (stmt-with-text *contexts* "int x = 0")))
      (se::apply-mutation-ops *contexts*
                              `((:set (:stmt1 . ,target)
                                      (:value1 . ,replacement)))))
    ;; NOTE: this adds braces but no semicolon, which isn't quite
    ;; right.
    (let ((function (find-function *contexts* "braced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      (is (eq 1 (count-matching-chars-in-stmt #\; function))))))

(deftest replace-unbraced-body-keeps-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2")))
      (se::apply-mutation-ops *contexts*
                       `((:set (:stmt1 . ,target)
                               (:value1 . ,target)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "unbraced_body"))))))

(deftest replace-unbraced-body-with-braced ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2"))
          (replacement (function-body *contexts*
                                      (find-function *contexts*
                                                     "full_stmt"))))
      (se::apply-mutation-ops *contexts*
                      `((:set (:stmt1 . ,target)
                              (:value1 . ,replacement)))))
    (let ((function (find-function *contexts* "unbraced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      (is (eq 1 (count-matching-chars-in-stmt #\; function))))))

(deftest cut-field-removes-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;")))
      (se::apply-mutation-ops *contexts*
                              `((:cut (:stmt1 . ,target)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 1
              (count-matching-chars-in-stmt #\; struct))))))

(deftest insert-field-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;")))
      (se::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,target)
                                         (:value1 . ,target)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 3
              (count-matching-chars-in-stmt #\; struct))))))

(deftest replace-field-keeps-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;"))
          (replacement (stmt-with-text *contexts* "int f2;")))
      (se::apply-mutation-ops *contexts*
                              `((:set (:stmt1 . ,target)
                                      (:value1 . ,replacement)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 2
              (count-matching-chars-in-stmt #\; struct))))))

(deftest insert-toplevel-adds-semicolon ()
  (with-fixture contexts
    (let ((location (stmt-starting-with-text *contexts* "struct"))
          (inserted (stmt-with-text *contexts* "int x = 0"))
          (semicolons (count-if {eq #\;} (genome *contexts*))))
      (se::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,location)
                                         (:value1 . ,inserted))))
      (is (eq (1+ semicolons)
              (count-if {eq #\;} (genome *contexts*)))))))

(deftest insert-toplevel-braced ()
  (with-fixture contexts
    (let ((location (stmt-starting-with-text *contexts* "struct"))
          (inserted (stmt-starting-with-text *contexts* "void list"))
          (semicolons (count-if {eq #\;} (genome *contexts*))))
      (se::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,location)
                                         (:value1 . ,inserted))))
      (is (eq semicolons
              (count-if {eq #\;} (genome *contexts*)))))))

(deftest splice-asts-and-text ()
  (with-fixture contexts
    (let ((location (stmt-with-text *contexts* "int x = 0"))
          (inserted (list (format nil "/*comment 1*/~%")
                          (se::ast-ref-ast (stmt-starting-with-text *contexts*
                                                                    "int x = 1"))
                          (format nil ";~%/*comment 2*/~%"))))
      (se::apply-mutation-ops *contexts*
                              `((:splice (:stmt1 . ,location)
                                         (:value1 . ,inserted))))

      (is (not (stmt-with-text *contexts* "int x = 0")))
      (is (stmt-with-text *contexts* "int x = 1"))
      (is (eq 1
              (->> (stmt-starting-with-text *contexts* "void full_stmt")
                   (function-body *contexts*)
                   (get-immediate-children *contexts*)
                   (remove-if-not #'ast-full-stmt)
                   (length))))
      (is (search "comment 1" (genome *contexts*)))
      (is (search "comment 2" (genome *contexts*))))))



(in-suite test)
(defsuite* clang-scopes-and-types)

(deftest scopes-are-correct ()
  (with-fixture scopes2-clang
    (is (equal (scopes *scopes* (stmt-with-text *scopes* "int b"))
               '(nil
                 ("a")
                 ("global"))))
    (is (equal (scopes *scopes* (stmt-with-text *scopes* "int c"))
               '(("b")
                 ("a")
                 ("global"))))
    (is (equal (scopes *scopes* (stmt-with-text *scopes* "char d"))
               '(nil
                 ("c" "b")
                 ("a")
                 ("global"))))))

(deftest types-are-correct ()
  (with-fixture scopes2-clang
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-with-text *scopes*
                                                      "int global")))
               '("int")))
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-with-text *scopes*
                                                      "char d")))
               '("char")))
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-starting-with-text *scopes*
                                                               "void foo")))
               '("int" "char")))))

(deftest unbound-vals-are-correct ()
  (with-fixture scopes2-clang
    (is (null (get-unbound-vals *scopes*
                                (stmt-with-text *scopes* "int global"))))
    (is (equal (get-unbound-vals *scopes*
                                 (stmt-starting-with-text *scopes* "c ="))
               '(("(|global|)" 2)
                 ("(|b|)" 0)
                 ("(|a|)" 1)
                 ("(|c|)" 0))))
    (is (equal (get-unbound-vals *scopes*
                                 (stmt-starting-with-text *scopes* "b ="))
               '(("(|b|)" 0))))
    (is (equal (get-unbound-vals *scopes*
                                 (stmt-starting-with-text *scopes* "d ="))
               '(("(|d|)" 0))))

    (is (equal (get-unbound-vals *scopes*
                                 (stmt-starting-with-text *scopes* "void foo"))
               ;; Note: clang-mutate would have a 1 here, but I think
               ;; 0 is correct.
               '(("(|global|)" 0))))))

(deftest unbound-funs-are-correct ()
  (with-fixture scopes2-clang
    (is (null (get-unbound-funs *scopes*
                                (stmt-with-text *scopes* "int global"))))
    (is (equal (get-unbound-funs *scopes*
                                 (stmt-with-text *scopes* "foo(0)"))
               '(("(|foo|)" t nil 1))))
    (is (equal (get-unbound-funs *scopes*
                                 (stmt-with-text *scopes* "bar()"))
               '(("(|bar|)" t nil 0))))
    (is (equal (get-unbound-funs *scopes*
                                 (stmt-starting-with-text *scopes* "void bar"))
               '(("(|foo|)" t nil 1)
                 ("(|bar|)" t nil 0))))))

(deftest move-statement-updates-scopes ()
  (with-fixture scopes2-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (apply-mutation *scopes*
                      `(clang-swap (:stmt1 . ,(stmt-with-text *scopes* "int c"))
                                   (:stmt2 . ,(stmt-with-text *scopes* "b = 0")))))
    (is (equal (scopes *scopes* (stmt-starting-with-text *scopes* "b ="))
               '(("b")
                 ("a")
                 ("global"))))))

(deftest cut-decl-updates-scopes ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "int global"))))
    (is (equal (scopes *scopes* (stmt-starting-with-text *scopes* "b ="))
               '(("c" "b")
                 ("a")
                 nil)))))

(deftest insert-decl-updates-types ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-insert (:stmt1 . ,(stmt-with-text *scopes* "foo(0)"))
                                   (:stmt2 . ,(stmt-with-text *scopes* "int b"))))
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-starting-with-text *scopes*
                                                               "void bar")))
               '("int")))))

(deftest cut-statement-updates-unbound-funs ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "foo(0)"))))
    (is (equal (get-unbound-funs *scopes*
                                 (stmt-starting-with-text *scopes* "void bar"))
               '(("(|bar|)" t nil 0))))))

(deftest insert-statement-updates-unbound-funs ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-insert (:stmt1 . ,(stmt-with-text *scopes* "int b"))
                                   (:stmt2 . ,(stmt-with-text *scopes*
                                                              "bar()"))))
    (is (equal (get-unbound-funs *scopes*
                                 (stmt-starting-with-text *scopes* "void foo"))
               '(("(|bar|)" t nil 0))))))

(deftest cut-statement-updates-unbound-vals ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-cut (:stmt1 . ,(stmt-starting-with-text *scopes*
                                                                    "c ="))))
    (is (null (get-unbound-vals *scopes*
                                (stmt-starting-with-text *scopes*
                                                         "void foo"))))))

(deftest cut-decl-updates-unbound-vals ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "int b"))))
    (is (equal (get-unbound-vals *scopes*
                                 (stmt-starting-with-text *scopes*
                                                          "void foo"))
               '(("(|global|)" 0)
                 ("(|b|)" nil))))))

(deftest insert-statement-updates-unbound-vals ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-insert (:stmt1 . ,(stmt-with-text *scopes*
                                                              "foo(0)"))
                                   (:stmt2 . ,(stmt-with-text *scopes*
                                                              "b = 0"))))
    (is (equal (get-unbound-vals *scopes*
                                 (stmt-starting-with-text *scopes*
                                                          "void bar"))
               '(("(|b|)" nil))))))
