;;; tests.lisp --- tests for the `software-evolution-library' package

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :software-evolution-library-test)
(enable-curry-compose-reader-macros :include-utf8)

;; Disable clang-format and any other helpers
(defmacro every-is (function &rest lists)
  (let ((args-sym (gensym "args")))
    `(mapc (lambda (&rest ,args-sym)
             (is (apply ,function ,args-sym)))
           ,@lists)))

(defun run-testbot (&rest a)
  (declare (ignorable a))
  (testbot-test #'test "SEL" +software-evolution-library-branch+))
(defun run-batch (&rest a) (declare (ignorable a)) (batch-test #'test))

(defsuite* test)                        ; The root test suite.

(defmacro sel-suite* (name documentation &optional (test-pre-check t))
  "Define NAME with DOCUMENTATION in the top level `test'.
Optional TEST-PRE-CHECK if present should be one of the following:
:SILENT --- silently skip this test suite
NIL ------- skip this test suite with the default warning
Otherwise - test-pre-check will be invoked at runtime
return non-nil when the test
suite should be run and nil otherwise."
  (with-gensyms (test)
    `(progn
       (when (or (boundp ',name) (fboundp ',name))
         (warn "Defining ~a with `sel-suite*' overwrites existing definition."
               ',name))
       (defsuite* (,name :in test :documentation ,documentation) ()
         (let ((,test (find-test ',name)))
           (cond
             ((stefil::test-was-run-p ,test)
              (warn "Skipped executing already run tests suite ~S"
                    (stefil::name-of ,test)))
             ,@(if (eq test-pre-check :silent)
                   '()
                   `(,@(if test-pre-check
                           `((,test-pre-check (run-child-tests)))
                           nil)
                       (t (warn "Skipped executing disabled tests suite ~S."
                                (stefil::name-of ,test)))))))))))

(defvar *genome*      nil "Genome used in tests.")
(defvar *soft*        nil "Software used in tests.")
(defvar *tfos*        nil "Another software used in tests.")
(defvar *gcd*         nil "Holds the gcd software object.")
(defvar *gcd-trace-path* nil "Holds the file of gcd traces.")
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
(defvar *test-suite*  nil "Holds condition synthesis test suite object.")

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

(define-constant +grep-prj-dir+ (append +etc-dir+ (list "grep-prj"))
  :test #'equalp
  :documentation "Path to directory holding the grep project.")

(define-constant +multi-file-dir+ (append +etc-dir+ (list "multi-file"))
  :test #'equalp
  :documentation "Path to directory holding the multi-file example.")

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
      :documentation "Path to the syntactic-contexts example.")

(define-constant +strings-dir+ (append +etc-dir+ (list "strings"))
  :test #'equalp
  :documentation "Path to the strings examples.")

(define-constant +typedef-dir+ (append +etc-dir+ (list "typedef"))
  :test #'equalp
  :documentation "Path to the typedef example.")

(define-constant +shadow-dir+ (append +etc-dir+ (list "shadow"))
  :test #'equalp
  :documentation "Path to the shadow example.")

(define-constant +unicode-dir+ (append +etc-dir+ (list "unicode"))
  :test #'equalp
  :documentation "Path to the unicode example.")

(define-constant +switch-macros-dir+ (append +etc-dir+ (list "switch-macros"))
  :test #'equalp
  :documentation "Path to the switch-macros example.")

(define-constant +fl-tiny-dir+ (append +etc-dir+ (list "fl-test"))
  :test #'equalp
  :documentation "Path to condition fault localization example.")

(define-constant +condition-synthesis-dir+
    (append +etc-dir+ (list "condition-synthesis"))
  :test #'equalp
  :documentation "Path to condition synthesis examples.")

(define-constant +cs-tiny-dir+ (append +condition-synthesis-dir+
                                       (list "tiny-test"))
  :test #'equalp
  :documentation "Path to condition synthesis tighten-condition example.")

(define-constant +cs-tighten-dir+ (append +condition-synthesis-dir+
                                          (list "test-tighten"))
  :test #'equalp
  :documentation "Path to condition synthesis tighten-condition example.")

(define-constant +cs-add-guard-dir+ (append +condition-synthesis-dir+
                                            (list "test-add-guard"))
  :test #'equalp
  :documentation "Path to condition synthesis add guard example.")

(define-constant +cs-divide-dir+ (append +condition-synthesis-dir+
                                         (list "divide"))
  :test #'equalp
  :documentation "Path to condition synthesis if to while repair example.")

(define-constant +assert-dir+ (append +etc-dir+ (list "assert"))
  :test #'equalp
  :documentation "Path to assert example.")

(define-constant +long-running-program-dir+
                 (append +etc-dir+  (list "long-running-program"))
  :test #'equalp
  :documentation "Path to long running program example.")

(define-constant +typedef-type-dir+
                 (append +etc-dir+ (list "typedef-type"))
  :test #'equalp
  :documentation "Path to the typedef-type program example")

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

(defun strings-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +strings-dir+))

(defun typedef-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +typedef-dir+))

(defun shadow-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +shadow-dir+))

(defun unicode-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +unicode-dir+))

(defun switch-macros-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +switch-macros-dir+))

(defun fl-tiny-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +fl-tiny-dir+))

(defun cs-tiny-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +cs-tiny-dir+))

(defun cs-tighten-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +cs-tighten-dir+))

(defun cs-add-guard-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +cs-add-guard-dir+))

(defun cs-divide-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +cs-divide-dir+))

(defun assert-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +assert-dir+))

(defun long-running-program-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +long-running-program-dir+))

(defun typedef-type-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +typedef-type-dir+))

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

(defixture typedef
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang :compiler "clang-3.7")
                    (typedef-dir "typedef.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture cpp-strings
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang :compiler "clang++")
                    (strings-dir "cpp-strings.cpp"))))
  (:teardown
   (setf *soft* nil)))

(defixture c-strings
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang)
                    (strings-dir "c-strings.c"))))
  (:teardown
   (setf *soft* nil)))

(defun inject-missing-swap-macro (obj)
  ;; Inject a macro that clang-mutate currently misses, then force the ASTs to
  ;; be recalculated by setting the genome-string.
  (->> (sel::make-clang-macro
         :name "swap_"
         :body "swap_(I,J) do { int t_; t_ = a[(I)]; a[(I)] = a[(J)]; a[(J)] = t_; } while (0)"
         :hash 1179176719466053316)
       (add-macro obj))
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

(defixture scopes-type-field-clang
  (:setup
    (setf *scopes*
          (from-file (make-instance 'clang
                                    :compiler "clang" :flags '("-g -m32 -O0"))
                     (scopes-dir "scopes-type-field.c"))))
  (:teardown
    (setf *scopes* nil)))

(defixture scopes-cxx-clang
  (:setup
    (setf *scopes*
          (from-file (make-instance 'clang-control-picks :compiler "clang")
                     (scopes-dir "scopes.cxx"))))
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

(defixture shadow-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang)
          (shadow-dir "shadow.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture unicode-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang)
          (unicode-dir "unicode.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture fl-tiny-clang
  (:setup (setf *soft*
                (from-file (make-instance 'clang)
                           (fl-tiny-dir "tiny-test.c")))
          (setf *test-suite*
                (make-instance 'test-suite
                  :test-cases
                  (iter (for i below 6)
                    (collecting
                     (make-instance 'test-case
                       :program-name (namestring (fl-tiny-dir "fitness.sh"))
                       :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(defixture cs-tiny-clang
  (:setup (setf *soft*
                (from-file (make-instance 'clang)
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

(defixture cs-tighten-clang
  (:setup (setf *soft*
                (from-file (make-instance 'clang)
                           (cs-tighten-dir "test-tighten.c")))
          (setf *test-suite*
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

(defixture cs-add-guard-clang
  (:setup (setf *soft*
                (from-file (make-instance 'clang)
                           (cs-add-guard-dir "test-add-guard.c")))
          (setf *test-suite*
                (make-instance 'test-suite
                  :test-cases
                  (iter (for i below 8)
                    (collecting
                     (make-instance 'test-case
                       :program-name (namestring (cs-add-guard-dir "fitness.sh"))
                       :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(defixture cs-divide-clang
  (:setup (setf *soft*
                (from-file (make-instance 'clang)
                           (cs-divide-dir "divide.c")))
          (setf *test-suite*
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


(defixture switch-macros-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang)
          (switch-macros-dir "switch-macros.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture assert-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang)
                     (assert-dir "assert.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture long-running-program-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang)
                     (long-running-program-dir "long-running-program.c"))))
  (:teardown
    (setf *soft* nil)))

(defixture typedef-type-clang
  (:setup
    (setf *soft*
          (from-file (make-instance 'clang)
                     (typedef-type-dir "typedef-type.c"))))
  (:teardown
    (setf *soft* nil)))


;;; ASM representation.
(sel-suite* asm-tests "ASM representation.")


(deftest simple-read ()
  (with-fixture gcd-asm
    (is (equal 'asm (type-of *gcd*)))))

(deftest idempotent-read-write ()
  (let ((a (software-evolution-library::temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file *gcd* a)
           (multiple-value-bind (out err ret)
               (software-evolution-library::shell "diff ~s ~a"
                                          (namestring (gcd-dir "gcd.s")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest idempotent-copy ()
  (with-fixture gcd-asm
   (is (software-evolution-library::equal-it *gcd* (copy *gcd*)))))

(deftest idempotent-read-copy-write ()
  (let ((a (software-evolution-library::temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file (copy *gcd*) a)
           (multiple-value-bind (out err ret)
               (software-evolution-library::shell "diff ~s ~a"
                                          (namestring (gcd-dir "gcd.s")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest edit-of-copy-does-not-change-original ()
  (with-fixture gcd-asm
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (mutate ant)
      (is (not (software-evolution-library::equal-it (genome ant) (genome *gcd*))))
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
    (let ((variant (copy *gcd*))
          (mutation
           (make-instance 'simple-swap
             :targets (list
                       (position-if [{scan "mov"} {aget :code}]
                                    (genome *gcd*))
                       (position-if [{scan "call"} {aget :code}]
                                    (genome *gcd*))))))
      (setf variant (apply-mutation variant mutation))
      (is (not (tree-equal (genome variant) (genome *gcd*) :test #'tree-equal)))
      (is (= (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-replace-operand-maintains-length ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'asm-replace-operand
                                             :targets (list 13 18)))
      (is (not (tree-equal (genome variant) (genome *gcd*) :test #'tree-equal)))
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
        (is (not (tree-equal (genome new) (genome *gcd*) :test #'tree-equal)))
        ;; (is (some [{equal :crossover} #'car] (edits new)))
        ;; (is (some [{equal :cut} #'caar] (second (edits new))))
        ))))


;;; ELF representation.
;;;
;;; NOTE: Currently failing because not populating .text section.
;;;
(sel-suite* elf-tests "ELF representation." :silent)


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
          (software-evolution-library::shell "diff ~s ~a"
                                     (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-copy-same-genome ()
  (with-fixture gcd-elf
   (is (software-evolution-library::equal-it (genome *gcd*)
                                     (genome (copy *gcd*))))))

(deftest elf-idempotent-read-copy-write ()
  (with-temp-file (a)
    (with-fixture gcd-elf
      (phenome (copy *gcd*) :bin a)
      (multiple-value-bind (out err ret)
          (software-evolution-library::shell "diff ~s ~a"
                                     (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-edit-of-copy-does-not-change-original ()
  (with-fixture gcd-elf
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (handler-case (mutate ant)
        (mutate (obj) (declare (ignorable obj)) nil))
      (is (not (software-evolution-library::equal-it (genome ant) (genome *gcd*))))
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
           (new-genome (software-evolution-library::elf-replace
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


;;; Clang representation.
(defun clang-mutate-available-p ()
  (zerop (third (multiple-value-list (shell "which clang-mutate")))))

(sel-suite* clang-tests "Clang representation." (clang-mutate-available-p))

(deftest simply-able-to-load-a-clang-software-object()
  (with-fixture hello-world-clang
    (is (not (null *hello-world*)))))

(deftest genome-change-clears-clang-software-object-fields ()
  (with-fixture hello-world-clang
    (setf (genome *hello-world*) "")
    (is (null  (asts *hello-world*)))
    (is (null  (stmt-asts *hello-world*)))
    (is (null  (non-stmt-asts *hello-world*)))
    (is (null  (functions *hello-world*)))
    (is (null  (prototypes *hello-world*)))
    (is (null  (includes *hello-world*)))
    (is (null  (macros *hello-world*)))
    (is (null  (globals *hello-world*)))
    (is (null  (fitness *hello-world*)))
    (is (zerop (hash-table-count (types *hello-world*))))))

(deftest asts-are-set-lazily ()
  (with-fixture hello-world-clang
    (is (null (slot-value *hello-world* 'ast-root))
        "ast-root is initially null")
    (is (asts *hello-world*)
        "ASTs are loaded when needed")
    (is (slot-value *hello-world* 'ast-root)
        "ast-root is set after loading ASTS.")))

(deftest asts-are-set-on-copy ()
  (with-fixture hello-world-clang
    (let ((new (copy *hello-world*)))
      (is (slot-value new 'ast-root)
          "ASTs set on copy")
      (is (eq (slot-value new 'ast-root) (slot-value *hello-world* 'ast-root))
          "Copy and original share ASTs")

      (apply-mutation new (make-instance 'clang-swap :object new))
      (is (eq (slot-value new 'ast-root) (slot-value (copy new) 'ast-root))
          "Additional copies do not cause updates"))))

(deftest splits-global-and-stmt-asts ()
  (with-fixture huf-clang
    (is (find-if [{string= "\"this is an example for huffman encoding\""}
                  #'source-text]
                 (non-stmt-asts *huf*))
        "Ensure known global is in `globals'.")
    (is (find-if [{string= "int i"} #'source-text]
                 (stmt-asts *huf*))
        "Ensure known local variable is in `stmts'.")
    (is (null (find :ParmVar (stmt-asts *huf*)
                    :key #'ast-class))
        "Ensure no ParmVar statement ASTs")
    (is (null (find :Function (stmt-asts *huf*)
                    :key #'ast-class))
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
    (let ((sel::*compilation-timeout* 0.0000001))
      (phenome *hello-world*))))

(deftest can-apply-mutation-w-value1 ()
  (with-fixture hello-world-clang
    (let* ((variant (copy *hello-world*))
           (stmt1 (stmt-with-text variant
                                  "printf(\"Hello, World!\\n\")")))
      (apply-mutation variant
        `(clang-replace
          (:stmt1 . ,stmt1)
          (:value1 . ,(make-literal :integer 0))))
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

(deftest clang-copies-are-independent ()
  (with-fixture hello-world-clang
    (let ((orig-genome (genome *hello-world*))
          (variant (copy *hello-world*)))
      (apply-mutation
       variant
       `(clang-cut (:stmt1 . ,(stmt-with-text variant
                                              "printf(\"Hello, World!\\n\")"))))
      (is (string= (genome *hello-world*) orig-genome))
      (is (not (string= (genome variant) orig-genome))))))

(deftest clang-copy-clears-genome-slot ()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*)))
      (is (null (slot-value (copy *hello-world*) 'genome)))

      (is (string= (genome *hello-world*)
                   (genome variant))))))

(deftest clang-copies-share-asts ()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*)))
      (is (eq (ast-root *hello-world*)
              (ast-root variant)))
      (is (> (size variant) 0)))))

(deftest clang-mutation-preserves-unmodified-subtrees ()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*)))
      (apply-mutation
       variant
       `(clang-cut (:stmt1 . ,(stmt-with-text variant
                                              "printf(\"Hello, World!\\n\")"))))
      (is (eq (ast-ref-ast (stmt-with-text *hello-world* "return 0"))
              (ast-ref-ast (stmt-with-text variant "return 0")))))))

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
      (is (eq :BinaryOperator     ; Guard
                   (ast-class (stmt-with-text var "a > b"))))
      (is (eq :BinaryOperator     ; Then
                   (ast-class (stmt-with-text var "a = a - b"))))
      (is (eq :BinaryOperator     ; Else
                   (ast-class (stmt-with-text var "b = b - a"))))
      ;; Wrap children and ensure changes are made.

      (setf var (wrap-child var (stmt-starting-with-text var "if (a > b)")
                            1))
      (setf var (wrap-child var (stmt-starting-with-text var "if (a > b)")
                            2))
      (is (eq :BinaryOperator     ; Guard
                   (ast-class (stmt-with-text var "a > b"))))
      (is (eq :CompoundStmt       ; Then
                   (ast-class (get-parent-ast var
                                              (stmt-with-text var "a = a - b")))))
      (is (eq :CompoundStmt       ; Then
                   (ast-class (get-parent-ast var
                                              (stmt-with-text var "b = b - a")))))
      ;; Ensure gcd remains unchanged.
      (is (eq :BinaryOperator     ; Guard
                   (ast-class (stmt-with-text *gcd* "a > b"))))
      (is (eq :BinaryOperator     ; Then
                   (ast-class (stmt-with-text *gcd* "a = a - b"))))
      (is (eq :BinaryOperator     ; Else
                   (ast-class (stmt-with-text *gcd* "b = b - a")))))))

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
      (is (member "\"second.c\"" includes :test #'equal))
      (is (member "\"third.c\"" includes :test #'equal)))))

(deftest clang-macros-initialized ()
  (with-fixture headers-clang
    (let ((macros (macros *headers*)))
      (is (listp macros))
      (is (= 2 (length macros)))
      (is (member "MAIN" (macros *headers*)
                  :key #'macro-name :test #'string=))
      (is (member "ANOTHER" (macros *headers*)
                  :key #'macro-name :test #'string=)))))

(deftest clang-types-initialized ()
  (with-fixture headers-clang
    (let ((types (types *headers*)))
      (is (hash-table-p types))
      (is (equal (list "bar" "char" "char*" "foo" "int")
                 (sort (mapcar #'type-name (hash-table-values types))
                       #'string<))))))

(deftest update-asts-doesnt-duplicate-includes ()
  (with-fixture headers-clang
    ;; each include only appears once in the genome
    ;; (all-matches includes start/end so length is double the number of
    ;; occurrences)
    (is (= 2 (->> (genome *headers*)
                  (all-matches "#include\\w* \"first.c\"")
                  (length))))
    (is (= 2 (->> (genome *headers*)
                  (all-matches "#include\\w* \"third.c\"")
                  (length))))))

(deftest add-macro-test ()
  (with-fixture hello-world-clang
    (add-macro *hello-world* (make-clang-macro :hash 3656618339188109385
                                               :name "ONE"
                                               :body "ONE 1"))
    (is (equal 1 (length (macros *hello-world*))))
    (is (not (null (search "#define ONE 1" (genome *hello-world*)))))))

(deftest find-macro-test ()
  (with-fixture hello-world-clang
    (add-macro *hello-world* (make-clang-macro :hash 3656618339188109385
                                               :name "ONE"
                                               :body "ONE 1"))
    (is (not (null (find-macro *hello-world* 3656618339188109385))))))

(deftest add-type-with-include-test ()
  (with-fixture fib-clang
    (add-type *fib* (make-clang-type :array ""
                                     :col 1
                                     :line 48
                                     :file "\/usr\/include\/stdio.h"
                                     :hash 3346600377836954008
                                     :i-file "<stdio.h>"
                                     :pointer t
                                     :reqs nil
                                     :size 8
                                     :name "FILE"))
    (is (equal 1 (length (includes *fib*))))))

(deftest add-bad-include-doesnt-change-number-of-asts ()
  (with-fixture hello-world-clang
    (let ((orig-num-asts (size *hello-world*)))
      (sel::add-include *hello-world* "<garbage.h>")
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest add-bad-type-doesnt-change-number-of-asts ()
  (with-fixture hello-world-clang
    (let ((orig-num-asts (size *hello-world*)))
      (sel::add-type *hello-world*
          (make-clang-type :decl "struct printf { chocolate cake; }"
                           :array "" :hash 0 :name "struct printf"))
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest add-new-type-changes-genome-and-types ()
  (with-fixture hello-world-clang
    (let ((orig-genome-length (length (genome *hello-world*)))
          (orig-num-types (hash-table-count (types *hello-world*)))
          (struct-str "struct printf { chocolate cake; }"))
      (sel::add-type *hello-world*
          (make-clang-type :decl struct-str
                           :array "" :hash 0 :name "struct printf"))
      ;; new type gets added to genome
      (is (= (+ orig-genome-length (length struct-str)
                (length (genome *hello-world*)))))
      (is (search struct-str (genome *hello-world*)))
      ;; new type is added to types
      (is (= (1+ orig-num-types) (hash-table-count (types *hello-world*)))))))

(deftest add-bad-macro-doesnt-change-number-of-asts ()
  (with-fixture hello-world-clang
    (let ((orig-num-asts (size *hello-world*)))
      (sel::add-macro *hello-world*
                     (sel::make-clang-macro :name "GARBAGE"
                                           :body "GARBAGE TRASH"
                                           :hash -4794347995631201955))
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest force-include-test ()
  (with-fixture hello-world-clang
    (let ((copy (copy *hello-world*)))
      (force-include copy "<system.h>")
      (force-include copy "<system.h>")
      (is (member "<system.h>" (includes copy) :test #'string=)
          "<system.h> should have been added the software object's includes")
      (is (not (equal (search "<system.h>" (genome copy) :from-end nil)
                      (search "<system.h>" (genome copy) :from-end t)))
          "<system.h> should have been added twice to the software object"))))

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
    (is (equalp (remove-if-not #'sel::full-stmt-filter
                               (stmt-asts *hello-world*))
                (mutation-targets *hello-world*
                                  :filter #'sel::full-stmt-filter)))))

(deftest clang-mutation-targets-stmt-pool-test ()
  "Ensure the stmt-pool parameter to mutation-targets works as anticipated"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (remove-if-not #'sel::full-stmt-filter
                                     (stmt-asts *hello-world*))))
      (is (equalp (remove-if-not #'sel::full-stmt-filter
                                 (stmt-asts *hello-world*))
                  (mutation-targets *hello-world*
                                    :stmt-pool #'bad-stmts))))))

(deftest clang-mutation-targets-expand-stmt-pool-restart-test ()
  "Ensure the expand-stmt-pool restart works as intended"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (remove-if-not #'sel::full-stmt-filter
                                     (stmt-asts *hello-world*))))
      ;; Before invoking the 'expand-stmt-pool filter, the
      ;; stmt pool does not include any full statements.
      ;; After its invocation, all full statements are returned.
      (is (equalp (remove-if-not #'sel::full-stmt-filter
                                 (stmt-asts *hello-world*))
                  (->> (handler-bind
                         ((no-mutation-targets
                           (lambda (c)
                             (declare (ignorable c))
                             (invoke-restart 'expand-stmt-pool))))
                       (mutation-targets *hello-world*
                                         :filter #'sel::full-stmt-filter
                                         :stmt-pool #'bad-stmts))))))))

(deftest clang-pick-general-does-not-throw-test ()
  "Ensure calling pick-general does not throw an exception"
  (with-fixture hello-world-clang
    (is (not (null (sel::pick-general *hello-world* #'stmt-asts))))))

(deftest clang-pick-general-full-stmt-no-matching-test ()
  "Ensure calling pick-general with a full-stmt filter
throws a no-mutation-targets error when there are no full stmts,
e.g. after a bad crossover"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (remove-if #'sel::full-stmt-filter
                                 (stmt-asts *hello-world*))))
      (signals no-mutation-targets
        (sel::pick-general *hello-world* #'bad-stmts
                          :filter #'sel::full-stmt-filter)))))

(deftest clang-pick-general-full-stmt-test ()
  "Ensure calling pick-general with a full-stmt filter returns a full
statement pick"
  (with-fixture hello-world-clang-control-picks
    (let ((pick (sel::pick-general *hello-world* #'stmt-asts
                                  :filter #'sel::full-stmt-filter)))
      (is (->> (aget :stmt1 pick)
               (ast-full-stmt))))))

(deftest clang-pick-general-same-class-no-matching-test ()
  "Ensure calling pick-general with a same-class filter throws
a no-mutation-targets error when a second statement with the same AST class
is not to be found"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (list (snippet->clang-ast '((:ast-class . :Nothing))))))
      (signals no-mutation-targets
        (sel::pick-general *hello-world* #'stmt-asts
                          :filter #'sel::same-class-filter
                          :second-pool #'bad-stmts)))))

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
    (signals no-mutation-targets (sel::pick-cut-decl *soft*))))

(deftest pick-swap-decls-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets (sel::pick-swap-decls *soft*))))

(deftest pick-rename-variable-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets (sel::pick-rename-variable *soft*))))

(deftest cpp-strings-works ()
  ;; On this example, clang-mutate generates ASTs that are out of
  ;; order. Check that asts->tree handles this case correctly.
  (with-fixture cpp-strings
    (is *soft*)
    (let ((stmt (stmt-with-text *soft* "x == \"test\"")))
      (is stmt)
      (is (eq :CXXOperatorCallExpr (ast-class stmt)))
      (is (every [{eq :ImplicitCastExpr} #'ast-class]
                 (get-immediate-children *soft* stmt))))))

(deftest typedef-workaround ()
  (with-fixture typedef
    (let ((typedef (stmt-starting-with-text *soft* "typedef")))
      (is typedef)
      (is (not (null (get-immediate-children *soft* typedef))))
      (is (equal '(:Record)
                 (mapcar #'ast-class
                         (get-immediate-children *soft* typedef)))))))

(deftest overlapping-sibling-asts ()
  ;; A combination of macros and case statements produces tricky
  ;; overlapping source ranges. Test that update-asts can handle it
  ;; correctly.
  (with-fixture switch-macros-clang
    (is (equal '(:CharacterLiteral :MacroExpansion)
               (->> (stmt-starting-with-text *soft* "case 'F'")
                    (get-immediate-children *soft*)
                    (mapcar #'ast-class))))))

(deftest replace-in-ast-subtree ()
  (let ((subtree '(:sub 1 2)))
    (equalp (replace-in-ast `(:root (:left 3 4) ,subtree)
                            `((,subtree (:right 5 6))))
            '(:root (:left 3 4) (:right 5 6)))))

(deftest replace-in-ast-string ()
  (equalp (replace-in-ast '(:root (:left "left") (:right "right"))
                          '(("right" "replacement"))
                          :test #'equal)
          '(:root (:left "left") (:right "replacement"))))

(deftest find-or-add-type-finds-existing-type ()
  (with-fixture gcd-clang
    (is (eq (find-if [{string= "int"} #'type-name]
                     (hash-table-values (types *gcd*)))
            (find-or-add-type *gcd* "int")))))

(deftest find-or-add-type-adds-new-type ()
  (with-fixture gcd-clang
    (is (null (find-if [{string= "float"} #'type-name]
                       (hash-table-values (types *gcd*)))))
    (let ((new-type (find-or-add-type *gcd* "int")))
      (is new-type "New type created.")
      (is (gethash (type-hash new-type) (types *gcd*))
          "New type is added to software.")
      (is (eq new-type (find-or-add-type *gcd* "int"))
          "Repeated call finds same type."))))

(deftest find-or-add-type-parses-pointers ()
  (with-fixture gcd-clang
    (is (eq (find-or-add-type *gcd* "*char")
            (find-or-add-type *gcd* "char" :pointer t)))))

(deftest var-decl-has-correct-types ()
  (let ((obj (make-instance 'clang
                            :genome "int x = sizeof(int);")))
    ;; A var decl should always directly reference the type of its
    ;; declaration. This is tricky due to the de-aggregating of types
    ;; done by asts->tree.

    ;; FIXME: if the RHS were "sizeof(int) + sizeof(char)" the decl
    ;; would reference both types, which is incorrect but probably
    ;; harmless.
    (is (equalp '("int")
                (mapcar [#'type-name {find-type obj}]
                        (sel::ast-types (first (asts obj))))))))

(deftest macro-expansion-has-correct-types ()
  ;; Types inside a macro expansion should be visible. This is trick
  ;; due to the de-aggregating of types done by asts->tree.
  (let ((obj (make-instance 'clang
                            :genome "#define CHARSIZE (sizeof (char))
int x = CHARSIZE;")))
    (is (equalp '("int" "char")
                (mapcar [#'type-name {find-type obj}]
                        (get-ast-types obj (first (asts obj))))))))

(deftest able-to-handle-multibyte-characters ()
  (with-fixture unicode-clang
    (is (stmt-with-text *soft* "int x = 0" :no-error))
    (is (stmt-with-text *soft* "\"2 bytes: Δ\"" :no-error))
    (is (stmt-with-text *soft* "int y = 1" :no-error))
    (is (string= (genome *soft*)
                 (file-to-string (unicode-dir "unicode.c"))))))


;;; Detailed clang mutation tests
;;;
;;; These all run the entire mutate method, rather that just
;;; apply-mutation, adjusting the good and bad picks to get
;;; predictable results. And they check the results of each mutation
;;; in as much detail as possible.
(sel-suite* clang-mutations "Detailed clang mutation tests."
            (clang-mutate-available-p))

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
      (is (stmt-with-text variant "return 00" :no-error)))))

(deftest insert-full-same-adds-same-class-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0"))
          (*clang-mutation-types* '((clang-insert-full-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if [{eq :ReturnStmt} #'ast-class]
                       (asts variant))
             (count-if [{eq :ReturnStmt} #'ast-class]
                       (asts *hello-world*)))))))

(deftest replace-changes-any-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "0"))
          (*good-asts* (asts-with-text *hello-world* "printf"))
          (*clang-mutation-types* '((clang-replace . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "return printf" :no-error)))))

(deftest replace-full-changes-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0"))
          (*good-asts* (asts-with-text *hello-world*
                                       "0" "printf(\"Hello, World!\\n\")"))
          (*clang-mutation-types* '((clang-replace-full . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if [{eq :CallExpr} #'ast-class]
                       (asts variant))
             (count-if [{eq :CallExpr} #'ast-class]
                       (asts *hello-world*)))))))

(deftest replace-same-changes-same-class ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "\"Hello, World!\\n\""))
          (*good-asts* (asts-with-text *hello-world*
                                       "0" "printf"))
          (*clang-mutation-types* '((clang-replace-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "printf(printf)" :no-error)))))

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
      (is (stmt-with-text variant "\"Hello, World!\\n\"" :no-error))
      (is (stmt-with-text variant "0" :no-error)))))

(deftest move-changes-any-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let* ((bad-1 "printf(\"Hello, World!\\n\")")
           (bad-2 "return 0")
           (*bad-asts* (asts-with-text *hello-world* bad-1 bad-2))
           (*clang-mutation-types* '((clang-move . 1))))
      (multiple-value-bind (variant mut) (mutate (copy *hello-world*))
        ;; If both targets are the same, genome will not change.
        (unless (eq (aget :stmt1 (targets mut)) (aget :stmt2 (targets mut)))
         (is (not (string= (genome *hello-world*) (genome variant)))
             "Move changes genome."))
        ;; Still exist (> 0).
        (is (stmt-with-text variant bad-1 :no-error)
            "Move doesn't remove \"Hello, World!\\n\".")
        (is (stmt-with-text variant bad-2 :no-error)
            "Move doesn't remove \"0\".")
        ;; No duplicates (< 2).
        (is
         (= 1 (length (all-matches-as-strings (quote-meta-chars bad-1)
                                              (genome variant))))
         "Move doesn't duplicate \"Hello, World!\\n\".")
        (is
         (= 1 (length (all-matches-as-strings (quote-meta-chars bad-2)
                                              (genome variant))))
         "Move doesn't duplicate \"0\".")))))

(deftest swap-full-changes-full-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let ((*clang-mutation-types* '((clang-swap-full . 1)))
          ;; Avoid swapping the function body
          (*bad-asts* (remove-if [{member _  '(:CompoundStmt :Function)}
                                  #'ast-class]
                                 (asts *hello-world*)))
          (variant (copy *hello-world*)))

      (multiple-value-bind  (variant mutation) (mutate variant)
        ;; We can't predict exactly what will be swapped. Just
        ;; sanity check.
        (is (ast-full-stmt (aget :stmt1 (targets mutation))))
        (is (ast-full-stmt (aget :stmt2 (targets mutation))))
        (is (stmt-with-text variant "printf" :no-error))
        (is (stmt-with-text variant "return 0" :no-error))))))

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
                  :targets (->> (stmt-with-text *nested* "puts('FOR-1')")
                                (get-parent-asts *nested*)
                                (third))))))
           "/* For loop. */"
           "puts('FOR-1');")
          "Promotes single-line body from within for loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('FOR-2')")
                                (get-parent-asts *nested*)
                                (third))))))
           "/* For loop with empty header. */"
           "puts('FOR-2');")
          "Promotes single-line body from within for loop 2.")
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
            "Promotes multi-line body from within while loop.")))

    (with-fixture gcd-wo-curlies-clang
      (let ((genome-string
             (let ((copy (copy *gcd*)))
               (genome
                (apply-mutation copy
                  (make-instance 'clang-promote-guarded
                    :object copy
                    :targets (stmt-starting-with-text *gcd* "if (a == 0)")))))))
        (is (and (subsequent-lines-p genome-string
                                     "printf(\"%g\\n\", b);"
                                     "while (b != 0)"))
            "Promotes unbraced then from within if wi/o else.")))))

(deftest if-to-while-test ()
  (with-fixture gcd-clang-control-picks
    (let ((*clang-mutation-types* '((if-to-while . 1)))
          (*bad-asts* (list (find-if [{eq :IfStmt} #'ast-class]
                                     (stmt-asts *gcd*)))))
      (multiple-value-bind  (variant mutation) (mutate (copy *gcd*))
        (is (eq :IfStmt
                (ast-class (targets mutation))))
        (let ((stmt (stmt-starting-with-text variant "while")))
          (is stmt)
          (is (eq :WhileStmt (ast-class stmt))))))))


;;;; Clang w/ mutation fodder representation.
(sel-suite* clang-w-fodder-tests "Clang w/ mutation fodder representation."
            (clang-mutate-available-p))


(deftest parse-source-snippet-body-statement ()
  (with-fixture gcd-clang
    (let ((asts (parse-source-snippet "x + y"
                                      `(("x" ,(type-from-trace-string "int"))
                                        ("y" ,(type-from-trace-string "char")))
                                      nil)))
      (is (eq 1 (length asts)))
      (is (eq :BinaryOperator (ast-class (car asts))))
      (is (equalp '(((:name . "y")) ((:name . "x")))
                  (get-unbound-vals *gcd* (car asts)))))))

(deftest parse-source-snippet-handles-includes ()
  (let ((asts (parse-source-snippet "printf(\"hello\")"
                                    nil '("<stdio.h>"))))
    (is (eq 1 (length asts)))
    (is (eq :CallExpr (ast-class (car asts))))
    (is (equalp '("<stdio.h>")
                (ast-includes (car asts))))))

(deftest parse-source-snippet-multiple-statements ()
  (let ((asts (parse-source-snippet "x = 1; y = 1"
                                    `(("x" ,(type-from-trace-string "int"))
                                      ("y" ,(type-from-trace-string "char")))
                                    nil)))
    (is (eq 2 (length asts)))
    (is (eq :BinaryOperator (ast-class (first asts))))
    (is (eq :BinaryOperator (ast-class (second asts))))))

(deftest parse-source-snippet-top-level ()
  (let ((asts (parse-source-snippet "int foo() { return 1; }" nil nil
                                    :top-level t)))
    (is (eq 1 (length asts)))
    (is (eq :Function (ast-class (car asts))))
    (is (eq :CompoundStmt (ast-class (function-body (make-instance 'clang)
                                                          (car asts)))))))

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
    (let ((variant (copy *hello-world*))
          (target (stmt-with-text *hello-world* "return 0")))
      (apply-mutation variant
                      `(clang-insert (:stmt1 . ,target)
                                     (:value1 . ,target)))
      (is (> (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest set-value-changes-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant
        `(clang-replace
          (:stmt1 . ,(find-if [{eq :StringLiteral} #'ast-class]
                              (asts variant)))
          (:literal1 . ,(make-literal :string "Hello, mutate!"))))
      (is (= (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest insert-fodder-lengthens-a-clang-w-fodder-software-object ()
  (with-fixture gcd-clang-w-fodder
    (let ((variant (copy *gcd*)))
      (handler-case
          (progn (apply-mutation variant (make-instance 'insert-fodder
                                                        :object variant))
                 (is (> (size variant) (size *gcd*)))
                 (is (string/= (genome variant) (genome *gcd*))))

        (mutate (e)
          ;; Fodder mutations may fail when bad variable bindings make
          ;; the snippet unparseable.
          (is (search "Failed to parse fodder" (text e))))))))

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


;;;; Clang utility methods.
(sel-suite* clang-utility "Clang utility methods."
            (clang-mutate-available-p))


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
                    (remove-if-not [{eq :CompoundStmt} #'ast-class])
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

(deftest find-var-type-returns-correct-type ()
  (flet
      ((get-var-type (var stmt-text)
         (&>> (stmt-with-text *soft* stmt-text)
              (get-vars-in-scope *soft*)
              (find-if [{string= var} {aget :name}])
              (find-var-type *soft*))))
    (with-fixture type-of-var-clang
      (let ((var-type1 (get-var-type "a" "return 0"))
            (var-type2 (get-var-type "a" "return 1"))
            (var-type3 (get-var-type "a" "return 2"))
            (var-type4 (get-var-type "a" "int a[N][N]"))
            (var-type5 (get-var-type "b" "return 2")))
        (is (null var-type4))
        (is (equal "[10][10]" (type-array var-type1)))
        (is (equal ""         (type-array var-type2)))
        (is (equal "[10][10]" (type-array var-type3)))
        (is (equal ""         (type-array var-type5)))
        (is (equal nil        (type-pointer var-type1)))
        (is (equal t          (type-pointer var-type2)))
        (is (equal nil        (type-pointer var-type3)))
        (is (equal t          (type-pointer var-type5)))
        (is (equal "int"      (type-name var-type1)))
        (is (equal "int"      (type-name var-type2)))
        (is (equal "int"      (type-name var-type3)))
        (is (equal "int*"     (type-name var-type5)))))))

(deftest find-var-type-handles-missing-declaration-type ()
  (with-fixture type-of-var-missing-decl-type-clang
    (is (null (&>> (stmt-with-text *soft* "dirs[0] = L")
                   (get-vars-in-scope *soft*)
                   (find-if [{string= "dirs"} {aget :name}])
                   (find-var-type *soft*))))))

(deftest typedef-type-returns-correct-type ()
  (with-fixture typedef-type-clang
    (let ((type1 (&>> (stmt-with-text *soft* "gint a")
                      (get-ast-types *soft*)
                      (car)
                      (find-type *soft*)
                      (typedef-type *soft*)))
          (type2 (&>> (stmt-with-text *soft* "gchar *b")
                      (get-ast-types *soft*)
                      (car)
                      (find-type *soft*)
                      (typedef-type *soft*))))
      (is (equal "int"  (type-name type1)))
      (is (equal nil    (type-pointer type1)))
      (is (equal "char" (type-name type2)))
      (is (equal t      (type-pointer type2))))))

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

(deftest rebind-vars-in-macro-test ()
  (with-fixture assert-clang
    (let* ((copy (copy *soft*))
           (stmt (stmt-with-text copy "assert(argc > 0)")))
      (is (equalp "assert((|someVal|) > 0)"
                  (->> (sel::rebind-vars stmt
                                        '(("(|argc|)" "(|someVal|)"))
                                        nil)
                       (source-text)))
          "rebind-vars did not rebind a variable within a macro"))))


;;;; Range representation.
(sel-suite* range-representation "Range representation.")


(deftest range-size ()
  (with-fixture range (is (= 6 (size *soft*)))))

(deftest range-lines ()
  (with-fixture range
    (is (tree-equal (lines *soft*)
                    '("one" "two" "three" "two" "two" "three")
                    :test #'string=))))

(deftest range-nth-test ()
  (with-fixture range
    (is (equal (mapcar {software-evolution-library::range-nth _ (genome *soft*)}
                       (loop :for i :below (size *soft*) :collect i))
               '(0 1 2 1 1 2)))))

(deftest range-subseq-test ()
  (with-fixture range
    ;; to
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 0 1)
                    '((0 . 0))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 0 2)
                    '((0 . 1))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 0 3)
                    '((0 . 2))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 0 4)
                    '((0 . 2) (1 . 1))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 0 5)
                    '((0 . 2) (1 . 1) (1 . 1))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 0 6)
                    '((0 . 2) (1 . 1) (1 . 2))))
    ;; from
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 1 7)
                    '((1 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 2 7)
                    '((2 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 3 7)
                    '((1 . 1) (1 . 2))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 4 7)
                    '((1 . 2))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 5 7)
                    '((2 . 2))))
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 6 7)
                    'NIL))
    ;; both
    (is (tree-equal (software-evolution-library::range-subseq (genome *soft*) 2 5)
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


;;;; Mutation analysis and statistics collection tests.
(sel-suite* mutation-analysis
    "Mutation analysis and statistics collection tests."
    (clang-mutate-available-p))


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
                            (:literal1 . ,(make-literal :integer 0))))))
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


;;;; Ancestry tests.
(sel-suite* ancestry "Ancestry tests."
            (clang-mutate-available-p))


(defclass clang-w-ancestry (clang ancestral) ())

(defixture hello-world-clang-w-ancestry
  (:setup
   (setf software-evolution-library::*next-ancestry-id* 0
         *hello-world*
         (from-file (make-instance 'clang-w-ancestry :compiler "clang"
                                   :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))
         *test* [#'length #'genome])
   (evaluate *test* *hello-world*))
  (:teardown
   (setf *hello-world* nil
         *test* nil
         software-evolution-library::*next-ancestry-id* 0)))

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


;;;; Diff tests.
(sel-suite* diff-tests "Diff tests.")


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


;;;; Population tests.
(sel-suite* population-tests "Population tests."
            (clang-mutate-available-p))


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


;;;; Helper functions to avoid hard-coded statement numbers.
(defun stmt-with-text (obj text &optional no-error)
  "Return the AST in OBJ holding TEXT.
Unless optional argument NO-ERROR is non-nil an error is raised if no
AST holding STMT is found."
  (or (find-if [{string= text} #'peel-bananas #'source-text]
               (asts obj))
      (if no-error
          nil
          (error "`stmt-with-text' failed to find ~S in ~S" text obj))))

(defun stmt-starting-with-text (obj text)
  (find-if (lambda (snippet)
             (and snippet
                  (equal 0
                         (search text
                                 (peel-bananas (source-text snippet))))))
           (asts obj)))

(deftest swap-can-recontextualize ()
  (with-fixture huf-clang
    ;; NOTE: Without the retries recontectualization can fail
    ;;       stochastically, not all possible rebinding variables
    ;;       work.
    ;;
    ;; TODO: Refine recontextualization with type information so that
    ;;       this *always* works.
    (let ((mut (make-instance 'clang-swap :targets
                              (list (cons :stmt1 (stmt-with-text *huf* "n > 0"))
                                    (cons :stmt2 (stmt-with-text *huf* "bc=0"))))))
      (is (iter (for var = (apply-mutation (copy *huf*) mut))
                (as count upfrom 0)
                (when (compile-p var) (return t))
                (when (> count 100) (return nil)))
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


;;;; Fix compilation tests.
(sel-suite* fix-compilation-tests "Fix compilation tests."
            (clang-mutate-available-p))


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


;;;; Crossover tests.
(sel-suite* clang-crossover "Crossover tests."
            (clang-mutate-available-p))


(defun select-intraprocedural-pair-with-adjustments-test (obj)
  (let ((function (first (functions obj))))
    (loop :for i :from 0 :to 25
          :do (progn (multiple-value-bind (pt1 pt2)
                         (select-intraprocedural-pair obj)
                       (multiple-value-bind (stmt1 stmt2)
                           (adjust-stmt-range obj pt1 pt2)
                         (is (<= (1+ (first (sel::stmt-range obj function)))
                                 stmt1
                                 (second (sel::stmt-range obj function))))
                         (is (<= (1+ (first (sel::stmt-range obj function)))
                                 stmt2
                                 (second (sel::stmt-range obj function))))
                         (is (full-stmt-p obj
                                          (sel::ast-at-index obj stmt1)))
                         (is (full-stmt-p obj
                                          (sel::ast-at-index obj stmt2)))))))))

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
                (sel::ancestor-after *fib*
                                    (->> "int fib"
                                         (stmt-starting-with-text *fib*)
                                         (function-body *fib*))
                                    (stmt-with-text *fib* "int x = 0"))))
    (is (equalp (->> (stmt-asts *fib*)
                     (remove-if-not [{eq :WhileStmt} #'ast-class])
                     (first))
                (sel::ancestor-after *fib*
                                    (->> "int fib"
                                         (stmt-starting-with-text *fib*)
                                         (function-body *fib*))
                                    (stmt-with-text *fib* "int t = x"))))
    (is (equalp (stmt-with-text *fib* "x = x + y")
                (sel::ancestor-after *fib*
                                    (->> "while "
                                         (stmt-starting-with-text *fib*)
                                         (get-immediate-children *fib*)
                                         (second))
                                    (stmt-with-text *fib* "x = x + y"))))))

(deftest ancestor-after-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (stmt-with-text *collatz* "int k = 0")
                (sel::ancestor-after *collatz*
                                    (->> "int collatz"
                                         (stmt-starting-with-text *collatz*)
                                         (function-body *collatz*))
                                    (stmt-with-text *collatz* "int k = 0"))))
    (is (equalp (stmt-with-text *collatz* "return k")
                (sel::ancestor-after *collatz*
                                    (->> "int collatz"
                                         (stmt-starting-with-text *collatz*)
                                         (function-body *collatz*))
                                    (stmt-with-text *collatz* "return k"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (remove-if-not [{eq :WhileStmt} #'ast-class])
                     (first))
                (sel::ancestor-after *collatz*
                                    (->> "int collatz"
                                         (stmt-starting-with-text *collatz*)
                                         (function-body *collatz*))
                                    (stmt-with-text *collatz* "m /= 2"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (remove-if-not [{eq :IfStmt} #'ast-class])
                     (first))
                (sel::ancestor-after *collatz*
                                    (->> "while"
                                         (stmt-starting-with-text *collatz*)
                                         (get-immediate-children *collatz*)
                                         (second))
                                    (stmt-with-text *collatz* "m /= 2"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (remove-if-not [{eq :IfStmt} #'ast-class])
                     (first))
                (sel::ancestor-after *collatz*
                                    (->> "while"
                                         (stmt-starting-with-text *collatz*)
                                         (get-immediate-children *collatz*)
                                         (second))
                                    (stmt-with-text *collatz* "m = 3*m + 1"))))
    (is (equalp (stmt-with-text *collatz* "++k")
                (sel::ancestor-after *collatz*
                                    (->> "while"
                                         (stmt-starting-with-text *collatz*)
                                         (get-immediate-children *collatz*)
                                         (second))
                                    (stmt-with-text *collatz* "++k"))))))

(deftest ancestor-after-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (sel::ancestor-after *soft*
                                    (stmt-starting-with-text *soft* "for (i = 0")
                                    (stmt-with-text *soft*
                                                    "printf(\"%d\\n\", i+j)"))))
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                (sel::ancestor-after *soft*
                                    (stmt-starting-with-text *soft*
                                                             "for (j = 0")
                                    (stmt-with-text *soft*
                                                    "printf(\"%d\\n\", i+j)"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (sel::ancestor-after *soft*
                                    (->> "int main"
                                         (stmt-starting-with-text *soft*)
                                         (function-body *soft*))
                                    (stmt-with-text *soft*
                                                    "printf(\"%d\\n\", i+j)"))))
    (is (equalp (stmt-with-text *soft* "return 0")
                (sel::ancestor-after *soft*
                                    (->> "int main"
                                         (stmt-starting-with-text *soft*)
                                         (function-body *soft*))
                                    (stmt-with-text *soft* "return 0"))))))

(deftest ancestor-after-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "case 2")
                (sel::ancestor-after *soft*
                                    (->> "switch"
                                         (stmt-starting-with-text *soft*)
                                         (get-immediate-children *soft*)
                                         (second))
                                    (->> "printf(\"%d\\n\", argc * argc)"
                                         (stmt-with-text *soft*)))))))

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
    (is (equalp (->> (remove-if-not [{eq :ForStmt} #'ast-class]
                                   (stmt-asts *soft*))
                    (first))
               (enclosing-full-stmt *soft*
                                    (->> (remove-if-not [{eq :ForStmt}
                                                         #'ast-class]
                                                        (stmt-asts *soft*))
                                         (first)))))
    (is (equalp (->> (remove-if-not [{eq :ForStmt} #'ast-class]
                                   (stmt-asts *soft*))
                    (second))
               (enclosing-full-stmt *soft*
                                    (->> (remove-if-not [{eq :ForStmt}
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
    (is (equalp (->> (remove-if-not [{eq :SwitchStmt} #'ast-class]
                                    (stmt-asts *soft*))
                     (first))
                (enclosing-full-stmt *soft*
                                     (->> (remove-if-not [{eq :SwitchStmt}
                                                          #'ast-class]
                                                         (stmt-asts *soft*))
                                          (first)))))
    (is (equalp (->> (remove-if-not [{eq :CaseStmt} #'ast-class]
                                    (stmt-asts *soft*))
                     (first))
                (enclosing-full-stmt *soft*
                                     (->> (remove-if-not [{eq :CaseStmt}
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
                     (find-if [{eq :WhileStmt} #'ast-class])
                     (get-immediate-children *collatz*)
                     (second))
                (enclosing-block *collatz* (stmt-with-text *collatz* "++k"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (find-if [{eq :IfStmt} #'ast-class])
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
                     (find-if [{eq :SwitchStmt} #'ast-class])
                     (get-immediate-children *soft*)
                     (second))
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                     (enclosing-block *soft*))))
    (is (equalp (->> (stmt-asts *soft*)
                     (find-if [{eq :SwitchStmt} #'ast-class])
                     (get-immediate-children *soft*)
                     (second))
                (->> (stmt-asts *soft*)
                     (remove-if-not [{eq :CaseStmt} #'ast-class])
                     (first)
                     (enclosing-block *soft*))))))

(deftest block-p-collatz-test ()
  (with-fixture collatz-clang
    (loop :for ast
          :in (stmt-asts *collatz*)
          :do (is (equal (eq :CompoundStmt (ast-class ast))
                         (block-p *collatz* ast))))))

(deftest block-p-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (loop :for ast
          :in (stmt-asts *soft*)
          :do (is (equal (or (eq :CompoundStmt (ast-class ast))
                             (eq :ForStmt (ast-class ast)))
                         (block-p *soft* ast))))))

(deftest block-p-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (loop :for ast
          :in (stmt-asts *soft*)
          :do (is (equal (eq :CompoundStmt (ast-class ast))
                         (block-p *soft* ast))))))

(deftest block-successor-collatz-test ()
  (with-fixture collatz-clang
    (is (eq :WhileStmt
            (->> (block-successor *collatz* (stmt-with-text *collatz*
                                                            "int k = 0"))
                 (ast-class))))
    (is (eq :ReturnStmt
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
    (is (eq :WhileStmt
            (->> (block-predeccessor *collatz*
                                     (stmt-with-text *collatz*
                                                     "printf(\"%d\\n\", k)"))
                 (ast-class))))
    (is (equal nil
               (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                             "m /= 2"))))
    (is (eq :IfStmt
            (->> (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                               "++k"))
                 (ast-class))))
    (is (eq :CallExpr
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
                 (remove-if-not [{eq :WhileStmt}
                                 #'ast-class])
                 (first))
            (stmt-with-text *fib* "int t = x")
            (->> (stmt-asts *fib*)
                 (remove-if-not [{eq :WhileStmt}
                                 #'ast-class])
                 (first)))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int t = x")
                          (->> (stmt-asts *fib*)
                               (remove-if-not [{eq :WhileStmt}
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

(deftest crossover-entire-text-of-a-function ()
  ;; Entire text of a function
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int a"))
           (a-stmt2 (stmt-with-text *scopes* "return a + b + c"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      ;; NOTE: Without the retries recontectualization can fail
      ;;       stochastically, not all possible rebinding variables
      ;;       work.
      ;;
      ;; TODO: Refine recontextualization with type information so
      ;;       that this *always* works.
      (is (iter (for (values variant a-pts b-pts ok effective-a-pts) =
                     (intraprocedural-2pt-crossover *scopes* *scopes*
                                                    a-stmt1 a-stmt2
                                                    b-stmt1 b-stmt2))
                (as count upfrom 0)
                (declare (ignorable a-pts b-pts effective-a-pts))
                (when (and ok
                           (compile-p variant)
                           (= (length (asts *scopes*))
                              (length (asts variant))))
                  (return t))
                (when (> count 100) (return nil)))
          "Able to crossover entire text of a function with 100 tries."))))

(deftest crossover-a-single-statement-the-first ()
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
               (length (asts variant))))))))

(deftest crossover-a-single-statement-the-last ()
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
               (length (asts variant))))))))

(deftest crossover-a-single-statement-complex ()
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
               (length (asts variant))))))))

(deftest crossover-a-statement-and-descendants ()
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
               (length (asts variant))))))))

(deftest crossover-a-statement-and-one-ancestor ()
  ;; A statement and one of its ancestors
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "c = 4"))
           (a-stmt2 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      ;; NOTE: Without the retries recontectualization can fail
      ;;       stochastically, not all possible rebinding variables
      ;;       work.
      ;;
      ;; TODO: Refine recontextualization with type information so
      ;;       that this *always* works.
      (is (iter (for (values variant a-pts b-pts ok effective-a-pts) =
                     (intraprocedural-2pt-crossover *scopes* *scopes*
                                                    a-stmt1 a-stmt2
                                                    b-stmt1 b-stmt2))
                (as count upfrom 0)
                (declare (ignorable a-pts b-pts effective-a-pts))
                (when (and ok
                           (compile-p variant)
                           (= (length (asts *scopes*))
                              (length (asts variant))))
                  (return t))
                (when (> count 100) (return nil)))
          "Able to crossover a statement and a ancestor with 100 tries."))))

(deftest crossover-a-statement-with-multiple-statements ()
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
        (is (stmt-with-text variant "a = 10" :no-error))
        (is (stmt-with-text variant "a = 11" :no-error))
        (is (stmt-with-text variant "a = 12" :no-error))
        (is (not (stmt-with-text variant "int b" :no-error)))))))

(deftest crossover-a-multiple-statements-with-a-single-statement ()
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
        (is (stmt-with-text variant "a = 8" :no-error))
        (is (not (stmt-with-text variant "int b" :no-error)))
        (is (not (stmt-with-text variant "b = 1" :no-error)))))))

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


;;;; Misc. mutation tests.
(sel-suite* misc-mutations "Misc. mutation tests."
            (clang-mutate-available-p))


(deftest single-decl-works ()
  (with-fixture scopes-clang
    (is (= 1 (length (ast-declares (stmt-with-text *scopes* "int a")))))))

(deftest multiple-decl-works ()
  (with-fixture scopes-clang
    ;; NOTE: Why isn't this statement reliably found?
    (when-let* ((ast (stmt-with-text *scopes* "int f, g" :no-error)))
      (is (= 2 (length (ast-declares ast)))))))

(deftest pick-for-loop-works ()
  (with-fixture scopes-clang
    (is (eq :ForStmt (->> (sel::pick-for-loop *scopes*)
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
    (is (eq :WhileStmt (->> (sel::pick-while-loop *scopes*)
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
                      (genome-string variant))))
      (let ((stmt (or (stmt-with-text variant "b = 13" :no-error)
                      (stmt-with-text variant "c = 13" :no-error))))
        (is stmt)
        ;; unbound-vals are updated correctly
        (let ((unbound (mapcar {aget :name} (get-unbound-vals variant stmt))))
          (is (or (equal unbound '("b"))
                  (equal unbound '("c")))))))
    (let ((variant (copy *scopes*)))
      (apply-mutation
          variant
        `(cut-decl (:stmt1 . ,(stmt-with-text *scopes* "int d"))))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))
    (when-let* ((variant (copy *scopes*))
                (id (stmt-with-text *scopes* "int f, g" :no-error)))
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
    (is (stmt-with-text obj "argc = argc * 2" :no-error))))

(deftest expand-arithmatic-op-works-complex-compound-assignment ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir
                         "complex-compound-assign.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc + ((argc*4) / rand())" :no-error))))

(deftest expand-arithmatic-op-works-increment ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "increment.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "i = i + 1" :no-error))))

(deftest expand-arithmatic-op-works-decrement ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "decrement.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc - 1" :no-error))))

(deftest expand-arithmatic-op-works-field-increment ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "field-increment.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "t.x = t.x + 1" :no-error))))

(deftest expand-arithmatic-op-works-field-decrement ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "field-decrement.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "t.x = t.x - 1" :no-error))))

(deftest expand-arithmatic-op-works-class-member-increment ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "class-member-increment.cpp"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "x = x + 1" :no-error))))

(deftest expand-arithmatic-op-works-class-member-decrement ()
  (let ((obj (from-file (make-instance 'clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "class-member-decrement.cpp"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "x = x - 1" :no-error))))


;;;; Adaptive-mutation tests.
(sel-suite* adaptive-mutation-tests "Adaptive-mutation tests.")


(deftest bad-cut-changes-mutation-probability ()
  (let* ((sel::*mutation-results-queue* #((cut . :worse) (cut . :dead)))
         (muts-0 '((cut . 1/2) (swap . 1)))
         (muts-1 (update-mutation-types muts-0))
         (muts-2 (update-mutation-types muts-1)))
    (is (< (aget 'cut muts-1) (aget 'cut muts-0))
        "Bad mutations lose probability.")
    (is (< (aget 'cut muts-2) (aget 'cut muts-1))
        "Bad mutations continue to lose probability.")))

(deftest mutation-queue-wraps-as-expected ()
  (let ((sel::*mutation-results-queue*
         (make-array 100
                     :element-type '(cons symbol symbol)
                     :initial-element (cons :nothing :nothing)))
        (sel::*mutation-results-queue-next* 0))
    (dotimes (n 100)
      (sel::queue-mutation 'cut :dead))
    (is (every [{equal 'cut} #'car] sel::*mutation-results-queue*)
        "`queue-mutation' fills `*mutation-results-queue*' as expected.")
    (sel::queue-mutation 'swap :better)
    (is (equalp (cons 'swap :better) (aref sel::*mutation-results-queue* 0))
        "`queue-mutation' wraps `*mutation-results-queue*' as expected.")))

(deftest update-mutation-types-returns-list-when-mutation-queue-unpopulated ()
  "Ensure update-mutation-types returns its first argument when the
*mutation-results-queue* is unpopulated"
  (let ((sel::*mutation-results-queue*
          (copy-seq sel::+initial-mutation-results-queue+))
        (sel::*mutation-results-queue-next* 0)
        (mutation-types (copy-seq *clang-mutation-types*)))
    (is (equalp mutation-types
                (update-mutation-types mutation-types)))))

(deftest update-mutation-types-returns-list-when-mutation-queue-populated ()
  "Ensure update-mutation-types returns a list when the
*mutation-results-queue* is populated"
  (let ((sel::*mutation-results-queue*
          (copy-seq sel::+initial-mutation-results-queue+))
        (sel::*mutation-results-queue-next* 0)
        (mutation-types (copy-seq *clang-mutation-types*)))
    (dotimes (n (length sel::+initial-mutation-results-queue+))
      (sel::queue-mutation 'cut :dead))
    (is (listp (update-mutation-types mutation-types)))))

(deftest adaptive-analyze-mutation-updates-results-queue-properly ()
  (let ((*fitness-predicate* #'<)
        (sel::*mutation-results-queue-next* 0)
        (sel::*mutation-results-queue*
         (copy-seq sel::+initial-mutation-results-queue+))
        (parent-a (make-instance 'clang :fitness 2))
        (parent-b (make-instance 'clang :fitness 2))
        (crossed  (make-instance 'clang :fitness 1))
        (mutant   (make-instance 'clang :fitness 0)))
      (adaptive-analyze-mutation mutant
                                 `(clang-cut ,parent-a 0
                                   ,crossed ,parent-b 0)
                                 {fitness})
      (is (equal :better (cdr (aref sel::*mutation-results-queue* 0))))))


;;;; Database tests.
(sel-suite* database "Database tests.")


(defixture json-database
  (:setup
    (setf *database*
          (with-open-file (in (make-pathname :name "euler-example.json"
                                             :directory +etc-dir+))
            (make-instance 'json-database :json-stream in))))
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


;;;; Instrumentation tests.
(sel-suite* instrumentation "Instrumentation tests."
            (clang-mutate-available-p))


(defun count-traceable (obj)
  "Return a count of full statements parented by compound statements"
  (count-if {can-be-made-traceable-p obj} (asts obj)))

(defun get-gcd-trace (bin)
  (with-temp-file (trace-file)
    (multiple-value-bind (stdout stderr errno)
        (let ((*shell-search-paths* nil))
          (shell-command (format nil "~a 4 8 2>~a" bin trace-file)))
      (declare (ignorable stdout stderr))
      (is (zerop errno))
      (let ((trace (read-trace trace-file 1)))
        (is (listp trace))
        trace))))

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
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 15 (length trace))))))))

(deftest instrumentation-insertion-w-filter-test ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*)
                                    :filter {remove-if-not
                                             [{eq 93} #'ast-counter]})))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 1 (length trace))))))))

(deftest instrumentation-insertion-w-function-exit-test ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*)
                                    :instrument-exit t)))
      ;; Do we insert the right number of printf statements?
      (is (<= (* 2 (count-traceable *gcd*))
              (count-traceable instrumented)))

      ;; Is function exit instrumented?
      (is (stmt-with-text instrumented
                          (format nil "write_trace_id(__sel_trace_file, ~du)"
                                  (-<>> (first (functions *gcd*))
                                        (function-body *gcd*)
                                        (position <> (asts *gcd*)
                                                  :test #'equalp))) :no-error))

      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 16 (length trace))))))))

(deftest instrumentation-insertion-w-points-test ()
  (with-fixture gcd-clang
    (let ((instrumented
           (handler-bind ((warning #'muffle-warning))
             (instrument (copy *gcd*)
               :points
               (iter (for ast in (stmt-asts *gcd*))
                     (for i upfrom 0)
                     (collect (cons ast (if (evenp i) '(1 2) '(3 4) ))))))))
      (is (scan (quote-meta-chars "write_trace_aux(__sel_trace_file")
              (genome-string instrumented))
        "We find code to print auxiliary values in the instrumented source.")
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every [«or {equalp #(1 2)} {equalp #(3 4)}»
                      {aget :aux}]
                     trace)))))))

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
      (let* ((else-counter (index-of-ast *gcd*
                                         (stmt-with-text *gcd* "b = b - a")))
             (matcher (quote-meta-chars
                       (format nil "write_trace_id(__sel_trace_file, ~du)"
                               else-counter))))
        (is (scan matcher (genome instrumented)))
        ;; The next line (after flushing) should be the else branch.
        (let ((location (position-if {scan matcher} (lines instrumented))))
          (is (scan (quote-meta-chars "b = b - a")
                    (nth (+ 2 location) (lines instrumented))))))
      ;; Finally, lets be sure we still compile.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (is (not (emptyp (get-gcd-trace bin))))))))

(deftest instrumentation-insertion-w-points-and-added-blocks-test ()
  (with-fixture gcd-wo-curlies-clang
    (let* ((cookie 1234)
           (instrumented
            (instrument (copy *gcd*)
              :points
              `((,(stmt-with-text *gcd* "b - a") ,cookie)))))
      ;; Instrumented program holds the value 1234.
      (is (scan (quote-meta-chars (format nil "~d" cookie))
                (genome-string instrumented))
          "The point trace value ~S appears in the instrumented program text."
          cookie)
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (find-if [{equalp `#(,cookie)} {aget :aux}]
                     trace)
              "The point trace value ~S appears in the trace" cookie))))))

(deftest instrumentation-after-insertion-mutation-test ()
  "Ensure after applying an insert mutation, the instrumented software
prints unique counters in the trace"
  (with-fixture gcd-clang
    (let* ((*matching-free-var-retains-name-bias* 1.0)
           (variant (copy *gcd*))
           (instrumented (copy *gcd*))
           (stmt1 (stmt-with-text variant "a = atoi(argv[1])"))
           (stmt2 (stmt-with-text variant "a = atoi(argv[1])")))
      (apply-mutation variant
        `(clang-insert (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (instrument instrumented)

      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout stderr))
          (is (zerop errno))
          (is (not (equal (find-if [{string= "a = atoi(argv[1])"}
                                    #'peel-bananas #'source-text]
                                    (asts variant)
                                    :from-end nil)
                          (find-if [{string= "a = atoi(argv[1])"}
                                    #'peel-bananas #'source-text]
                                    (asts variant)
                                    :from-end t)))
              "a = atoi(argv[1]) was not inserted into the genome")
          (is (search (format nil "write_trace_id(__sel_trace_file, ~du)"
                              (->> (find-if [{string= "a = atoi(argv[1])"}
                                             #'peel-bananas #'source-text]
                                            (asts variant)
                                            :from-end nil)
                                   (index-of-ast variant)))
                      (genome instrumented))
              "instrumentation was not added for the inserted statement")
          (is (search (format nil "write_trace_id(__sel_trace_file, ~du)"
                              (->> (find-if [{string= "a = atoi(argv[1])"}
                                             #'peel-bananas #'source-text]
                                            (asts variant)
                                            :from-end t)
                                   (index-of-ast variant)))
                      (genome instrumented))
              "instrumentation was not added for the original statement"))))))

(deftest instrumentation-print-unbound-vars ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-unbound-vals
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))))
    (is (scan (quote-meta-chars "write_trace_variables(__sel_trace_file")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (= (length trace) (count-if {assoc :c} trace))
            "Counter in every trace element.")
        (is (> (count-if {assoc :scopes} trace) 0)
            "Variable list in some trace elements.")
        (is (> (length trace) (count-if {aget :scopes} trace))
            "Variable list not populated in every trace element.")))))

(deftest instrumentation-print-in-scope-vars ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))))
    (is (scan (quote-meta-chars "write_trace_variables(__sel_trace_file")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (= (length trace) (count-if {assoc :c} trace))
            "Counter in every trace element.")
        (is (= (length trace) (count-if {assoc :SCOPES} trace))
            "Variable list in every trace element.")
        (is (= (length trace) (count-if {aget :SCOPES} trace))
            "Variable list populated in every trace element.")
        (is (not (null (mappend {aget :SCOPES} trace)))
            "Variable list not always empty.")))))

(defun trace-var-equal (var-name value scopes)
  (let ((var (find-if [{string= var-name} {aref _ 0}] scopes)))
    (or (null var)
        (equal (aref var 2) value))))

(deftest instrumentation-print-strings ()
  (with-fixture c-strings
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter ast
                                          :print-strings t)))))
    (is (scan (quote-meta-chars "write_trace_blobs(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print strings in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *soft* :bin bin))))
          "Successfully compiled instrumented SOFT.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (every [{trace-var-equal "test" "test"} {aget :scopes}]
                   trace)
            "Variable 'test' always has expected value.")
        (is (every [{trace-var-equal "x" "4"} {aget :scopes}]
                   trace)
            "Variable 'x' always has expected value.")))))

(deftest instrumentation-print-cpp-strings ()
  (with-fixture cpp-strings
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter ast
                                          :print-strings t)))))
    (is (scan (quote-meta-chars "write_trace_blobs(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print strings in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *soft* :bin bin))))
          "Successfully compiled instrumented SOFT.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (every [{trace-var-equal "x" "4"} {aget :scopes}]
                   trace)
            "Variable 'x' always has expected value.")))))

(deftest instrumentation-print-vars-after ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions-after
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))))
    (is (scan (quote-meta-chars "write_trace_variables(__sel_trace_file")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (not (null (mappend {aget :SCOPES} trace)))
            "Variable list not always empty.")))))

(deftest instrumentation-print-vars-handles-shadowing ()
  (with-fixture shadow-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))))
    (is (scan (quote-meta-chars "write_trace_variables(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *soft* :bin bin))))
          "Successfully compiled instrumented program.")
      (let ((trace (get-gcd-trace bin)))
        (is (every [{eq 1} #'length {aget :scopes}]
                   trace)
            "No duplicate variables.")

        (is (every [«or {equalp '(#("x" "int" 1))}
                        {equalp '(#("x" "short" 0))}»
                    {aget :scopes}]
                   trace)
            "Variables have correct type and value.")))))

(deftest instrumentation-handles-binary-search ()
  (with-fixture binary-search-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *binary-search* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-unbound-vals
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))))))

(defvar *project*)
(defixture clang-project
  (:setup
   (setf *project*
         (-> (make-instance 'clang-project
                :build-command "make"
                :build-target "foo"
                :compilation-database
                  `(((:file .
                      ,(-> (make-pathname :directory +multi-file-dir+
                                          :name "foo"
                                          :type "cpp")
                           (namestring)))
                     (:directory .
                      ,(-> (make-pathname :directory +multi-file-dir+)
                           (directory-namestring)))
                     (:command . "make"))
                    ((:file .
                      ,(-> (make-pathname :directory +multi-file-dir+
                                          :name "bar"
                                          :type "cpp")
                           (namestring)))
                     (:directory .
                      ,(-> (make-pathname :directory +multi-file-dir+)
                           (directory-namestring)))
                     (:command . "make"))))
              (from-file (make-pathname :directory +multi-file-dir+)))))
    (:teardown (setf *project* nil)))

(deftest can-instrument-clang-project ()
  (with-fixture clang-project
    (instrument *project* :functions
                (list (lambda (instrumenter ast)
                        (var-instrument {get-unbound-vals
                                         (software instrumenter)}
                                        instrumenter
                                        ast))))
    (with-temp-file (bin)
      (with-temp-build-dir ((directory-namestring
                             (make-pathname :directory +multi-file-dir+)))
        (is (zerop (second (multiple-value-list (phenome *project* :bin bin))))
            "Successfully compiled instrumented project."))
      (with-temp-file (trace-file)
        (multiple-value-bind (stdout stderr errno)
            (let ((*shell-search-paths* nil))
              (shell-command (format nil "~a 2>~a" bin trace-file)))
          (declare (ignorable stdout stderr))
          (is (zerop errno))
          (let ((trace (read-trace trace-file 1)))
            (is (listp trace))
            (is (not (emptyp trace)))
            (is (every «and {aget :c} {aget :f}» trace))
            (is (some «and [{equalp '(#("x" "int" 0))} {aget :scopes}]
                           [{eq 0} {aget :f}]»
                    trace))
            (is (some «and [{equalp '(#("y" "int" 1))} {aget :scopes}]
                           [{eq 1} {aget :f}]»
                    trace))))))))

(deftest instrumentation-skips-nameless-variable ()
  (let ((soft (make-instance 'clang
                             :genome "int test(int) { return 1; }")))
    (instrument soft :functions
                (list (lambda (instrumenter ast)
                        (var-instrument {get-vars-in-scope
                                         (software instrumenter)}
                                        instrumenter
                                        ast))))
    (is (not (scan (quote-meta-chars "write_trace_variables(__sel_trace_file")
               (genome soft)))
        "We don't find code to print variables in the instrumented source.")))

(deftest instrumentation-preserves-aux-data ()
  (with-fixture gcd-clang
    (let* ((stmt (stmt-starting-with-text *gcd* "if (a == 0)"))
           (index (index-of-ast *gcd* stmt)))
      (setf (ast-aux-data stmt) '((:foo . t)))

      (instrument *gcd* :functions
                  (list (lambda (instrumenter ast)
                          (when (aget :foo (ast-aux-data ast))
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))

      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
            "Successfully compiled instrumented GCD.")
        (is (equal '((:foo . t)) (ast-aux-data stmt)))
        (let ((trace (get-gcd-trace bin)))
          (is (listp trace) "We got a trace.")
          (is (some {aget :scopes} trace))
          (is (every «or [#'not {aget :scopes}]
                         [{eq index} {aget :c}]»
                     trace)))))))


;;;; Traceable tests.
(sel-suite* traceable-tests "Traceable tests."
            (clang-mutate-available-p))


(define-software clang-traceable (clang traceable) ())

(defixture traceable-gcd
  (:setup (setf *gcd* (from-file
                       (make-instance 'clang-traceable)
                       (make-pathname :name "gcd"
                                      :type "c"
                                      :directory +gcd-dir+)))))

(defvar *gcd-inputs* '((:bin "1071" "1029")
                       (:bin "555" "666")
                       (:bin "678" "987")
                       (:bin "8767" "653")
                       (:bin "16777216" "512")
                       (:bin "16" "4")
                       (:bin "315" "831")
                       (:bin "513332" "91583315")
                       (:bin "112" "135")
                       (:bin "310" "55"))
  "Example test inputs for GCD.")

(defvar *gcd-test-suite*
  (make-instance
   'test-suite
   :test-cases
   (iter (for input in *gcd-inputs*)
     (collecting (make-instance 'test-case
                   :program-name (car input)
                   :program-args (cdr input))))))

(deftest run-traceable-gcd ()
  (with-fixture traceable-gcd
    (instrument *gcd* :trace-env t)
    (setf (traces *gcd*) (mapcar {collect-trace *gcd*}
                                 (test-cases *gcd-test-suite*)))
    (is (= (length (traces *gcd*)) (length *gcd-inputs*)))
    (is (every {every {aget :c}} (mapcar {aget :trace} (traces *gcd*))))))

(deftest run-traceable-gcd-w/collect-traces ()
  (with-fixture traceable-gcd
    (instrument *gcd* :trace-env t)
    (collect-traces *gcd* *gcd-test-suite*)
    (is (= (length (traces *gcd*)) (length *gcd-inputs*)))
    (is (every {every {aget :c}} (mapcar {aget :trace} (traces *gcd*))))))

(define-software collect-traces-handles-directory-phenomes-mock
    (sel::ast sel::traceable)
  ((phenome-dir :initarg phenome-dir :accessor phenome-dir :copier :direct)))

(defmethod phenome ((obj collect-traces-handles-directory-phenomes-mock)
                    &key (bin (temp-file-name)))
  (let ((dir (ensure-directory-pathname bin)))
    (setf (phenome-dir obj) dir)
    (ensure-directories-exist dir)))

(deftest collect-traces-handles-directory-phenomes ()
  (let ((obj (make-instance 'collect-traces-handles-directory-phenomes-mock)))
    (handler-bind ((trace-error (lambda (c)
                                  (declare (ignorable c))
                                  (invoke-restart 'sel::ignore-empty-trace))))
      (collect-traces obj (make-instance 'test-suite)))
    (is (not (probe-file (phenome-dir obj)))
        "collect-traces did not remove a phenome directory")))

(deftest long-running-program-killed-test ()
  (with-fixture long-running-program-clang
    (with-temp-file (bin)
      (phenome *soft* :bin bin)
      (let ((proc (start-test bin
                              (make-instance 'test-case :program-name bin)
                              :wait nil)))
        (finish-test proc :kill-signal 15 :timeout 4)
        (is (not (eq :running (process-status proc)))
            "finish-test did not kill a long running process")))))


;;;; Tests of declaration and type databases on clang objects.
(sel-suite* declaration-type-databases
    "Tests of declaration and type databases on clang objects."
    (clang-mutate-available-p))


(deftest huf-knows-types ()
  (with-fixture huf-clang
    (is (and (hash-table-p (types *huf*))
             (not (zerop (hash-table-count (types *huf*)))))
        "Huf software object has a type database.")
    (is (= 9 (count-if «and #'type-pointer
                            [#'not {find #\(} #'type-name]»
                       (hash-table-values (types *huf*))))
        "Huf has nine pointer types.")
    (is (= 6 (count-if [#'not #'emptyp #'type-array]
                       (hash-table-values (types *huf*))))
        "Huf has six array types.")
    (is (= 3 (count-if [{string= "int"} #'type-name]
                       (hash-table-values (types *huf*))))
        "Huf has three different \"int\" types (some are array and pointer).")))

(deftest huf-finds-type-info-for-variables ()
  (with-fixture huf-clang
    (let ((type (->> (stmt-with-text *huf* "p = test")
                     (get-vars-in-scope *huf*)
                     (find-if [{string= "strbit"} {aget :name}])
                     (find-var-type *huf*))))
      (is type "Found type for \"strbit\" in huf.")
      (is (string= "[100]" (type-array type))
          "Variable \"strbit\" in huf is a dynamically sized array.")
      (is (not (type-pointer type))
          "Variable \"strbit\" in huf is not a pointer."))))


;;;; Lisp representation.
(sel-suite* lisp-tests "Lisp representation.")


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


;;;; Mutations and evaluation of clang expressions in Lisp form.
(sel-suite* clang-expression-tests
    "Mutation and evaluation of clang expressions in Lisp form."
    (clang-mutate-available-p))


;;;; Mutations of clang expressions in Lisp form.
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


;;;; Evaluation of clang expressions in Lisp form.
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


;;;; Utility tests.
(sel-suite* utility "Utility tests.")


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

(deftest which-test ()
  (is (null (which "dsjafpoarue")))
  (is (not (null (which "clang")))))


;;;; Project tests.
(sel-suite* clang-project-tests "Project tests."
            (clang-mutate-available-p))


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

(deftest clang-project-test ()
  (let ((project
          (-> (make-instance 'clang-project
                :build-command "make"
                :build-target "grep"
                :compilation-database
                  `(((:file .
                      ,(-> (make-pathname :directory +grep-prj-dir+
                                          :name "grep"
                                          :type "c")
                           (namestring)))
                     (:directory .
                      ,(-> (make-pathname :directory +grep-prj-dir+)
                           (directory-namestring)))
                     (:command .
                      ,(format nil "cc -c -o grep ~a"
                                   (-> (make-pathname :directory +grep-prj-dir+
                                                      :name "grep"
                                                      :type "c")
                                       (namestring)))))))
              (from-file (make-pathname :directory +grep-prj-dir+)))))
    (is (equal "make" (build-command project)))
    (is (equal "grep" (build-target project)))
    (is (equal 1 (length (evolve-files project))))
    (is (equal "grep.c" (car (first (evolve-files project)))))
    (is (equal "cc" (compiler (cdr (first (evolve-files project))))))
    (is (equal (list "-I" (namestring (make-pathname :directory +grep-prj-dir+))
                     "-c" "-o" "grep")
               (flags (cdr (first (evolve-files project))))))))

(deftest apply-mutations-to-project-unique-test ()
  (with-fixture clang-project
    (let ((proj (copy *project*)))
      (multiple-value-bind (objs muts)
          (apply-mutations proj
                           (make-instance 'clang-cut
                             :object proj
                             :targeter (lambda (obj)
                                         (mapcar (lambda (stmt)
                                                   (list (cons :stmt1 stmt)))
                                                 (bad-stmts obj)))
                             :picker (lambda (obj)
                                       (list (cons :stmt1
                                                   (random-elt
                                                     (bad-stmts obj))))))
                           10)
        (declare (ignorable objs))
        (is (< 1 (-> (mapcar {targets} muts)
                     (remove-duplicates :test #'equalp)
                     (length))))))))

(deftest apply-picked-mutations-to-project-unique-test ()
  (with-fixture clang-project
    (let ((proj (copy *project*)))
      (multiple-value-bind (objs muts)
          (apply-mutations proj
                           (make-instance 'clang-cut
                             :object proj
                             :targeter (lambda (obj)
                                         (mapcar (lambda (stmt)
                                                   (list (cons :stmt1 stmt)))
                                                 (bad-stmts obj)))
                             :picker (lambda (obj)
                                       (list (cons :stmt1
                                                   (random-elt
                                                     (bad-stmts obj))))))
                           10)
        (declare (ignorable objs))
        (is (< 1 (-> (mapcar {targets} muts)
                     (remove-duplicates :test #'equalp)
                     (length))))))))


;;;; Condition synthesis tests.
(sel-suite* condition-synthesis "Condition synthesis tests."
            (clang-mutate-available-p))


(deftest flip-works ()
  (is (string= (sel::flip "") ""))
  (is (string= (sel::flip "0000") "0001"))
  (is (string= (sel::flip "0001") "001")))

(deftest synthesize-all-conditions ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         (synths (sel::synthesize-conditions substs)))
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
    (is (sel::entails (first substs) eq-cond "0"))
    (is (not (sel::entails (first substs) eq-cond "1")))
    (is (not (sel::entails (second substs) eq-cond "0")))
    (is (sel::entails (second substs) eq-cond "1"))
    (is (sel::entails (third substs) eq-cond "0"))
    (is (not (sel::entails (third substs) eq-cond "1")))))

(deftest entails-neq-works ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         (eq-cond '(:neq "x" "int" "10")))
    (is (sel::entails (first substs) eq-cond "1"))
    (is (not (sel::entails (first substs) eq-cond "0")))
    (is (not (sel::entails (second substs) eq-cond "1")))
    (is (sel::entails (second substs) eq-cond "0"))
    (is (sel::entails (third substs) eq-cond "1"))
    (is (not (sel::entails (third substs) eq-cond "0")))))

(deftest find-best-condition-finds-good-conditions ()
  (let* ((substs '((("x" "int" "5") ("y" "int" "6"))
                   (("x" "int" "10") ("y" "int" "6"))
                   (("x" "int" "15") ("y" "int" "4") ("z" "int" "2"))))
         ;; shuffle the synthesized conditions so we don't always find the same
         ;; one first
         (synths (sel::shuffle (sel::synthesize-conditions substs))))
    (is (equal (sel::find-best-condition '("1" "0" "1") substs synths)
               '(:neq "x" "int" "10")))
    (is (member (sel::find-best-condition '("1" "1" "0") substs synths)
                (list '( :eq "y"  "int" "6")
                      '( :neq "x" "int" "15")
                      '( :neq "y"  "int" "4")
                      '( :neq "z"  "int" "2"))
                :test #'equal))))

(deftest tiny-test-repair-works ()
  (with-fixture cs-tiny-clang
    (let* ((repair-mut (make-instance 'loosen-condition
                                      :object *soft*
                                      :targets (stmt-with-text *soft* "x > 5")))
           (repaired-prog (sel::synthesize-condition *soft*
                                                    *test-suite*
                                                    repair-mut)))
      (is repaired-prog)
      (is (stmt-with-text repaired-prog "(x > 5) || (x == 5)")))))

(deftest test-tighten-repair ()
  (with-fixture cs-tighten-clang
    (let* ((repair-mut (make-instance
                        'sel::tighten-condition
                        :object *soft*
                        :targets (stmt-with-text *soft* "x >= 5")))
           (repaired-prog (sel::synthesize-condition *soft* *test-suite*
                                                    repair-mut)))
      (is repaired-prog)
      (is (stmt-with-text repaired-prog "(x >= 5) && !(x == 5)")))))

(deftest test-add-condition-repair-target-24 ()
  (with-fixture cs-add-guard-clang
    (let* ((repair-mut
            (make-instance 'add-condition
                           :object *soft*
                           :targets (stmt-starting-with-text *soft*
                                                             "if (x >= 5)")))
           (repaired-prog (sel::synthesize-condition *soft* *test-suite*
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
               (second (get-immediate-children repaired-prog outer))))))))))

(deftest test-add-condition-repair-target-30 ()
  (with-fixture cs-add-guard-clang
    (let* ((repair-mut (make-instance
                        'add-condition
                        :object *soft*
                        :targets (stmt-starting-with-text
                                  *soft*
                                  "printf(\"x is larger")))
           (repaired-prog (sel::synthesize-condition *soft* *test-suite*
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
               (second (get-immediate-children repaired-prog outer))))))))))

(deftest test-if-to-while-repair ()
  (with-fixture cs-divide-clang
    (let* ((repair-mut
            (make-instance 'if-to-while-tighten-condition
                           :object *soft*
                           :targets (stmt-starting-with-text *soft*
                                                             "if (x >= 2)")))
           (repaired-prog (sel::synthesize-condition *soft* *test-suite*
                                                    repair-mut)))
      (is repaired-prog)
      (let ((stmt (stmt-starting-with-text repaired-prog "while")))
        (is stmt)
        (is (eq :WhileStmt (ast-class stmt)))
        (is (equal "(x >= 2) && !(x == 2)"
                   (->> (get-immediate-children repaired-prog stmt)
                        (first)
                        (source-text)
                        (peel-bananas))))))))



;;;; Selection tests.
(sel-suite* selection "Selection tests.")


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
    (is (sel::lexicase-better-p '(1 2 0) a b))
    (is (not (sel::lexicase-better-p '(1 2 0) b a)))
    (is (sel::lexicase-better-p '(1 0 2) b a))
    (is (not (sel::lexicase-better-p '(1 0 2) a b)))))

(deftest lexicase-better-p-tie ()
  (let ((a #(0 1 1))
        (b #(0 1 1)))
    (is (not (sel::lexicase-better-p '(0 1 2) a b)))
    (is (not (sel::lexicase-better-p '(0 1 2) b a)))))

(deftest dominates-all-trivial-test ()
  (is (sel::dominates-all (list #'>)
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
               (sel::crowding-distance (first *population*))))
    (is (equal infinity
               (sel::crowding-distance (second *population*))))
    (is (equal infinity
               (sel::crowding-distance (third *population*))))))

(deftest crowding-distance-lexicase-works ()
  (let ((*population*
         (list (make-instance 'simple :fitness '(#(1 0) #(1 1 0 0)))
               (make-instance 'simple :fitness '(#(0.5 0.5) #(0.5 0.5 0.5 0.5)))
               (make-instance 'simple :fitness '(#(0 1) #(0 0 1 1))))))

    (is (equal infinity
               (sel::crowding-distance (first *population*))))
    (is (equal 2
               (sel::crowding-distance (second *population*))))
    (is (equal infinity
               (sel::crowding-distance (third *population*))))))

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


;;;; Style features tests.
(sel-suite* style-features "Style features tests."
            (clang-mutate-available-p))


(deftest uni-grams-ht-test ()
  (let* ((sentence (list "the" "quick" "brown" "fox"
                         "the" "lazy" "brown" "dog"
                         "the" "quick" "hare"))
         (ht (sel::uni-grams sentence)))
    (is (= 7 (length (hash-table-keys ht))))
    (is (= 3 (gethash "the" ht 0)))
    (is (= 2 (gethash "quick" ht 0)))
    (is (= 2 (gethash "brown" ht 0)))
    (is (= 1 (gethash "fox" ht 0)))
    (is (= 1 (gethash "lazy" ht 0)))
    (is (= 1 (gethash "dog" ht 0)))
    (is (= 1 (gethash "hare" ht 0)))))

(deftest to-feature-vector-test ()
  (let* ((sentence (list "the" "quick" "brown" "fox"
                         "the" "lazy" "brown" "dog"
                         "the" "quick" "hare"))
         (ht (sel::uni-grams sentence))
         (fv (sel::to-feature-vector ht (list "the" "quick" "brown" "fox"
                                             "lazy" "dog" "hare"))))
    (is (= 7 (length fv)))
    (is (equalp fv (vector 3 2 2 1 1 1 1)))))

(deftest function-ast-depths-are-zero ()
  (with-fixture variety-clang
    (let ((asts (functions *variety*)))
      (is (= 4 (length asts)))
      ;; depths of all function asts are 0 (all top-level)
      (is (every #'zerop (mapcar {sel::ast-depth *variety*} asts))))))

(deftest max-depth-ast-functions-is-0 ()
  (with-fixture variety-clang
    (is (zerop (sel::max-depth-ast *variety* (functions *variety*))))))

(deftest max-depth-ret-stmts-is-2 ()
  (with-fixture variety-clang
    (let ((return-stmts (remove-if-not [{eql :ReturnStmt} #'ast-class]
                                       (asts *variety*))))
      (is (= 2 (sel::max-depth-ast *variety* return-stmts))))))

(deftest merge-max-picks-larger ()
  (bind (((:values vec1 meta1) (with-fixture variety-clang
                                 (sel::max-depth-ast-extractor *variety*)))
         ((:values vec2 meta2) (with-fixture gcd-clang
                             (sel::max-depth-ast-extractor *gcd*)))
         ((:values vecr _) (sel::merge-max vec1 meta1 vec2 meta2)))
    (is (= 1 (length vec1)))
    (is (= 1 (length vec2)))
    (is (= 1 (length vecr)))
    (is (= (elt vecr 0)
           (max (elt vec1 0)
                (elt vec2 0))))))

(deftest avg-depth-ast-node-type-function-is-0 ()
  (with-fixture variety-clang
    (is (zerop (sel::ast-node-type-avg-depth *variety* :Function)))))

(deftest avg-depth-ast-node-type-return-stmts ()
  (with-fixture variety-clang
    (is (= 2 (sel::ast-node-type-avg-depth *variety* :ReturnStmt)))))

(deftest node-type-counts ()
  (with-fixture variety-clang
    ;; list of (ast-class, occurrences)
    (bind (((:values vec denom) (sel::ast-node-type-tf-extractor *variety*))
           (ast-counts (mapcar #'cons
                               sel::*clang-c-ast-classes*
                               (mapcar {* denom} (coerce vec 'list)))))
      ;; for each ast-class, verify occurrence count is correct
      (iter (for (type . count) in ast-counts)
            (is (= count
                   (count-if [{equal type} #'ast-class]
                             (asts *variety*))))
            (finally (return t))))))

(deftest ast-keywords-auto-counts ()
  (with-fixture variety-clang
    (let ((auto-counts
           (iter (for keyword in sel::*clang-c-keywords*)
                 (collect
                     (cons (reduce #'+ (mapcar {sel::auto-count-keyword keyword}
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
           (iter (for keyword in sel::*clang-c-keywords*)
                 (collect
                     (cons (reduce #'+ (mapcar {sel::search-keyword *variety*
                                                                   keyword}
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
             (0 . "inline") (11 . "int") (0 . "long") (0 . "register")
             (0 . "restrict") (0 . "return") (0 . "short") (0 . "signed")
             (1 . "sizeof") (0 . "static") (1 . "struct") (0 . "switch")
             (0 . "typedef") (1 . "union") (0 . "unsigned") (1 . "void")
             (0 . "volatile") (0 . "while")))))))

(deftest ast-keyword-tf-extractor-correct ()
  (with-fixture variety-clang
    (let ((ls-count (-<>> (sel::ast-keyword-tf-extractor *variety*)
                          (coerce <> 'list)
                          (mapcar {* 44}))))
      (is (equal
           (mapcar #'cons
                   ls-count
                   sel::*clang-c-keywords*)
           '((0 . "alignof") (0 . "auto") (2 . "break") (2 . "case") (1 . "char")
             (1 . "const") (1 . "continue") (1 . "default") (1 . "do")
             (3 . "double") (1 . "else") (2 . "enum") (0 . "extern") (0 . "float")
             (1 . "for") (3 . "goto") (1 . "if") (0 . "inline") (11 . "int")
             (0 . "long") (0 . "register") (0 . "restrict") (4 . "return")
             (0 . "short") (0 . "signed") (1 . "sizeof") (0 . "static")
             (1 . "struct") (1 . "switch") (2 . "typedef") (1 . "union")
             (0 . "unsigned") (1 . "void") (0 . "volatile") (2 . "while")))))))

(deftest small-bi-grams-count-example ()
  (let* ((ls (list "the" "tortoise" "and" "the" "hare" "and" "the" "race"))
         (bi-grams (sel::bi-grams ls :key #'identity))
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
           (bi-grams (sel::bi-grams functions :key #'ast-class))
           (keys (hash-table-keys bi-grams))
           (vals (hash-table-values bi-grams)))
      (is (= 1 (length keys) (length vals)))
      (is (equal (car keys) (cons :Function :Function)))
      (is (= (1- (length functions)) (car vals))))))

(deftest small-ast-bigrams-count-example ()
  (with-fixture variety-clang
    (flet ((asts-by-type (type)
             (remove-if-not [{eq type} #'ast-class]
                            (asts *variety*))))
      (let* ((asts (list (first (asts-by-type :Function))
                         (first (asts-by-type :IntegerLiteral))
                         (first (asts-by-type :DeclStmt))))
             (bi-grams (sel::bi-grams asts :key #'ast-class))
             (keys (hash-table-keys bi-grams))
             (vals (hash-table-values bi-grams))
             (sorted-keys (list (cons :Function :IntegerLiteral)
                                (cons :IntegerLiteral :DeclStmt))))
        (is (= 2 (length keys) (length vals)))
        (is (equal sorted-keys
                   (sort keys (lambda (k1 k2)
                                (or (string< (car k1) (car k2))
                                    (and (string= (car k1) (car k2))
                                         (string< (cdr k1) (cdr k2))))))))
        (is (equal '(1 1) vals))))))


;;;; Clang syntactic contexts.
(sel-suite* clang-syntactic-contexts "Clang syntactic contexts."
            (clang-mutate-available-p))


;; Tests of basic clang mutation operators
(defun count-matching-chars-in-stmt (char stmt)
  (let ((ast (if (listp stmt) (car stmt) stmt)))
    (count-if {eq char} (source-text ast))))

(defun find-function (obj name)
  (find-if [{string= name} #'ast-name]
           (functions obj)))

(deftest cut-full-stmt-removes-semicolon ()
  (with-fixture contexts
    (sel::apply-mutation-ops *contexts*
                            `((:cut (:stmt1 . ,(stmt-with-text
                                                *contexts* "int x = 0")))))
    (is (eq 0
            (count-matching-chars-in-stmt
             #\;
             (find-function *contexts* "full_stmt"))))))

(deftest insert-full-stmt-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0")))
      (sel::apply-mutation-ops *contexts*
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
      (sel::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,target)
                                         (:value1 . ,inserted)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest replace-full-stmt-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0"))
          (replacement (stmt-with-text *contexts* "int x = 1")))
      (sel::apply-mutation-ops *contexts*
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
      (sel::apply-mutation-ops *contexts*
                              `((:set (:stmt1 . ,target)
                                      (:value1 . ,replacement)))))
    (is (eq 0 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest insert-non-full-stmt-into-fullstmt-context-makes-full ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x"))
          (location (stmt-starting-with-text *contexts* "if (1)")))
      (sel::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,location)
                                         (:value1 . ,target))))
      (is (not (ast-full-stmt target))))
    (is (->> (find-function *contexts* "braced_body")
             (function-body *contexts*)
             (get-immediate-children *contexts*)
             (first)
             (ast-full-stmt)))))

(deftest cut-list-elt-removes-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b")))
      (sel::apply-mutation-ops *contexts*
                             `((:cut (:stmt1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a,  int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest insert-list-elt-adds-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b")))
      (sel::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,target)
                                         (:value1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b,int b, int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-list-elt-keeps-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b"))
          (replacement (stmt-with-text *contexts* "int a")))
      (sel::apply-mutation-ops *contexts*
                              `((:set (:stmt1 . ,target)
                                      (:value1 . ,replacement)))))
    (is (starts-with-subseq
         "void list(int a, int a, int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest cut-final-list-elt-removes-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c")))
      (sel::apply-mutation-ops *contexts*
                              `((:cut (:stmt1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b)"
         (source-text (find-function *contexts* "list"))))))

(deftest insert-final-list-elt-adds-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c")))
      (sel::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,target)
                                         (:value1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b, int c,int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-final-list-elt-keeps-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c"))
          (replacement (stmt-with-text *contexts* "int a")))
      (sel::apply-mutation-ops *contexts*
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
      (sel::apply-mutation-ops *contexts*
                              `((:set (:stmt1 . ,target)
                                      (:value1 . ,replacement)))))
    (let ((function (find-function *contexts* "braced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      (is (eq 1 (count-matching-chars-in-stmt #\; function)))
      ;; Braces should be part of a new CompoundStmt AST rather than
      ;; free-floating text.
      (is (eq 2 (count-if «and [{eq :CompoundStmt} #'ast-class]
                               {ancestor-of *contexts* function}»
                          (stmt-asts *contexts*)))))))

(deftest cut-unbraced-body-adds-nullstmt ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2")))
      (sel::apply-mutation-ops *contexts*
                              `((:cut (:stmt1 . ,target)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "unbraced_body"))))
    (is (eq :NullStmt
            (&>> (stmt-starting-with-text *contexts* "if (2)")
                 (get-immediate-children *contexts*)
                 (second)
                 (ast-class))))))

(deftest cut-braced-body-adds-nullstmt ()
  (with-fixture contexts
    (let ((target (->> (stmt-with-text *contexts* "int x = 1")
                       (get-parent-ast *contexts*))))
      (sel::apply-mutation-ops *contexts*
                              `((:cut (:stmt1 . ,target)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "braced_body"))))
    (is (eq :NullStmt
            (&>> (stmt-starting-with-text *contexts* "if (1)")
                 (get-immediate-children *contexts*)
                 (second)
                 (ast-class))))))

(deftest replace-unbraced-body-keeps-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2")))
      (sel::apply-mutation-ops *contexts*
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
      (sel::apply-mutation-ops *contexts*
                      `((:set (:stmt1 . ,target)
                              (:value1 . ,replacement)))))
    (let ((function (find-function *contexts* "unbraced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      (is (eq 1 (count-matching-chars-in-stmt #\; function))))))

(deftest insert-after-full-stmt-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0")))
      (sel::apply-mutation-ops *contexts*
                               `((:insert-after (:stmt1 . ,target)
                                                (:value1 . ,target)))))
    (let ((function (find-function *contexts* "full_stmt")))
      (is (eq 2 (count-matching-chars-in-stmt #\; function)))
      ;; Each copy of "int x = 0" has a DeclStmt and a VarDecl
      (is (eq 4 (count-if [{string= "int x = 0"} #'source-text]
                          (asts *contexts*)))))))

(deftest insert-after-braced-body-adds-trailing-semicolon ()
  (with-fixture contexts
    (let ((target (->> (stmt-with-text *contexts* "int x = 1")
                       (get-parent-ast *contexts*)))
          (inserted (stmt-with-text *contexts* "int x = 0")))
      (sel::apply-mutation-ops *contexts*
                               `((:insert-after (:stmt1 . ,target)
                                                (:value1 . ,inserted)))))
    (let ((function (find-function *contexts* "braced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\; function)))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      ;; Each copy of "int x = 0" has a DeclStmt and a VarDecl
      (is (eq 4 (count-if [{string= "int x = 0"} #'source-text]
                          (asts *contexts*)))))))

(deftest insert-braced-after-braced-body-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (->> (stmt-with-text *contexts* "int x = 1")
                       (get-parent-ast *contexts*))))
      (sel::apply-mutation-ops *contexts*
                               `((:insert-after (:stmt1 . ,target)
                                                (:value1 . ,target)))))
    (let ((function (find-function *contexts* "braced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\; function)))
      (is (eq 3 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 3 (count-matching-chars-in-stmt #\} function)))
      ;; Each copy of "int x = 1" has a DeclStmt and a VarDecl
      (is (eq 4 (count-if [{string= "int x = 1"} #'source-text]
                          (asts *contexts*)))))))

;; FIXME: this behavior is not implemented, so we can get incorrect
;; trees in some cases.
;; (deftest insert-before-unbraced-body-adds-braces ()
;;   ;; clang-mutate will simply insert at the given location, leading to
;;   ;; text like
;;   ;; if (2) int x = 1; x = 2

;;   ;; This is problematic when working directly on the ASTs because
;;   ;; both statements end up as direct children of the "if". We can fix
;;   ;; that by wrapping them in braces.
;;   (with-fixture contexts
;;     (let ((target (stmt-with-text *contexts* "x = 2"))
;;           (insert (stmt-with-text *contexts* "int x = 1")))
;;       (sel::apply-mutation-ops *contexts*
;;                               `((:insert (:stmt1 . ,target)
;;                                          (:value1 . ,insert)))))
;;     (let ((function (find-function *contexts* "unbraced_body")))
;;       (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
;;       (is (eq 2 (count-matching-chars-in-stmt #\} function)))
;;       (is (eq 2 (count-matching-chars-in-stmt #\; function)))
;;       (is (eq 2 (count-if «and [{string= "CompoundStmt"} #'ast-class]
;;                                {ancestor-of *contexts* function}»
;;                           (stmt-asts *contexts*)))))))

(deftest cut-field-removes-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;")))
      (sel::apply-mutation-ops *contexts*
                              `((:cut (:stmt1 . ,target)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 1
              (count-matching-chars-in-stmt #\; struct))))))

(deftest insert-field-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;")))
      (sel::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,target)
                                         (:value1 . ,target)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 3
              (count-matching-chars-in-stmt #\; struct))))))

(deftest replace-field-keeps-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;"))
          (replacement (stmt-with-text *contexts* "int f2;")))
      (sel::apply-mutation-ops *contexts*
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
      (sel::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,location)
                                         (:value1 . ,inserted))))
      (is (eq (1+ semicolons)
              (count-if {eq #\;} (genome *contexts*)))))))

(deftest insert-toplevel-braced ()
  (with-fixture contexts
    (let ((location (stmt-starting-with-text *contexts* "struct"))
          (inserted (stmt-starting-with-text *contexts* "void list"))
          (semicolons (count-if {eq #\;} (genome *contexts*))))
      (sel::apply-mutation-ops *contexts*
                              `((:insert (:stmt1 . ,location)
                                         (:value1 . ,inserted))))
      (is (eq semicolons
              (count-if {eq #\;} (genome *contexts*)))))))

(deftest splice-asts-and-text ()
  (with-fixture contexts
    (let ((location (stmt-with-text *contexts* "int x = 0"))
          (inserted (list (format nil "/*comment 1*/~%")
                          (sel::ast-ref-ast (stmt-starting-with-text *contexts*
                                                                    "int x = 1"))
                          (format nil ";~%/*comment 2*/~%"))))
      (sel::apply-mutation-ops *contexts*
                              `((:splice (:stmt1 . ,location)
                                         (:value1 . ,inserted))))

      (is (not (stmt-with-text *contexts* "int x = 0" :no-error)))
      (is (stmt-with-text *contexts* "int x = 1" :no-error))
      (is (eq 1
              (->> (stmt-starting-with-text *contexts* "void full_stmt")
                   (function-body *contexts*)
                   (get-immediate-children *contexts*)
                   (remove-if-not #'ast-full-stmt)
                   (length))))
      (is (search "comment 1" (genome *contexts*)))
      (is (search "comment 2" (genome *contexts*))))))



;;;; Clang scope and type tests.
(sel-suite* clang-scopes-and-types "Clang scope and type tests."
            (clang-mutate-available-p))


(defun compare-scopes (result expected)
  (is (equal (mapcar {mapcar {aget :name}} result)
             expected))
  (iter (for var-info in (apply #'append result))
        (is (aget :type var-info))
        (is (aget :decl var-info))
        (is (aget :scope var-info))))

(deftest scopes-are-correct ()
  (with-fixture scopes2-clang
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int b"))
                    '(nil
                      ("a")
                      ("global")))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int c"))
                    '(("b")
                      ("a")
                      ("global")))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "char d"))
                    '(nil
                      ("c" "b")
                      ("a")
                      ("global")))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "return"))
                    '(("c" "b")
                      ("a")
                      ("global")))))

(deftest cxx-method-scopes-are-correct ()
  (with-fixture scopes-cxx-clang
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int y"))
                    '(nil
                      ("x")
                      nil))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int z"))
                    '(nil
                      ("y")
                      ("x")
                      nil))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "y = 0"))
                    '(("z")
                      ("y")
                      ("x")
                      nil))))

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
               '("char" "int")))))

(deftest unbound-vals-are-correct ()
  (flet
      ((compare-vals (result expected)
         (is (equal (mapcar {aget :name} result)
                    expected))
         (iter (for var-info in result)
               (is (aget :type var-info))
               (is (aget :decl var-info))
               (is (aget :scope var-info)))))

    (with-fixture scopes2-clang
      (is (null (get-unbound-vals *scopes*
                                  (stmt-with-text *scopes* "int global"))))
      (compare-vals (get-unbound-vals *scopes*
                                      (stmt-starting-with-text *scopes* "c ="))
                    '("global" "b" "a" "c"))
      (compare-vals (get-unbound-vals *scopes*
                                      (stmt-starting-with-text *scopes* "b ="))
                    '("b"))
      (compare-vals (get-unbound-vals *scopes*
                                      (stmt-starting-with-text *scopes* "d ="))
                    '("d"))

      (compare-vals (get-unbound-vals *scopes*
                                      (->> "void foo"
                                           (stmt-starting-with-text *scopes* )))
                    '("global")))))

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

(deftest scopes-type-field-is-correct ()
  (with-fixture scopes-type-field-clang
    (is (equal "char"
               (->> (scopes *scopes* (stmt-with-text *scopes* "return 0"))
                    (lastcar)
                    (first)
                    (aget :type)
                    (find-type *scopes*)
                    (type-name)))
        "char variable should have been found at the global scope")))

(deftest get-vars-in-scope-keep-globals-flag ()
  (with-fixture scopes-type-field-clang
    (is (equal "time_args"
               (->> (get-vars-in-scope *scopes*
                                       (stmt-with-text *scopes* "return 0")
                                       t)
                    (first)
                    (aget :name)))
        "time_args variable should have been found at the global scope")
    (is (equal nil (get-vars-in-scope *scopes*
                                      (stmt-with-text *scopes* "return 0")
                                      nil))
        "no variables should have been found outside the global scope")))

(deftest move-statement-updates-scopes ()
  (with-fixture scopes2-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (apply-mutation *scopes*
                      `(clang-swap (:stmt1 . ,(stmt-with-text *scopes* "int c"))
                                   (:stmt2 . ,(stmt-with-text *scopes* "b = 0")))))
    (compare-scopes (scopes *scopes* (stmt-starting-with-text *scopes* "b ="))
                    '(("b")
                      ("a")
                      ("global")))))

(deftest cut-decl-updates-scopes ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "int global"))))
    (compare-scopes (scopes *scopes* (stmt-starting-with-text *scopes* "b ="))
                    '(("c" "b")
                      ("a")
                      nil))))

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
    (let ((unbound (get-unbound-vals *scopes*
                                     (stmt-starting-with-text *scopes*
                                                              "void foo"))))
      (is (equal (mapcar {aget :name} unbound) '("global" "b")))
      (is (aget :decl (find-if [{string= "global"} {aget :name}] unbound)))
      ;; b is now undeclared
      (is (not (aget :decl (find-if [{string= "b"} {aget :name}] unbound)))))))

(deftest insert-statement-updates-unbound-vals ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-insert (:stmt1 . ,(stmt-with-text *scopes*
                                                              "foo(0)"))
                                   (:stmt2 . ,(stmt-with-text *scopes*
                                                              "b = 0"))))
    ;; "b" is not defined in this context so it will be rebound
    (let ((unbound (get-unbound-vals *scopes*
                                 (stmt-starting-with-text *scopes*
                                                          "void bar"))))
      (is (eq 1 (length unbound)))
      (is (string= "global" (aget :name (car unbound))))
      (is (equalp (stmt-with-text *scopes* "int global")
                  (aget :decl (car unbound))))
      (is (eq (ast-root *scopes*) (ast-ref-ast (aget :scope (car unbound)))))
      (is (aget :type (car unbound))))))


;;;; Types and traces tests.
(sel-suite* type-traces-tests "Types and traces tests."
            (clang-mutate-available-p))


(deftest type-trace-string-test ()
  (is (equalp "int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "*int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer t
                                                  :array ""
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "[]int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array "[]"
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "[5]int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array "[5]"
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "*[]int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer t
                                                  :array "[]"
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "*[5]int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer t
                                                  :array "[5]"
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "const int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :const t
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "volatile int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :volatile t
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "*restrict int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer t
                                                  :array ""
                                                  :restrict t
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "auto int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :Auto
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "static int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :static
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "extern int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :extern
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "register int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :register
                                                  :hash 0
                                                  :reqs nil)))))

(deftest type-from-trace-string-test ()
  (is (equalp (type-from-trace-string "int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*int")
              (make-clang-type :name "int"
                               :pointer t
                               :array ""
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "[]int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array "[]"
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "[5]int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array "[5]"
                               :size 5
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*[]int")
              (make-clang-type :name "int"
                               :pointer t
                               :array "[]"
                               :size nil
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*[5]int")
              (make-clang-type :name "int"
                               :pointer t
                               :array "[5]"
                               :size 5
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "const int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :const t
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "volatile int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :volatile t
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*restrict int")
              (make-clang-type :name "int"
                               :pointer t
                               :array ""
                               :restrict t
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "auto int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :Auto
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "static int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :Static
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "extern int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :Extern
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "register int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :Register
                               :hash 0
                               :reqs nil))))

(deftest type-decl-string-test ()
  (is (equalp "int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "int *"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer t
                                                 :array ""
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "const int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :const t
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "volatile int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :volatile t
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "restrict int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :restrict t
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "auto int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :Auto
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "static int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :Static
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "extern int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :Extern
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "register int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :Register
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "struct struct_type"
              (type-decl-string (make-clang-type :name "struct_type"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :None
                                                 :decl "struct struct_type;"
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "union union_type"
              (type-decl-string (make-clang-type :name "union_type"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :None
                                                 :decl "union union_type;"
                                                 :hash 0
                                                 :reqs nil)))))


;;;; Clang tokenizer tests.
(sel-suite* clang-tokenizer "Clang tokenizer tests."
            (clang-mutate-available-p))


(deftest case-tokens ()
  (with-fixture variety-clang
    (let* ((root (stmt-starting-with-text *variety* "case 1"))
           (tokens (tokens *variety* (list root)))
           (switch-tokens
            (mapcar #'make-keyword
                    ;; case 1:
                    (list "case" "int-literal" ":"
                          ;; printf("%d\n", argc
                          "identifier" "(" "string-literal" "," "identifier"
                          ;; + argc);
                          "+" "identifier" ")"))))
      (is (equal tokens switch-tokens)))))

(deftest do-while-tokens ()
  (with-fixture variety-clang
    (let* ((root (stmt-starting-with-text *variety* "do {"))
           (tokens (tokens *variety* (list root))))
      (is (equal tokens
                 ;; do {
                 (mapcar #'make-keyword
                         (list "do" "{"
                               ;; run.foo++
                               "identifier" "." "identifier" "++"
                               ;; } while (run.foo
                               "}" "while" "(" "identifier" "." "identifier"
                               ;; < 8)
                               "<" "int-literal" ")")))))))

(deftest designatedinitexpr-tokens ()
  (with-fixture variety-clang
    (let* ((roots (remove-if-not {eq :DesignatedInitExpr}
                                 (asts *variety*)
                                 :key #'ast-class))
           (test-roots (list (nth 0 roots)
                             (nth 1 roots)
                             (nth 3 roots)))
           (token-lists (iter (for root in test-roots)
                              (collect (tokens *variety* (list root))
                                into token-lists)
                              (finally (return token-lists)))))
      (is (= 3 (length token-lists)))
      (is (equal (nth 0 token-lists)
                 ;; [0 ... 1 ].y = 1.0
                 (mapcar #'make-keyword
                         (list "[" "int-literal" "..." "int-literal" "]"
                               "." "identifier" "=" "float-literal"))))
      (is (equal (nth 1 token-lists)
                 ;; [1].x = 2.0
                 (mapcar #'make-keyword
                         (list "[" "int-literal" "]" "." "identifier"
                               "=" "float-literal"))))
      (is (equal (nth 2 token-lists)
                 ;; .y = 2.0
                 (mapcar #'make-keyword
                         (list "." "identifier" "=" "float-literal")))))))

(deftest function-tokens ()
  (with-fixture variety-clang
    (let* ((root (stmt-starting-with-text *variety* "int add3"))
           (tokens (tokens *variety* (list root))))
      (is (equal
           tokens
           ;; int add3(int x) {
           (mapcar #'make-keyword
                   (list "int" "identifier" "(" "int" "identifier"
                         ")" "{"
                         ;; printf("...", __func__)
                         "identifier" "(" "string-literal" "," "__func__" ")"
                         ;; return x + 3 }
                         "return" "identifier" "+" "int-literal" "}")))))))

(deftest mixed-tokens ()
  (with-fixture variety-clang
    (let* ((root (stmt-starting-with-text *variety* "tun->foo"))
           (tokens (tokens *variety* (list root))))
      (is
       (equal
        tokens
        (mapcar #'make-keyword
                (list
                 ;; tun -> foo = _Generic(
                 "identifier" "->" "identifier" "=" "generic" "("
                 ;; tun -> foo, int: 12
                 "identifier" "->" "identifier" "," "int" ":" "int-literal"
                 ;; , char: 'q')
                 "," "char" ":" "char-literal" ")")))))))

(deftest memberexpr-tokens ()
  (with-fixture variety-clang
    (let* ((roots (remove-if-not {eq :MemberExpr}
                                 (asts *variety*)
                                 :key #'ast-class))
           (token-lists (mapcar [{tokens *variety*} #'list] roots)))
      (is (every [{<= 3} #'length] token-lists))
      (is (every (lambda (ls)
                   (or (equal ls (mapcar #'make-keyword
                                         (list "identifier" "." "identifier")))
                       (equal ls (mapcar #'make-keyword
                                         (list "identifier" "->" "identifier")))
                       (equal ls (mapcar #'make-keyword
                                         (list "identifier" "[" "int-literal"
                                               "]" "." "identifier")))))
                 token-lists)))))

(deftest parenexpr-tokens ()
  (with-fixture variety-clang
    (let* ((roots (remove-if-not {eq :ParenExpr}
                                 (asts *variety*)
                                 :key #'ast-class))
           (token-lists (mapcar [{tokens *variety*} #'list] roots)))
      (is (= 1 (length token-lists)))
      (is (every [{<= 3} #'length] token-lists))
      (is (every [{eql (make-keyword "(")} #'first] token-lists))
      (is (every [{eql (make-keyword ")")} #'lastcar] token-lists)))))

(deftest parmvar-tokens ()
  (with-fixture variety-clang
    (let* ((parmvars (remove-if-not {eq :ParmVar}
                                    (asts *variety*)
                                    :key #'ast-class))
           (token-lists (mapcar [{tokens *variety*} #'list] parmvars))
           (argv-tokens (mapcar #'make-keyword
                                (list "char" "*" "*" "identifier")))
           (int-tokens (mapcar #'make-keyword
                               (list "int" "identifier"))))
      (is (= 5 (length token-lists)))
      (is (every [{<= 2} #'length] token-lists))
      (is (member int-tokens token-lists :test #'equal))
      (is (member argv-tokens token-lists :test #'equal)))))

(deftest record-tokens ()
  (with-fixture variety-clang
    (let* ((records (remove-if-not {eq :Record}
                                   (asts *variety*)
                                   :key #'ast-class))
           (token-lists (mapcar [{tokens *variety*} #'list] records))
           (union-tokens (mapcar #'make-keyword
                                 (list "union" "{"
                                       "int" "identifier"
                                       "char" "identifier" "}")))
           (struct-tokens (mapcar #'make-keyword
                                  (list "struct" "identifier" "{"
                                        "double" "identifier"
                                        "double" "identifier" "}"))))
      (is (= 2 (length token-lists)))
      (is (member union-tokens token-lists :test #'equal))
      (is (member struct-tokens token-lists :test #'equal)))))

(deftest while-tokens ()
  (with-fixture variety-clang
    (let* ((root (remove-if-not {eq :WhileStmt}
                                (asts *variety*)
                                :key #'ast-class))
           (tokens (tokens *variety* root))
           (while-tokens
            (mapcar #'make-keyword
                    ;; while((next = va_arg(nums, int))
                    (list "while" "(" "(" "identifier" "=" "macro" ")"
                          ;; && num_args > 0) {
                          "&&" "identifier" ">" "int-literal" ")" "{"
                          ;; sum += next
                          "identifier" "+=" "identifier"
                          ;; num_args-- }
                          "identifier" "--" "}"))))
      (is (equal tokens while-tokens)))))

#+nil
(deftest rinard-fault-loc ()
  (with-fixture fl-tiny-clang
    (with-temp-file (trace-file)
      (let* ((copy *soft*)
             (instrumented
              (instrument copy :trace-file trace-file))
             ;; pick first test to be "negative", arbitrarily
             (made-up-bad-test (list (nth 4 (test-cases *test-suite*))
                                     (nth 5 (test-cases *test-suite*))))
             (read-trace-fn
              (lambda (accumulated-results is-good-trace &optional test_id)
                (if (probe-file trace-file)
                    (with-open-file (trace-stream trace-file)
                      (prog1 (rinard-incremental
                                      trace-stream
                                      accumulated-results
                                      is-good-trace
                                      test_id)
                      ;; instrumentation appends to trace file for
                      ;; some reason. Remove it before each use to
                      ;; start with an empty trace.
                      (ignore-errors (delete-file trace-file))))
                    (error "Something went wrong with trace file: ~a"
                           trace-file)))))
        (with-temp-file (bin)
          (multiple-value-bind (bin phenome-exit stderr stdout src)
              (phenome instrumented)
            (declare (ignorable stdout src))

            (if (zerop phenome-exit)
                (let* ((trace-results (collect-fault-loc-traces
                                            bin
                                            *test-suite*
                                            read-trace-fn
                                            made-up-bad-test))
                       ;; Should be only 9 statements, only AST ids.
                       ;; The first 5 elements are always the same, in
                       ;; order.  The rest may be in an arbitrary
                       ;; order.  Compare only the first 5.
                       (bad-stmts (mapcar #'cdar (rinard 5 instrumented
                                                         trace-results)))
                       (gold-set-prefix (list 54 23 12 10 4)))
                  ;(format t "BAD:  ~{~a~^,~}~%" bad-stmts)
                  ;(format t "GOLD: ~{~a~^,~}~%" gold-set-prefix)
                  (is (equal bad-stmts gold-set-prefix)))
                (progn
                  (error "Couldn't build phenome: ~a" stderr)
                  (is nil)))))))))
