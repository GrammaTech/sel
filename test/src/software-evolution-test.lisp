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

(defun batch-test (&optional args)
  "Run tests in 'batch' mode, printing results as a string."
  (declare (ignorable args))

  (let* ((*test-progress-print-right-margin* (expt 2 20))
         (failures (coerce (stefil::failure-descriptions-of
                            (without-debugging (test)))
                           'list)))
    (if failures
        (progn (format *error-output* "FAILURES~%")
               (mapc [{format *error-output* "  ~a~%"}
                      #'stefil::name-of
                      #'stefil::test-of
                      #'car #'stefil::test-context-backtrace-of]
                     failures)
               (quit 1))
        (format *error-output* "SUCCESS~%"))))

(defun testbot-test (&optional args)
  "Run tests in 'testbot' mode, pushing the results to datamanager"
  (declare (ignorable args))
  (when (null (getenv "GTHOME"))
    (error "$GTHOME must be defined prior to datamanager submission"))

  (let ((*print-test-run-progress* nil)
        (current-time (get-universal-time))
        (batch-id (uuid:make-v4-uuid))
        (test-results (without-debugging (test))))
    (maphash
     (lambda (test run)
       (with-temp-file (xml-file-path)
         (with-open-file (xml-out-stream xml-file-path
                                         :direction :output
                                         :if-does-not-exist :create
                                         :element-type '(unsigned-byte 8))
           (cxml:with-xml-output (cxml:make-octet-stream-sink xml-out-stream)
             (let ((failures (stefil::number-of-added-failure-descriptions-of
                              run)))
               (cxml:with-element "test_run"
                 (cxml:with-element "name"
                   (cxml:text (string-downcase
                               (format nil "~a" (stefil::name-of test)))))
                 (cxml:with-element "host"
                   (cxml:text (machine-instance)))
                 (cxml:with-element "branch"
                   (cxml:text "master"))
                 (cxml:with-element "project"
                   (cxml:text (string-downcase (package-name :se))))
                 (cxml:with-element "genus"
                   (cxml:text "regressions"))
                 (multiple-value-bind (sec minute hour day month year)
                     (decode-universal-time current-time)
                   (cxml:with-element "date_time"
                     (cxml:text
                      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
                              year month day hour minute sec))))
                 (cxml:with-element "key_value"
                   (cxml:with-element "key"
                     (cxml:text "BatchID"))
                   (cxml:with-element "text"
                     (cxml:text (with-output-to-string (str)
                                  (print-object batch-id str)))))
                 (cxml:with-element "key_value"
                   (cxml:with-element "key"
                     (cxml:text "User"))
                   (cxml:with-element "text"
                     (cxml:text (getenv "USER"))))
                 (cxml:with-element "key_value"
                   (cxml:with-element "key"
                     (cxml:text "Result"))
                   (cxml:with-element "text"
                     (cxml:text (if (zerop failures) "Pass" "Fail")))
                   (cxml:with-element "number"
                     (cxml:text (if (zerop failures) "5" "1"))))))))
         (shell "python ~a/gtr/scons/tools/gtnetcat.py datamanager 55555 ~a"
                (getenv "GTHOME")
                xml-file-path)))
     (stefil::run-tests-of test-results))))

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
                  {aget :src-text}]
                 (non-stmt-asts *huf*))
        "Ensure known global is in `globals'.")
    (is (find-if [{string= "int i"} {aget :src-text}]
                 (stmt-asts *huf*))
        "Ensure known local variable is in `stmts'.")
    (is (null (find "ParmVar" (stmt-asts *huf*)
                    :key {aget :ast-class} :test #'string=))
        "Ensure no ParmVar statement ASTs")
    (is (null (find "Function" (stmt-asts *huf*)
                    :key {aget :ast-class} :test #'string=))
        "Ensure no Function statement ASTs")
    (is (apply #'< (mapcar {aget :counter} (stmt-asts *huf*)))
        "Ensure statement ASTs ordered by increasing counter")))

;; Check if the two AST lists differ. Do a smoke test with
;; the list lengths; if they match, use the :src-text
;; field as a proxy for equality. Strict equality isn't
;; useful because of nondeterministic fields like :src-file.
(defun different-asts (this that)
  (or (not (equal (length this) (length that)))
      (not (every (lambda (x y)
                    (string= (aget :src-text x) (aget :src-text y)))
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
          (:value1 . ((:src-text . "/* FOO */")))))
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

(deftest intraprocedural-2pt-crossover-does-not-crash ()
  (with-fixture intraprocedural-2pt-crossover-bug-clang
    (let ((variant (intraprocedural-2pt-crossover
                     (copy *soft*)
                     (copy *soft*)
                     27 42 27 42)))
      (is (string/= (genome variant)
                    "")))))


;;; Misc. clang tests

(deftest able-to-wrap-statements-in-blocks ()
  (with-fixture gcd-wo-curlies-clang
    (let ((var (copy *gcd*)))
      ;; Setup, ensure everything is what we thing it should be.
      (is (string= "BinaryOperator"     ; Guard
                   (aget :ast-class (ast-with-text var "a > b"))))
      (is (string= "BinaryOperator"     ; Then
                   (aget :ast-class (ast-with-text var "a = a - b"))))
      (is (string= "BinaryOperator"     ; Else
                   (aget :ast-class (ast-with-text var "b = b - a"))))
      ;; Wrap children and ensure changes are made.
      (let ((if-counter
             (aget :counter
                   (second (remove-if-not [{string= "IfStmt"} {aget :ast-class}]
                                          (asts var))))))
        (setf var (wrap-child var if-counter 1))
        (setf var (wrap-child var if-counter 2)))
      (is (string= "BinaryOperator"     ; Guard
                   (aget :ast-class (ast-with-text var "a > b"))))
      (is (string= "CompoundStmt"       ; Then
                   (aget :ast-class (get-parent-ast var
                                      (ast-with-text var "a = a - b")))))
      (is (string= "CompoundStmt"       ; Then
                   (aget :ast-class (get-parent-ast var
                                      (ast-with-text var "b = b - a")))))
      ;; Ensure gcd remains unchanged.
      (is (string= "BinaryOperator"     ; Guard
                   (aget :ast-class (ast-with-text *gcd* "a > b"))))
      (is (string= "BinaryOperator"     ; Then
                   (aget :ast-class (ast-with-text *gcd* "a = a - b"))))
      (is (string= "BinaryOperator"     ; Else
                   (aget :ast-class (ast-with-text *gcd* "b = b - a")))))))

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
                 (sort (mapcar {aget :type} types) #'string<))))))

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
                    `((:decl . "struct printf { chocolate cake; }")))
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest add-bad-macro-doesnt-change-number-of-asts ()
  (with-fixture hello-world-clang
    (let ((orig-num-asts (size *hello-world*)))
      (se::add-macro *hello-world*
                     "GARBAGE" "#ifndef GARBAGE DO_SOMETHING_FORGET_ENDIF()")
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest clang-expression-test ()
  (flet ((test-conversion (obj pair)
           (destructuring-bind (text expected-expression) pair
             (let ((result (expression obj (ast-with-text obj text))))
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

(deftest clang-pick-general-full-stmt-no-matching-test ()
  "Ensure calling pick-general with the :full-stmt flag set to true
throws a no-mutation-targets error when there are no full stmts,
e.g. after a bad crossover"
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets
      (se::pick-general *soft*
                        (remove-if {aget :full-stmt}
                                   (stmt-asts *soft*))
                        :full-stmt t))))

(deftest clang-pick-general-same-class-no-matching-test ()
  "Ensure calling pick-general with the :same-class flag set to true throws
a no-mutation-targets error when a second statement with the same AST class
is not to be found"
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets
      (se::pick-general *soft*
                        (stmt-asts *soft*)
                        :second-pool `(((:ast-class . "Nothing")
                                        (:full-stmt . nil)))
                        :same-class t))))

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

(defvar *good-asts* nil "Control pick-good")
(defvar *bad-asts* nil "Control pick-bad")
(define-software clang-control-picks (clang) ())
(defmethod good-stmts ((obj clang-control-picks))
  (or *good-asts* (remove-if {aget :is-decl} (asts obj))))
(defmethod bad-stmts ((obj clang-control-picks))
  (or *bad-asts* (remove-if {aget :is-decl} (asts obj))))

(defixture hello-world-clang-control-picks
  (:setup
    (setf *hello-world*
      (from-file (make-instance 'clang-control-picks :compiler "clang"
                                :flags '("-g -m32 -O0"))
                 (hello-world-dir "hello_world.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defun asts-with-text (obj &rest texts)
  (mapcar [{get-ast obj} {stmt-with-text obj}] texts))

(deftest cut-full-removes-full-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-cut-full . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (< (count-if {aget :full-stmt} (asts variant))
             (count-if {aget :full-stmt} (asts *hello-world*)))))))

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
      (is (> (count-if {aget :full-stmt} (asts variant))
             (count-if {aget :full-stmt} (asts *hello-world*)))))))

(deftest insert-adds-any-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf"))
          (*good-asts* (asts-with-text *hello-world* "printf"))
          (*clang-mutation-types* '((clang-insert . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "printfprintf")))))

(deftest insert-same-adds-same-class ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "0"))
          (*clang-mutation-types* '((clang-insert-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "00")))))

(deftest insert-full-same-adds-same-class-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0"))
          (*clang-mutation-types* '((clang-insert-full-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if [{equal "ReturnStmt"} {aget :ast-class}]
                       (asts variant))
             (count-if [{equal "ReturnStmt"} {aget :ast-class}]
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
      (is (> (count-if [{equal "CallExpr"} {aget :ast-class}]
                       (asts variant))
             (count-if [{equal "CallExpr"} {aget :ast-class}]
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
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt1 (targets mutation)))))
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt2 (targets mutation)))))

        ;; Not a very interesting test: this can only replace a
        ;; statement with itself, but sometimes there are whitespace
        ;; changes. Just compare AST classes to avoid spurious
        ;; failures.
        (is (equal (mapcar {aget :ast-class} (asts variant))
                   (mapcar {aget :ast-class} (asts *hello-world*))))))))

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
                                  {aget :ast-class}]
                                 (asts *hello-world*)))
          (variant (copy *hello-world*)))

      (multiple-value-bind  (variant mutation) (mutate variant)
        ;; We can't predict exactly what will be swapped. Just
        ;; sanity check.
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt1 (targets mutation)))))
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt2 (targets mutation)))))
        (is (stmt-with-text variant "printf"))
        (is (stmt-with-text variant "return 0"))))))

(deftest swap-full-same-changes-same-class-full-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-swap-full-same . 1)))
          (variant (copy *hello-world*)))
      (multiple-value-bind  (variant mutation) (mutate variant)
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt1 (targets mutation)))))
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt2 (targets mutation)))))

        ;; Not a very interesting test: this can only swap a
        ;; statement with itself, but sometimes there are whitespace
        ;; changes. Just compare AST classes to avoid spurious
        ;; failures.
        (is (equal (mapcar {aget :ast-class} (asts variant))
                   (mapcar {aget :ast-class} (asts *hello-world*))))))))

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
             (genome-string
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('WHILE')")
                                (get-ast *nested*)
                                (get-parent-asts *nested*)
                                (third))))))
           "/* While loop. */"
           "puts('WHILE');")
          "Promotes single-line body from within while loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome-string
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('DO')")
                                (get-ast *nested*)
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Do loop. */"
           "puts('DO');")
          "Promotes single-line body from within do loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome-string
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('FOR')")
                                (get-ast *nested*)
                                (get-parent-asts *nested*)
                                (third))))))
           "/* For loop. */"
           "puts('FOR');")
          "Promotes single-line body from within for loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome-string
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('IF-1')")
                                (get-ast *nested*)
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Single child. */"
           "puts('IF-1');")
          "Promotes single-line sole branch of if.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome-string
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('IF-2')")
                                (get-ast *nested*)
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Empty then. */"
           "puts('IF-2');")
          "Promotes single-line else of if w/o then.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome-string
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('IF-3')")
                                (get-ast *nested*)
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Empty else. */"
           "puts('IF-3');")
          "Promotes single-line then of if w/o else.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome-string
              (apply-mutation copy
                (make-instance 'clang-promote-guarded
                  :object copy
                  :targets (->> (stmt-with-text *nested* "puts('IF-3')")
                                (get-ast *nested*)
                                (get-parent-asts *nested*)
                                (third))))))
           "/* Empty else. */"
           "puts('IF-3');")
          "Promotes single-line then of if w/o else.")
      (let ((genome-string
             (let ((copy (copy *nested*)))
               (genome-string
                (apply-mutation copy
                  (make-instance 'clang-promote-guarded
                    :object copy
                    :targets (->> (stmt-with-text *nested* "puts('MULTILINE')")
                                  (get-ast *nested*)
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


;;; Clang w/ mutation fodder representation
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

;;; Clang utility methods
(deftest asts-populated-on-creation ()
  (with-fixture hello-world-clang
    (is (= 10 (length (asts *hello-world*))))))

(deftest parent-ast-p-true-test()
  (with-fixture hello-world-clang
    (is (parent-ast-p *hello-world*
          (get-ast *hello-world* (stmt-with-text *hello-world*
                                                  "return 0"))
          (get-ast *hello-world* (stmt-with-text *hello-world*
                                                  "0"))))))

(deftest parent-ast-p-false-test()
  (with-fixture hello-world-clang
    (is (not (parent-ast-p *hello-world*
               (get-ast *hello-world* (stmt-with-text *hello-world*
                                                       "0"))
               (get-ast *hello-world* (stmt-with-text *hello-world*
                                                       "return 0")))))))

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
                                    {aget :ast-class}])
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
    (let ((variant (copy *hello-world*))
          (op (make-instance 'clang-insert
                :targets '((:stmt1 . 1)
                           (:literal1 . "/* nothing */")))))
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
    (let ((variant (copy *hello-world*))
          (op (make-instance 'clang-cut :targets '((:stmt1 . 2)))))
      (apply-mutation variant op)
      (analyze-mutation variant (list op nil nil *hello-world* nil nil) *test*)
      (is (equal :worse (first (second (first (hash-table-alist
                                               *mutation-stats*)))))
          "`analyze-mutation' notices worse improvement"))))

(deftest mutation-stats-notices-same ()
  (with-fixture hello-world-clang-w-fitness
    (evaluate *test* *hello-world*)
    (is (numberp (fitness *hello-world*)))
    (let ((variant (copy *hello-world*))
          (op (make-instance 'clang-swap
                :targets '((:stmt1 . 2) (:stmt2 . 2)))))
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
  (->> (asts obj)
       (find-if [{string= text} #'peel-bananas {aget :src-text}])
       (aget :counter)))

(defun ast-with-text (obj text)
  (when-let ((id (stmt-with-text obj text)))
    (get-ast obj id)))

(defun stmt-starting-with-text (obj text)
  (let ((the-snippet
         (find-if
          (lambda (snippet)
            (and snippet
                 (equal 0
                        (search text
                                (peel-bananas (aget :src-text snippet))))))
          (asts obj))))
    (aget :counter the-snippet)))

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
    (is (eq stmt (stmt-with-text obj "test(0)")))))

;; Check that the ASTLister traversal and the ASTMutate traversal see
;; the same number of ASTs.
(deftest ast-lister-finds-same-number-of-ids ()
  (with-fixture huf-clang
    (is (= (size *huf*)
           (count '#\Newline (clang-mutate *huf* '(:list)))))))

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

(defun select-intraprocedural-pair-with-adjustments-test (obj)
  (let ((function (first (functions obj))))
    (loop :for i :from 0 :to 25
          :do (progn (multiple-value-bind (pt1 pt2)
                         (select-intraprocedural-pair obj)
                       (multiple-value-bind (stmt1 stmt2)
                           (adjust-stmt-range obj pt1 pt2)
                         (is (<= (1+ (first (aget :stmt-range function)))
                                 stmt1
                                 (second (aget :stmt-range function))))
                         (is (<= (1+ (first (aget :stmt-range function)))
                                 stmt2
                                 (second (aget :stmt-range function))))
                         (is (full-stmt-p obj stmt1))
                         (is (full-stmt-p obj stmt2))))))))

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
    (is (equal (aget :counter (first (stmt-asts *fib*)))
               (common-ancestor *fib*
                                (stmt-with-text *fib* "int x = 0")
                                (stmt-with-text *fib* "int y = 1"))))
    (is (equal (aget :counter (first (stmt-asts *fib*)))
               (common-ancestor *fib*
                                (stmt-with-text *fib* "int x = 0")
                                (stmt-with-text *fib* "int t = x"))))
    (is (equal (aget :counter (second (remove-if-not {string= "CompoundStmt"}
                                                     (stmt-asts *fib*)
                                                     :key {aget :ast-class})))
               (common-ancestor *fib*
                                (stmt-with-text *fib* "int t = x")
                                (stmt-with-text *fib* "x = x + y"))))))

(deftest common-ancestor-collatz-test ()
  (with-fixture collatz-clang
    (is (equal (aget :counter (first (stmt-asts *collatz*)))
               (common-ancestor *collatz*
                                (stmt-with-text *collatz* "int k = 0")
                                (stmt-with-text *collatz* "return k"))))
    (is (equal (aget :counter (first (stmt-asts *collatz*)))
               (common-ancestor *collatz*
                                (stmt-with-text *collatz* "int k = 0")
                                (stmt-with-text *collatz* "m /= 2"))))
    (is (equal (aget :counter (first (stmt-asts *collatz*)))
               (common-ancestor *collatz*
                                (stmt-with-text *collatz* "m /= 2")
                                (stmt-with-text *collatz* "return k"))))
    (is (equal (aget :counter (fourth (remove-if-not {aget :full-stmt}
                                                     (stmt-asts *collatz*))))
               (common-ancestor *collatz*
                                (stmt-with-text *collatz* "m /= 2")
                                (stmt-with-text *collatz* "m = 3*m + 1"))))
    (is (equal (aget :counter (second (remove-if-not {string= "CompoundStmt"}
                                                     (stmt-asts *collatz*)
                                                     :key {aget :ast-class})))
               (common-ancestor *collatz*
                                (stmt-with-text *collatz* "m /= 2")
                                (stmt-with-text *collatz* "++k"))))))

(deftest common-ancestor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal (aget :counter (first (stmt-asts *soft*)))
               (common-ancestor *soft*
                                (stmt-with-text *soft* "int i")
                                (stmt-with-text *soft* "return 0"))))
    (is (equal (aget :counter (first (stmt-asts *soft*)))
               (common-ancestor *soft*
                                (stmt-with-text *soft*
                                                "int i")
                                (stmt-with-text *soft*
                                                "printf(\"%d\\n\", i+j)"))))
    (is (equal (aget :counter (first (remove-if-not {string= "ForStmt"}
                                                    (stmt-asts *soft*)
                                                    :key {aget :ast-class})))
               (common-ancestor *soft*
                                (first (remove-if-not {string= "ForStmt"}
                                                      (stmt-asts *soft*)
                                                      :key {aget :ast-class}))
                                (stmt-with-text *soft*
                                                "printf(\"%d\\n\", i+j)"))))
    (is (equal (aget :counter (second (remove-if-not {string= "ForStmt"}
                                                     (stmt-asts *soft*)
                                                     :key {aget :ast-class})))
               (common-ancestor *soft*
                                (second (remove-if-not {string= "ForStmt"}
                                                       (stmt-asts *soft*)
                                                       :key {aget :ast-class}))
                                (stmt-with-text *soft*
                                                "printf(\"%d\\n\", i+j)"))))))

(deftest common-ancestor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal (aget :counter (first (stmt-asts *soft*)))
               (common-ancestor *soft*
                                (->> "printf(\"%d\\n\", argc)"
                                     (stmt-with-text *soft*))
                                (->> "printf(\"%d\\n\", argc + argc)"
                                     (stmt-with-text *soft*)))))
    (is (equal (aget :counter (first (stmt-asts *soft*)))
               (common-ancestor *soft*
                                (->> "printf(\"%d\\n\", argc + argc)"
                                     (stmt-with-text *soft*))
                                (->> "return 0"
                                     (stmt-with-text *soft*)))))
    (is (equal (aget :counter (second (remove-if-not {string= "CompoundStmt"}
                                                     (stmt-asts *soft*)
                                                     :key {aget :ast-class})))
               (common-ancestor *soft*
                                (->> "printf(\"%d\\n\", argc + argc)"
                                     (stmt-with-text *soft*))
                                (->> "printf(\"%d\\n\", argc * argc)"
                                     (stmt-with-text *soft*)))))
    (is (equal (aget :counter (first (remove-if-not {string= "CaseStmt"}
                                                    (stmt-asts *soft*)
                                                    :key {aget :ast-class})))
               (common-ancestor *soft*
                                (->> "printf(\"%d\\n\", argc + argc)"
                                     (stmt-with-text *soft*))
                                (->> (stmt-asts *soft*)
                                     (remove-if-not [{string= "CaseStmt"}
                                                     {aget :ast-class}])
                                     (first)
                                     (aget :counter)))))))

(deftest ancestor-after-fib-test ()
  (with-fixture fib-clang
    (is (equal (stmt-with-text *fib* "int x = 0")
               (se::ancestor-after *fib*
                                   (->> (stmt-asts *fib*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (first)
                                        (aget :counter))
                                   (stmt-with-text *fib* "int x = 0"))))
    (is (equal (->> (stmt-asts *fib*)
                    (remove-if-not [{string= "WhileStmt"}
                                    {aget :ast-class}])
                    (first)
                    (aget :counter))
               (se::ancestor-after *fib*
                                   (->> (stmt-asts *fib*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (first)
                                        (aget :counter))
                                   (stmt-with-text *fib* "int t = x"))))
    (is (equal (stmt-with-text *fib* "x = x + y")
               (se::ancestor-after *fib*
                                   (->> (stmt-asts *fib*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (second)
                                        (aget :counter))
                                   (stmt-with-text *fib* "x = x + y"))))))

(deftest ancestor-after-collatz-test ()
  (with-fixture collatz-clang
    (is (equal (stmt-with-text *collatz* "int k = 0")
               (se::ancestor-after *collatz*
                                   (->> (stmt-asts *collatz*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (first)
                                        (aget :counter))
                                   (stmt-with-text *collatz* "int k = 0"))))
    (is (equal (stmt-with-text *collatz* "return k")
               (se::ancestor-after *collatz*
                                   (->> (stmt-asts *collatz*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (first)
                                        (aget :counter))
                                   (stmt-with-text *collatz* "return k"))))
    (is (equal (->> (stmt-asts *collatz*)
                    (remove-if-not [{string= "WhileStmt"}
                                    {aget :ast-class}])
                    (first)
                    (aget :counter))
               (se::ancestor-after *collatz*
                                   (->> (stmt-asts *collatz*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (first)
                                        (aget :counter))
                                   (stmt-with-text *collatz* "m /= 2"))))
    (is (equal (->> (stmt-asts *collatz*)
                    (remove-if-not [{string= "IfStmt"}
                                    {aget :ast-class}])
                    (first)
                    (aget :counter))
               (se::ancestor-after *collatz*
                                   (->> (stmt-asts *collatz*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (second)
                                        (aget :counter))
                                   (stmt-with-text *collatz* "m /= 2"))))
    (is (equal (->> (stmt-asts *collatz*)
                    (remove-if-not [{string= "IfStmt"}
                                    {aget :ast-class}])
                    (first)
                    (aget :counter))
               (se::ancestor-after *collatz*
                                   (->> (stmt-asts *collatz*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (second)
                                        (aget :counter))
                                   (stmt-with-text *collatz* "m = 3*m + 1"))))
    (is (equal (stmt-with-text *collatz* "++k")
               (se::ancestor-after *collatz*
                                   (->> (stmt-asts *collatz*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (second)
                                        (aget :counter))
                                   (stmt-with-text *collatz* "++k"))))))

(deftest ancestor-after-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "ForStmt"}
                                    {aget :ast-class}])
                    (second)
                    (aget :counter))
               (se::ancestor-after *soft*
                                   (->> (stmt-asts *soft*)
                                        (remove-if-not [{string= "ForStmt"}
                                                        {aget :ast-class}])
                                        (first)
                                        (aget :counter))
                                   (stmt-with-text *soft*
                                                   "printf(\"%d\\n\", i+j)"))))
    (is (equal (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
               (se::ancestor-after *soft*
                                   (->> (stmt-asts *soft*)
                                        (remove-if-not [{string= "ForStmt"}
                                                        {aget :ast-class}])
                                        (second)
                                        (aget :counter))
                                   (stmt-with-text *soft*
                                                   "printf(\"%d\\n\", i+j)"))))
    (is (equal (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "ForStmt"}
                                    {aget :ast-class}])
                    (first)
                    (aget :counter))
               (se::ancestor-after *soft*
                                   (->> (stmt-asts *soft*)
                                        (first)
                                        (aget :counter))
                                   (stmt-with-text *soft*
                                                   "printf(\"%d\\n\", i+j)"))))
    (is (equal (stmt-with-text *soft* "return 0")
               (se::ancestor-after *soft*
                                   (->> (stmt-asts *soft*)
                                        (first)
                                        (aget :counter))
                                   (stmt-with-text *soft* "return 0"))))))

(deftest ancestor-after-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "CaseStmt"}
                                    {aget :ast-class}])
                    (second)
                    (aget :counter))
               (se::ancestor-after *soft*
                                   (->> (stmt-asts *soft*)
                                        (remove-if-not [{string= "CompoundStmt"}
                                                        {aget :ast-class}])
                                        (second)
                                        (aget :counter))
                                   (->> "printf(\"%d\\n\", argc * argc)"
                                        (stmt-with-text *soft*)))))))

(deftest full-stmt-successors-fib-test ()
  (with-fixture fib-clang
    (is (equal '("DeclStmt" "WhileStmt" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *fib* "int x = 0")
                    (full-stmt-successors *fib*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))
    (is (equal '("BinaryOperator" "BinaryOperator" "CompoundStmt"
                 "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *fib* "int t = x")
                    (full-stmt-successors *fib*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))
    (is (equal '("CompoundStmt")
               (->> (stmt-with-text *fib* "return x")
                    (full-stmt-successors *fib*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))))

(deftest full-stmt-successors-collatz-test ()
  (with-fixture collatz-clang
    (is (equal '("WhileStmt" "CallExpr" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *collatz* "int k = 0")
                    (full-stmt-successors *collatz*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))
    (is (equal '("CompoundStmt" "UnaryOperator" "CompoundStmt"
                 "CallExpr" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *collatz* "m /= 2")
                    (full-stmt-successors *collatz*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))
    (is (equal '("CompoundStmt")
               (->> (stmt-with-text *collatz* "return k")
                    (full-stmt-successors *collatz*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))))

(deftest full-stmt-successors-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal '("DeclStmt" "ForStmt" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *soft* "int i")
                    (full-stmt-successors *soft*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))
    (is (equal '("ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                    (full-stmt-successors *soft*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))))

(deftest full-stmt-successors-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal '("SwitchStmt" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
                    (full-stmt-successors *soft*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))
    (is (equal '("CompoundStmt" "ReturnStmt" "CompoundStmt")
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                    (full-stmt-successors *soft*)
                    (apply #'append)
                    (mapcar {aget :ast-class}))))))

(deftest enclosing-full-stmt-collatz-test ()
  (with-fixture collatz-clang
    (is (equal (stmt-with-text *collatz* "m = 3*m + 1")
               (enclosing-full-stmt *collatz*
                                    (stmt-with-text *collatz* "3"))))
    (is (equal (stmt-with-text *collatz* "m = 3*m + 1")
               (enclosing-full-stmt *collatz*
                                    (stmt-with-text *collatz*
                                                    "m = 3*m + 1"))))
    (is (equal (->> (stmt-asts *collatz*)
                    (first)
                    (aget :counter))
               (enclosing-full-stmt *collatz*
                                    (->> (stmt-asts *collatz*)
                                         (first)
                                         (aget :counter)))))))

(deftest enclosing-full-stmt-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                    (enclosing-full-stmt *soft*))))
    (is (equal (->> (remove-if-not [{string= "ForStmt"}{aget :ast-class}]
                                   (stmt-asts *soft*))
                    (first)
                    (aget :counter))
               (enclosing-full-stmt *soft*
                                    (->> (remove-if-not [{string= "ForStmt"}
                                                         {aget :ast-class}]
                                                        (stmt-asts *soft*))
                                         (first)))))
    (is (equal (->> (remove-if-not [{string= "ForStmt"}{aget :ast-class}]
                                   (stmt-asts *soft*))
                    (second)
                    (aget :counter))
               (enclosing-full-stmt *soft*
                                    (->> (remove-if-not [{string= "ForStmt"}
                                                         {aget :ast-class}]
                                                        (stmt-asts *soft*))
                                         (second)))))
    (is (equal (->> (stmt-asts *soft*)
                    (first)
                    (aget :counter))
               (enclosing-full-stmt *soft*
                                    (->> (stmt-asts *soft*)
                                         (first)
                                         (aget :counter)))))))

(deftest enclosing-full-stmt-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
                    (enclosing-full-stmt *soft*))))
    (is (equal (->> (remove-if-not [{string= "SwitchStmt"}{aget :ast-class}]
                                   (stmt-asts *soft*))
                    (first)
                    (aget :counter))
               (enclosing-full-stmt *soft*
                                    (->> (remove-if-not [{string= "SwitchStmt"}
                                                         {aget :ast-class}]
                                                        (stmt-asts *soft*))
                                         (first)
                                         (aget :counter)))))
    (is (equal (->> (remove-if-not [{string= "CaseStmt"}{aget :ast-class}]
                                   (stmt-asts *soft*))
                    (first)
                    (aget :counter))
               (enclosing-full-stmt *soft*
                                    (->> (remove-if-not [{string= "CaseStmt"}
                                                         {aget :ast-class}]
                                                        (stmt-asts *soft*))
                                         (first)
                                         (aget :counter)))))
    (is (equal (->> (remove-if-not [{string= "BreakStmt"}{aget :ast-class}]
                                   (stmt-asts *soft*))
                    (first)
                    (aget :counter))
               (enclosing-full-stmt *soft*
                                    (->> (remove-if-not [{string= "BreakStmt"}
                                                         {aget :ast-class}]
                                                        (stmt-asts *soft*))
                                         (first)
                                         (aget :counter)))))))

(deftest enclosing-block-collatz-test ()
  (with-fixture collatz-clang
    (is (equal (->> (stmt-asts *collatz*)
                    (remove-if-not [{string= "CompoundStmt"}
                                    {aget :ast-class}])
                    (first)
                    (aget :counter))
               (enclosing-block *collatz* (stmt-with-text *collatz*
                                                          "int k = 0"))))
    (is (equal (->> (stmt-asts *collatz*)
                    (remove-if-not [{string= "CompoundStmt"}
                                    {aget :ast-class}])
                    (second)
                    (aget :counter))
               (enclosing-block *collatz* (stmt-with-text *collatz* "++k"))))
    (is (equal (->> (stmt-asts *collatz*)
                    (remove-if-not [{string= "CompoundStmt"}
                                    {aget :ast-class}])
                    (third)
                    (aget :counter))
               (enclosing-block *collatz* (stmt-with-text *collatz* "m /= 2"))))
    (is (equal 0
               (enclosing-block *collatz*
                                (->> (stmt-asts *collatz*)
                                     (remove-if-not [{string= "CompoundStmt"}
                                                     {aget :ast-class}])
                                     (first)
                                     (aget :counter)))))))

(deftest enclosing-block-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal (->> (remove-if-not [{string= "ForStmt"}{aget :ast-class}]
                                   (stmt-asts *soft*))
                    (second)
                    (aget :counter))
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                    (enclosing-block *soft*))))
    (is (equal (->> (remove-if-not [{string= "CompoundStmt"}{aget :ast-class}]
                                   (stmt-asts *soft*))
                    (first)
                    (aget :counter))
               (enclosing-block *soft*
                                (->> (remove-if-not [{string= "ForStmt"}
                                                     {aget :ast-class}]
                                                    (stmt-asts *soft*))
                                     (first)
                                     (aget :counter)))))
    (is (equal (->> (remove-if-not [{string= "ForStmt"}{aget :ast-class}]
                                   (stmt-asts *soft*))
                    (first)
                    (aget :counter))
               (enclosing-block *soft*
                                (->> (remove-if-not [{string= "ForStmt"}
                                                     {aget :ast-class}]
                                                    (stmt-asts *soft*))
                                     (second)
                                     (aget :counter)))))
    (is (equal 0
               (enclosing-block *soft*
                                (->> (stmt-asts *soft*)
                                     (first)
                                     (aget :counter)))))))

(deftest enclosing-block-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal (aget :counter (first (stmt-asts *soft*)))
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
                    (enclosing-block *soft*))))
    (is (equal (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "CompoundStmt"}{aget :ast-class}])
                    (second)
                    (aget :counter))
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                    (enclosing-block *soft*))))
    (is (equal (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "CompoundStmt"}{aget :ast-class}])
                    (second)
                    (aget :counter))
               (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "CaseStmt"}{aget :ast-class}])
                    (first)
                    (aget :counter)
                    (enclosing-block *soft*))))))

(deftest block-p-collatz-test ()
  (with-fixture collatz-clang
    (loop :for ast
          :in (stmt-asts *collatz*)
          :do (is (equal (string= "CompoundStmt" (aget :ast-class ast))
                         (block-p *collatz* ast))))))

(deftest block-p-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (loop :for ast
          :in (stmt-asts *soft*)
          :do (is (equal (or (string= "CompoundStmt" (aget :ast-class ast))
                             (string= "ForStmt" (aget :ast-class ast)))
                         (block-p *soft* ast))))))

(deftest block-p-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (loop :for ast
          :in (stmt-asts *soft*)
          :do (is (equal (string= "CompoundStmt" (aget :ast-class ast))
                         (block-p *soft* ast))))))

(deftest block-successor-collatz-test ()
  (with-fixture collatz-clang
    (is (equal "WhileStmt"
               (->> (block-successor *collatz* (stmt-with-text *collatz*
                                                               "int k = 0"))
                    (get-ast *collatz*)
                    (aget :ast-class))))
    (is (equal "ReturnStmt"
               (->> (block-successor *collatz*
                                     (stmt-with-text *collatz*
                                                     "printf(\"%d\\n\", k)"))
                    (get-ast *collatz*)
                    (aget :ast-class))))
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
                                (->> (remove-if-not [{string= "ForStmt"}
                                                     {aget :ast-class}]
                                                    (stmt-asts *soft*))
                                     (second)
                                     (aget :counter)))))
    (is (equal (stmt-with-text *soft* "return 0")
               (block-successor *soft*
                                (->> (remove-if-not [{string= "ForStmt"}
                                                     {aget :ast-class}]
                                                    (stmt-asts *soft*))
                                     (first)
                                     (aget :counter)))))))

(deftest block-successor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                    (block-successor *soft*))))
    (is (equal (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "DefaultStmt"}{aget :ast-class}])
                    (first)
                    (aget :counter))
               (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "CaseStmt"}{aget :ast-class}])
                    (second)
                    (aget :counter)
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
                    (get-ast *collatz*)
                    (aget :ast-class))))
    (is (equal nil
               (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                             "m /= 2"))))
    (is (equal "IfStmt"
               (->> (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                                  "++k"))
                    (get-ast *collatz*)
                    (aget :ast-class))))
    (is (equal "CallExpr"
               (->> (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                                  "return k"))
                    (get-ast *collatz*)
                    (aget :ast-class))))))

(deftest block-predeccessor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                    (block-predeccessor *soft*))))
    (is (equal nil
               (block-successor *soft*
                                (->> (remove-if-not [{string= "ForStmt"}
                                                     {aget :ast-class}]
                                                    (stmt-asts *soft*))
                                     (second)
                                     (aget :counter)))))
    (is (equal (stmt-with-text *soft* "int j")
               (block-predeccessor *soft*
                                   (->> (remove-if-not [{string= "ForStmt"}
                                                        {aget :ast-class}]
                                                       (stmt-asts *soft*))
                                        (first)
                                        (aget :counter)))))))

(deftest block-predeccessor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
                    (block-predeccessor *soft*))))
    (is (equal (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "CaseStmt"}{aget :ast-class}])
                    (first)
                    (aget :counter))
               (->> (stmt-asts *soft*)
                    (remove-if-not [{string= "CaseStmt"}{aget :ast-class}])
                    (second)
                    (aget :counter)
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
        (is (equal (cons (stmt-with-text *fib* "x = x + y")
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
                                 {aget :ast-class}])
                 (first)
                 (aget :counter))
            (stmt-with-text *fib* "int t = x")
            (->> (stmt-asts *fib*)
                 (remove-if-not [{string= "WhileStmt"}
                                 {aget :ast-class}])
                 (first)
                 (aget :counter)))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equal (cons (stmt-with-text *fib* "int t = x")
                         (->> (stmt-asts *fib*)
                              (remove-if-not [{string= "WhileStmt"}
                                              {aget :ast-class}])
                              (first)
                              (aget :counter)))
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
        (is (equal (cons (stmt-with-text *collatz* "m /= 2")
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
        (is (equal (cons (stmt-with-text *collatz* "++k")
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
        (is (equal (cons (stmt-with-text *soft* "printf(\"%d\\n\", i+j)")
                         (stmt-with-text *soft* "return 0"))
                   effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (->> (stmt-asts *soft*)
                 (remove-if-not [{string= "ForStmt"}
                                 {aget :ast-class}])
                 (second)
                 (aget :counter))
            (stmt-with-text *soft* "return 0")
            (->> (stmt-asts *soft*)
                 (remove-if-not [{string= "ForStmt"}
                                 {aget :ast-class}])
                 (second)
                 (aget :counter))
            (stmt-with-text *soft* "return 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equal (cons (->> (stmt-asts *soft*)
                              (remove-if-not [{string= "ForStmt"}
                                              {aget :ast-class}])
                              (second)
                              (aget :counter))
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
        (is (< (length (stmt-asts variant))
               (length (stmt-asts *soft*))))
        (is (equal (cons (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc)")
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
        (is (equal (cons (stmt-with-text *fib* "int x = 0")
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
        (is (equal (cons (stmt-with-text *fib* "int x = 0")
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
        (is (equal (cons (stmt-with-text *collatz* "int k = 0")
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
        (is (equal (cons (stmt-with-text *collatz* "int k = 0")
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
        (is (equal (cons (stmt-with-text *collatz* "int k = 0")
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
        (is (equal (cons (stmt-with-text *soft* "int i")
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
        (is (equal (cons (stmt-with-text *soft* "int i")
                         (stmt-with-text *soft* "printf(\"%d\\n\", i+j)"))
                   effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "int i")
            (->> (stmt-asts *soft*)
                 (remove-if-not [{string= "ForStmt"}
                                 {aget :ast-class}])
                 (first)
                 (aget :counter))
            (stmt-with-text *soft* "int i")
            (->> (stmt-asts *soft*)
                 (remove-if-not [{string= "ForStmt"}
                                 {aget :ast-class}])
                 (first)
                 (aget :counter)))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equal (cons (stmt-with-text *soft* "int i")
                         (->> (stmt-asts *soft*)
                              (remove-if-not [{string= "ForStmt"}
                                              {aget :ast-class}])
                              (first)
                              (aget :counter)))
                   effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
            (copy *soft*)
            (copy *soft*)
            (stmt-with-text *soft* "int i")
            (->> (stmt-asts *soft*)
                 (remove-if-not [{string= "ForStmt"}
                                 {aget :ast-class}])
                 (second)
                 (aget :counter))
            (stmt-with-text *soft* "int i")
            (->> (stmt-asts *soft*)
                 (remove-if-not [{string= "ForStmt"}
                                 {aget :ast-class}])
                 (second)
                 (aget :counter)))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equal (cons (stmt-with-text *soft* "int i")
                         (->> (stmt-asts *soft*)
                              (remove-if-not [{string= "ForStmt"}
                                              {aget :ast-class}])
                              (second)
                              (aget :counter)))
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
        (is (< (length (stmt-asts variant))
               (length (stmt-asts *soft*))))
        (is (equal (cons (stmt-with-text *soft*
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
        (is (equal (cons (stmt-with-text *soft* "printf(\"%d\\n\", argc)")
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
        (is (equal effective-a-pts target-a-pts))))))

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
        (is (equal effective-a-pts target-a-pts)))))
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
        (is (equal effective-a-pts target-a-pts))))))

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
               (length (asts variant))))))))

(deftest single-decl-works ()
  (with-fixture scopes-clang
    (let ((ast (get-ast *scopes*
                        (stmt-with-text *scopes* "int a"))))
      (is (= 1 (length (aget :declares ast)))))))

(deftest multiple-decl-works ()
  (with-fixture scopes-clang
    (when-let* ((it (stmt-with-text *scopes* "int f, g"))
                (ast (get-ast *scopes* it)))
      (is (= 2 (length (aget :declares ast)))))))

(deftest pick-for-loop-works ()
  (with-fixture scopes-clang
    (is (string= "ForStmt" (->> (se::pick-for-loop *scopes*)
                                (aget :stmt1)
                                (get-ast *scopes*)
                                (aget :ast-class)))
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
                                  (get-ast *scopes*)
                                  (aget :ast-class)))
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
           (list (get-ast *scopes*
                          (stmt-with-text *scopes* "int a")))))
      (apply-mutation variant
                      (make-instance 'swap-decls :object variant))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))
    ;; Check if swap-decls works when only one decl is in the
    ;; selected scope. The expected behavior is to crawl up to
    ;; the first enclosing scope with at least two decls.
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (get-ast *scopes*
                          (stmt-with-text *scopes* "int d")))))
      (apply-mutation variant
                      (make-instance 'swap-decls :object variant))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest rename-variable-works ()
  (with-fixture scopes-clang
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (get-ast *scopes*
                          (stmt-with-text *scopes* "b = 1")))))
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
                                             [{eq 93} {aget :counter}]})))
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
                                  (aget :body (first (functions *gcd*))))))

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
               (iter (for i below (size *gcd*))
                     (if (evenp i)
                         (collect (list i :even))
                         (collect (list i :odd))))))))
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
      (let* ((else-counter (stmt-with-text *gcd* "b = b - a"))
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
              `((,(aget :counter (ast-with-text *gcd* "b - a")) ,cookie)))))
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
  [{mapcar [#'peel-bananas #'car]} {aget :unbound-vals}]
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
                                        [{apply #'append} {aget :scopes}]})))
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
    (is (= 7 (count-if {aget :pointer} (types *huf*)))
        "Huf has seven pointer types.")
    (is (= 3 (count-if [#'not #'emptyp {aget :array}] (types *huf*)))
        "Huf has three array types.")
    (is (= 3 (count-if [{string= "int"} {aget :type}] (types *huf*)))
        "Huf has three different \"int\" types (some are array and pointer).")))

(deftest huf-finds-type-info-for-variables ()
  (with-fixture huf-clang
    (let ((type (type-of-var *huf* "strbit")))
      (is type "Found type for \"strbit\" in huf.")
      (is (string= "[]" (aget :array type))
          "Variable \"strbit\" in huf is a dynamically sized array.")
      (is (not (aget :pointer type))
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
  (let ((*population* (list (make-instance 'simple :fitness '(1 1 1))
                            (make-instance 'simple :fitness '(0 0 0))
                            (make-instance 'simple :fitness '(0 0 0))))
        (*pareto-comparison-set-size* 3)
        (*tournament-selector* #'pareto-selector))

    (is (equal (first *population*)
               (tournament :size 3)))))

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


;; Camouflage tests
(in-suite test)
(defsuite* test-camouflage)

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
