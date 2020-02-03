(defpackage :software-evolution-library/test/util
  (:nicknames :sel/test/util)
  (:use :common-lisp
        :alexandria
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/stefil-plus)
  (:export :+etc-dir+
           :+gcd-dir+
           :+grep-prj-dir+
           :+multi-file-dir+
           ;; Variables referenced in tests
           :*tfos*
           :*soft*
           :*gcd*
           :*binary-search*))
(in-package :software-evolution-library/test/util)

(defvar *tfos* nil "Another software used in tests.")
(defvar *soft* nil "Software used in tests.")
(defvar *gcd* nil "Holds the gcd software object.")
(defvar *binary-search* nil "Holds the binary_search software object.")

(define-constant +etc-dir+
    (append (pathname-directory
             #.(or *compile-file-truename*
                   *load-truename*
                   *default-pathname-defaults*))
            (list "test" "etc"))
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

(define-constant +unbound-fun-dir+ (append +etc-dir+ (list "unbound-fun"))
  :test #'equalp
  :documentation "Location of the unbound-fun example directory")

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

(define-constant +simple-macros-dir+ (append +etc-dir+ (list "simple-macros"))
  :test #'equalp
  :documentation "Path to the simple-macros example.")

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

(define-constant +java-dir+ (append +etc-dir+ (list "java" "non-instrumented"))
  :test #'equalp
  :documentation "Path to directory holding java.")

(define-constant +javascript-dir+ (append +etc-dir+ (list "javascript"))
  :test #'equalp
  :documentation "Path to directory holding Javascript test programs.")

(define-constant +asm-test-dir+ (append +etc-dir+ (list "asm-test"))
  :test #'equalp
  :documentation "Path to asm-test examples.")

(define-constant +software-dir+
    (append (butlast (butlast +etc-dir+)) (list "software"))
  :test #'equalp
  :documentation "Path to sel/software directory (software source components)")

(define-constant +coq-test-dir+ (append +etc-dir+ (list "coq"))
  :test #'equalp
  :documentation "Path to Coq test examples.")


;;; Functions on constants.
(defun gcd-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +gcd-dir+))

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

(defun unbound-fun-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +unbound-fun-dir+))

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

(defun simple-macros-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +simple-macros-dir+))

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

(defun java-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +java-dir+))

(defun asm-test-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +asm-test-dir+))

(defun software-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +software-dir+))

(defun coq-test-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +coq-test-dir+))

(defun javascript-dir (path)
  (merge-pathnames-as-file (make-pathname :directory +javascript-dir+)
                           path))


;;; Software.
(define-software soft (software)
  ((genome :initarg :genome :accessor genome :initform nil)))
(define-software clang-traceable (clang binary-traceable) ())
(define-software new-clang-traceable (new-clang binary-traceable) ())
(define-software java-traceable  (java sexp-traceable) ())
(define-software javascript-traceable  (javascript sexp-traceable) ())
(define-software javascript-traceable-project  (javascript-project sexp-traceable) ())
(define-software clang-control-picks (clang) ())
(define-software new-clang-control-picks (new-clang) ())
(define-software collect-traces-handles-directory-phenomes-mock
    (source binary-traceable)
  ((phenome-dir :initarg phenome-dir :accessor phenome-dir :initform nil
                :copier :direct)))
(define-software clang-styleable-test-class (clang styleable) ())
(define-software new-clang-styleable-test-class (new-clang styleable) ())
(define-software mutation-failure-tester (clang) ())

(defvar *soft-mutate-errors* nil
  "Control when mutations on soft objects throw errors.")
(defvar *good-asts* nil "Control pick-good")
(defvar *bad-asts* nil "Control pick-bad")


;;; Methods.
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

(defvar *test-mutation-count* 0)
(defmethod mutate ((soft mutation-failure-tester))
  (incf *test-mutation-count*)
  ;; Every other mutation fails
  (when (zerop (mod *test-mutation-count* 2))
    (error (make-condition 'mutate)))
  soft)


;;;
;;; Task support
;;;
(defclass child-task (task) ())
(defclass parent-task (task) ())
(defmethod task-job ((task parent-task) runner)
  (declare (ignore runner))
  (let ((index 0))
    (lambda ()
      (if (<= (incf index) 20)
          (make-instance 'child-task
            :object (format nil "~A-~D"
                            (task-object task) index))))))
(defmethod process-task ((task child-task) runner)
  (task-save-result runner (task-object task)) ;; save the object
  (sleep 1)) ;; sleep 1 second


;;; Fixtures
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

(defvar *range-ref* nil)
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
  (:setup (setf *gcd* (from-file (make-instance 'asm) (gcd-dir "gcd.s.intel"))))
  (:teardown (setf *gcd* nil)))

(defixture gcd-asm-heap-att
  (:setup
   (setf *gcd* (from-file (make-instance 'asm-heap)
                          (gcd-dir "gcd.s.att"))))
  (:teardown (setf *gcd* nil)))

(defixture gcd-asm-heap-intel
  (:setup
   (setf *gcd* (from-file (make-instance 'asm-heap)
                          (gcd-dir "gcd.s.intel"))))
  (:teardown (setf *gcd* nil)))

(defixture gcd-elf
  (:setup
   (let ((arch (intern (string-upcase (subseq (shell "uname -m") 0 3)))))
     (setf *gcd* (from-file (make-instance (case arch
                                             (x86 'elf-x86)
                                             (mips 'elf-mips)))
                            (gcd-dir "gcd")))))
  (:teardown (setf *gcd* nil)))

(defun fully-every (fn seq &rest other-seqs)
  "A version of EVERY that also checks that all sequences are
of the same length"
  (let ((len (length seq)))
    (and (every (lambda (s) (= (length s) len)) other-seqs)
         (apply #'every fn seq other-seqs))))

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

(defixture hello-world-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript-traceable)
                    (javascript-dir #P"hello-world/hello-world.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture fib-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript-traceable)
                    (javascript-dir #P"fib/fib.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture trivial-json
  (:setup
   (setf *soft*
         (from-file (make-instance 'json) (javascript-dir #P"trivial.json"))))
  (:teardown
   (setf *soft* nil)))

(let ((fib-path (merge-pathnames-as-file (javascript-dir #P"fib-project/")
                                         "fib.js"))
      (app-path (merge-pathnames-as-file (javascript-dir #P"fib-project/")
                                         "app.js"))
      fib-contents app-contents)
  (defixture fib-project-javascript
    (:setup
     (setf fib-contents (file-to-string fib-path)
           app-contents (file-to-string app-path)
           *soft*
           (from-file (make-instance 'javascript-traceable-project
                        :component-class 'javascript-traceable)
                      (javascript-dir #P"fib-project/"))))
    (:teardown
     (setf *soft* nil)
     (string-to-file fib-contents fib-path)
     (string-to-file app-contents app-path))))

(defixture csurf-asm-calc
  (:setup (setf *soft*
                (from-file
                 (make-instance 'csurf-asm)
                 (asm-test-dir "calc.s.intel"))))
  (:teardown
   (setf *soft* nil)))

(defixture task-runner
  (:setup (setf
           *soft*
           (list (run-task (make-instance 'parent-task :object "test1") 10)
                 (run-task (make-instance 'parent-task :object "test2") 20)))
          ;; wait for all the threads to terminate
          (mapcar 'bt:join-thread (task-runner-workers (first *soft*)))
          (mapcar 'bt:join-thread (task-runner-workers (second *soft*))))
  (:teardown
   (setf *soft* nil)))
