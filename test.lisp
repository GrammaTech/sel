;;; test.lisp --- tests for the `software-evolution-library' package
(defpackage :software-evolution-library/test
  (:nicknames :sel/test)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :metabang-bind
   :named-readtables
   :curry-compose-reader-macros
   :arrow-macros
   :iterate
   :split-sequence
   :cl-ppcre
   :cl-store
   :snooze
   :clack
   :drakma
   #+gt :testbot
   :trace-db
   :uiop
   ;; TODO: Remove those which aren't actually needed for testing.
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/command-line
   :software-evolution-library/command-line-rest
   :software-evolution-library/rest
   :software-evolution-library/components/instrument
   :software-evolution-library/components/clang-instrument
   :software-evolution-library/components/clang-tokens
   :software-evolution-library/components/condition-synthesis
   :software-evolution-library/components/fault-loc
   :software-evolution-library/components/fix-compilation
   :software-evolution-library/components/fodder-database
   :software-evolution-library/components/formatting
   :software-evolution-library/components/in-memory-fodder-database
   :software-evolution-library/components/java-instrument
   :software-evolution-library/components/javascript-instrument
   :software-evolution-library/components/json-fodder-database
   :software-evolution-library/components/lexicase
   :software-evolution-library/components/multi-objective
   :software-evolution-library/components/pliny-fodder-database
   :software-evolution-library/components/searchable
   :software-evolution-library/components/serapi-io
   :software-evolution-library/components/test-suite
   :software-evolution-library/components/traceable
   :software-evolution-library/software/adaptive-mutation
   :software-evolution-library/software/ancestral
   :software-evolution-library/software/asm
   :software-evolution-library/software/asm-heap
   :software-evolution-library/software/super-mutant
   :software-evolution-library/software/asm-super-mutant
   :software-evolution-library/software/super-mutant-clang
   :software-evolution-library/software/super-mutant-project
   :software-evolution-library/software/ast
   :software-evolution-library/software/cil
   :software-evolution-library/software/clang
   :software-evolution-library/software/new-clang
   :software-evolution-library/software/clang-expression
   :software-evolution-library/software/clang-project
   :software-evolution-library/software/clang-w-fodder
   :software-evolution-library/software/coq
   :software-evolution-library/software/coq-project
   :software-evolution-library/software/csurf-asm
   :software-evolution-library/software/diff
   :software-evolution-library/software/elf
   :software-evolution-library/software/elf-cisc
   :software-evolution-library/software/elf-risc
   :software-evolution-library/software-evolution-library
   :software-evolution-library/software/expression
   :software-evolution-library/software/forth
   :software-evolution-library/software/java
   :software-evolution-library/software/java-project
   :software-evolution-library/software/javascript
   :software-evolution-library/software/javascript-project
   :software-evolution-library/software/json
   :software-evolution-library/software/sexp
   :software-evolution-library/software/lisp
   :software-evolution-library/software/llvm
   :software-evolution-library/software/parseable
   :software-evolution-library/software/project
   :software-evolution-library/software/simple
   :software-evolution-library/software/source
   :software-evolution-library/software/styleable
   :software-evolution-library/software/with-exe
   :software-evolution-library/stefil-plus)
  (:import-from :hunchentoot)
  (:import-from :osicat :file-permissions :pathname-as-directory)
  ;; Shadow DEFTEST for control of clang/new-clang in tests
  (:shadow :deftest)
  (:shadowing-import-from :common-lisp :type)
  (:shadowing-import-from :software-evolution-library :size)
  (:shadowing-import-from :clack :stop)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:shadowing-import-from :uiop :getenv :quit :parameter-error)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:export :test :batch-test :testbot-test))
(in-package :software-evolution-library/test)
(named-readtables:in-readtable :sel-readtable)

(defvar *this-file*
  #.(or *compile-file-truename*
        *load-truename*
        *default-pathname-defaults*))

#-gt
(progn
  ;;; External replacement for GT-specific test submission helpers
  (defvar *success* nil "Variable indicating test success or failure.")
  (defun batch-test (test project branch &optional args)
    "Run tests in 'batch' mode, printing results as a string."
    (declare (ignorable project branch args))

    (let* ((stefil::*test-progress-print-right-margin* (expt 2 20))
           (failures (coerce (stefil::failure-descriptions-of
                              (without-debugging (funcall test)))
                             'list)))
      (setf *success*
            (if failures
                (prog1 nil
                  (format *error-output* "FAILURES~%")
                  (mapc [{format *error-output* "  ~a~%"}
                         #'stefil::name-of
                         #'stefil::test-of
                         #'car #'stefil::test-context-backtrace-of]
                        failures))
                (prog1 t
                  (format *error-output* "SUCCESS~%")))))))

(defroot test)                          ; The root test suite.

;; Disable clang-format and any other helpers
(defmacro every-is (function &rest lists)
  (let ((args-sym (gensym "args")))
    `(mapc (lambda (&rest ,args-sym)
             (is (apply ,function ,args-sym)))
           ,@lists)))

(defun run-batch (&rest a)
  (declare (ignorable a))
  #+ccl (setf ccl::*interactive-streams-initialized* nil)
  (batch-test #'test "SEL" +software-evolution-library-branch+))

(defvar *genome*      nil "Genome used in tests.")
(defvar *soft*        nil "Software used in tests.")
(defvar *base*        nil "Software used in diff/merge tests.")
(defvar *left*        nil "Software used in diff/merge tests.")
(defvar *right*       nil "Software used in diff/merge tests.")
(defvar *tfos*        nil "Another software used in tests.")
(defvar *gcd*         nil "Holds the gcd software object.")
(defvar *gcd-trace-path* nil "Holds the file of gcd traces.")
(defvar *binary-search* nil "Holds the binary_search software object.")
(defvar *empty-while* nil "Holds the empty-while software object.")
(defvar *headers*     nil "Holds the headers software object.")
(defvar *hello-world* nil "Holds the hello world software object.")
(defvar *sqrt*        nil "Holds the hello world software object.")
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
(defvar *java-file-name* nil "File name to be tested in a test case.")
(defvar *coq*         nil "Coq software object.")
(defvar *clack-port*  9003 "Default port for clack web server instance.")
(defvar *clack* nil "Web server instance used in tests.")
(defvar *rest-client*  nil "Client-id (cid) for REST API test client.")
(defvar *clack-delay* 0.5 "Seconds to delay after starting server")

(defun initialize-clack ()
  (let ((tries 0))
    (handler-bind
        ((usocket:socket-error
          (lambda (e)
            (warn "Starting web server on ~a failed with ~a" *clack-port* e)
            (if (< tries 20)
                (progn (incf tries)
                       (invoke-restart 'try-a-new-port))
                (error e)))))
      (restart-case
          (prog1
              ;; Inhibit the clack "Hunchentoot server is started." messages.
              (let ((*standard-output* (make-broadcast-stream)))
                (clack:clackup (snooze:make-clack-app) :port *clack-port*))
            ;; Wait for a second before continuing, to ensure the server is up.
	    (sleep *clack-delay*))
        (try-a-new-port ()
          :report "Try using a new port"
          (incf *clack-port*)
          (prog1 (clack:clackup (snooze:make-clack-app)
				:port *clack-port*)
	    (sleep *clack-delay*)))))))

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

(define-constant +maven-prj-dir+ (append +java-dir+ (list "SimpleMaven"))
  :test #'equalp
  :documentation "Path to directory holding the SimpleMaven java project.")

(define-constant +java-jars-dir+ (append +java-dir+ (list "Jars"))
  :test #'equalp
  :documentation "Path to directory holding the Build Folder jars.")

(define-constant +javascript-dir+ (append +etc-dir+ (list "javascript"))
  :test #'equalp
  :documentation "Path to directory holding Javascript test programs.")

(define-constant +asm-test-dir+ (append +etc-dir+ (list "asm-test"))
  :test #'equalp
  :documentation "Path to asm-test examples.")

(define-constant +software-dir+
    (append (butlast (butlast sel/test::+etc-dir+)) (list "software"))
  :test #'equalp
  :documentation "Path to sel/software directory (software source components)")

(define-constant +coq-test-dir+ (append +etc-dir+ (list "coq"))
  :test #'equalp
  :documentation "Path to Coq test examples.")

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

(defmethod good-stmts ((obj clang-control-picks))
  (or *good-asts* (stmt-asts obj)))
(defmethod bad-stmts ((obj clang-control-picks))
  (or *bad-asts* (stmt-asts obj)))

(defmethod good-stmts ((obj new-clang-control-picks))
  (or *good-asts* (stmt-asts obj)))
(defmethod bad-stmts ((obj new-clang-control-picks))
  (or *bad-asts* (stmt-asts obj)))

(defvar *test-mutation-count* 0)
(defmethod mutate ((soft mutation-failure-tester))
  (incf *test-mutation-count*)
  ;; Every other mutation fails
  (when (zerop (mod *test-mutation-count* 2))
    (error (make-condition 'mutate)))
  soft)

;;;
;;; Clang/NewClang migration support
;;;

;;; Shadow DEFTEST from stefil-plus to add
;;; controllable binding of the clang front end

(defun call-with-clang-versions (fn)
  (if *new-clang?*
      (prog2
          (setf *make-statement-fn* #'make-statement-new-clang)
        (funcall fn))
      (prog2
          (setf *make-statement-fn* #'make-statement*)
          (funcall fn))))

(defmacro deftest (name args &body body)
  `(sel/stefil+:deftest ,name ,args
     (call-with-clang-versions (lambda () ,@body))))

(defun make-clang (&rest key-args)
  (apply #'make-instance (if *new-clang?* 'new-clang 'clang) key-args))

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

(defixture rest-server
  (:setup (unless *clack* (setf *clack* (initialize-clack))))
  (:teardown (clack:stop *clack*)(setf *clack* nil)(setf *rest-client* nil)))

(let (old-standard-out old-error-out)
  (defixture fact-rest-server
    (:setup
     (setf old-standard-out *standard-output*
           old-error-out *error-output*
           *standard-output* (make-broadcast-stream)
           *error-output* (make-broadcast-stream))
     (define-command-rest (fact-entry :environment (*population*))
         ((n integer) &spec +common-command-line-options+)
       "Test that canonical REST endpoints work. Computes factorial."
       #.(format nil
                 "~%Built from SEL ~a, and ~a ~a.~%"
                 +software-evolution-library-version+
                 (lisp-implementation-type) (lisp-implementation-version))
       (declare (ignorable quiet verbose))
       (if help
           (let ((*standard-output* (make-broadcast-stream)))
             (show-help-for-fact-entry))
           (factorial n)))
     (unless *clack* (setf *clack* (initialize-clack))))
    (:teardown
     (clack:stop *clack*)
     (setf *clack* nil
           *rest-client* nil
           *standard-output* old-standard-out
           *error-output* old-error-out))))

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
          (make-clang
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
         (from-file (make-clang :compiler "clang")
                    (gcd-dir "gcd.c"))))
  (:teardown
   (setf *gcd* nil)))

(defixture gcd-wo-curlies-clang
  (:setup
   (setf *gcd*
         (from-file (make-clang :compiler "clang")
                    (gcd-dir "gcd-wo-curlies.c"))))
  (:teardown
   (setf *gcd* nil)))

(defixture headers-clang
  (:setup
   (setf *headers*
         (from-file (make-clang
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
         (from-file (make-clang :compiler "clang"
                                :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defixture sqrt-clang
  (:setup
   (setf *sqrt*
         (from-file (make-clang)
                    (make-pathname :name "sqrt"
                                   :type "c"
                                   :directory +etc-dir+))))
  (:teardown
   (setf *sqrt* nil)))

(defixture print-env-clang
  (:setup (setf *soft*
                (from-file (make-clang :compiler "clang")
                           (make-pathname :directory +etc-dir+
                                          :name "print-env"
                                          :type "c"))))
  (:teardown (setf *soft* nil)))

(defun fully-every (fn seq &rest other-seqs)
  "A version of EVERY that also checks that all sequences are
of the same length"
  (let ((len (length seq)))
    (and (every (lambda (s) (= (length s) len)) other-seqs)
         (apply #'every fn seq other-seqs))))

(defun make-clang-type* (&rest args)
  (if *new-clang?*
      (apply #'make-new-clang-type* args)
      (apply #'make-clang-type :allow-other-keys t args)))

(defun make-new-clang-type* (&key array hash i-file pointer reqs name
                               decl const volatile restrict storage-class
                               qual (desugared nil desugared-p)
                               typedef
                               &allow-other-keys)
  ;; TODO:  HASH DECL REQS TYPEDEF
  (declare (ignorable hash decl reqs typedef))
  (let ((type (make-instance 'new-clang-type
                :qual qual
                :desugared (if desugared-p desugared qual)
                :typedef typedef)))
    (make-instance 'nct+ :type type
                   :storage-class storage-class
                   :modifiers (sel/sw/new-clang::pack-nct+-modifiers
                               pointer const volatile restrict)
                   :array array
                   :name name
                   :i-file i-file
                   :compute-slots nil)))

(defun make-clang-control-picks (&rest args)
  (apply #'make-instance
         (if *new-clang?* 'new-clang-control-picks 'clang-control-picks)
         :allow-other-keys t
         args))

(defixture hello-world-clang-control-picks
  (:setup
   (setf *hello-world*
         (from-file (make-clang-control-picks :compiler "clang-3.7"
                                              :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defixture empty-function-body-crossover-bug-clang
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang"
                                :flags '("-g -m32 -O0"))
                    (clang-crossover-dir
                     "empty-function-body-crossover-bug.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture select-intraprocedural-pair-non-null-clang
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang"
                                :flags '("-g -m32 -O0"))
                    (clang-crossover-dir
                     "select-intraprocedural-pair-non-null.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture intraprocedural-2pt-crossover-bug-clang
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang"
                                :flags '("-g -m32 -O0"))
                    (clang-crossover-dir
                     "intraprocedural-2pt-crossover-bug.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture no-mutation-targets-clang
  (:setup
   (setf *soft* (from-file (make-clang)
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
                           (lisp-bugs-dir
                            "no-insert-fodder-decl-mutation-targets.c"))))
  (:teardown
   (setf *database* nil)
   (setf *soft* nil)))

(defixture contexts
  (:setup
   (setf *contexts*
         (from-file (make-clang :compiler "clang-3.7")
                    (contexts-dir "contexts.c"))))
  (:teardown
   (setf *contexts* nil)))

(defixture typedef
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang-3.7")
                    (typedef-dir "typedef.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture cpp-strings
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang++")
                    (strings-dir "cpp-strings.cpp"))))
  (:teardown
   (setf *soft* nil)))

(defixture c-strings
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (strings-dir "c-strings.c"))))
  (:teardown
   (setf *soft* nil)))

(defun inject-missing-swap-macro (obj)
  ;; Inject a macro that clang-mutate currently misses, then force the ASTs to
  ;; be recalculated by setting the genome-string.
  (->> (make-clang-macro
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
         (with-open-file (in (make-pathname :name "euler-example"
                                            :type "json"
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
         (from-file (make-clang)
                    (coalesce-while-loop-dir "empty-while.c"))))
  (:teardown
   (setf *empty-while* nil)))

(defixture while-with-no-precedent-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (coalesce-while-loop-dir "no-precedent.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture huf-clang
  (:setup
   (setf *huf*
         (from-file (make-clang :compiler "gcc" :flags '("-g -m32 -O0"))
                    (huf-dir "huf.c")))
   (inject-missing-swap-macro *huf*))
  (:teardown
   (setf *huf* nil)))

(defixture nested-clang
  (:setup
   (setf *nested*
         (from-file (make-clang :compiler "gcc" :flags '("-g -m32 -O0"))
                    (nested-dir "nested.c")))
   (inject-missing-swap-macro *nested*))
  (:teardown
   (setf *nested* nil)))

(defixture scopes-clang
  (:setup
   (setf *scopes*
         (from-file (make-clang-control-picks
                     :compiler "clang" :flags '("-g -m32 -O0"))
                    (scopes-dir "scopes.c"))))
  (:teardown
   (setf *scopes* nil)))

(defixture scopes2-clang
  (:setup
   (setf *scopes*
         (from-file (make-clang-control-picks
                     :compiler "clang" :flags '("-g -m32 -O0"))
                    (scopes-dir "scopes2.c"))))
  (:teardown
   (setf *scopes* nil)))

(defixture scopes-type-field-clang
  (:setup
   (setf *scopes*
         (from-file (make-clang
                     :compiler "clang" :flags '("-g -m32 -O0"))
                    (scopes-dir "scopes-type-field.c"))))
  (:teardown
   (setf *scopes* nil)))

(defixture scopes-cxx-clang
  (:setup
   (setf *scopes*
         (from-file (make-clang-control-picks :compiler "clang")
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
         (from-file (make-clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g" "-c"))
                    (collatz-dir "collatz.c"))))
  (:teardown
   (setf *collatz* nil)))

(defixture fib-clang
  (:setup
   (setf *fib*
         (from-file (make-clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g" "-c"))
                    (fib-dir "fib.c"))))
  (:teardown
   (setf *fib* nil)))

(defixture crossover-no-compound-stmt-clang
  (:setup
   (setf *soft*
         (from-file (make-clang
                      :compiler "clang"
                      :flags '("-m32" "-O0" "-g"))
                    (clang-crossover-dir
                     "crossover-no-compound-stmt.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture crossover-switch-stmt-clang
  (:setup
   (setf *soft*
         (from-file (make-clang
                      :compiler "clang"
                      :flags '("-m32" "-O0" "-g"))
                    (clang-crossover-dir
                     "crossover-switch-stmt.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture tidy-adds-braces-clang
  (:setup
   (setf *soft*
         (from-file (make-clang
                      :compiler "clang"
                      :flags '("-m32" "-O0" "-g"))
                    (clang-tidy-dir "tidy-adds-braces.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture type-of-var-clang
  (:setup
   (setf *soft*
         (from-file (make-clang
                      :compiler "clang"
                      :flags '("-m32" "-O0" "-g"))
                    (type-of-var-dir "type-of-var.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture type-of-var-missing-decl-type-clang
  (:setup
   (setf *soft*
         (from-file (make-clang
                      :compiler "clang"
                      :flags '("-m32" "-O0" "-g"))
                    (type-of-var-dir "missing-decl-type.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture variety-clang
  (:setup
   (setf *variety*
         (from-file (make-clang
                      :compiler "clang"
                      :flags '("-m32" "-O0" "-g"))
                    (variety-dir "variety.c"))))
  (:teardown
   (setf *variety* nil)))

(defixture shadow-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (shadow-dir "shadow.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture unicode-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (unicode-dir "unicode.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture fl-tiny-clang
  (:setup (setf *soft*
                (from-file (make-clang)
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


(defixture switch-macros-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (switch-macros-dir "switch-macros.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture simple-macros-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (simple-macros-dir "simple-macros.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture assert-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (assert-dir "assert.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture long-running-program-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (long-running-program-dir "long-running-program.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture typedef-type-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (typedef-type-dir "typedef-type.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture unbound-fun-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (unbound-fun-dir "unbound-fun.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture general-fixture-java
  (:setup
   (setf *soft*
         (from-file (make-instance 'java)
                    (java-dir (concatenate 'string
                                *java-file-name* ".java")))))
  (:teardown
   (setf *soft* nil)))

(defixture general-fixture-java-traceable
  (:setup
   (setf *soft*
         (from-file (make-instance 'java-traceable)
                    (java-dir (concatenate 'string
                                *java-file-name* ".java")))))
  (:teardown
   (setf *soft* nil)))

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


;;; ASM representation.
(defsuite asm-tests "ASM representation.")

(deftest simple-read ()
  (with-fixture gcd-asm
    (is (equal 'asm (type-of *gcd*)))))

(deftest (idempotent-read-write :long-running) ()
  (let ((a (temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file *gcd* a)
           (multiple-value-bind (out err ret)
               (shell "diff ~s ~a"
		      (namestring (gcd-dir "gcd.s.intel")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest idempotent-copy ()
  (with-fixture gcd-asm
    (is (equal-it *gcd* (copy *gcd*) nil '(oid)))))

(deftest (idempotent-read-copy-write :long-running) ()
  (let ((a (temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file (copy *gcd*) a)
           (multiple-value-bind (out err ret)
               (shell "diff ~s ~a" (namestring (gcd-dir "gcd.s.intel")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest edit-of-copy-does-not-change-original ()
  (with-fixture gcd-asm
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (is (with-retries (100)
            ;; Multiple tries to apply a mutation creating a
            ;; difference.  Stochastically some might result in the
            ;; same genome, e.g. by swapping to identical
            ;; instructions.
            (handler-bind
                ((no-mutation-targets
                  (lambda (c)
                    (declare (ignorable c))
                    (invoke-restart 'try-another-mutation))))
              (mutate ant))
            (when (not (equal-it (genome ant) (genome *gcd*)))
              (return t)))
          "In 100 tries, a mutation results in a different mutated genome.")
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

(deftest (asm-can-form-a-phenome :long-running) ()
  (with-fixture gcd-asm
    (with-temp-file (bin)
      (ignore-phenome-errors
       (is (phenome *gcd* :bin bin)
           "Phenome works on an ASM software object.")
       (is (probe-file bin)
           "Phenome creates the binary file for an ASM software object.")
       (is (nth-value 2 (shell "~a 1 1" bin))
           "Phenome creates a runnable binary for an ASM software object.")))))

(deftest homologous-crossover-same-same ()
  (with-fixture gcd-asm
    (is (tree-equal (genome *gcd*)
                    (genome (homologous-crossover *gcd* (copy *gcd*)))))))

(deftest homologous-crossover-with-cut ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*))
          (target 40))
      ;; apply cut to variant
      (apply-mutation variant
		      (make-instance 'simple-cut
				     :object variant
				     :targets target))
      (multiple-value-bind (crossed cross-a cross-b)
          (homologous-crossover *gcd* variant)
        (declare (ignorable cross-a cross-b))
        ;; If copied before cut, size is 1 smaller. If after it's the
        ;; same.  If equal it could go either way
        (cond
          ((< cross-b target) (is (= (1- (size *gcd*)) (size crossed))))
          ((= cross-b target) (is (or (= (1- (size *gcd*)) (size crossed))
                                      (= (size *gcd*) (size crossed)))))
          ((> cross-b target) (is (= (size *gcd*) (size crossed)))))))))

(deftest homologous-crossover-with-insert ()
  ;; NOTE: If crossover changes, this test may fail sporadically: behavior
  ;; depends on whether crossover target is before or after insert-pt.
  ;; TODO: This test documents a shortcoming in the crossover impl that we
  ;; want to fix. If the cross-point in A refers to an index that was previouly
  ;; copied from another part of A, we will select cross-b as the source of the
  ;; copied statement (which might not be close to the ideal point for a
  ;; homologous crossover.
  (with-fixture gcd-asm
    (let ((insert-pt 40)
          (stmt-to-insert 60)
          (variant (copy *gcd*)))
      (apply-mutation variant
		      (make-instance 'simple-insert
				     :object variant
				     :targets (list insert-pt stmt-to-insert)))
      (multiple-value-bind (crossed cross-a cross-b)
          (homologous-crossover variant *gcd*)
        (declare (ignorable cross-b))
        (cond
          ((< cross-a insert-pt)
           (is (= (size crossed) (size *gcd*))))
          ((= cross-a insert-pt)
           (is (= (size crossed)
                  (- (size *gcd*) (- stmt-to-insert insert-pt)))))
          (t (is (= (size crossed) (1+ (size *gcd*))))))))))

(deftest homologous-crossover-with-swap ()
  ;; NOTE: If crossover changes, this test may fail sporadically: behavior
  ;; depends on whether crossover target is exactly on a swap point or not.
  ;; TODO: This test documents a shortcoming in the crossover impl that we
  ;; want to fix. If the cross-point in A refers to an index that was previouly
  ;; swapped in B, we will select cross-b as that new location.
  (with-fixture gcd-asm
    (let ((targets (list 20 60))
          (variant (copy *gcd*)))
      (apply-mutation
       variant
       (make-instance 'simple-swap :object variant :targets targets))
      (multiple-value-bind (crossed cross-a cross-b)
          (homologous-crossover *gcd* variant)
        (declare (ignorable cross-b))
        (cond
          ((= cross-a 20)
           (is (= (size crossed) (- (size *gcd*) 40))))
          ((= cross-a 60)
           (is (= (size crossed) (+ (size *gcd*) 40))))
          (t (is (= (size crossed) (size *gcd*)))))))))



;;; ASM-HEAP representation.

(deftest simple-asm-heap-read-intel ()
  (with-fixture gcd-asm-heap-intel
    (is (equal 'asm-heap (type-of *gcd*)))
    (is (equal (asm-syntax *gcd*) ':intel))))

(deftest simple-asm-heap-read-att ()
  (with-fixture gcd-asm-heap-att
    (is (equal 'asm-heap (type-of *gcd*)))
    (is (equal (asm-syntax *gcd*) ':att))))

(defun idempotent-asm-heap-read-write (filename)
  (let ((a (temp-file-name)))
    (unwind-protect
	 (to-file *gcd* a)
      (multiple-value-bind (out err ret)
	  (shell "diff ~s ~a"
		 (namestring (gcd-dir filename)) a)
	(declare (ignorable out err))
	(is (= 0 ret)))
      (delete-file a))))

(deftest (idempotent-asm-heap-read-write-intel :long-running) ()
  (with-fixture gcd-asm-heap-intel
    (idempotent-asm-heap-read-write "gcd.s.intel")))

(deftest (idempotent-asm-heap-read-write-att :long-running) ()
  (with-fixture gcd-asm-heap-att
    (idempotent-asm-heap-read-write "gcd.s.att")))

(deftest (idempotent-asm-heap-copy-intel :long-running) ()
  (with-fixture gcd-asm-heap-intel
    (is (equal-it *gcd* (copy *gcd*) nil '(oid)))))

(deftest (idempotent-asm-heap-copy-att :long-running) ()
  (with-fixture gcd-asm-heap-att
    (is (equal-it *gcd* (copy *gcd*) nil '(oid)))))

(defun idempotent-asm-heap-read-copy-write (filename)
  (let ((a (temp-file-name)))
    (unwind-protect
	 (to-file (copy *gcd*) a)
      (multiple-value-bind (out err ret)
	  (shell "diff ~s ~a" (namestring (gcd-dir filename)) a)
	(declare (ignorable out err))
	(is (= 0 ret)))
      (delete-file a))))

(deftest (idempotent-asm-heap-read-copy-write-intel :long-running) ()
  (with-fixture gcd-asm-heap-intel
    (idempotent-asm-heap-read-write "gcd.s.intel")))

(deftest (idempotent-asm-heap-read-copy-write-att :long-running) ()
  (with-fixture gcd-asm-heap-att
    (idempotent-asm-heap-read-write "gcd.s.att")))

(defun edit-of-asm-heap-copy-does-not-change-original ()
  (let ((orig-hash (sxhash (genome *gcd*)))
	(variant (copy *gcd*)))
    (is (with-retries (100)
          ;; Multiple tries to apply a mutation creating a difference.
          ;; Stochastically some might result in the same genome,
          ;; e.g. by swapping to identical instructions.
          (handler-bind
              ((no-mutation-targets
                (lambda (c)
                  (declare (ignorable c))
                  (invoke-restart 'try-another-mutation))))
            (mutate variant))
          (when (not (equal-it (genome variant) (genome *gcd*)))
            (return t)))
	"In 100 tries, a mutation results in a different mutated genome.")
    (is (equal orig-hash (sxhash (genome *gcd*))))))

(deftest edit-of-asm-heap-copy-does-not-change-original-intel ()
  (with-fixture gcd-asm-heap-intel
    (edit-of-asm-heap-copy-does-not-change-original)))

(deftest edit-of-asm-heap-copy-does-not-change-original-att ()
  (with-fixture gcd-asm-heap-intel
    (edit-of-asm-heap-copy-does-not-change-original)))

(defun asm-heap-cut-actually-shortens ()
  (let ((variant (copy *gcd*)))
      (apply-mutation variant (make-instance 'simple-cut :targets 4))
      (is (< (length (genome variant)) (length (genome *gcd*))))))

(deftest asm-heap-cut-actually-shortens-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-cut-actually-shortens)))

(deftest asm-heap-cut-actually-shortens-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-cut-actually-shortens)))

(defun asm-heap-insertion-actually-lengthens ()
  (let ((variant (copy *gcd*)))
    (apply-mutation variant (make-instance 'simple-insert
					   :targets (list 4 8)))
    (is (> (length (genome variant)) (length (genome *gcd*))))))

(deftest asm-heap-insertion-actually-lengthens-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-insertion-actually-lengthens)))

(deftest asm-heap-insertion-actually-lengthens-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-insertion-actually-lengthens)))

;;;
;;; Note that these operands chosen (movsd, call) are
;;; both in common with both intel and att syntax (so work for both).
;;;
(defun asm-heap-swap-maintains-length ()
  (let ((variant (copy *gcd*))
	(mutation
	 (make-instance 'simple-swap
			:targets (list
				  (position-if
				   [{equalp "movsd"} #'asm-line-info-opcode]
				   (genome *gcd*))
				  (position-if
				   [{equalp "call"} #'asm-line-info-opcode]
				   (genome *gcd*))))))
    (setf variant (apply-mutation variant mutation))
    (is (not (equalp (genome variant) (genome *gcd*))))
    (is (= (length (genome variant)) (length (genome *gcd*))))))

(deftest asm-heap-swap-maintains-length-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-swap-maintains-length)))

(deftest asm-heap-swap-maintains-length-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-swap-maintains-length)))

(defun asm-heap-replace-operand-maintains-length ()
  (let ((variant (copy *gcd*)))
    (apply-mutation variant (make-instance 'asm-replace-operand
					   :targets (list 16 17)))
    (is (not (tree-equal (genome variant) (genome *gcd*) :test #'tree-equal)))
    (is (= (length (genome variant)) (length (genome *gcd*))))))

(deftest asm-heap-replace-operand-maintains-length-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-replace-operand-maintains-length)))

(deftest asm-heap-replace-operand-maintains-length-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-replace-operand-maintains-length)))

(defun asm-heap-replace-operand-changes-operand ()
  (let ((variant (copy *gcd*)))
    ;; need a deep copy of the heap
    (setf (lines variant)(lines *gcd*))
    (apply-mutation variant (make-instance 'asm-replace-operand
					   :targets (list 16 17)))
    (is (not (equalp (asm-line-info-operands (elt (genome variant) 16))
		     (asm-line-info-operands (elt (genome *gcd*) 16)))))))

(deftest asm-heap-replace-operand-changes-operand-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-replace-operand-changes-operand)))

(deftest asm-heap-replace-operand-changes-operand-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-replace-operand-changes-operand)))

(defun asm-heap-simple-crossover-test ()
  (let ((variant (copy *gcd*)))
    (apply-mutation variant (make-instance 'simple-cut :targets 0))
    (let ((new (crossover variant *gcd*)))
      (is (not (tree-equal (genome new) (genome *gcd*) :test #'tree-equal))))))

(deftest asm-heap-simple-crossover-test-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-simple-crossover-test)))

(deftest asm-heap-simple-crossover-test-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-simple-crossover-test)))

(defun asm-heap-can-form-a-phenome ()
  (with-temp-file (bin)
    (ignore-phenome-errors
     (is (phenome *gcd* :bin bin)
	 "Phenome works on an ASM-HEAP software object.")
     (is (probe-file bin)
	 "Phenome creates the binary file for an ASM-HEAP software object.")
     (is (nth-value 2 (shell "~a 1 1" bin))
       "Phenome creates a runnable binary for an ASM-HEAP software object."))))

(deftest (asm-heap-can-form-a-phenome-intel :long-running) ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-can-form-a-phenome)))

(deftest (asm-heap-can-form-a-phenome-att :long-running) ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-can-form-a-phenome)))

(defun asm-heap-homologous-crossover-same-same ()
  (is (tree-equal (genome *gcd*)
		  (genome (homologous-crossover *gcd* (copy *gcd*)))
		  :test 'equalp)))

(deftest asm-heap-homologous-crossover-same-same-intel ()
  (with-fixture gcd-asm-heap-intel
    (asm-heap-homologous-crossover-same-same)))

(deftest asm-heap-homologous-crossover-same-same-att ()
  (with-fixture gcd-asm-heap-att
    (asm-heap-homologous-crossover-same-same)))


;;; ASM-SUPER-MUTANT representation.
(defsuite asm-super-mutant-tests
    "ASM-SUPER-MUTANT representation."
  ;; Only run these tests if we found libpapi.so.
  *lib-papi*)

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

;;; Command Line tests
(defsuite cl-tests "Command Line tool tests.")

(define-command fact-cl-entry
    (n &spec +common-command-line-options+)
  "Test that canonical REST endpoints work. Computes factorial."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (if help
      (show-help-for-fact-cl-entry)
      (factorial n)))

(deftest run-factorial-cl-func ()
  (let ((*standard-output* (make-broadcast-stream)))
    (is (eql (fact-cl-entry 5 :verbose 3) 120))
    #|(is (eql (fact-cl-entry 52235215 :help T) nil))|#))


;;; REST tests
(defsuite rest-tests "REST API tests.")

(defun rest-test-create-client ()
  "Returns 2 values: new client id or nil, and status.
 Assumes service is running."
  (let* ((params '(("max-population-size" . "1024")))
	   (result nil))
      (multiple-value-bind (stream status)
	  (drakma:http-request
	   (format nil "http://127.0.0.1:~D/client" *clack-port*)
	   :method :post
	   :content-type "application/json"
	   :content (json:encode-json-to-string params)
	   :want-stream t)
	(if (= status 200)
	    (setf result (read stream)))
	(if (symbolp result)
	    (setf result (symbol-name result)))
	(values result status))))

(defun rest-test-create-software (type cid)
  "Given type of Software object and client-id, returns 2
 values: new software oid or nil, and status.
 Assumes service is running."
  (let* ((path (namestring (hello-world-dir "hello_world.c")))
	 (params `(("path" . ,path)
		   ("compiler" . "clang")
		   ("flags" . ,(list "-I" (namestring
					   (make-pathname
					    :directory +headers-dir+))))))
	 (result nil))
    (multiple-value-bind (stream status)
	(drakma:http-request
	 (format nil "http://127.0.0.1:~D/soft?cid=~A&type=~A"
		 *clack-port* cid type)
	 :method :post
	 :content-type "application/json"
	 :content (json:encode-json-to-string params)
	 :want-stream t)
      (if (= status 200)
	  (setf result (read stream)))
      (if (symbolp result)
	  (setf result (symbol-name result)))

      (values result status))))

(defun rest-test-get-new-client ()
  "Always creates a new REST client and returns (1) new client id,
 (2) http status code. The new client id is stored in *rest-client*."
  (multiple-value-bind (cid status)
      (rest-test-create-client)
    (setf *rest-client* cid) ; store the new client for further tests
    (values cid status)))

(defun rest-test-get-client ()
  "If REST client already has been created, return it.
 Else, create one and return the client id (cid)."
  (or *rest-client* (rest-test-get-new-client)))

(deftest (rest-create-client :long-running) ()
  ;; test ensures the web service is running and it can create a new client,
  ;; tests Create Client (HTTP POST) method.
  (with-fixture rest-server
    (multiple-value-bind (cid status) (rest-test-get-new-client)
      (is (eql status 200))
      (is (stringp cid))
      (is (string-equal (subseq cid 0 7) "client-")))))

(deftest (rest-create-software :long-running) ()
  ;; test ensures the web service is running and it can create a new software
  ;; object. Tests Create Software (HTTP POST) method.
  (with-fixture rest-server
    (let ((cid (rest-test-get-client)))
      (multiple-value-bind (oid status)
	  (rest-test-create-software
           "SOFTWARE-EVOLUTION-LIBRARY/SOFTWARE/CLANG:CLANG" cid)
        (is (eql status 200))
        (is (integerp oid))))))

(define-async-job four-types-1
    ((a integer) (b string) (c float) (d boolean))
  "Test that the four supported types can be passed via REST."
  (format nil "~A: ~D, ~A: ~S, ~A: ~F, ~A: ~A"
          (type-of a) a
          (type-of b) b
          (type-of c) c
          (type-of d) d))

(deftest run-async-job-func ()
  (let ((result
         (sel/rest:apply-async-job-func 'four-types-1 10 "twenty" 30.1 t)))
    (is (search "10" result))
    (is (search "\"twenty\"" result))
    (is (search "30.1" result))
    (is (search " T" result))))

(define-command-rest (four-types-2)
    ((a integer) (b string) (c float) (d boolean)
     &spec +common-command-line-options+)
  "Test that the four supported types can be passed to an endpoint via REST."
  ""
  (declare (ignorable help quiet verbose load eval out-dir read-seed))
  (format nil "~A: ~D, ~A: ~S, ~A: ~F, ~A: ~A"
          (type-of a) a
          (type-of b) b
          (type-of c) c
          (type-of d) d))

(deftest run-rest-command-line-func ()
  (let ((result
         (four-types-2 10 "twenty" 30.1 t)))
    (is (search "10" result))
    (is (search "\"twenty\"" result))
    (is (search "30.1" result))
    (is (search " T" result))))

(define-command-rest (fact-entry-cl)
    ((n integer) &spec +common-command-line-options+)
  "Test that canonical REST endpoints work. Computes factorial."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (if help
      (show-help-for-fact-entry-cl)
      (factorial n)))

(deftest (run-rest-factorial-cl-func :long-running) ()
  (let ((*standard-output* (make-broadcast-stream)))
    (is (eql (fact-entry-cl 5 :verbose 3) 120))
    (is (eql (fact-entry-cl 52235215 :help T) nil))))

(deftest (run-rest-factorial-cl-func-2 :long-running) ()
  (with-fixture fact-rest-server
    (let ((*standard-output* (make-broadcast-stream)))
      ;; This produces a warning that FACT-ENTRY is undefined,
      ;; but the function is defined by the fixture
      (is (eql (funcall (symbol-function 'fact-entry) 5 :verbose 3) 120))
      (is (eql (funcall (symbol-function 'fact-entry) 52235215 :help T) nil)))))

(defun rest-endpoint-test-create-fact-job (client-id json-input)
  "Returns new job name or nil.
 Assumes service is running."
  (multiple-value-bind (stream status)
      (drakma:http-request
       (format nil "http://127.0.0.1:~D/rest-fact-entry?cid=~A"
               *clack-port* client-id)
       :method :post
       :content-type "application/json"
       :content (json:encode-json-to-string json-input)
       :want-stream t)
    (if (= status 200)
        (read stream)
        (format nil "Status code ~A" status))))

(deftest (run-rest-factorial-remote-1 :long-running) ()
  (with-fixture fact-rest-server
    (multiple-value-bind (cid status) (rest-test-get-new-client)
      (declare (ignore status))
      (let ((*standard-output* (make-broadcast-stream))
            (result
             (symbol-name
              (rest-endpoint-test-create-fact-job cid '(("n" . 5))))))
        (is (stringp result))
        (is (starts-with-subseq "REST-FACT-ENTRY" result))))))

(deftest (run-rest-factorial-remote-2 :long-running) ()
  (with-fixture fact-rest-server
    (multiple-value-bind (cid status) (rest-test-get-new-client)
      (declare (ignore status))
      (let* ((*standard-output* (make-broadcast-stream))
             (result
              (symbol-name
               (rest-endpoint-test-create-fact-job
                cid
                '(("n" . 5) ("help" . T))))))
        (is (stringp result))
        (is (starts-with-subseq "REST-FACT-ENTRY" result))))))


;;; CSURF-ASM representation.
(defsuite csurf-asm-tests "CSURF-ASM representation.")

(deftest dynamic-linker-path-has-been-set ()
  (is *dynamic-linker-path* "Ensure `*dynamic-linker-path*' has been set."))

;; simple test to see if the whole file parsed correctly
(deftest parser-test-1 ()
  (with-fixture csurf-asm-calc
    (is (= (length (genome *soft*)) 828))))

(deftest parser-test-2 ()
  (with-fixture csurf-asm-calc
    (is (eq (asm-line-info-type (elt (genome *soft*) 0)) :empty))))

(deftest parser-test-3 ()
  (with-fixture csurf-asm-calc
    (is (eq (asm-line-info-type (elt (genome *soft*) 1)) :decl))))

(deftest parser-test-4 ()
  (with-fixture csurf-asm-calc
    (let ((op-line (find :op (genome *soft*) :key 'asm-line-info-type)))
      (is (and (equalp (asm-line-info-opcode op-line) "sub")
	       (equalp (asm-line-info-operands op-line)
		       '(("rsp" "comma" 8))))))))

(deftest parser-test-5 ()
  (with-fixture csurf-asm-calc
    (is (= (iter (for x in-vector (genome *soft*))
                 (counting (eq (asm-line-info-type x) :op))) 283))))

(deftest csurf-asm-configures ()
  (with-fixture csurf-asm-calc
    (apply-config *soft* (asm-test-dir "calc.log"))
    (is (= 5 (length (weak-symbols *soft*))))
    (iter (for str in '("_Jv_RegisterClasses" "_ITM_registerTMCloneTable"
                        "_ITM_deregisterTMCloneTable" "__imp___gmon_start__"
                        "__gmon_start__"))
          (is (member str (weak-symbols *soft*) :test #'equal)))
    (is (= 1 (length (linked-files *soft*))))
    (is (equal "/lib/x86_64-linux-gnu/libc.so.6"
               (first (linked-files *soft*))))))

(defun csurf-tools-available-p ()
  (zerop (nth-value 2 (shell "which ~a" *elf-edit-symtab-path*))))

(defsuite test-csurf-asm-compile "Tests that rebuild csurf-asm objects"
  (csurf-tools-available-p))

(deftest csurf-asm-can-compile ()
  (with-fixture csurf-asm-calc
    (apply-config *soft* (asm-test-dir "calc.log"))
    (with-temp-file (bin)
      (multiple-value-bind (bin errno)
          (with-cwd ((make-pathname :directory +asm-test-dir+))
            (phenome *soft* :bin bin))
        (is (zerop errno) "Calc compilation successful.")
        (is (= 4 (parse-number (shell "~a + 2 2" bin)))
            "Calc executable is executable and functional.")))))


;;;
;;; Test the TASK-RUNNER modules.
;;;
(defsuite task-runner-tests "TASK-RUNNER tests.")

;; simple test to see if the whole file parsed correctly
(deftest (task-runner-1 :long-running) ( )
  (let (length)
    (is (with-retries (100)
          (with-fixture task-runner
            (setf length (length (task-runner-results (first *soft*)))))
          (when (= length 20)
            (return t))))))

(deftest (task-runner-2 :long-running) ()
  (with-fixture task-runner
    (is (= (length (task-runner-results (second *soft*))) 20))))

(deftest (task-runner-3 :long-running) ()
  (with-fixture task-runner
    (is (= (task-runner-completed-tasks (first *soft*)) 20))
    (is (= (task-runner-completed-tasks (second *soft*)) 20))
    (is (= (task-runner-completed-jobs (first *soft*)) 1))
    (is (= (task-runner-completed-jobs (second *soft*)) 1))))

(deftest (task-runner-4 :long-running) ()
  (with-fixture task-runner
    (is (= (count "test1" (task-runner-results (first *soft*))
		  :test 'equal :key (lambda (s) (subseq s 0 5)))
           20))
    (is (= (count "test2" (task-runner-results (second *soft*))
		  :test 'equal :key (lambda (s) (subseq s 0 5)))
           20))))

(deftest some-task-similar-to-some ()
  (let ((runner1 (run-task (make-instance 'some-task
                             :object (iota 10)
                             :pred {= 5})
                           2))
        (runner2 (run-task (make-instance 'some-task
                             :object (iota 10)
                             :pred {= 15})
                           2)))
    (mapcar #'bt:join-thread (task-runner-workers runner1))
    (mapcar #'bt:join-thread (task-runner-workers runner2))
    (is (equal '(T) (task-runner-results runner1)))
    (is (eql (first (task-runner-results runner1))
             (some {= 5} (iota 10))))
    (is (null (task-runner-results runner2)))
    (is (eql (first (task-runner-results runner2))
             (some {= 15} (iota 10))))))

(deftest simple-task-map ()
  (let ((results (task-map 2 {+ 1} '(1 2 3))))
    ;; Ensure correct results are returned by multi-threaded task-map
    ;; (in any order).
    (mapc (lambda (result) (is (member result results))) '(2 3 4))))


;;; ELF representation.
;;;
;;; NOTE: Currently failing because not populating .text section.
;;;
(defsuite elf-tests "ELF representation." :silent)


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
          (shell "diff ~s ~a" (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-copy-same-genome ()
  (with-fixture gcd-elf
    (is (equal-it (genome *gcd*)
                  (genome (copy *gcd*))))))

(deftest elf-idempotent-read-copy-write ()
  (with-temp-file (a)
    (with-fixture gcd-elf
      (phenome (copy *gcd*) :bin a)
      (multiple-value-bind (out err ret)
          (shell "diff ~s ~a"
                 (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-edit-of-copy-does-not-change-original ()
  (with-fixture gcd-elf
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (handler-case (mutate ant)
        (mutate (obj) (declare (ignorable obj)) nil))
      (is (not (equal-it (genome ant) (genome *gcd*))))
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
          (to-copy (position-if [{eql 1} #'length {aget :code}] (genome *gcd*))))
      (apply-mutation variant (list :insert 0 to-copy))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equal-it (bytes *gcd*) (bytes variant)))))))

(deftest elf-replace-changes-but-maintains-length ()
  (with-fixture gcd-elf
    (let* ((variant (copy *gcd*))
           ;; See FIND-SMALL in `elf-insertion-changes-but-maintains-lengthens'
           (to-copy (position-if [{eql 1} #'length {aget :code}] (genome *gcd*)))
           (new-genome (elf-replace
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
  (zerop (nth-value 2 (shell "which clang-mutate"))))

(defsuite clang-tests "Clang representation." (clang-mutate-available-p))

(deftest simply-able-to-load-a-clang-software-object()
  (with-fixture hello-world-clang
    (is (not (null *hello-world*)))))

(deftest (genome-change-clears-clang-software-object-fields :long-running) ()
  (with-fixture hello-world-clang
    (is (not (null (stmt-asts *hello-world*))))
    (is (not (null (functions *hello-world*))))
    (is (not (null (prototypes *hello-world*))))
    (is (not (null (includes *hello-world*))))
    ;; The following were already nil, so this test
    ;; is not testing that they were cleared
    (is (null (non-stmt-asts *hello-world*)))
    (is (null (macros *hello-world*)))
    (is (null (fitness *hello-world*)))
    (setf (genome *hello-world*) "")
    (is (null  (asts *hello-world*)))
    (is (null  (stmt-asts *hello-world*)))
    (is (null  (non-stmt-asts *hello-world*)))
    (is (null  (functions *hello-world*)))
    (is (null  (prototypes *hello-world*)))
    (is (null  (includes *hello-world*)))
    (is (null  (macros *hello-world*)))
    (is (null  (fitness *hello-world*)))
    ;; This is not zero for new clang, as it contains
    ;; builtins
    (if *new-clang?*
        (is (zerop (count-if #'type-i-file
                             (hash-table-values (types *hello-world*)))))
        (is (zerop (hash-table-count (types *hello-world*)))))))

(deftest normalize-flags-test ()
  (is (equal (normalize-flags "/foo/" (list "-Wall"))
             (list "-Wall")))
  (is (equal (normalize-flags "/foo/" (list "-I/bar/"))
             (list "-I" "/bar/")))
  (is (equal (normalize-flags "/foo/" (list "-I" "/bar/"))
             (list "-I" "/bar/")))
  (is (equal (normalize-flags "/foo/" (list "-L/bar/"))
             (list "-L" "/bar/")))
  (is (equal (normalize-flags "/foo/" (list "-L" "/bar/"))
             (list "-L" "/bar/")))
  (is (find "/foo/" (normalize-flags "/foo/" (list "-I."))
            :test (lambda (s1 s2) (search s1 s2 :test #'equal))))
  (is (find "/foo/" (normalize-flags "/foo/" (list "-I" "."))
            :test (lambda (s1 s2) (search s1 s2 :test #'equal))))
  (is (find "/foo/" (normalize-flags "/foo/" (list "-L."))
            :test (lambda (s1 s2) (search s1 s2 :test #'equal))))
  (is (find "/foo/" (normalize-flags "/foo/" (list "-L" "."))
            :test (lambda (s1 s2) (search s1 s2 :test #'equal)))))

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
      (is (ast-equal-p (slot-value new 'ast-root)
                       (slot-value *hello-world* 'ast-root))
          "Copy and original share ASTs")

      (apply-mutation new (make-instance 'clang-swap :object new))
      (is (ast-equal-p (slot-value new 'ast-root)
                       (slot-value (copy new) 'ast-root))
          "Additional copies do not cause updates"))))

(deftest (splits-global-and-stmt-asts :long-running) ()
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
          (ignore-phenome-errors
           (phenome *hello-world* :bin bin))
        (declare (ignorable stderr stdout src))
        (is (probe-file bin))
        (is (= 0 errno))))))

(deftest can-apply-mutation-w-value1 ()
  (with-fixture hello-world-clang
    (let* ((variant (copy *hello-world*))
           (stmt1 (stmt-with-text variant
                                  "printf(\"Hello, World!\\n\");")))
      (apply-mutation variant
        `(clang-replace
          (:stmt1 . ,stmt1)
          (:value1 . ,(make-literal 0))))
      (is (different-asts (asts variant) (asts *hello-world*)))
      (is (not (equal (genome variant) (genome *hello-world*)))))))

(deftest can-apply-mutation-w-value2 ()
  (with-fixture sqrt-clang
    (let* ((variant (copy *sqrt*))
           (integer-constant
            (second (remove-if-not
                     [{equal :IntegerLiteral} #'ast-class]
                     (asts variant)))))
      (apply-mutation variant
        `(clang-replace
          (:stmt1 . ,integer-constant)
          (:value1 . ,(make-literal 0))))
      (is (different-asts (asts variant) (asts *sqrt*)))
      (is (not (equal (genome variant) (genome *sqrt*))))
      (is (stmt-with-text variant "0")))))

(deftest cut-shortens-a-clang-software-object()
  (with-fixture hello-world-clang
    (let* ((variant (copy *hello-world*))
           (stmt1 (stmt-with-text variant
                                  "printf(\"Hello, World!\\n\");")))
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
                                 "printf(\"Hello, World!\\n\");"))
          (stmt2 (stmt-with-text *hello-world*
                                 "return 0;")))
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
                                 "printf(\"Hello, World!\\n\");"))
          (stmt2 (stmt-with-text *hello-world*
                                 "return 0;")))
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
        `(clang-cut (:stmt1 . ,(stmt-with-text
                                variant "printf(\"Hello, World!\\n\");"))))
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
      (is (ast-equal-p (ast-root *hello-world*)
                       (ast-root variant)))
      (is (> (size variant) 0)))))

(deftest clang-mutation-preserves-unmodified-subtrees ()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*)))
      (apply-mutation
          variant
        `(clang-cut (:stmt1 . ,(stmt-with-text
                                variant "printf(\"Hello, World!\\n\");"))))
      (is (ast-equal-p (stmt-with-text *hello-world* "return 0;")
                       (stmt-with-text variant "return 0;"))))))

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

(deftest can-serialize-a-clang-software-obj ()
  (with-fixture hello-world-clang
    (with-temp-file (store-file)
      (store *hello-world* store-file)
      (is (equalp (genome (restore store-file)) (genome *hello-world*))))))


;;; Misc. clang tests

(deftest able-to-wrap-statements-in-blocks ()
  (with-fixture gcd-wo-curlies-clang
    (let ((var (copy *gcd*)))
      ;; Setup, ensure everything is what we thing it should be.
      (is (eq :BinaryOperator     ; Guard
              (ast-class (stmt-with-text var "a > b"))))
      (is (eq :BinaryOperator     ; Then
              (ast-class (stmt-with-text var "a = a - b;"))))
      (is (eq :BinaryOperator     ; Else
              (ast-class (stmt-with-text var "b = b - a;"))))
      ;; Wrap children and ensure changes are made.

      (setf var (wrap-child var (stmt-starting-with-text var "if (a > b)")
                            1))
      (setf var (wrap-child var (stmt-starting-with-text var "if (a > b)")
                            2))
      (is (eq :BinaryOperator     ; Guard
              (ast-class (stmt-with-text var "a > b"))))
      (is (eq :CompoundStmt       ; Then
              (ast-class (get-parent-ast var
                           (stmt-with-text var "a = a - b;")))))
      (is (eq :CompoundStmt       ; Then
              (ast-class (get-parent-ast var
                           (stmt-with-text var "b = b - a;")))))
      ;; Ensure gcd remains unchanged.
      (is (eq :BinaryOperator     ; Guard
              (ast-class (stmt-with-text *gcd* "a > b"))))
      (is (eq :BinaryOperator     ; Then
              (ast-class (stmt-with-text *gcd* "a = a - b;"))))
      (is (eq :BinaryOperator     ; Else
              (ast-class (stmt-with-text *gcd* "b = b - a;")))))))

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
      (ast-root *headers*)
      (is (listp includes))
      ;; As JR explained, "first.c" is handled
      ;; differently in the old clang front end, in
      ;; the types table, not the includes attribute.
      ;; TODO: determine if it is ok to have "first.c"
      ;; here instead.
      (is (= (if *new-clang?* 3 2) (length includes)))
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
      (is (subsetp (list "bar" "char" "char*" "foo" "int")
                   (mapcar (lambda (s) (remove #\Space s))
                           (sort (mapcar #'type-name (hash-table-values types))
                                 #'string<))
                   :test #'equal)))))

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
                                     :hash 3346600377836954008
                                     :i-file "<stdio.h>"
                                     :pointer t
                                     :reqs nil
                                     :name "FILE"))
    (is (equal 1 (length (includes *fib*))))))

(deftest add-bad-include-doesnt-change-number-of-asts ()
  (with-fixture hello-world-clang
    (let ((orig-num-asts (size *hello-world*)))
      (add-include *hello-world* "<garbage.h>")
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest add-bad-type-doesnt-change-number-of-asts ()
  (with-fixture hello-world-clang
    (let ((orig-num-asts (size *hello-world*)))
      (add-type *hello-world*
                (make-clang-type :decl "struct printf { chocolate cake; }"
                                 :array "" :hash 0 :name "struct printf"))
      (is (equal orig-num-asts (size *hello-world*))))))

(deftest add-new-type-changes-genome-and-types ()
  (with-fixture hello-world-clang
    (let ((orig-genome-length (length (genome *hello-world*)))
          (orig-num-types (hash-table-count (types *hello-world*)))
          (struct-str "struct printf { chocolate cake; }"))
      (add-type *hello-world*
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
      (add-macro *hello-world*
                 (make-clang-macro :name "GARBAGE"
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

(deftest (clang-expression-test :long-running) ()
  (flet ((test-conversion (obj pair)
           (destructuring-bind (text expected-expression) pair
             (let ((result (expression obj (stmt-with-text obj text))))
               (is (equalp result expected-expression)
                   "Statement ~S yields ~S not ~S."
                   text result expected-expression)))))
    (append
     (with-fixture gcd-clang
       (mapc {test-conversion *gcd*}
             '(("b = b - a;" (:= :b (:- :b :a)))
               ("a = a - b;" (:= :a (:- :a :b)))
               ("b != 0"    (:!= :b 0))
               ("a > b"     (:> :a :b))
               ("a == 0"    (:== :a 0)))))
     (with-fixture binary-search-clang
       (mapc {test-conversion *binary-search*}
             '(("mid = (start + end) / 2;"
                (:= :mid (:/ (:+ :start :end) 2)))
               ("haystack[i] = malloc(256 * sizeof(*haystack[i]));"
                (:= (:|[]| :haystack :i)
                 (:malloc (:* 256
                              (:sizeof (:unary-* (:|[]| :haystack :i))))))))))
     (with-fixture huf-clang
       (mapc {test-conversion *huf*}
             '(("h->h = malloc(sizeof(int)*s);"
                (:= (:-> :h :h) (:malloc (:* (:sizeof :int) :s))))
               ("heap->h = realloc(heap->h, heap->s + heap->cs);"
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
    (is (equalp (remove-if-not #'full-stmt-filter
                               (stmt-asts *hello-world*))
                (mutation-targets *hello-world*
                                  :filter #'full-stmt-filter)))))

(deftest clang-mutation-targets-stmt-pool-test ()
  "Ensure the stmt-pool parameter to mutation-targets works as anticipated"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (remove-if-not #'full-stmt-filter
                                     (stmt-asts *hello-world*))))
      (is (equalp (remove-if-not #'full-stmt-filter
                                 (stmt-asts *hello-world*))
                  (mutation-targets *hello-world*
                                    :stmt-pool #'bad-stmts))))))

(deftest clang-mutation-targets-expand-stmt-pool-restart-test ()
  "Ensure the expand-stmt-pool restart works as intended"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (remove-if-not #'full-stmt-filter
                                     (stmt-asts *hello-world*))))
      ;; Before invoking the 'expand-stmt-pool filter, the
      ;; stmt pool does not include any full statements.
      ;; After its invocation, all full statements are returned.
      (is (equalp (remove-if-not #'full-stmt-filter
                                 (stmt-asts *hello-world*))
                  (->> (handler-bind
                           ((no-mutation-targets
                             (lambda (c)
                               (declare (ignorable c))
                               (invoke-restart 'expand-stmt-pool))))
                         (mutation-targets *hello-world*
                                           :filter #'full-stmt-filter
                                           :stmt-pool #'bad-stmts))))))))

(deftest clang-pick-general-does-not-throw-test ()
  "Ensure calling pick-general does not throw an exception"
  (with-fixture hello-world-clang
    (is (not (null (pick-general *hello-world* #'stmt-asts))))))

(deftest clang-pick-general-full-stmt-no-matching-test ()
  "Ensure calling pick-general with a full-stmt filter
throws a no-mutation-targets error when there are no full stmts,
e.g. after a bad crossover"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (remove-if #'full-stmt-filter
                                 (stmt-asts *hello-world*))))
      (signals no-mutation-targets
               (pick-general *hello-world* #'bad-stmts
                             :filter #'full-stmt-filter)))))

(deftest clang-pick-general-full-stmt-test ()
  "Ensure calling pick-general with a full-stmt filter returns a full
statement pick"
  (with-fixture hello-world-clang-control-picks
    (let ((pick (pick-general *hello-world* #'stmt-asts
                              :filter #'full-stmt-filter)))
      (is (->> (aget :stmt1 pick)
               (ast-full-stmt))))))

(deftest clang-pick-general-same-class-no-matching-test ()
  "Ensure calling pick-general with a same-class filter throws
a no-mutation-targets error when a second statement with the same AST class
is not to be found"
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (list (make-clang-ast
                             :node (from-alist 'clang-ast-node
                                               '((:class . :Nothing)))))))
      (signals no-mutation-targets
               (pick-general *hello-world* #'stmt-asts
                             :filter #'same-class-filter
                           :second-pool #'bad-stmts)))))

(deftest clang-promote-guarded-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (handler-case
        (progn
          (build-op (make-instance 'clang-promote-guarded :object *soft*)
                    *soft*)
          (is nil "build-op should have thrown no-mutation-targets error"))
      (error (e)
        (is (equal (type-of e) 'no-mutation-targets)
            "build-op should have thrown no-mutation-targets error")))))



(deftest pick-cut-decl-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets (pick-cut-decl *soft*))))

(deftest pick-swap-decls-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets (pick-swap-decls *soft*))))

(deftest pick-rename-variable-throws-error-if-no-targets-test ()
  (with-fixture no-mutation-targets-clang
    (signals no-mutation-targets (pick-rename-variable *soft*))))

(deftest (cpp-strings-works :long-running) ()
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

(deftest simple-macro-expansion ()
  (with-fixture simple-macros-clang
    ;; Without the -DDEBUG on the command line we only see two
    ;; instances of the FUNCTION_LIKE_DEBUG macro.
    (is (= 2 (count-if [{eql :MACROEXPANSION} #'ast-class] (asts *soft*))))
    ;; Even in this case we still see the "ifdef DEBUG" lines in the
    ;; source text of the top level compound statement.
    (is (search "#ifdef DEBUG"
                (ast-text (find-if [{eql :COMPOUNDSTMT} #'ast-class]
                                   (asts *soft*)))))))

(deftest overlapping-sibling-asts ()
  ;; A combination of macros and case statements produces tricky
  ;; overlapping source ranges. Test that update-asts can handle it
  ;; correctly.
  (with-fixture switch-macros-clang
    (let ((overlapping-children
           (->> (stmt-starting-with-text *soft* "case 'F'")
                (get-immediate-children *soft*))))
      (is (= 2 (length overlapping-children)))
      (is (member :MacroExpansion
                  (mapcar #'ast-class overlapping-children))))))

(deftest replace-in-ast-subtree ()
  (let ((subtree (make-clang-ast
                  :node (make-clang-ast-node :class :sub)
                  :children '("1" "2"))))
    (is (equalp (replace-in-ast
                 (make-clang-ast
                  :node (make-clang-ast-node :class :root)
                  :children `(,(make-clang-ast
                                :node (make-clang-ast-node :class :left)
                                :children (list "3" "4"))
                               ,subtree))
                 `((,subtree . ,(make-clang-ast
                                 :node (make-clang-ast-node :class :right)
                                 :children (list "5" "6"))))
                 :test #'equalp)
                (make-clang-ast
                 :node (make-clang-ast-node :class :root)
                 :children `(,(make-clang-ast
                               :node (make-clang-ast-node :class :left)
                               :children (list "3" "4"))
                              ,(make-clang-ast
                                :node (make-clang-ast-node :class :right)
                                :children (list "5" "6"))))))))

(deftest replace-in-ast-string ()
  (is (equalp (replace-in-ast (make-clang-ast
                               :node (make-clang-ast-node :class :root)
                               :children
                               (list (make-clang-ast
                                      :node (make-clang-ast-node :class :left)
                                      :children '("left"))
                                     (make-clang-ast
                                      :node (make-clang-ast-node :class :right)
                                      :children '("right"))))
                              '(("right" . "replacement"))
                              :test #'equal)
              (make-clang-ast
               :node (make-clang-ast-node :class :root)
               :children
               (list (make-clang-ast
                      :node (make-clang-ast-node :class :left)
                      :children '("left"))
                     (make-clang-ast
                      :node (make-clang-ast-node :class :right)
                      :children '("replacement")))))))

(deftest find-or-add-type-finds-existing-type ()
  (with-fixture gcd-clang
    #+(OR)
    (is (eql (find-if [{string= "int"} #'type-name]
                      (hash-table-values (types *gcd*)))
             (find-or-add-type *gcd* "int")))
    (is (find (find-or-add-type *gcd* "int")
              (hash-table-values (types *gcd*))))))

(deftest find-or-add-type-adds-new-type ()
  (with-fixture gcd-clang
    ;; The new clang front end loads a large number of types
    ;; that are defined in headers, but not used in the program
    #+nil
    (is (null (find-if [{string= "float"} #'type-name]
                       (hash-table-values (types *gcd*)))))
    (let ((new-type (find-or-add-type *gcd* "int")))
      (is new-type "New type created.")
      (is (gethash (type-hash new-type) (types *gcd*))
          "New type is added to software.")
      (let ((itp (find-or-add-type *gcd* "int")))
        (is (not (type-pointer itp))
            "int type should not a pointer type")
        (is (not (type-const itp))
            "int type should not a const type")
        (is (not (type-volatile itp))
            "int type should not a volatile type")
        (is (eql (type-storage-class itp) :none)
            "int type should have no storage class")
        (is (eql new-type itp)
            "Repeated call finds same type.")))))

(deftest find-or-add-type-parses-pointers ()
  (with-fixture gcd-clang
    (is (eql (find-or-add-type *gcd* "*char")
             (find-or-add-type *gcd* "char" :pointer t)))))

(deftest var-decl-has-correct-types ()
  (let* ((obj (make-clang :genome "int x = sizeof(int);"))
         (*soft* obj))
    ;; A var decl should always directly reference the type of its
    ;; declaration. This is tricky due to the de-aggregating of types
    ;; done by asts->tree.

    ;; FIXME: if the RHS were "sizeof(int) + sizeof(char)" the decl
    ;; would reference both types, which is incorrect but probably
    ;; harmless.
    (is (member "int"
                (mapcar [#'type-name {find-type obj}]
                        (ast-types (first (asts obj))))
                :test #'equalp))))

(deftest macro-expansion-has-correct-types ()
  ;; Types inside a macro expansion should be visible. This is trick
  ;; due to the de-aggregating of types done by asts->tree.
  (let* ((obj (make-clang :genome "#define CHARSIZE (sizeof (char))
int x = CHARSIZE;")))
    (let ((types
           (sort (mapcar [#'type-name {find-type obj}]
                         (get-ast-types obj (first (asts obj))))
                 #'string<)))
      (is (or (equal '("char" "int") types)
              (equal '("char" "int" "unsigned long") types))))))

(deftest able-to-handle-multibyte-characters ()
  (handler-bind (#+sbcl (sb-int:stream-encoding-error
                         (lambda (c)
                           (declare (ignorable c))
                           (invoke-restart 'use-encoding :utf-8))))
    (with-fixture unicode-clang
      (is (stmt-with-text *soft* "int x = 0" :no-error t))
      (is (stmt-with-text *soft* "\"2 bytes: Δ\"" :no-error t))
      (is (stmt-with-text *soft* "int y = 1" :no-error t))
      (is (string= (genome *soft*)
                   (file-to-string (unicode-dir "unicode.c")))))))


;;; Detailed clang mutation tests
;;;
;;; These all run the entire mutate method, rather that just
;;; apply-mutation, adjusting the good and bad picks to get
;;; predictable results. And they check the results of each mutation
;;; in as much detail as possible.
(defsuite clang-mutations "Detailed clang mutation tests."
            (clang-mutate-available-p))

(defixture gcd-clang-control-picks
  (:setup
   (setf *gcd*
         (from-file (make-clang-control-picks :compiler "clang-3.7")
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
          (*bad-asts* (asts-with-text *hello-world* "return 0;"))
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
      (is (stmt-with-text variant "return 00;" :no-error t)))))

(deftest insert-full-same-adds-same-class-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0;"))
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
      (is (stmt-with-text variant "return printf;" :no-error t)))))

(deftest replace-full-changes-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0;"))
          (*good-asts* (asts-with-text *hello-world*
                                       "0" "printf(\"Hello, World!\\n\");"))
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
      (is (stmt-with-text variant "printf(printf);" :no-error t)))))

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
      (is (stmt-with-text variant "\"Hello, World!\\n\"" :no-error t))
      (is (stmt-with-text variant "0" :no-error t)))))

(deftest move-changes-any-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let* ((bad-1 "printf(\"Hello, World!\\n\");")
           (bad-2 "return 0;")
           (*bad-asts* (asts-with-text *hello-world* bad-1 bad-2))
           (*clang-mutation-types* '((clang-move . 1))))
      (multiple-value-bind (variant mut) (mutate (copy *hello-world*))
        ;; If both targets are the same, genome will not change.
        (unless (eq (aget :stmt1 (targets mut)) (aget :stmt2 (targets mut)))
          (is (not (string= (genome *hello-world*) (genome variant)))
              "Move changes genome."))
        ;; Still exist (> 0).
        (is (stmt-with-text variant bad-1 :no-error t)
            "Move doesn't remove \"Hello, World!\\n\".")
        (is (stmt-with-text variant bad-2 :no-error t)
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
        (is (stmt-with-text variant "printf" :no-error t))
        (is (stmt-with-text variant "return 0;" :no-error t))))))

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

(deftest (unguard-conditional-compound-statements :long-running) ()
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
                  :targets (->> (stmt-with-text *nested* "puts('WHILE');")
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
                  :targets (->> (stmt-with-text *nested* "puts('DO');")
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
                  :targets (->> (stmt-with-text *nested* "puts('FOR-1');")
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
                  :targets (->> (stmt-with-text *nested* "puts('FOR-2');")
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
                  :targets (->> (stmt-with-text *nested* "puts('IF-1');")
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
                  :targets (->> (stmt-with-text *nested* "puts('IF-2');")
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
                  :targets (->> (stmt-with-text *nested* "puts('IF-3');")
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
                  :targets (->> (stmt-with-text *nested* "puts('IF-3');")
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
                    :targets (->> (stmt-with-text *nested* "puts('MULTILINE');")
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
(defsuite clang-w-fodder-tests "Clang w/ mutation fodder representation."
            (clang-mutate-available-p))


(deftest (clang-parse-source-snippet-body-statement :long-running) ()
  (with-fixture gcd-clang
    (let ((asts (parse-source-snippet
                  :clang
                  "x + y"
                  :unbound-vals `(("x" ,(type-from-trace-string "int"))
                                  ("y" ,(type-from-trace-string "char"))))))
      (is (eq 1 (length asts)))
      (is (eq :BinaryOperator (ast-class (car asts))))
      (is (equalp '(((:name . "y")) ((:name . "x")))
                  (get-unbound-vals *gcd* (car asts)))))))

(deftest clang-parse-source-snippet-handles-includes ()
  (let ((asts (parse-source-snippet
                :clang
                "printf(\"hello\")"
                :unbound-vals nil
                :includes '("<stdio.h>"))))
    (is (eq 1 (length asts)))
    (is (eq :CallExpr (ast-class (car asts))))
    (is (equalp '("<stdio.h>")
                (ast-includes (car asts))))))

(deftest clang-parse-source-snippet-multiple-statements ()
  (let ((asts (parse-source-snippet
                :clang
                "x = 1; y = 1"
                :unbound-vals `(("x" ,(type-from-trace-string "int"))
                                ("y" ,(type-from-trace-string "char")))
                :includes nil)))
    (is (eq 2 (length asts)))
    (is (eq :BinaryOperator (ast-class (first asts))))
    (is (eq :BinaryOperator (ast-class (second asts))))))

(deftest clang-parse-source-snippet-top-level ()
  (let ((asts (parse-source-snippet
                :clang
                "int foo() { return 1; }"
                :unbound-vals nil
                :top-level t)))
    (is (eq 1 (length asts)))
    (is (eq :Function (ast-class (car asts))))
    (is (eq :CompoundStmt (ast-class (function-body (make-clang)
                                                    (car asts)))))))

(deftest clang-parse-source-snippet-preamble ()
  (let ((asts (parse-source-snippet
                :clang
                "int *p = A + 10;"
                :unbound-vals nil
                :preamble "static int A[10];")))
    (is (eq :DeclStmt (ast-class (first asts))))))

(deftest (clang-parse-source-snippet-keep-comments :long-running) ()
  (let ((asts1 (parse-source-snippet
                 :clang
                 "/*POTENTIAL FLAW */ strlen(0);"
                 :unbound-vals nil
                 :includes '("<string.h>")
                 :keep-comments t))
        (asts2 (parse-source-snippet
                 :clang
                 (format nil "// POTENTIAL FLAW~% strlen(0);")
                 :unbound-vals nil
                 :includes '("<string.h>")
                 :keep-comments t)))
    (is (not (null (search "/*POTENTIAL FLAW */"
                           (source-text (first asts1))))))
    (is (not (null (search "// POTENTIAL FLAW"
                           (source-text (first asts2))))))))

(deftest simply-able-to-load-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (is (not (null *hello-world*)))))

(deftest (insert-fodder-decl-mutation-throws-error-if-no-targets-test
          :long-running) ()
  (with-fixture no-insert-fodder-decl-mutation-targets-clang
    (signals no-mutation-targets
      (apply-mutation *soft* (make-instance 'insert-fodder-decl
                               :object *soft*)))))

(deftest
    (insert-decl-lengthens-a-clang-w-fodder-software-object :long-running) ()
  (handler-bind ((mutate ; TODO: Maybe better to fix the hello-world C file.
                  (lambda (e)
                    (if (find-restart 'keep-partial-asts)
                        (invoke-restart 'keep-partial-asts)
                        (error e)))))
    (with-fixture hello-world-clang-w-fodder
      (let ((variant (copy *hello-world*)))
        (apply-mutation variant (make-instance 'insert-fodder-decl
                                  :object variant))
        (is (> (size variant)
               (size *hello-world*)))
        (is (string/= (genome variant)
                      (genome *hello-world*)))))))

(deftest (insert-decl-rename-lengthens-and-insinuates-a-clang-w-fodder
          :long-running) ()
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
          (target (stmt-with-text *hello-world* "return 0;")))
      (apply-mutation variant
        `(clang-insert (:stmt1 . ,target)
                       (:value1 . ,target)))
      (is (> (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest (set-value-changes-a-clang-w-fodder-software-object
          :long-running) ()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant
        `(clang-replace
          (:stmt1 . ,(find-if [{eq :StringLiteral} #'ast-class]
                              (asts variant)))
          (:literal1 . ,(to-ast 'clang-ast `(:StringLiteral "Hello, mutate!")))))
      (is (= (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest (insert-fodder-lengthens-a-clang-w-fodder-software-object
          :long-running) ()
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

(deftest (pick-bad-fodder-works :long-running) ()
  (with-fixture hello-world-clang-w-fodder
    (let ((mut (make-instance 'insert-fodder :object *hello-world*)))
      (is (not (null (targets mut))))
      (is (not (null (aget :stmt1 (targets mut)))))
      (is (not (null (aget :value1 (targets mut))))))))

(deftest (pick-decl-fodder-works :long-running) ()
  (with-fixture hello-world-clang-w-fodder
    (let ((mut (make-instance 'insert-fodder-decl :object *hello-world*)))
      (is (not (null (targets mut))))
      (is (not (null (aget :stmt1 (targets mut)))))
      (is (not (null (aget :value1 (targets mut))))))))


;;;; Clang utility methods.
(defsuite clang-utility "Clang utility methods."
            (clang-mutate-available-p))


(deftest asts-populated-on-creation ()
  (with-fixture hello-world-clang
    (is (= 10 (length (asts *hello-world*))))))

(deftest parent-ast-p-true-test()
  (with-fixture hello-world-clang
    (is (parent-ast-p *hello-world*
                      (stmt-with-text *hello-world* "return 0;")
                      (stmt-with-text *hello-world* "0")))))

(deftest parent-ast-p-false-test()
  (with-fixture hello-world-clang
    (is (not (parent-ast-p *hello-world*
                           (stmt-with-text *hello-world* "0")
                           (stmt-with-text *hello-world* "return 0;"))))))

(deftest (tidy-a-clang-software-object :long-running) ()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*)))
      (clang-tidy variant)
      (is (= (size variant)
             (size *hello-world*))))))

(deftest (tidy-adds-braces :long-running) ()
  (with-fixture tidy-adds-braces-clang
    (let ((variant (copy *soft*)))
      (clang-tidy variant)
      (is (= 2 (->> (stmt-asts variant)
                    (remove-if-not [{eq :CompoundStmt} #'ast-class])
                    (length)))))))

(deftest (format-a-clang-software-object :long-running) ()
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
         (some->> (stmt-with-text *soft* stmt-text)
                  (get-vars-in-scope *soft*)
                  (find-if [{name= var} {aget :name}])
                  (find-var-type *soft*))))
    (with-fixture type-of-var-clang
      (let ((var-type1 (get-var-type "a" "return 0;"))
            (var-type2 (get-var-type "a" "return 1;"))
            (var-type3 (get-var-type "a" "return 2;"))
            (var-type4 (get-var-type "a" "int a[N][N];"))
            (var-type5 (get-var-type "b" "return 2;")))
        (is (null var-type4))
        (is (equal (if *new-clang?* "[10]" "[10][10]") (type-array var-type1)))
        (is (equal ""         (type-array var-type2)))
        (is (equal "[10][10]" (type-array var-type3)))
        (is (equal ""         (type-array var-type5)))
        (is (equal nil        (type-pointer var-type1)))
        (is (equal t          (type-pointer var-type2)))
        (is (equal nil        (type-pointer var-type3)))
        (is (equal t          (type-pointer var-type5)))
        (is (equal (if *new-clang?* "int (*)" "int") (type-name var-type1)))
        (is (equal "int"      (type-name var-type2)))
        (is (equal "int"      (type-name var-type3)))
        (is (equal "int*"     (remove #\Space (type-name var-type5))))))))

(deftest find-var-type-handles-missing-declaration-type ()
  (with-fixture type-of-var-missing-decl-type-clang
    (is (null (some->> (stmt-with-text *soft* "dirs[0] = L;")
                       (get-vars-in-scope *soft*)
                       (find-if [{string= "dirs"} {aget :name}])
                       (find-var-type *soft*))))))

(deftest typedef-type-returns-correct-type ()
  (with-fixture typedef-type-clang
    (let ((type1 (some->> (stmt-with-text *soft* "gint a;")
                          (get-ast-types *soft*)
                          (car)
                          (find-type *soft*)
                          (typedef-type *soft*)))
          (type2 (some->> (stmt-with-text *soft* "gchar *b;")
                          (get-ast-types *soft*)
                          (car)
                          (find-type *soft*)
                          (typedef-type *soft*)))
          (type3 (some->> (stmt-with-text *soft* "gcharp p;")
                          (get-ast-types *soft*)
                          (car)
                          (find-type *soft*)
                          (typedef-type *soft*))))
      (is (equal "int"  (type-name type1)))
      (is (equal nil    (type-pointer type1)))
      (is (equal "char" (type-name type2)))
      (is (equal t      (type-pointer type2)))
      (is (equal "char" (type-name type3)))
      (is (equal t      (type-pointer type3))))))

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
    (labels ((peel-banana-tree (tree)
               (if *new-clang?*
                   (cond ((stringp tree)
                          (peel-bananas tree))
                         ((consp tree)
                          (let ((car (peel-banana-tree (car tree)))
                                (cdr (peel-banana-tree (cdr tree))))
                            (if (and (eql car (car tree)) (eql cdr (cdr tree)))
                                tree
                                (cons car cdr))))
                         (t tree))
                   tree)))
      (let* ((copy (copy *soft*))
             (stmt (stmt-with-text copy "assert(argc > 0);")))
        (is (equalp (peel-banana-tree "assert((|someVal|) > 0);")
                    (->> (rebind-vars stmt
                                      (peel-banana-tree
                                       '(("(|argc|)" "(|someVal|)")))
                                      nil)
                         (source-text)))
            "rebind-vars did not rebind a variable within a macro")))))


;;;; Java representation.
(defun java-mutate-available-p ()
  (zerop (nth-value 2 (shell "which java-mutator"))))

(defsuite java-tests "JAVA representation."
  ;; (java-mutate-available-p)
  :silent)

;; Copy software object.
(deftest (java-test-copy :long-running) ()
  "Checks that when a deep copy of a software object is created. If the
  genome is updated in the original whenever the copy is modified, then
  there is a problem with the directives for the genome slot value
  defined in the java software object."
  (let ((*java-file-name* "TestSimple"))
    (with-fixture general-fixture-java
      (let ((temporary-obj (copy *soft*)))
        (setf (slot-value temporary-obj 'genome) "new genome")
        (is (not (equal (genome temporary-obj) (genome *soft*))))))))

(deftest (insert_testsimple :long-running) ()
  "Check if print stmt was inserted in the new genome, but not in original."
  (let ((*java-file-name* "TestSimple_WhileForIfPrint_2"))
    (with-fixture general-fixture-java
      (let ((before-genome (genome *soft*))
            (after-genome
             (nest
              (genome)
              (apply-mutation *soft*)
              (make-instance 'java-insert :targets)
              (list (cons :stmt1 (java-make-literal :integer 2)))
              (cons :value1)
              (java-make-literal :string)
              "System.out.println(\"THIS STATEMENT INSERTED\");"))
            (target "THIS STATEMENT INSERTED"))
        (is (not (scan-to-strings target before-genome)))
        (is (scan-to-strings target after-genome))))))

(deftest (add-import-test-java-1 :long-running) ()
  "Check if import was inserted into file with no package name."
  (let ((*java-file-name* "TestSimple_WhileForIfPrint_2"))
    (with-fixture general-fixture-java
      (let ((before-genome (genome *soft*))
            (after-genome (genome (add-import *soft* "java.util.LinkedList")))
            (target "java[.]util[.]LinkedList"))
        (is (not (scan-to-strings target before-genome)))
        (is (scan-to-strings target after-genome))))))

(deftest (add-import-test-java-2 :long-running) ()
  "Check if import was inserted into file with package name."
  (let ((*java-file-name* "TestSimple_package_name"))
    (with-fixture general-fixture-java
      (let ((before-genome (genome *soft*))
            (after-genome (genome (add-import *soft* "java.util.LinkedList")))
            (target "java[.]util[.]LinkedList"))
        (is (not (scan-to-strings target before-genome)))
        (is (scan-to-strings target after-genome))))))

(defun is-genome-modified-by-instrumentation-of (file)
  (let ((*java-file-name* file))
    (with-fixture general-fixture-java
      (let ((inst (copy *soft*))
            (target "java[.]io[.]PrintWriter"))
        (instrument inst)
        (is (not (scan-to-strings target (genome *soft*)))
            "Original version of ~a does not include ~a." file target)
        (is (scan-to-strings target (genome inst))
            "Instrumented version of ~a does include ~a." file target)))))

;;; Instrumentation tests.
(deftest (instrument-testsimple :long-running) ()
  "Currently checks whether file was modified with instrumentation commands."
  (is-genome-modified-by-instrumentation-of "TestSimple"))

(deftest (instrument-testsimple-with-one-pck :long-running) ()
  "Similar to `instrument_testsimple', but with one package name."
  (is-genome-modified-by-instrumentation-of "TestSimple_package_name"))

(deftest (instrument-testsimple-with-mult-pcks :long-running) ()
  "Similar to `instrument_testsimple', but with long package name."
  (is-genome-modified-by-instrumentation-of "TestSimple_longer_package_name"))

;; Phenome tests.
(defun is-phenome-execution-script-created-for (file)
  (let ((*java-file-name* file))
    (with-fixture general-fixture-java
      (instrument *soft*)
      (with-temp-file (bin)
        (phenome *soft* :bin bin)
        (is (probe-file bin))))))

(deftest (phenome-testsimple :long-running) ()
  "Runs the phenome function and checks if execution script was created."
  (is-phenome-execution-script-created-for "TestSimple"))

(deftest (phenome-testsimple-with-one-pck :long-running) ()
  "Runs the phenome function and checks if execution script was created."
  (is-phenome-execution-script-created-for "TestSimple_package_name"))

(deftest (phenome-testsimple-with-mult-pcks :long-running) ()
  "Runs the phenome function and checks if execution script was created."
  (is-phenome-execution-script-created-for "TestSimple_longer_package_name"))

;; Collect traces tests.
(defun is-exact-output-of-collect-traces-expected-for
    (file &optional (target '((:TRACE ((:C . 1) (:SCOPES))
                               ((:C . 2) (:SCOPES)))
                              (:INPUT :BIN))))
  (let ((*java-file-name* file))
    (with-fixture general-fixture-java-traceable
      (instrument *soft*)
      (is (equalp (-<>> (make-instance 'test-suite
                          :test-cases (list (make-instance 'test-case
                                              :program-name :bin)))
                        (collect-traces *soft*)
                        (get-trace <> 0))
                  target)))))

(deftest (collect-traces-testsimple :long-running) ()
  "Check exact output of collect-traces with the expected result."
  (is-exact-output-of-collect-traces-expected-for "TestSimple"))

(deftest (collect-traces-testsimple-with-one-pck :long-running) ()
  "Similar to `collect-traces-testsimple', but with one package name."
  (is-exact-output-of-collect-traces-expected-for "TestSimple_package_name"))

(deftest (collect-traces-testsimple-with-mult-pcks :long-running) ()
  "Similar to `collect-traces-testsimple', but with longer package name."
  (is-exact-output-of-collect-traces-expected-for
   "TestSimple_longer_package_name"))

(deftest (collect-traces-testsimple-whileforifprint :long-running) ()
  "Similar to `collect-traces-testsimple', but with larger trace file."
  (is-exact-output-of-collect-traces-expected-for
   "TestSimple_WhileForIfPrint"
   '((:TRACE
      ((:C . 1)  (:SCOPES))
      ((:C . 2)  (:SCOPES . (("test1" "int" 0))))
      ((:C . 3)  (:SCOPES . (("test1" "int" 0))))
      ((:C . 4)  (:SCOPES . (("test1" "int" 0))))
      ((:C . 3)  (:SCOPES . (("test1" "int" 1))))
      ((:C . 4)  (:SCOPES . (("test1" "int" 1))))
      ((:C . 3)  (:SCOPES . (("test1" "int" 2))))
      ((:C . 4)  (:SCOPES . (("test1" "int" 2))))
      ((:C . 5)  (:SCOPES . (("test1" "int" 3))))
      ((:C . 6)  (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 7)  (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 8)  (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 7)  (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 8)  (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 9)  (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 10) (:SCOPES . (("k" "int" 1)("test1" "int" 3)
                             ("test2" "int" 30))))
      ((:C . 11) (:SCOPES . (("k" "int" 2)("test1" "int" 3))))
      ((:C . 12) (:SCOPES . (("k" "int" 2)("test1" "int" 3))))
      ((:C . 13) (:SCOPES . (("k" "int" 2)("test1" "int" 3)))))
     (:INPUT :BIN))))

(deftest (collect-traces-testsimple-whileforifprint-2 :long-running) ()
  "Similar to `collect-traces-testsimple', but with larger trace file."
  (is-exact-output-of-collect-traces-expected-for
   "TestSimple_WhileForIfPrint_2"
   '((:TRACE
      ((:C . 1) (:SCOPES))
      ((:C . 2) (:SCOPES . (("test1" "int" 0))))
      ((:C . 3) (:SCOPES . (("test1" "int" 0))))
      ((:C . 4) (:SCOPES . (("test1" "int" 0))))
      ((:C . 3) (:SCOPES . (("test1" "int" 1))))
      ((:C . 4) (:SCOPES . (("test1" "int" 1))))
      ((:C . 3) (:SCOPES . (("test1" "int" 2))))
      ((:C . 4) (:SCOPES . (("test1" "int" 2))))
      ((:C . 5) (:SCOPES . (("test1" "int" 3))))
      ((:C . 6) (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 7) (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 6) (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 7) (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 8) (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 9) (:SCOPES . (("k" "int" 1)("test1" "int" 3)
                            ("test2" "int" 30)))))
     (:INPUT :BIN))))

(defixture java-project
  (:setup
   (setf *soft*
         (with-warnings-as-notes 3
           (from-file
            (make-instance 'java-project
              :build-command "./gt-harness.sh build"
              :artifacts
              (list (format nil "target/~
                          simpleMultifileMaven-1.~
                          0-SNAPSHOT-jar-with-dependencies.jar")))
            (make-pathname :directory +maven-prj-dir+)))))
  (:teardown
   (setf *soft* nil)))

(deftest (java-project-test :long-running) ()
  (with-fixture java-project
    (is (equal "./gt-harness.sh build" (build-command *soft*)))
    (is (equal 2 (length (evolve-files *soft*))))
    (is (find "src/main/java/com/simple/multi/maven/app/App.java"
              (evolve-files *soft*) :key #'car :test #'string=))
    (is (find "src/main/java/com/simple/multi/maven/extras/SharedClassImpl.java"
              (evolve-files *soft*) :key #'car :test #'string=))
    (is (equal "java" (compiler (cdr (first (evolve-files *soft*))))))))

(deftest (java-build-folder-jar-test :long-running) ()
  "Tests if applicable file names in a build-folder are found."
  (with-temp-dir-of (temp-dir) (make-pathname :directory +java-jars-dir+)
                    (is (equal 9 (length (get-files-jar temp-dir))))))


;;;; Javascript representation.
(defun acorn-available-p ()
  (which "acorn"))

(defsuite javascript-tests "Javascript representation." (acorn-available-p))

(deftest simply-able-to-load-a-javascript-software-object ()
  (with-fixture hello-world-javascript
    (is (not (null *soft*)))))

(deftest (can-parse-a-javascript-software-object :long-running) ()
  (with-fixture hello-world-javascript
    (is (= 6 (length (asts *soft*))))
    (is (equal (file-to-string (javascript-dir #P"hello-world/hello-world.js"))
               (genome *soft*))))
  (with-fixture fib-javascript
    (is (= 40 (length (asts *soft*))))
    (is (equal (file-to-string (javascript-dir #P"fib/fib.js"))
               (genome *soft*)))))

(deftest can-parse-a-json-software-object ()
  (with-fixture trivial-json
    (is (not (null (asts *soft*))))
    (is (eq :OBJECTEXPRESSION
            (ast-class (ast-node (ast-root *soft*)))))))

(deftest cut-shortens-a-javascript-software-object ()
  (with-fixture fib-javascript
    (let* ((variant (copy *soft*))
           (stmt1 (stmt-with-text variant "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (not (equal (genome variant)
                      (genome *soft*))))
      (is (< (size variant)
             (size *soft*))))))

(deftest insert-lengthens-a-javascript-software-object ()
  (with-fixture fib-javascript
    (let* ((variant (copy *soft*))
           (stmt1 (stmt-with-text variant "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-insert
                        :targets (list (cons :stmt1 stmt1)
                                       (cons :value1 stmt1))))
      (is (not (equal (genome variant)
                      (genome *soft*))))
      (is (> (size variant)
             (size *soft*))))))

(deftest swap-changes-a-javascript-software-object ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;"))
          (stmt2 (stmt-with-text *soft* "num--;")))
      (apply-mutation variant
                      (make-instance 'parseable-swap
                        :targets (list (cons :stmt1 stmt1)
                                       (cons :stmt2 stmt2))))
      (is (not (equal (genome variant)
                      (genome *soft*))))
      (is (= (size variant)
             (size *soft*))))))

(deftest javascript-copies-are-independent ()
  (with-fixture fib-javascript
    (let ((orig-genome (genome *soft*))
          (variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (string= (genome *soft*) orig-genome))
      (is (not (string= (genome variant) orig-genome))))))

(deftest javascript-copy-clears-genome-slot ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*)))
      (is (null (slot-value (copy *soft*) 'genome)))
      (is (string= (genome *soft*)
                   (genome variant))))))

(deftest javascript-copies-share-asts ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*)))
      (is (ast-equal-p (ast-root *soft*)
                       (ast-root variant)))
      (is (> (size variant) 0)))))

(deftest javascript-mutation-preserves-unmodified-subtrees ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (ast-equal-p (stmt-with-text *soft* "num--;")
                       (stmt-with-text variant "num--;"))))))

(deftest javascript-parse-source-snippet-works ()
  (is (equal 1 (length (parse-source-snippet :javascript "j = 0")))))

(deftest (can-format-a-javascript-software-object :long-running) ()
  (with-fixture fib-javascript
    (is (not (string= (genome (copy *soft*))
                      (genome (prettier (copy *soft*))))))
    (is (not (string= (genome (copy *soft*))
                      (genome (format-genome (copy *soft*))))))
    (is (string= (genome (prettier (copy *soft*)))
                 (genome (format-genome (copy *soft*)))))))

(deftest javascript-can-rebind-vars ()
  (with-fixture fib-javascript
    (is (string= "temp = b;"
                 (trim-whitespace
                  (->> (rebind-vars (stmt-with-text *soft* "temp = a;")
                                    (list (list "a" "b"))
                                    nil)
                    (source-text)
                    (peel-bananas)))))))

(deftest javascript-get-vars-in-scope ()
  (with-fixture fib-javascript
    (is (equal (list "temp" "b" "a" "num")
               (->> (stmt-with-text *soft* "temp = a;")
                    (get-vars-in-scope *soft*)
                    (mapcar {aget :name}))))))

(deftest javascript-get-unbound-vals ()
  (with-fixture fib-javascript
    (is (equal `((:name . "temp") (:name . "a"))
               (->> (stmt-with-text *soft* "temp = a;")
                    (get-unbound-vals *soft*))))))

(deftest javascript-get-unbound-funs ()
  (with-fixture fib-javascript
    (is (equal `(("fibonacci" nil nil 1))
               (->> (stmt-with-text *soft* "fibonacci(10);")
                    (get-unbound-funs *soft*))))))

(deftest (javascript-instrument-and-collect-traces :long-running) ()
  (with-fixture fib-javascript
    (let ((instrumented (instrument *soft*)))
      (collect-traces instrumented
                      (make-instance 'test-suite
                        :test-cases (list (make-instance 'test-case
                                            :program-name "node"
                                            :program-args (list :bin)))))
      (is (equal 1 (n-traces (traces instrumented))))
      (is (equal '((:TRACE ((:C . 0))  ((:C . 36)) ((:C . 3))
                           ((:C . 12)) ((:C . 17)) ((:C . 21))
                           ((:C . 27)) ((:C . 31)) ((:C . 17))
                           ((:C . 21)) ((:C . 27)) ((:C . 31))
                           ((:C . 17)) ((:C . 21)) ((:C . 27))
                           ((:C . 31)) ((:C . 17)) ((:C . 21))
                           ((:C . 27)) ((:C . 31)) ((:C . 17))
                           ((:C . 21)) ((:C . 27)) ((:C . 31))
                           ((:C . 17)) ((:C . 21)) ((:C . 27))
                           ((:C . 31)) ((:C . 17)) ((:C . 21))
                           ((:C . 27)) ((:C . 31)) ((:C . 17))
                           ((:C . 21)) ((:C . 27)) ((:C . 31))
                           ((:C . 17)) ((:C . 21)) ((:C . 27))
                           ((:C . 31)) ((:C . 17)) ((:C . 21))
                           ((:C . 27)) ((:C . 31)) ((:C . 34)))
                   (:INPUT "node" :BIN))
                 (get-trace (traces instrumented) 0))))))

(deftest (javascript-instrument-and-collect-traces-with-vars :long-running) ()
  (with-fixture fib-javascript
    (let ((instrumented
            (instrument *soft*
                        :functions
                        (list (lambda (instrumenter ast)
                                (var-instrument
                                  {get-vars-in-scope (software instrumenter)}
                                  instrumenter
                                  ast))))))
      (collect-traces instrumented
                      (make-instance 'test-suite
                        :test-cases (list (make-instance 'test-case
                                            :program-name "node"
                                            :program-args (list :bin)))))
      (is (equalp 1 (n-traces (traces instrumented))))
      (is (equalp '((:C . 21)(:SCOPES #("temp" "number" 1 nil)
                                      #("b" "number" 0 nil)
                                      #("a" "number" 1 nil)
                                      #("num" "number" 10 nil)))
                  (nth 5 (aget :trace (get-trace (traces instrumented) 0))))))))

(deftest (javascript-parsing-test :long-running) ()
  (labels ((parse-test (path parsing-mode &rest ast-classes)
             (let ((soft (from-file (make-instance 'javascript
                                      :parsing-mode parsing-mode)
                                    (javascript-dir path))))
               (is (not (null (asts soft))))
               (is (equal (genome soft) (file-to-string (javascript-dir path))))
               (mapc (lambda (ast-class)
                       (is (find ast-class (asts soft) :key #'ast-class)))
                     ast-classes))))
    (mapc {apply #'parse-test}
          '((#P"parsing/array-destructuring.js"
             :script :ArrayPattern)
            (#P"parsing/arrow-function-expression.js"
             :script :ArrowFunctionExpression)
            (#P"parsing/await-expression.js"
             :script :AwaitExpression)
            (#P"parsing/class-declaration.js"
             :script :ClassDeclaration)
            (#P"parsing/class-expression.js"
             :script :ClassExpression)
            (#P"parsing/conditional-expression.js"
             :script :ConditionalExpression)
            (#P"parsing/debugger-statement.js"
             :script :DebuggerStatement)
            (#P"parsing/empty-statement.js"
             :script :EmptyStatement)
            (#P"parsing/export-specifier.js"
             :module :ExportSpecifier)
            (#P"parsing/expression-statement.js"
             :script :ExpressionStatement)
            (#P"parsing/function-declaration.js"
             :script :FunctionDeclaration)
            (#P"parsing/function-expression.js"
             :script :FunctionExpression)
            (#P"parsing/if.js"
             :script :IfStatement)
            (#P"parsing/import-specifier.js"
             :module :ImportSpecifier)
            (#P"parsing/labeled-statement.js"
             :script :LabeledStatement)
            (#P"parsing/loops.js"
             :script :ForStatement :ForInStatement :ForOfStatement
             :WhileStatement :DoWhileStatement)
            (#P"parsing/new-expression.js"
             :script :NewExpression)
            (#P"parsing/object-destructuring.js"
             :script :ObjectPattern)
            (#P"parsing/object-expression.js"
             :script :ObjectExpression)
            (#P"parsing/property.js"
             :script :Property)
            (#P"parsing/sequence-expression.js"
             :script :SequenceExpression)
            (#P"parsing/spread-element.js"
             :script :SpreadElement)
            (#P"parsing/switch.js"
             :script :SwitchStatement)
            (#P"parsing/tagged-template-expression.js"
             :script :TaggedTemplateExpression)
            (#P"parsing/try-catch-throw.js"
             :script :TryStatement :CatchClause :ThrowStatement)
            (#P"parsing/with-statement.js"
             :script :WithStatement)
            (#P"parsing/yield-expression.js"
             :script :YieldExpression)))))

(deftest array-destructuring-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/array-destructuring.js"))))
    (is (equal (list "d" "c" "b" "a" "arr")
               (->> (asts soft)
                    (remove-if-not {traceable-stmt-p soft})
                    (lastcar)
                    (get-vars-in-scope soft)
                    (mapcar {aget :name}))))))

(deftest object-destructuring-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/object-destructuring.js"))))
    (is (equal (list "q" "p" "o")
               (->> (asts soft)
                    (remove-if-not {traceable-stmt-p soft})
                    (lastcar)
                    (get-vars-in-scope soft)
                    (mapcar {aget :name}))))))

(deftest for-in-loop-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/loops.js"))))
    (is (find "i" (->> (stmt-with-text soft "console.log(arr[i]);")
                       (get-vars-in-scope soft)
                       (mapcar {aget :name}))
              :test #'equal))))

(deftest for-of-loop-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/loops.js"))))
    (is (find "val" (->> (stmt-with-text soft "console.log(val);")
                         (get-vars-in-scope soft)
                         (mapcar {aget :name}))
              :test #'equal))))

(deftest javascript.newline.post-processing.1 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "") nil)
      "position-after-leading-newline on empty string"))

(deftest javascript.newline.post-processing.2 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "x") nil)
      "position-after-leading-newline on string with no whitespace or newline"))

(deftest javascript.newline.post-processing.3 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "   ") nil)
      "position-after-leading-newline on string with whitespace only, no newline"))

(deftest javascript.newline.post-processing.4 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline " x") nil)
      "position-after-leading-newline on string with whitespace, no newline"))

(deftest javascript.newline.post-processing.5 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline (string #\Newline)) 1)
      "position-after-leading-newline on newline"))

(deftest javascript.newline.post-processing.6 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               (concatenate 'string (string #\Newline) "x"))
              1)
      "position-after-leading-newline on newline + other stuff"))

(deftest javascript.newline.post-processing.7 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               (concatenate 'string (string #\Newline) "// foo "))
              1)
      "position-after-leading-newline on newline, comment"))

(deftest javascript.newline.post-processing.8 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               (concatenate 'string  "// foo " (string #\Newline) "   "))
              8)
      "position-after-leading-newline on comment, newline "))

(deftest javascript.newline.post-processing.9 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
                "  // foo ")
              nil)
      "position-after-leading-newline on comment"))

(deftest javascript.newline.post-processing.10 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "/")
              nil)
      "position-after-leading-newline slash at EOL not a comment"))

(deftest javascript.newline.post-processing.11 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline " / ")
              nil)
      "position-after-leading-newline slash not at EOL not a comment"))

(defun to-js-ast (tree)
  (labels ((to-js-ast- (tree)
             (assert (or (stringp tree)
                         (and (listp tree) (keywordp (car tree))))
                     (tree)
                     "Every subtree must be a string or start with AST keyword")
             (nest
              (if (stringp tree)
                  tree)
              (case (car tree)
                (:j (make-javascript-ast
                     :node (make-javascript-ast-node)
                     :children (mapcar #'to-js-ast- (cdr tree))))
                (:c (make-conflict-ast
                     :child-alist (mapcar
                                   (lambda (pair)
                                     (destructuring-bind (key . value) pair
                                       (cons key (when value
                                                   (to-js-ast- value)))))
                                   (second tree))
                     :children (mapcar #'to-js-ast- (cddr tree))))))))
    (to-js-ast- tree)))

(defixture javascript-ast-w-conflict
  (:setup
    (setf *soft* (make-instance 'javascript
                   :ast-root (to-js-ast '(:j "top"
                              (:j "left"
                               (:c ((:old . nil) (:my . "a") (:your . "b"))))
                              (:j "right"))))))
  (:teardown
    (setf *soft* nil)))

(deftest javascript-and-conflict-basic-parseable-ast-functionality ()
  (with-fixture javascript-ast-w-conflict
    (is (javascript-ast-p (ast-root *soft*)))         ; We actually have ASTs.
    (is (every #'ast-path (cdr (asts *soft*))))       ; Non-root ast have path.
    (is (javascript-ast-p (copy (ast-root *soft*))))) ; Copy works.
  (with-fixture javascript-ast-w-conflict
    ;; Access ASTs.
    (is (string= "top" (get-ast *soft* '(0))))
    (is (javascript-ast-p (get-ast *soft* '(1))))
    (is (string= "left" (get-ast *soft* '(1 0))))
    (is (javascript-ast-p (get-ast *soft* '(2))))
    (is (string= "right" (get-ast *soft* '(2 0))))
    ;; Set AST with (setf (get-ast ...) ...).
    (setf (get-ast *soft* '(2 0)) "RIGHT")
    (is (string= "RIGHT" (get-ast *soft* '(2 0))))
    (setf (get-ast *soft* '(1)) (make-javascript-ast
                                  :node (make-javascript-ast-node :class :foo)))
    (is (eql :foo (ast-class (get-ast *soft* '(1))))))
  (with-fixture javascript-ast-w-conflict
    (replace-ast *soft* '(1)
                 (make-javascript-ast
                  :node (make-javascript-ast-node :class :foo)))
    (is (eql :foo (ast-class (get-ast *soft* '(1)))))))

(deftest javascript-and-conflict-replace-ast ()
  (with-fixture javascript-ast-w-conflict
    (let ((cnf (find-if {typep _ 'conflict-ast} ; [{subtypep _ 'conflict-ast} #'type-of]
                        (asts *soft*))))
      (setf (get-ast *soft* (ast-path cnf))
            (aget :my (conflict-ast-child-alist cnf))))

    (is (equalp (mapc-ast (ast-root *soft*) #'ast-path)
                '(NIL ((1)) ((2)))))
    (is (string= (ast-text (ast-root *soft*)) "topleftaright"))))


;;;; Javascript project.
(defun npm-available-p ()
  (which "npm"))

(defsuite javascript-project-tests "Javascript project."
  (npm-available-p))

(deftest (can-parse-a-javascript-project :long-running) ()
  (with-fixture fib-project-javascript
    (is (equal 2 (length (evolve-files *soft*))))
    (is (not (null (asts *soft*))))))

(deftest (javascript-project-instrument-uninstrument-is-identity
          :long-running) ()
  (with-fixture fib-project-javascript
    (is (string= (genome *soft*)
                 (genome (uninstrument (instrument (copy *soft*))))))))

(deftest (javascript-project-instrument-and-collect-traces :long-running) ()
  (with-fixture fib-project-javascript
    (let ((instrumented (instrument *soft*)))
      (collect-traces instrumented
                      (make-instance 'test-suite :test-cases
                        (list (make-instance 'test-case
                                :program-name (namestring
                                                (javascript-dir
                                                 #P"fib-project/test.sh"))
                                :program-args (list :bin "1")))))
      (is (equal 1 (n-traces (traces instrumented))))
      (is (equalp '(((:C . 0)  (:F . 1))
                    ((:C . 0)  (:F . 0))
                    ((:C . 6)  (:F . 1))
                    ((:C . 12) (:F . 1))
                    ((:C . 29) (:F . 1))
                    ((:C . 48) (:F . 1))
                    ((:C . 55) (:F . 1))
                    ((:C . 70) (:F . 1))
                    ((:C . 11) (:F . 0))
                    ((:C . 20) (:F . 0))
                    ((:C . 25) (:F . 0))
                    ((:C . 29) (:F . 0))
                    ((:C . 35) (:F . 0))
                    ((:C . 39) (:F . 0))
                    ((:C . 42) (:F . 0)))
                  (aget :trace (get-trace (traces instrumented) 0)))))))

(deftest (javascript-project-instrument-and-collect-traces-with-vars
          :long-running) ()
  (with-fixture fib-project-javascript
    (let ((instrumented
            (instrument *soft*
                        :functions
                        (list (lambda (instrumenter ast)
                                (var-instrument
                                  {get-vars-in-scope (software instrumenter)}
                                  instrumenter
                                  ast))))))
      (collect-traces instrumented
                      (make-instance 'test-suite :test-cases
                        (list (make-instance 'test-case
                                :program-name (namestring
                                                (javascript-dir
                                                 #P"fib-project/test.sh"))
                                :program-args (list :bin "1")))))
      (is (equal 1 (n-traces (traces instrumented))))
      (is (equalp '((:C . 29)(:F . 0)(:SCOPES #("temp" "number" 1 nil)
                                              #("b" "number" 0 nil)
                                              #("a" "number" 1 nil)
                                              #("num" "number" 1 nil)))
                  (nth 11 (aget :trace (get-trace (traces instrumented) 0))))))))


;;;; Range representation.
(defsuite range-representation "Range representation.")


(deftest range-size ()
  (with-fixture range (is (= 6 (size *soft*)))))

(deftest range-lines ()
  (with-fixture range
    (is (tree-equal (lines *soft*)
                    '("one" "two" "three" "two" "two" "three")
                    :test #'string=))))

(deftest range-nth-test ()
  (with-fixture range
    (is (equal (mapcar {range-nth _ (genome *soft*)}
                       (loop :for i :below (size *soft*) :collect i))
               '(0 1 2 1 1 2)))))

(deftest range-subseq-test ()
  (with-fixture range
    ;; to
    (is (tree-equal (range-subseq (genome *soft*) 0 1)
                    '((0 . 0))))
    (is (tree-equal (range-subseq (genome *soft*) 0 2)
                    '((0 . 1))))
    (is (tree-equal (range-subseq (genome *soft*) 0 3)
                    '((0 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 0 4)
                    '((0 . 2) (1 . 1))))
    (is (tree-equal (range-subseq (genome *soft*) 0 5)
                    '((0 . 2) (1 . 1) (1 . 1))))
    (is (tree-equal (range-subseq (genome *soft*) 0 6)
                    '((0 . 2) (1 . 1) (1 . 2))))
    ;; from
    (is (tree-equal (range-subseq (genome *soft*) 1 7)
                    '((1 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 2 7)
                    '((2 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 3 7)
                    '((1 . 1) (1 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 4 7)
                    '((1 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 5 7)
                    '((2 . 2))))
    (is (tree-equal (range-subseq (genome *soft*) 6 7)
                    'NIL))
    ;; both
    (is (tree-equal (range-subseq (genome *soft*) 2 5)
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
(defsuite mutation-analysis
            "Mutation analysis and statistics collection tests."
            (clang-mutate-available-p))


(defvar *test* nil "Variable to hold evaluation function for tests.")

(defixture hello-world-clang-w-fitness
  (:setup
   (setf *hello-world*
         (from-file (make-clang :compiler "clang"
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
                            (:literal1 . ,(make-literal 0))))))
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

(locally (declare
          #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note style-warning))
  ;; The above `sb-ext:muffle-conditions' helps us ignore a "Cannot
  ;; find a type specializer for cut-and-swap" style warning.  Longer
  ;; term it would be better to update the `compose-mutations' macro
  ;; so that this warning is not triggered.
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
      (let (variant op)
        ;; Multiple tries to get around stochastic failures.
        ;; The mutation may make random choices which fail the test.
        (is (with-retries (100)
              (setf variant (copy *hello-world*))
              (setf op (make-instance 'swap-and-cut :object variant))
              (apply-mutation variant op)
              (when (and (different-asts (asts variant)
                                         (asts *hello-world*))
                         (not (equal (genome variant)
                                     (genome *hello-world*)))
                         (< (size variant)
                            (size *hello-world*)))
                (return t))))))))


;;;; Ancestry tests.
(defsuite clang-ancestry "Ancestry tests."
            (clang-mutate-available-p))


(defclass clang-w-ancestry (clang ancestral) ())

(defixture hello-world-clang-w-ancestry
  (:setup
   (setf sel/sw/ancestral::*next-ancestry-id* 0
         *hello-world*
         (from-file (make-instance 'clang-w-ancestry :compiler "clang"
                                   :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))
         *test* [#'length #'genome])
   (evaluate *test* *hello-world*))
  (:teardown
   (setf *hello-world* nil
         *test* nil
         *next-ancestry-id* 0)))

(deftest apply-mutation-logs-ancestry ()
  (with-fixture hello-world-clang-w-ancestry
    (let ((op (make-instance 'clang-cut
                :object *hello-world*
                :targets `((:stmt1 . ,(stmt-with-text *hello-world*
                                                      "return 0;"))))))
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
      (is (= 0
             (plist-get :id
                        (first (plist-get :cross-with
                                          (first (ancestors crossed)))))))
      (is (equal
           'from-file
           (plist-get :how
                      (first (plist-get :cross-with
                                        (first (ancestors crossed))))))))))

(deftest (graphing-ancestry :long-running) ()
  (with-fixture hello-world-clang-w-ancestry
    (apply-mutation *hello-world*
      (make-instance 'clang-cut
        :object *hello-world*
        :targets `((:stmt1 . ,(stmt-with-text *hello-world*
                                              "return 0;")))))
    (with-temp-file (save-base)
      (multiple-value-bind (stdout stderr errno)
          (save-ancestry *hello-world*
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
        (is (zerop errno))))))


;;; CSURF-ASM ancestry tests.
(defsuite csurf-asm-ancestry "Ancestry tests.")


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


;;;; Diff tests.
(defsuite diff-tests "Diff tests.")


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
(defsuite population-tests "Population tests."
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
(defun stmt-with-text (obj text &key no-error (trim t))
  "Return the AST in OBJ holding TEXT.
Unless optional argument NO-ERROR is non-nil an error is raised if no
AST holding STMT is found."
  (when trim
    (setf text (trim-whitespace text)))
  (or (let ((result
             (find-if [{string= text} (if trim #'trim-whitespace #'identity)
                       #'peel-bananas #'source-text]
                      (asts obj))))
        result)
      (if no-error
          nil
          (error "`stmt-with-text' failed to find ~S in ~S"
		 text
		 (mapcar [#'peel-bananas #'source-text] (asts obj))))))

(defun stmt-starting-with-text (obj text)
  (find-if (lambda (ast)
             (and ast
                  (equal 0
                         (search text
                                 (peel-bananas (source-text ast))))))
           (asts obj)))

(deftest (swap-can-recontextualize :long-running) ()
  (with-fixture huf-clang
    (let ((mut (make-instance 'clang-swap :targets
                              (list (cons :stmt1 (stmt-with-text *huf* "n > 0"))
                                    (cons :stmt2 (stmt-with-text *huf* "bc=0;"))))))
      (is (with-retries (100)
            ;; NOTE: Without the retries recontectualization can fail
            ;;       stochastically, not all possible rebinding
            ;;       variables work.
            ;;
            ;; TODO: Refine recontextualization with type information
            ;;       so that this *always* works.
            (for var = (apply-mutation (copy *huf*) mut))
            (when (phenome-p var) (return t)))
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

(deftest (swap-makes-expected-change :long-running) ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*))
          (text-1 "n > 0")
          (text-2 "bc=0;"))
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

(deftest (swap-at-different-levels-can-recontextualize :long-running) ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*))
          (text-1 "n > 0")
          (text-2 "bn++;"))
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

(deftest (insert-can-recontextualize :long-running) ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons 'clang-insert
              (list (cons :stmt1
                          (stmt-with-text variant "bc=0;"))
                    (cons :stmt2
                          (stmt-with-text variant "n > 0")))))
      (is (phenome-p variant)))))

(deftest (insert-makes-expected-change :long-running) ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons 'clang-insert
              (list (cons :stmt1 (stmt-with-text variant "bc=0;"))
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
  (let* ((obj (make-clang
                :genome "void test(int x);
                          int main(int argc, char **argv) {
                            test(0); return 0;
                           }"))
         (stmt (stmt-with-text obj "test(0);"))
         (*matching-free-function-retains-name-bias* 1.0))
    (apply-mutation obj
      `(clang-replace (:stmt1 . ,stmt) (:stmt2 . ,stmt)))
    (is (string= (source-text stmt)
                 (source-text (stmt-with-text obj "test(0);"))))))

;; huf.c only contains one user function with 3 parameters,
;; check that random-function-name can find it.
(deftest (finds-function-binding :long-running) ()
  (with-fixture huf-clang
    (is (string= "inttobits"
                 (random-function-name (functions *huf*)
                                       :original-name "foo"
                                       :arity 3)))))


;;;; Fix compilation tests.
(defsuite fix-compilation-tests "Fix compilation tests."
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
                (with-open-file (in (make-pathname :name "euler-example"
                                                   :type "json"
                                                   :directory +etc-dir+))
                  (make-instance 'json-database :json-stream in))))
  (:teardown (setf *database* nil)))

(deftest (fix-compilation-inserts-missing-include :long-running) ()
  (with-fixture broken-compilation
    (is (scan (format nil "\#include <~a>" "stdio.h")
              (genome-string (fix-compilation *broken-clang* 1)))))
  (with-fixture broken-compilation-gcc
    (is (scan (format nil "\#include <~a>" "stdio.h")
              (genome-string (fix-compilation *broken-gcc* 1))))))

(deftest (fix-compilation-inserts-declaration-and-initializes :long-running) ()
  (let ((*compilation-fixers*
         (remove-if-not
          «or {starts-with-subseq ":(\\d+):\\d+: error: use of undeclared"}
           {starts-with-subseq ":(\\d+):\\d+: error: (‘|')(\\S+)(’|')"}»
           *compilation-fixers*
          :key #'car)))
    (with-fixture broken-compilation
      (is (scan (quote-meta-chars "missing_variable =")
                (genome (fix-compilation *broken-clang* 4)))))
    (with-fixture broken-compilation-gcc
      (is (scan (quote-meta-chars "missing_variable =")
                ;; Without the retries this test can fail stochastically.
                (iter (for fixed =
                           (fix-compilation
                            (handler-bind
                                ((mutate
                                  (lambda (e)
                                    (declare (ignorable e))
                                    (invoke-restart 'keep-partial-asts))))
                              (copy *broken-gcc*))
                            4))
                      (unless (zerop (length (genome fixed)))
                        (return (genome fixed)))))))))

(deftest (fix-compilation-declare-var-as-pointer :long-running) ()
  (let ((*compilation-fixers*
         (remove-if-not
          «or {starts-with-subseq ":(\\d+):(\\d+): error: invalid type arg"}
           {starts-with-subseq ":(\\d+):(\\d+): error: indirection requir"}»
           *compilation-fixers*
          :key #'car)))
    (with-temp-file (genome ".c")
      (string-to-file "int main(int argc, char **argv) {
                        int y = 0;
                        return *y;
                      }"
                      genome)
      (let ((broken-clang (from-file (make-clang
                                       :compiler "clang"
                                       :flags '("-m32" "-O0" "-g"))
                                     genome))
            (broken-gcc   (from-file (make-clang
                                       :compiler "gcc"
                                       :flags '("-m32" "-O0" "-g"))
                                     genome)))
        (is (phenome-p (fix-compilation broken-clang 1)))
        (is (phenome-p (fix-compilation broken-gcc 1)))))))


;;;; Crossover tests.
(defsuite clang-crossover "Crossover tests."
            (clang-mutate-available-p))


(defun select-intraprocedural-pair-with-adjustments-test (obj)
  (let ((function (first (functions obj))))
    (loop :for i :from 0 :to 25
       :do (progn (multiple-value-bind (pt1 pt2)
                      (select-intraprocedural-pair obj)
                    (multiple-value-bind (stmt1 stmt2)
                        (adjust-stmt-range obj pt1 pt2)
                      (is (<= (1+ (first (stmt-range obj function)))
                              stmt1
                              (second (stmt-range obj function))))
                      (is (<= (1+ (first (stmt-range obj function)))
                              stmt2
                              (second (stmt-range obj function))))
                      (is (full-stmt-p obj
                                       (ast-at-index obj stmt1)))
                      (is (full-stmt-p obj
                                       (ast-at-index obj stmt2)))))))))

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
                                 (stmt-with-text *fib* "int t = x;")
                                 (stmt-with-text *fib* "x = x + y;")))
        (equal '(0 . 0)
               (nesting-relation *fib*
                                 (stmt-with-text *fib* "int t = x;")
                                 (stmt-with-text *fib* "int t = x;")))
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
                                 (stmt-with-text *fib* "return x;"))))))

(deftest nesting-relation-increasing-scope-collatz-test ()
  (with-fixture collatz-clang
    (is (equal '(0 . 2)
               (nesting-relation *collatz*
                                 (stmt-with-text *collatz* "int k = 0")
                                 (stmt-with-text *collatz* "m /= 2;"))))))

(deftest nesting-relation-decreasing-scope-collatz-test ()
  (with-fixture collatz-clang
    (is (equal '(2 . 0)
               (nesting-relation *collatz*
                                 (stmt-with-text *collatz* "m /= 2;")
                                 (stmt-with-text *collatz* "return k;"))))))

(deftest nesting-relation-increasing-scope-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal '(0 . 2)
               (nesting-relation *soft*
                                 (stmt-with-text *soft* "int i")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);"))))))

(deftest nesting-relation-decreasing-scope-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal '(2 . 0)
               (nesting-relation *soft*
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);")
                                 (stmt-with-text *soft*
                                                 "return 0;"))))))

(deftest nesting-relation-increasing-scope-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal '(0 . 1)
               (nesting-relation *soft*
                                 (->> "printf(\"%d\\n\", argc);"
                                      (stmt-with-text *soft*))
                                 (->> "printf(\"%d\\n\", argc + argc);"
                                      (stmt-with-text *soft*)))))))

(deftest nesting-relation-decreasing-scope-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal '(1 . 0)
               (nesting-relation *soft*
                                 (->> "printf(\"%d\\n\", argc + argc);"
                                      (stmt-with-text *soft*))
                                 (->> "return 0;"
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
                                 (stmt-with-text *fib* "x = x + y;"))))))

(deftest common-ancestor-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (->> (stmt-starting-with-text *collatz* "int collatz")
                     (function-body *collatz*))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "int k = 0")
                                 (stmt-with-text *collatz* "return k;"))))
    (is (equalp (->> (stmt-starting-with-text *collatz* "int collatz")
                     (function-body *collatz*))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "int k = 0")
                                 (stmt-with-text *collatz* "m /= 2;"))))
    (is (equalp (->> (stmt-starting-with-text *collatz* "int collatz")
                     (function-body *collatz*))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "m /= 2;")
                                 (stmt-with-text *collatz* "return k;"))))
    (is (equalp (stmt-starting-with-text *collatz* "if")
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "m /= 2;")
                                 (stmt-with-text *collatz* "m = 3*m + 1;"))))
    (is (equalp (->> (stmt-starting-with-text *collatz* "while")
                     (get-immediate-children *collatz*)
                     (second))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "m /= 2;")
                                 (stmt-with-text *collatz* "++k;"))))
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
                                 (stmt-with-text *soft* "return 0;"))))
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (common-ancestor *soft*
                                 (stmt-with-text *soft*
                                                 "int i")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (common-ancestor *soft*
                                 (stmt-starting-with-text *soft* "for (i = 0")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (common-ancestor *soft*
                                 (stmt-starting-with-text *soft* "for (j = 0")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);"))))))

(deftest common-ancestor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (common-ancestor *soft*
                                 (->> "printf(\"%d\\n\", argc);"
                                      (stmt-with-text *soft*))
                                 (->> "printf(\"%d\\n\", argc + argc);"
                                      (stmt-with-text *soft*)))))
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (common-ancestor *soft*
                                 (->> "printf(\"%d\\n\", argc + argc);"
                                      (stmt-with-text *soft*))
                                 (->> "return 0;"
                                      (stmt-with-text *soft*)))))
    (is (equalp (->> (stmt-starting-with-text *soft* "switch")
                     (get-immediate-children *soft*)
                     (second))
                (common-ancestor *soft*
                                 (->> "printf(\"%d\\n\", argc + argc);"
                                      (stmt-with-text *soft*))
                                 (->> "printf(\"%d\\n\", argc * argc);"
                                      (stmt-with-text *soft*)))))
    (is (equalp (stmt-starting-with-text *soft* "case 1")
                (common-ancestor *soft*
                                 (->> "printf(\"%d\\n\", argc + argc);"
                                      (stmt-with-text *soft*))
                                 (stmt-starting-with-text *soft* "case 1"))))))

(deftest ancestor-after-fib-test ()
  (with-fixture fib-clang
    (is (equalp (stmt-with-text *fib* "int x = 0;")
                (ancestor-after *fib*
                                     (->> "int fib"
                                          (stmt-starting-with-text *fib*)
                                          (function-body *fib*))
                                     (stmt-with-text *fib* "int x = 0;"))))
    (is (equalp (->> (stmt-asts *fib*)
                     (remove-if-not [{eq :WhileStmt} #'ast-class])
                     (first))
                (ancestor-after *fib*
                                     (->> "int fib"
                                          (stmt-starting-with-text *fib*)
                                          (function-body *fib*))
                                     (stmt-with-text *fib* "int t = x;"))))
    (is (equalp (stmt-with-text *fib* "x = x + y;")
                (ancestor-after *fib*
                                     (->> "while "
                                          (stmt-starting-with-text *fib*)
                                          (get-immediate-children *fib*)
                                          (second))
                                     (stmt-with-text *fib* "x = x + y;"))))))

(deftest ancestor-after-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (stmt-with-text *collatz* "int k = 0;")
                (ancestor-after *collatz*
                                     (->> "int collatz"
                                          (stmt-starting-with-text *collatz*)
                                          (function-body *collatz*))
                                     (stmt-with-text *collatz* "int k = 0;"))))
    (is (equalp (stmt-with-text *collatz* "return k;")
                (ancestor-after *collatz*
                                     (->> "int collatz"
                                          (stmt-starting-with-text *collatz*)
                                          (function-body *collatz*))
                                     (stmt-with-text *collatz* "return k;"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (remove-if-not [{eq :WhileStmt} #'ast-class])
                     (first))
                (ancestor-after *collatz*
                                     (->> "int collatz"
                                          (stmt-starting-with-text *collatz*)
                                          (function-body *collatz*))
                                     (stmt-with-text *collatz* "m /= 2;"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (remove-if-not [{eq :IfStmt} #'ast-class])
                     (first))
                (ancestor-after *collatz*
                                     (->> "while"
                                          (stmt-starting-with-text *collatz*)
                                          (get-immediate-children *collatz*)
                                          (second))
                                     (stmt-with-text *collatz* "m /= 2;"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (remove-if-not [{eq :IfStmt} #'ast-class])
                     (first))
                (ancestor-after *collatz*
                                     (->> "while"
                                          (stmt-starting-with-text *collatz*)
                                          (get-immediate-children *collatz*)
                                          (second))
                                     (stmt-with-text
                                      *collatz* "m = 3*m + 1;"))))
    (is (equalp (stmt-with-text *collatz* "++k;")
                (ancestor-after *collatz*
                                     (->> "while"
                                          (stmt-starting-with-text *collatz*)
                                          (get-immediate-children *collatz*)
                                          (second))
                                     (stmt-with-text *collatz* "++k;"))))))

(deftest ancestor-after-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (ancestor-after *soft*
                                     (stmt-starting-with-text
                                      *soft* "for (i = 0")
                                     (stmt-with-text
                                      *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
                (ancestor-after *soft*
                                     (stmt-starting-with-text *soft*
                                                              "for (j = 0")
                                     (stmt-with-text
                                      *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (ancestor-after *soft*
                                     (->> "int main"
                                          (stmt-starting-with-text *soft*)
                                          (function-body *soft*))
                                     (stmt-with-text
                                      *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-with-text *soft* "return 0;")
                (ancestor-after *soft*
                                     (->> "int main"
                                          (stmt-starting-with-text *soft*)
                                          (function-body *soft*))
                                     (stmt-with-text *soft* "return 0;"))))))

(deftest ancestor-after-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "case 2")
                (ancestor-after *soft*
                                     (->> "switch"
                                          (stmt-starting-with-text *soft*)
                                          (get-immediate-children *soft*)
                                          (second))
                                     (->> "printf(\"%d\\n\", argc * argc);"
                                          (stmt-with-text *soft*)))))))

(deftest enclosing-full-stmt-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (stmt-with-text *collatz* "m = 3*m + 1;")
                (enclosing-full-stmt *collatz*
                                     (stmt-with-text *collatz* "3"))))
    (is (equalp (stmt-with-text *collatz* "m = 3*m + 1;")
                (enclosing-full-stmt *collatz*
                                     (stmt-with-text *collatz*
                                                     "m = 3*m + 1;"))))
    (is (equalp (stmt-with-text *collatz* "int k = 0;")
                (enclosing-full-stmt *collatz*
                                     (stmt-with-text
                                      *collatz* "int k = 0;"))))))

(deftest enclosing-full-stmt-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
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
    (is (equalp (stmt-with-text *soft* "int i;")
                (enclosing-full-stmt *soft*
                                     (stmt-with-text *soft* "int i;"))))))

(deftest enclosing-full-stmt-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
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
                                                           "int k = 0;"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (find-if [{eq :WhileStmt} #'ast-class])
                     (get-immediate-children *collatz*)
                     (second))
                (enclosing-block *collatz* (stmt-with-text *collatz* "++k;"))))
    (is (equalp (->> (stmt-asts *collatz*)
                     (find-if [{eq :IfStmt} #'ast-class])
                     (get-immediate-children *collatz*)
                     (second))
                (enclosing-block *collatz*
                                 (stmt-with-text *collatz* "m /= 2;"))))
    (is (null (nest (enclosing-block *collatz*)
                    (function-body *collatz*)
                    (stmt-starting-with-text *collatz* "int collatz"))))))

(deftest (enclosing-block-no-compound-stmt-test :long-running) ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
                     (enclosing-block *soft*))))
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (enclosing-block *soft*
                                 (stmt-starting-with-text *soft*
                                                          "for (i = 0"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (enclosing-block *soft* (stmt-starting-with-text
                                         *soft* "for (j = 0"))))
    (is (null (enclosing-block *soft*
                               (->> (stmt-starting-with-text *soft* "int main")
                                    (function-body *soft*)))))))

(deftest enclosing-block-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (->> (stmt-starting-with-text *soft* "int main")
                     (function-body *soft*))
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
                     (enclosing-block *soft*))))
    (is (equalp (->> (stmt-asts *soft*)
                     (find-if [{eq :SwitchStmt} #'ast-class])
                     (get-immediate-children *soft*)
                     (second))
                (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
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
                                                            "int k = 0;"))
                 (ast-class))))
    (is (eq :ReturnStmt
            (->> (block-successor *collatz*
                                  (stmt-with-text *collatz*
                                                  "printf(\"%d\\n\", k);"))
                 (ast-class))))
    (is (equal nil
               (block-successor *collatz* (stmt-with-text *collatz*
                                                          "m /= 2;"))))
    (is (equal nil
               (block-successor *collatz* (stmt-with-text *collatz*
                                                          "++k;"))))
    (is (equal nil
               (block-successor *collatz* (stmt-with-text *collatz*
                                                          "return k;"))))))

(deftest block-successor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
                    (block-successor *soft*))))
    (is (equal nil
               (block-successor *soft*
                                (stmt-starting-with-text *soft* "for (j = 0"))))
    (is (equalp (stmt-with-text *soft* "return 0;")
                (block-successor *soft* (stmt-starting-with-text
                                         *soft* "for (i = 0"))))))

(deftest block-successor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
                    (block-successor *soft*))))
    (is (equalp (stmt-starting-with-text *soft* "default:")
                (->> (stmt-starting-with-text *soft* "case 2:")
                     (block-successor *soft*))))))

(deftest block-predeccessor-collatz-test ()
  (with-fixture collatz-clang
    (is (equal nil
               (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                             "int k = 0;"))))
    (is (eq :WhileStmt
            (->> (block-predeccessor *collatz*
                                     (stmt-with-text *collatz*
                                                     "printf(\"%d\\n\", k);"))
                 (ast-class))))
    (is (equal nil
               (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                             "m /= 2;"))))
    (is (eq :IfStmt
            (->> (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                               "++k;"))
                 (ast-class))))
    (is (eq :CallExpr
            (->> (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                               "return k;"))
                 (ast-class))))))

(deftest block-predeccessor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
                    (block-predeccessor *soft*))))
    (is (equal nil
               (block-successor *soft*
                                (stmt-starting-with-text *soft* "for (j = 0"))))
    (is (equalp (stmt-with-text *soft* "int j;")
                (block-predeccessor *soft* (stmt-starting-with-text
                                            *soft* "for (i = 0"))))))

(deftest block-predeccessor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal nil
               (->> (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
                    (block-predeccessor *soft*))))
    (is (equalp (stmt-starting-with-text *soft* "case 1:")
                (->> (stmt-starting-with-text *soft* "case 2:")
                     (block-predeccessor *soft*))))))

(deftest (crossover-2pt-outward-fib-test :long-running) ()
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *fib*)
                                         (copy *fib*)
                                         (stmt-with-text *fib* "x = x + y;")
                                         (stmt-with-text *fib* "return x;")
                                         (stmt-with-text *fib* "x = x + y;")
                                         (stmt-with-text *fib* "return x;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "x = x + y;")
                          (stmt-with-text *fib* "return x;"))
                    effective-a-pts)))))
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *fib*)
           (copy *fib*)
           (stmt-with-text *fib* "int t = x;")
           (->> (stmt-asts *fib*)
                (remove-if-not [{eq :WhileStmt}
                                #'ast-class])
                (first))
           (stmt-with-text *fib* "int t = x;")
           (->> (stmt-asts *fib*)
                (remove-if-not [{eq :WhileStmt}
                                #'ast-class])
                (first)))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int t = x;")
                          (->> (stmt-asts *fib*)
                               (remove-if-not [{eq :WhileStmt}
                                               #'ast-class])
                               (first)))
                    effective-a-pts))))))

(deftest (crossover-2pt-outward-collatz-test :long-running) ()
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "m /= 2;")
                                         (stmt-with-text *collatz* "++k;")
                                         (stmt-with-text *collatz* "m /= 2;")
                                         (stmt-with-text *collatz* "++k;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (equalp (cons (stmt-with-text *collatz* "m /= 2;")
                          (stmt-with-text *collatz* "++k;"))
                    effective-a-pts)))))
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "++k;")
                                         (stmt-with-text *collatz* "return k;")
                                         (stmt-with-text *collatz* "++k;")
                                         (stmt-with-text *collatz* "return k;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "++k;")
                          (stmt-with-text *collatz* "return k;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-outward-no-compound-stmt-test :long-running) ()
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
           (stmt-with-text *soft* "return 0;")
           (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-starting-with-text *soft* "for (j = 0")
           (stmt-with-text *soft* "return 0;")
           (stmt-starting-with-text *soft* "for (j = 0")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-starting-with-text *soft* "for (j = 0")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-outward-switch-stmt-test :long-running) ()
  (with-fixture crossover-switch-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
           (stmt-with-text *soft* "return 0;")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-inward-fib-test :long-running) ()
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *fib*)
                                         (copy *fib*)
                                         (stmt-with-text *fib* "int x = 0;")
                                         (stmt-with-text *fib* "return x;")
                                         (stmt-with-text *fib* "int x = 0;")
                                         (stmt-with-text *fib* "return x;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int x = 0;")
                          (stmt-with-text *fib* "return x;"))
                    effective-a-pts)))))
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *fib*)
                                         (copy *fib*)
                                         (stmt-with-text *fib* "int x = 0;")
                                         (stmt-with-text *fib* "x = x + y;")
                                         (stmt-with-text *fib* "int x = 0;")
                                         (stmt-with-text *fib* "x = x + y;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int x = 0;")
                          (stmt-with-text *fib* "x = x + y;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-inward-collatz-test :long-running) ()
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz* "return k;")
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz* "return k;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "int k = 0;")
                          (stmt-with-text *collatz* "return k;"))
                    effective-a-pts)))))
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz*
                                                         "m = 3*m + 1;")
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz*
                                                         "m = 3*m + 1;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "int k = 0;")
                          (stmt-with-text *collatz* "m = 3*m + 1;"))
                    effective-a-pts)))))
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz* "++k;")
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz* "++k;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "int k = 0;")
                          (stmt-with-text *collatz* "++k;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-inward-no-compound-stmt-test :long-running) ()
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "int i;")
           (stmt-with-text *soft* "return 0;")
           (stmt-with-text *soft* "int i;")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i;")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "int i;")
           (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
           (stmt-with-text *soft* "int i;")
           (stmt-with-text *soft* "printf(\"%d\\n\", i+j);"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i;")
                          (stmt-with-text *soft* "printf(\"%d\\n\", i+j);"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "int i;")
           (stmt-starting-with-text *soft* "for (i = 0")
           (stmt-with-text *soft* "int i;")
           (stmt-starting-with-text *soft* "for (i = 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i;")
                          (stmt-starting-with-text *soft* "for (i = 0"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "int i;")
           (stmt-starting-with-text *soft* "for (j = 0")
           (stmt-with-text *soft* "int i;")
           (stmt-starting-with-text *soft* "for (j = 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i;")
                          (stmt-starting-with-text *soft* "for (j = 0"))
                    effective-a-pts))))))

(deftest (crossover-2pt-inward-switch-stmt-test :long-running) ()
  (with-fixture crossover-switch-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc * argc);")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc * argc);"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft*
                                          "printf(\"%d\\n\", argc + argc);")
                          (stmt-with-text *soft*
                                          "printf(\"%d\\n\", argc * argc);"))
                    effective-a-pts)))))
  (with-fixture crossover-switch-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
           (stmt-with-text *soft* "return 0;")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts))))))

(deftest (basic-2pt-crossover-works :long-running) ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int d;"))
           (a-stmt2 (stmt-with-text *scopes* "d = 5;"))
           (b-stmt1 (stmt-with-text *scopes* "int e;"))
           (b-stmt2 (stmt-with-text *scopes* "c = 10;"))
           (target-a-pts (cons a-stmt1 a-stmt2)))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (equalp effective-a-pts target-a-pts))))))

(deftest (crossover-can-match-nesting :long-running) ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "--d;"))
           (a-stmt2 (stmt-with-text *scopes* "int e;"))
           (b-stmt1 (stmt-with-text *scopes* "c = 6;"))
           (b-stmt2 (stmt-with-text *scopes* "e = 8;"))
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
    (let* ((a-stmt1 (stmt-with-text *scopes* "--d;"))
           (a-stmt2 (stmt-with-text *scopes* "int e;"))
           (b-stmt1 (stmt-with-text *scopes* "a = 13;"))
           (b-stmt2 (stmt-with-text *scopes* "c = 15;"))
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

(deftest (crossover-can-rebind-text :long-running) ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int b;"))
           (a-stmt2 (stmt-with-text *scopes* "int c;"))
           (b-stmt1 (stmt-with-text *scopes* "a = 13;"))
           (b-stmt2 (stmt-with-text *scopes* "a = 13;")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant)))))
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int b;"))
           (a-stmt2 (stmt-with-text *scopes* "int c;"))
           (b-stmt1 (stmt-with-text *scopes* "d = 5;"))
           (b-stmt2 (stmt-with-text *scopes* "--d;")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))))))

(deftest (crossover-entire-text-of-a-function :long-running) ()
  ;; Entire text of a function
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int a;"))
           (a-stmt2 (stmt-with-text *scopes* "return a + b + c;"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (is (with-retries (100)
            ;; NOTE: Without the retries recontectualization can fail
            ;;       stochastically, not all possible rebinding
            ;;       variables work.
            ;;
            ;; TODO: Refine recontextualization with type information
            ;;       so that this *always* works.
            (for (values variant a-pts b-pts ok effective-a-pts) =
                 (intraprocedural-2pt-crossover *scopes* *scopes*
                                                a-stmt1 a-stmt2
                                                b-stmt1 b-stmt2))
            (declare (ignorable a-pts b-pts effective-a-pts))
            (when (and ok
                       (phenome-p variant)
                       (= (length (asts *scopes*))
                          (length (asts variant))))
              (return t)))
          "Able to crossover entire text of a function with 100 tries."))))

(deftest (crossover-a-single-statement-the-first :long-running) ()
  ;; A single statement (the first one)
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int a;"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant))))))))

(deftest (crossover-a-single-statement-the-last :long-running) ()
  ;; A single statement (the last one)
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "return a + b + c;"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant))))))))

(deftest (crossover-a-single-statement-complex :long-running) ()
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
        (is (phenome-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant))))))))

(deftest (crossover-a-statement-and-descendants :long-running) ()
  ;; A statement and one of its descendants
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (a-stmt2 (stmt-with-text *scopes* "c = 4;"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant))))))))

(deftest (crossover-a-statement-and-one-ancestor :long-running) ()
  ;; A statement and one of its ancestors
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "c = 4;"))
           (a-stmt2 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (is (with-retries (100)
            ;; NOTE: Without the retries recontectualization can fail
            ;;       stochastically, not all possible rebinding
            ;;       variables work.
            ;;
            ;; TODO: Refine recontextualization with type information
            ;;       so that this *always* works.
            (for (values variant a-pts b-pts ok effective-a-pts) =
                 (intraprocedural-2pt-crossover *scopes* *scopes*
                                                a-stmt1 a-stmt2
                                                b-stmt1 b-stmt2))
            (declare (ignorable a-pts b-pts effective-a-pts))
            (when (and ok
                       (phenome-p variant)
                       (= (length (asts *scopes*))
                          (length (asts variant))))
              (return t)))
          "Able to crossover a statement and a ancestor with 100 tries."))))

(deftest (crossover-a-statement-with-multiple-statements :long-running) ()
  ;; Replace a single statement with multiple statements
  (with-fixture scopes-clang
    (let ((a-stmt (stmt-with-text *scopes* "int b;"))
          (b-stmt1 (stmt-with-text *scopes* "c = 10;"))
          (b-stmt2 (stmt-with-text *scopes* "g = 12;")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt a-stmt
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (> (length (asts variant))
               (length (asts *scopes*))))
        ;; a is the only var in scope so all these assignments should
        ;; be rebound.
        (is (stmt-with-text variant "a = 10;" :no-error t))
        (is (stmt-with-text variant "a = 11;" :no-error t))
        (is (stmt-with-text variant "a = 12;" :no-error t))
        (is (not (stmt-with-text variant "int b;" :no-error t)))))))

(deftest (crossover-a-multiple-statements-with-a-single-statement
          :long-running) ()
  ;; Replace multiple statements with a single statement
  (with-fixture scopes-clang
    (let ((a-stmt1 (stmt-with-text *scopes* "int b;"))
          (a-stmt2 (stmt-with-text *scopes* "b = 1;"))
          (b-stmt (stmt-with-text *scopes* "e = 8;")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt b-stmt)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (< (length (asts variant))
               (length (asts *scopes*))))
        ;; a is the only var in scope so this assignment should
        ;; be rebound.
        (is (stmt-with-text variant "a = 8;" :no-error t))
        (is (not (stmt-with-text variant "int b;" :no-error t)))
        (is (not (stmt-with-text variant "b = 1;" :no-error t)))))))

(deftest (intraprocedural-2pt-crossover-does-not-crash :long-running) ()
  (with-fixture intraprocedural-2pt-crossover-bug-clang
    (let ((variant (intraprocedural-2pt-crossover
                    (copy *soft*)
                    (copy *soft*)
                    (stmt-with-text *soft* "printf(\"%d\\n\", argc + 1 < NUM);")
                    (stmt-with-text *soft* "argc--;")
                    (stmt-with-text *soft* "printf(\"%d\\n\", argc + 1 < NUM);")
                    (stmt-with-text *soft* "argc--;"))))
      (is (string/= (genome variant)
                    "")))))


;;;; Misc. mutation tests.
(defsuite misc-mutations "Misc. mutation tests."
            (clang-mutate-available-p))


(deftest single-decl-works ()
  (with-fixture scopes-clang
    (is (= 1 (length (ast-declares (stmt-with-text *scopes* "int a;")))))))

(deftest multiple-decl-works ()
  (with-fixture scopes-clang
    ;; NOTE: Why isn't this statement reliably found?
    (when-let* ((ast (stmt-with-text *scopes* "int f, g;" :no-error t)))
      (is (= 2 (length (ast-declares ast)))))))

(deftest (pick-for-loop-works :long-running) ()
  (with-fixture scopes-clang
    (is (eq :ForStmt (->> (pick-for-loop *scopes*)
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

(deftest (explode-for-loop-mutation-works :long-running) ()
  "Test conversion of for loop variants computing factorials to while loops"
  (let ((simple-loop       (from-file (make-clang)
                                      (explode-for-loop-dir
                                       "simple-loop.c")))
        (no-initialization (from-file (make-clang)
                                      (explode-for-loop-dir
                                       "loop-no-initialization.c")))
        (no-conditional    (from-file (make-clang)
                                      (explode-for-loop-dir
                                       "loop-no-conditional.c")))
        (no-increment      (from-file (make-clang)
                                      (explode-for-loop-dir
                                       "loop-no-increment.c")))
        (no-body           (from-file (make-clang)
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

(deftest (pick-while-loop-works :long-running) ()
  (with-fixture scopes-clang
    (is (eq :WhileStmt (->> (pick-while-loop *scopes*)
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

(deftest (delete-decl-stmts-works :long-running) ()
  (with-fixture scopes-clang
    (let ((variant (copy *scopes*)))
      (apply-mutation
          variant
        `(cut-decl (:stmt1 . ,(stmt-with-text *scopes* "int a;"))))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant))))
      (let ((stmt (or (stmt-with-text variant "b = 13;" :no-error t)
                      (stmt-with-text variant "c = 13;" :no-error t))))
        (is stmt)
        ;; unbound-vals are updated correctly
        (let ((unbound (mapcar {aget :name} (get-unbound-vals variant stmt))))
          (is (or (fully-every #'name= unbound '("b"))
                  (fully-every #'name= unbound '("c")))))))
    (let ((variant (copy *scopes*)))
      (apply-mutation
          variant
        `(cut-decl (:stmt1 . ,(stmt-with-text *scopes* "int d;"))))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))
    (when-let* ((variant (copy *scopes*))
                (id (stmt-with-text *scopes* "int f, g;" :no-error t)))
      (apply-mutation variant `(cut-decl (:stmt1 . ,id)))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest (swap-decls-works :long-running) ()
  (with-fixture scopes-clang
    ;; Check if the basic swap-decls mutation works.
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (stmt-with-text *scopes* "int a;"))))
      (apply-mutation variant
        (make-instance 'swap-decls :object variant))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest (rename-variable-works :long-running) ()
  (with-fixture scopes-clang
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (stmt-with-text *scopes* "b = 1;"))))
      (apply-mutation variant
        (make-instance 'rename-variable :object variant))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest expand-arithmatic-op-throws-error-if-no-arithmatic-ops ()
  (let ((obj (from-file (make-clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "no-compound-assign.c"))))
    (signals no-mutation-targets
      (build-op (make-instance 'expand-arithmatic-op :object obj) obj))))

(deftest expand-arithmatic-op-works-simple-compound-assignment ()
  (let ((obj (from-file (make-clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir
                         "simple-compound-assign.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc * 2" :no-error t))))

(deftest expand-arithmatic-op-works-complex-compound-assignment ()
  (let ((obj (from-file (make-clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir
                         "complex-compound-assign.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc + ((argc*4) / rand())" :no-error t))))

(deftest expand-arithmatic-op-works-increment ()
  (let ((obj (from-file (make-clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "increment.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "i = i + 1" :no-error t))))

(deftest expand-arithmatic-op-works-decrement ()
  (let ((obj (from-file (make-clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "decrement.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc - 1" :no-error t))))

(deftest expand-arithmatic-op-works-field-increment ()
  (let ((obj (from-file (make-clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "field-increment.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "t.x = t.x + 1" :no-error t))))

(deftest expand-arithmatic-op-works-field-decrement ()
  (let ((obj (from-file (make-clang
                          :compiler "clang"
                          :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "field-decrement.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "t.x = t.x - 1" :no-error t))))

(deftest expand-arithmatic-op-works-class-member-increment ()
  (let ((obj (nest (from-file (make-clang
                                :compiler "clang"
                                :flags '("-g" "-m32" "-O0")))
                   (expand-arithmatic-op-dir "class-member-increment.cpp"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "x = x + 1" :no-error t))))

(deftest expand-arithmatic-op-works-class-member-decrement ()
  (let ((obj (nest (from-file (make-clang
                                :compiler "clang"
                                :flags '("-g" "-m32" "-O0")))
                   (expand-arithmatic-op-dir "class-member-decrement.cpp"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "x = x - 1" :no-error t))))


;;;; Adaptive-mutation tests.
(defsuite adaptive-mutation-tests "Adaptive-mutation tests.")


(deftest bad-cut-changes-mutation-probability ()
  (let* ((*mutation-results-queue* #((cut . :worse) (cut . :dead)))
         (muts-0 '((cut . 1/2) (swap . 1)))
         (muts-1 (update-mutation-types muts-0))
         (muts-2 (update-mutation-types muts-1)))
    (is (< (aget 'cut muts-1) (aget 'cut muts-0))
        "Bad mutations lose probability.")
    (is (< (aget 'cut muts-2) (aget 'cut muts-1))
        "Bad mutations continue to lose probability.")))

(deftest mutation-queue-wraps-as-expected ()
  (let ((*mutation-results-queue*
         (make-array 100
                     :element-type '(cons symbol symbol)
                     :initial-element (cons :nothing :nothing)))
        (sel/sw/adaptive-mutation::*mutation-results-queue-next* 0))
    (dotimes (n 100)
      (queue-mutation 'cut :dead))
    (is (every [{equal 'cut} #'car] *mutation-results-queue*)
        "`queue-mutation' fills `*mutation-results-queue*' as expected.")
    (queue-mutation 'swap :better)
    (is (equalp (cons 'swap :better) (aref *mutation-results-queue* 0))
        "`queue-mutation' wraps `*mutation-results-queue*' as expected.")))

(deftest update-mutation-types-returns-list-when-mutation-queue-unpopulated ()
  "Ensure update-mutation-types returns its first argument when the
*mutation-results-queue* is unpopulated"
         (let ((*mutation-results-queue*
                (copy-seq +initial-mutation-results-queue+))
               (sel/sw/adaptive-mutation::*mutation-results-queue-next* 0)
        (mutation-types (copy-seq *clang-mutation-types*)))
    (is (equalp mutation-types
                (update-mutation-types mutation-types)))))

(deftest update-mutation-types-returns-list-when-mutation-queue-populated ()
  "Ensure update-mutation-types returns a list when the
*mutation-results-queue* is populated"
         (let ((*mutation-results-queue*
                (copy-seq +initial-mutation-results-queue+))
               (sel/sw/adaptive-mutation::*mutation-results-queue-next* 0)
               (mutation-types (copy-seq *clang-mutation-types*)))
           (dotimes (n (length +initial-mutation-results-queue+))
      (queue-mutation 'cut :dead))
    (is (listp (update-mutation-types mutation-types)))))

(deftest adaptive-analyze-mutation-updates-results-queue-properly ()
  (let ((*fitness-predicate* #'<)
        (sel/sw/adaptive-mutation::*mutation-results-queue-next* 0)
        (*mutation-results-queue*
         (copy-seq +initial-mutation-results-queue+))
        (parent-a (make-clang :fitness 2))
        (parent-b (make-clang :fitness 2))
        (crossed  (make-clang :fitness 1))
        (mutant   (make-clang :fitness 0)))
    (adaptive-analyze-mutation mutant
                               `(clang-cut ,parent-a 0
                                           ,crossed ,parent-b 0)
                               {fitness})
    (is (equal :better (cdr (aref *mutation-results-queue* 0))))))


;;;; Database tests.
(defsuite database "Database tests.")


(defixture json-database
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example"
                                            :type "json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in))))
  (:teardown
   (setf *database* nil)))

(deftest json-database-find-snippet-respects-class ()
  (with-fixture json-database
    (is (null (-<>> (find-snippets *database* :ast-class "CompoundStmt")
                    (remove "CompoundStmt" <> :test #'string=
                            :key {aget :class}))))))

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
(defsuite instrumentation "Instrumentation tests."
            (clang-mutate-available-p))


(defun count-traceable (obj)
  "Return a count of full statements parented by compound statements"
  (count-if {can-be-made-traceable-p obj} (asts obj)))

(defun get-gcd-trace (bin)
  (with-temp-file (trace-file)
    (let ((errno (nth-value 2 (run-program (format nil "~a 4 8 2>~a"
                                                   bin trace-file)))))
      (is (zerop errno))
      (let ((trace (read-binary-trace trace-file)))
        (is (listp trace))
        trace))))


(deftest instrumented-p-test ()
  (with-fixture gcd-clang
    (is (not (instrumented-p *gcd*)))
    (is (instrumented-p (instrument (copy *gcd*))))))

(deftest (instrumentation-insertion-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*) :trace-file :stderr)))
      ;; Do we insert the right number of printf statements?
      (is (<= (count-traceable *gcd*)
              (count-traceable instrumented)))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 15 (length trace))))))))

(deftest (instrumentation-insertion-w-filter-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*)
                          :filter (lambda (obj ast)
                                    (eq 92 (index-of-ast obj ast)))
                          :trace-file :stderr)))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 1 (length trace))))))))

(deftest (instrumentation-insertion-w-function-exit-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*)
                          :instrument-exit t
                          :trace-file :stderr)))
      ;; Do we insert the right number of printf statements?
      (is (<= (count-traceable *gcd*)
              (count-traceable instrumented)))

      ;; Is function exit instrumented?
      (is (stmt-with-text
           instrumented
           (format
            nil
            "__write_trace_id(__sel_trace_file, __sel_trace_file_lock, ~du)"
            (position (function-body *gcd* (first (functions *gcd*)))
                      (asts *gcd*)
                      :test #'equalp))
           :no-error t))

      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 16 (length trace))))))))

(deftest (instrumentation-insertion-w-points-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented
           (handler-bind ((warning #'muffle-warning))
             (instrument (copy *gcd*)
               :points
               (iter (for ast in (stmt-asts *gcd*))
                     (for i upfrom 0)
                     (collect (cons ast (if (evenp i) '(1 2) '(3 4) ))))
               :trace-file :stderr))))
      (is (scan (quote-meta-chars "__write_trace_aux(__sel_trace_file")
                (genome-string instrumented))
          "We find code to print auxiliary values in the instrumented source.")
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every [«or {equalp #(1 2)} {equalp #(3 4)}»
                      {aget :aux}]
                     trace)))))))

(deftest (instrumentation-insertion-w-trace-file-test :long-running) ()
  (with-fixture gcd-clang
    (with-temp-file (trace)
      (with-temp-file (bin)
        (let ((instrumented
               (instrument (copy *gcd*) :trace-file trace)))
          (is (scan (quote-meta-chars trace) (genome-string instrumented)))
          (is (zerop (nth-value 1 (ignore-phenome-errors
                                   (phenome instrumented :bin bin)))))
          (is (probe-file bin))
          (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
            (declare (ignorable stdout stderr))
            (is (zerop errno))
            (is (probe-file trace))))))))

(deftest (instrumentation-handles-missing-curlies-test :long-running) ()
  (with-fixture gcd-wo-curlies-clang
    (let ((instrumented (instrument (copy *gcd*) :trace-file :stderr)))
      ;; Ensure we were able to instrument an else branch w/o curlies.
      (let* ((else-counter (index-of-ast *gcd*
                                         (stmt-with-text *gcd* "b = b - a;")))
             (matcher (format nil "__write_trace_id\\(.*~du\\)"
                              else-counter)))
        (is (scan matcher (genome instrumented)))
        ;; The next line (after flushing) should be the else branch.
        (let ((location (position-if {scan matcher} (lines instrumented))))
          (is (scan (quote-meta-chars "b = b - a;")
                    (nth location (lines instrumented))))))
      ;; Finally, lets be sure we still compile.
      (with-temp-file (bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (is (not (emptyp (get-gcd-trace bin))))))))

(deftest
    (instrumentation-insertion-w-points-and-added-blocks-test :long-running) ()
  (with-fixture gcd-wo-curlies-clang
    (let* ((cookie 1234)
           (instrumented
            (instrument (copy *gcd*)
              :points
              `((,(stmt-with-text *gcd* "b - a") ,cookie))
              :trace-file :stderr)))
      ;; Instrumented program holds the value 1234.
      (is (scan (quote-meta-chars (format nil "~d" cookie))
                (genome-string instrumented))
          "The point trace value ~S appears in the instrumented program text."
          cookie)
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (find-if [{equalp `#(,cookie)} {aget :aux}]
                       trace)
              "The point trace value ~S appears in the trace" cookie))))))

(deftest (instrumentation-after-insertion-mutation-test :long-running) ()
  "Ensure after applying an insert mutation, the instrumented software
prints unique counters in the trace"
  (with-fixture gcd-clang
    (let* ((*matching-free-var-retains-name-bias* 1.0)
           (variant (copy *gcd*))
           (instrumented (copy *gcd*))
           (stmt1 (stmt-with-text variant "a = atoi(argv[1]);"))
           (stmt2 (stmt-with-text variant "a = atoi(argv[1]);")))
      (apply-mutation variant
        `(clang-insert (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (instrument instrumented)

      (with-temp-file (bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout stderr))
          (is (zerop errno))
          (is (not (equal (find-if [{string= "a = atoi(argv[1]);"}
                                    #'peel-bananas #'source-text]
                                   (asts variant)
                                   :from-end nil)
                          (find-if [{string= "a = atoi(argv[1]);"}
                                    #'peel-bananas #'source-text]
                                   (asts variant)
                                   :from-end t)))
              "a = atoi(argv[1]) was not inserted into the genome")
          (is (search
               (format
                nil
                "__write_trace_id(__sel_trace_file, __sel_trace_file_lock, ~du)"
                (->> (find-if [{string= "a = atoi(argv[1]);"}
                               #'peel-bananas #'source-text]
                              (asts variant)
                              :from-end nil)
                  (index-of-ast variant)))
               (genome instrumented))
              "instrumentation was not added for the inserted statement")
          (is (search
               (format
                nil
                "__write_trace_id(__sel_trace_file, __sel_trace_file_lock, ~du)"
                (->> (find-if [{string= "a = atoi(argv[1]);"}
                               #'peel-bananas #'source-text]
                              (asts variant)
                              :from-end t)
                  (index-of-ast variant)))
               (genome instrumented))
              "instrumentation was not added for the original statement"))))))

(deftest (instrumentation-print-unbound-vars :long-running) ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-unbound-vals
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (= (length trace) (count-if {assoc :c} trace))
            "Counter in every trace element.")
        (is (> (count-if {assoc :scopes} trace) 0)
            "Variable list in some trace elements.")
        (is (> (length trace) (count-if {aget :scopes} trace))
            "Variable list not populated in every trace element.")))))

(deftest (instrumentation-print-in-scope-vars :long-running) ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *gcd* :bin bin))))
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

(deftest (instrumentation-print-strings :long-running) ()
  (with-fixture c-strings
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                          instrumenter ast
                                          :print-strings t)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_blobs(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print strings in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented SOFT.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (every [{trace-var-equal "test" "test"} {aget :scopes}]
                   trace)
            "Variable 'test' always has expected value.")
        (is (every [{trace-var-equal "x" "4"} {aget :scopes}]
                   trace)
            "Variable 'x' always has expected value.")))))

(deftest (instrumentation-print-cpp-strings :long-running) ()
  (with-fixture cpp-strings
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                          instrumenter ast
                                          :print-strings t)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_blobs(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print strings in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented SOFT.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (every [{trace-var-equal "x" "4"} {aget :scopes}]
                   trace)
            "Variable 'x' always has expected value.")))))

(deftest (instrumentation-print-vars-after :long-running) ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions-after
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (not (null (mappend {aget :SCOPES} trace)))
            "Variable list not always empty.")))))

(deftest (instrumentation-print-vars-handles-shadowing :long-running) ()
  (with-fixture shadow-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented program.")
      (let ((trace (get-gcd-trace bin)))
        (is (every [{eql 1} #'length {aget :scopes}]
                   trace)
            "No duplicate variables.")

        (is (every [«or {equalp '(#("x" "int" 1 nil))}
                        {equalp '(#("x" "short" 0 nil))}»
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
(let ((foo-path (make-pathname :directory +multi-file-dir+
                               :name "foo"
                               :type "cpp"))
      (bar-path (make-pathname :directory +multi-file-dir+
                               :name "bar"
                               :type "cpp"))
      foo-contents bar-contents)
  (defixture clang-project
    ;; Has to preserve some files which are overwritten by the test.
    (:setup
     (setf foo-contents (file-to-string foo-path)
           bar-contents (file-to-string bar-path)
           *project*
           (from-file
            (make-instance 'clang-project
              :build-command "make foo"
              :artifacts '("foo")
              :compilation-database
              `(((:file . ,(namestring foo-path))
                 (:directory . ,(directory-namestring
                                 (make-pathname :directory +multi-file-dir+)))
                 (:command . "make"))
                ((:file . ,(namestring bar-path))
                 (:directory . ,(directory-namestring
                                 (make-pathname :directory +multi-file-dir+)))
                 (:command . "make"))))
            (make-pathname :directory +multi-file-dir+))))
    (:teardown (setf *project* nil)
               (string-to-file foo-contents foo-path)
               (string-to-file bar-contents bar-path))))

(defixture grep-project
  (:setup
   (setf *project*
         (-> (make-instance 'clang-project
               :build-command "make grep"
               :artifacts '("grep")
               :compilation-database
               (list
                (list
                 (cons :file
                       (-> (make-pathname :directory +grep-prj-dir+
                                          :name "grep"
                                          :type "c")
                           (namestring)))
                 (cons :directory
                       (-> (make-pathname :directory +grep-prj-dir+)
                           (directory-namestring)))
                 (cons :command
                       (format nil "cc -c -o grep ~a"
                               (-> (make-pathname :directory +grep-prj-dir+
                                                  :name "grep"
                                                  :type "c")
                                   (namestring)))))))
             (from-file (make-pathname :directory +grep-prj-dir+)))))
  (:teardown (setf *project* nil)))

(defixture grep-bear-project
  (:setup
   (setf *project*
         (from-file (make-instance 'clang-project
                      :build-command "make grep"
                      :artifacts '("grep"))
                    (make-pathname :directory +grep-prj-dir+))))
  (:teardown (setf *project* nil)))

(deftest (can-instrument-clang-project :long-running) ()
  (with-fixture clang-project
    (instrument *project* :functions
                (list (lambda (instrumenter ast)
                        (var-instrument {get-unbound-vals
                                         (software instrumenter)}
                                        instrumenter
                                        ast)))
                :trace-file :stderr)
    (with-temp-file (bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *project* :bin bin))))
          "Successfully compiled instrumented project.")
      (with-temp-file (trace-file)
        (let ((errno (nth-value 2 (run-program (format nil "~a 2>~a"
                                                       bin trace-file)))))
          (is (zerop errno))
          (let ((trace (read-binary-trace trace-file)))
            (is (listp trace))
            (is (not (emptyp trace)))
            (is (every «and {aget :c} {aget :f}» trace))
            (is (some «and [{equalp '(#("x" "int" 0 nil))} {aget :scopes}]
                           [{eq 0} {aget :f}]»
                      trace))
            (is (some «and [{equalp '(#("y" "int" 1 nil))} {aget :scopes}]
                           [{eq 1} {aget :f}]»
                      trace))))))))

(deftest instrumentation-skips-nameless-variable ()
  (handler-bind ((mutate ; Ignore obvious problem in following genome.
                  (lambda (e)
                    (if (find-restart 'keep-partial-asts)
                        (invoke-restart 'keep-partial-asts)
                        (error e)))))
    (let ((soft (make-clang
                  :genome "int test(int) { return 1; }")))
      (instrument soft :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                          instrumenter
                                          ast))))
      (is (not (scan (quote-meta-chars
                      "__write_trace_variables(__sel_trace_file")
                     (genome soft)))
          "No code to print variables in the instrumented source."))))

(deftest (instrumentation-preserves-aux-data :long-running) ()
  (with-fixture gcd-clang
    (let* ((stmt (stmt-starting-with-text *gcd* "if (a == 0)"))
           (index (index-of-ast *gcd* stmt)))
      (apply-mutation *gcd*
        `(clang-replace
          (:stmt1 . ,stmt)
          (:value1 . ,(copy stmt :aux-data '((:foo . t))))))

      (instrument *gcd* :functions
                  (list (lambda (instrumenter ast)
                          (when (aget :foo (ast-aux-data ast))
                            (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                            instrumenter
                                            ast))))
                  :trace-file :stderr)

      (with-temp-file (bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome *gcd* :bin bin))))
            "Successfully compiled instrumented GCD.")
        (is (member '(:foo . t)
                    (mappend #'ast-aux-data (asts *gcd*))
                    :test #'equalp))
        (let ((trace (get-gcd-trace bin)))
          (is (listp trace) "We got a trace.")
          (is (some {aget :scopes} trace))
          (is (every «or [#'not {aget :scopes}]
                         [{eq index} {aget :c}]»
                     trace)))))))

(deftest (uninstrument-instrument-is-identity :long-running) ()
  (with-fixture gcd-clang
    (let ((orig (copy *gcd*))
          (instrumented (copy *gcd*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture gcd-wo-curlies-clang
    (let ((orig (copy *gcd*))
          (instrumented (copy *gcd*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture clang-project
    (let ((orig (copy *project*))
          (instrumented (copy *project*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture shadow-clang
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture binary-search-clang
    (let ((orig (copy *binary-search*))
          (instrumented (copy *binary-search*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture c-strings
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture cpp-strings
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity"))))

(defun do-multi-threaded-instrument-clang-test (obj)
  (let ((st-instrumented
          (instrument (copy obj)
                      :functions
                      (list (lambda (instrumenter ast)
                              (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                              instrumenter
                                              ast)))
                      :num-threads 1))
        (mt-instrumented
          (instrument (copy obj)
                      :functions
                      (list (lambda (instrumenter ast)
                              (var-instrument {get-vars-in-scope
                                                (software instrumenter)}
                                              instrumenter
                                              ast)))
                      :num-threads 4)))
    (is (equalp (mapcar #'ast-class (asts st-instrumented))
                (mapcar #'ast-class (asts mt-instrumented)))
        "`instrument` should yield the same ASTs regardless of the ~
         number of threads utilized.")

    (uninstrument st-instrumented :num-threads 1)
    (uninstrument mt-instrumented :num-threads 4)
    (is (equalp (mapcar #'ast-class (asts st-instrumented))
                (mapcar #'ast-class (asts mt-instrumented)))
        "`uninstrument` should yield the same ASTs regardless of the ~
         number of threads utilized.")))

(defsuite clang-instrumentation-tests "Tests for Clang instrumentation."
  (clang-mutate-available-p))

(deftest (multi-threaded-clang-instrument-test :long-running) ()
  (with-fixture clang-project
    (do-multi-threaded-instrument-clang-test *project*))
  (with-fixture grep-bear-project
    (do-multi-threaded-instrument-clang-test *project*)))

(defsuite java-instrumentation-tests "Tests for Java instrumentation."
  ;; (zerop (nth-value 2 (shell "which java-mutator")))
  :silent)

(deftest (multi-threaded-java-instrument-test :long-running) ()
  (with-fixture java-project
    (let ((st-instrumented
            (instrument (copy *soft*) :num-threads 1))
          (mt-instrumented
            (instrument (copy *soft*) :num-threads 4)))
      (is (equalp (genome st-instrumented)
                  (genome mt-instrumented))
          "`instrument` should yield the same genome regardless of the ~
           number of threads utilized.")

      (uninstrument st-instrumented :num-threads 1)
      (uninstrument mt-instrumented :num-threads 4)
      (is (equalp (genome st-instrumented)
                  (genome mt-instrumented))
          "`uninstrument` should yield the same genome regardless of the ~
           number of threads utilized."))))


;;;; Traceable tests.
(defsuite traceable-tests "Traceable tests."
            (clang-mutate-available-p))

(defixture traceable-gcd
  (:setup (setf *gcd* (from-file
                       (if *new-clang?*
                           (make-instance 'new-clang-traceable)
                           (make-instance 'clang-traceable))
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

(deftest (run-traceable-gcd :long-running) ()
  (with-fixture traceable-gcd
    (instrument *gcd*)
    (collect-traces *gcd* *gcd-test-suite*)
    (setf (traces *gcd*)
          (mapcar {get-trace (traces *gcd*)} (iota (n-traces (traces *gcd*)))))
    (is (= (length (traces *gcd*)) (length *gcd-inputs*)))
    (is (every {every {aget :c}} (mapcar {aget :trace} (traces *gcd*))))))

(deftest (run-traceable-gcd-w/collect-traces :long-running) ()
  (with-fixture traceable-gcd
    (instrument *gcd*)
    (collect-traces *gcd* *gcd-test-suite*)
    (setf (traces *gcd*)
          (mapcar {get-trace (traces *gcd*)} (iota (n-traces (traces *gcd*)))))
    (is (every {every {aget :c}} (mapcar {aget :trace} (traces *gcd*))))))

(defmethod phenome ((obj collect-traces-handles-directory-phenomes-mock)
                    &key (bin (temp-file-name)))
  (let ((dir (ensure-directory-pathname bin)))
    (setf (phenome-dir obj) dir)
    (ensure-directories-exist dir)))

(deftest collect-traces-handles-directory-phenomes ()
  (let ((obj (make-instance 'collect-traces-handles-directory-phenomes-mock)))
    (handler-bind ((trace-error (lambda (c)
                                  (declare (ignorable c))
                                  (invoke-restart 'ignore-empty-trace))))
      (collect-traces obj (make-instance 'test-suite)))
    (is (not (probe-file (phenome-dir obj)))
        "collect-traces did not remove a phenome directory")))

(deftest (long-running-program-killed-test :long-running) ()
  (with-fixture long-running-program-clang
    (with-temp-file (bin)
      (phenome *soft* :bin bin)
      (let ((proc (start-test bin
                              (make-instance 'test-case :program-name bin)
                              :wait nil))
            (*process-kill-timeout* 4))
        (finish-test proc)
        (is (not (process-running-p proc))
            "finish-test did not kill a long running process")))))

(deftest (env-variables-passed-through-to-test-suites :long-running) ()
  (with-fixture print-env-clang
    (with-temp-file (bin)
      (phenome *soft* :bin bin)
      (is (string=
           (concatenate 'string "__sel_bar" '(#\Newline))
           (stream-to-string
            (process-output-stream
             (start-test bin
                         (make-instance 'test-case
                           :program-name bin
                           :program-args '("__sel_foo"))
                         :wait t
                         :output :stream
                         :env '(("__sel_foo" . "__sel_bar"))))))))))


;;;; Tests of declaration and type databases on clang objects.
(defsuite declaration-type-databases
            "Tests of declaration and type databases on clang objects."
            (clang-mutate-available-p))


(deftest (huf-knows-types :long-running) ()
  (with-fixture huf-clang
    (is (and (hash-table-p (types *huf*))
             (not (zerop (hash-table-count (types *huf*)))))
        "Huf software object has a type database.")
    (let ((pointer-types
           (remove-if-not
            «and #'type-pointer
                 [#'not #'type-const]
                 [#'not {find #\(} #'type-name]»
            (hash-table-values (types *huf*)))))
      (is (subsetp '("char" "heap_t" "int" "huffcode_t"
                     "huffcode_t*" "long" "void")
                   (mapcar [{remove #\Space} #'type-name]
                           pointer-types)
                   :test #'equal)
          "Huf has seven expected pointer types"))
    (let ((const-pointer-types
           (remove-if-not
            «and #'type-pointer
                 #'type-const
                 [#'not {find #\(} #'type-name]»
            (hash-table-values (types *huf*)))))
      (is (subsetp '("char" "void")
                   (mapcar [{remove #\Space} #'type-name]
                           const-pointer-types)
                   :test #'equal)
          "Huf has two expected pointer types"))
    (let ((array-types
           (remove-if
            [#'emptyp #'type-array]
            (hash-table-values (types *huf*)))))
      (is (>= (count "int" array-types :key #'type-name :test #'equal) 1)
          "Huf has at least 1 int array type")
      (is (>= (count "char" array-types :key #'type-name :test #'equal) 3)
          "Huf has at least 3 char array types")
      (is (>= (count "long" array-types :key #'type-name :test #'equal) 2)
          "Huf has at least 2 long array types"))
    (is (<= 3 (count-if [{string= "int"} #'type-name]
                       (hash-table-values (types *huf*))))
        "Huf has at least three different \"int\" types ~
         (some are array and pointer).")))

(deftest (huf-finds-type-info-for-variables :long-running) ()
  (with-fixture huf-clang
    (let ((type (->> (stmt-with-text *huf* "p = test;")
                     (get-vars-in-scope *huf*)
                     (find-if [{name= "strbit"} {aget :name}])
                     (find-var-type *huf*))))
      (is type "Found type for \"strbit\" in huf.")
      (is (string= "[100]" (type-array type))
          "Variable \"strbit\" in huf is a dynamically sized array.")
      (is (not (type-pointer type))
          "Variable \"strbit\" in huf is not a pointer."))))


;;;; Lisp representation.
(defsuite sexp-tests "Sexp representation.")


(defvar *clang-expr*  nil "The clang expression (sexp) software object.")
(defixture clang-expr
  (:setup
   (setf *clang-expr*
         (make-instance 'clang-expression
           :genome (copy-tree '(:+ 1 (:* 2 (:- 3 :y)))))))
  (:teardown
   (setf *clang-expr* nil)))

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


;;;; Mutations and evaluation of clang expressions in Lisp form.
(defsuite clang-expression-tests
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
(defsuite utility-tests "Utility tests.")

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

(deftest file-to-string-non-utf8-encoding ()
  #-ccl       ; CCL silently reads w/o warning despite bad encoding...
  (let ((path (make-pathname :directory +etc-dir+ :defaults "latin-1.c")))
    (is (string= (file-to-string path)
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
  (is (not (null (which "ls")))))

(deftest shell-directory-test ()
  (multiple-value-bind (stdout stderr errno)
      (shell "pwd" :directory "/tmp/")
    (is (equal "/tmp" (trim-whitespace stdout)))
    (is (equal "" stderr))
    (is (zerop errno))))

(deftest (read-and-write-shell-files :long-running) ()
  (let ((test-string "Hello world. Hello world. Hello world."))
    (is (nest
         (string= test-string)
         (with-retries (10))
         ;; NOTE: Give it 10 tries to account for rare stochastic
         ;; end-of-file errors in which I think we're going from
         ;; writing to reading too quickly for the file system.
         (handler-case
             (with-temp-file (temp.xz)
               (write-shell-file (out temp.xz "xz")
                 (write-line test-string out))
               (return (read-shell-file (in temp.xz "xzcat")
                         (read-line in))))
           (error (c) (declare (ignorable c)) nil))))))

(deftest (read-and-write-bytes-shell-files :long-running) ()
  (let ((byte #x25))
    (is (nest
         (equal byte)
         (with-retries (10))
         ;; NOTE: see note in `read-and-write-shell-files'
         (handler-case
             (with-temp-file (temp.xz)
               (write-shell-file (out temp.xz "xz")
                 (write-byte byte out))
               (return
                 (read-shell-file (in temp.xz "xzcat")
                   (read-byte in))))
           (error (c) (declare (ignorable c)) nil))))))

(deftest (cl-store-read-and-write-shell-files :long-running) ()
  (let ((it (make-instance 'software :fitness 37)))
    (is (nest
         (= (fitness it))
         (fitness)
         (with-retries (10))
         ;; NOTE: see note in `read-and-write-shell-files'
         (handler-case
             (with-temp-file (temp.xz)
               (write-shell-file (out temp.xz "xz")
                 (store it out))
               (return
                 (read-shell-file (in temp.xz "xzcat")
                   (restore in))))
           (error (c) (declare (ignorable c)) nil))))))


;;;; Command-line tests.
(defsuite command-line-tests "Command line tests")

;;; FIXME: this does not work if (sel/test:test) is run while
;;; in some directory other than the sel root directory.
(deftest guess-language-test ()
  (is (eql (if *new-clang?* 'new-clang 'clang)
           (guess-language #P"this/foo.cpp")))
  (is (eql 'json (guess-language #P"this/foo.json")))
  (is (eql 'json (guess-language #P"this/foo.json" #P"this/bar.json")))
  (is (eql 'clang-project
           (guess-language (make-pathname :directory +grep-prj-dir+))))
  (is (eql 'simple (guess-language #P"this/Makefile")))
  (is (null (guess-language #P"foo.js" #P"bar.lisp"))))

(deftest resolving-languages-works ()
  (mapc (lambda (pair)
          (destructuring-bind (args result) pair
            (is result (apply #'resolve-language-from-language-and-source
                              args))))
        `((("c") 'clang)
          (("C++") 'clang)
          (("CL") 'lisp)
          (("JSON") 'json)
          (("C" ,(make-pathname :directory +grep-prj-dir+)) 'clang-project)
          (("java" ,(make-pathname :directory +grep-prj-dir+)) 'java-project))))

(deftest (create-software-guesses-clang-project :long-running) ()
  (let ((sw (create-software
             (make-pathname :directory +grep-prj-dir+)
             :build-command
             (concatenate 'string
               (namestring (make-pathname :directory +grep-prj-dir+
                                          :name "build"
                                          :type "sh"))
               " --full --nonsense-arg"))))
    (is sw)
    (is (eql 'clang-project (type-of sw)))
    (is (not (string= "./" (subseq (build-command sw) 0 2))))
    (is (search "--full" (build-command sw)))))


;;;; Project tests.
(defsuite clang-project-tests "Project tests."
            (clang-mutate-available-p))


(defvar *s1*)
(defvar *s2*)
(defvar *project*)
(defixture project
  (:setup
   (setf *s1* (make-instance 'simple))
   (setf (lines *s1*) (list "s1-genome"))
   (setf *s2* (make-instance 'simple))
   (setf (lines *s2*) (list "s2-genome"))
   (setf *project* (make-instance 'project
                     :evolve-files `(("s1" . ,*s1*)
                                     ("s2" . ,*s2*)))))
  (:teardown (setf *project* nil)))

(defmethod test-method ((obj simple) value)
  value)

(deftest to-file-fails-with-nil-path ()
  (with-fixture project
    (signals error (to-file *project* nil))))

(deftest (simple-to-from-file-without-project-dir-works :long-running) ()
  (with-fixture project
    (setf (project-dir *project*) nil)
    (with-temp-dir (file)
      (progn
        (to-file *project* file)
        (is (member :user-read (file-permissions file)))
        (is (member :user-write (file-permissions file)))
        (let ((s1-2 (from-file (make-instance 'simple)
                               (make-pathname :name "s1"
                                              :directory file)))
              (s2-2 (from-file (make-instance 'simple)
                               (make-pathname :name "s2"
                                              :directory file))))
          (is (equalp (genome *s1*) (genome s1-2)))
          (is (equalp (genome *s2*) (genome s2-2))))))))

(deftest ignored-paths-are-ignored ()
  (is (sel/sw/project::ignored-path-p
       "README" :ignore-paths '("README")))
  (is (sel/sw/project::ignored-path-p
       "README" :ignore-paths '("*")))
  (is (not (sel/sw/project::ignored-path-p
            "Makefile" :ignore-paths '("README"))))
  (is (sel/sw/project::ignored-path-p
       "etc/foo" :ignore-paths '("etc/*")))
  (is (sel/sw/project::ignored-path-p
       "./etc/foo" :ignore-paths '("etc/*")))
  (is (sel/sw/project::ignored-path-p
       "./etc/foo/bar/baz" :ignore-paths '("etc/**/*")))
  (is (not (sel/sw/project::ignored-path-p
            "Makefile" :ignore-paths '("etc/*"))))
  (is (not (sel/sw/project::ignored-path-p
            "src/foo" :ignore-paths '("etc/*"))))
  (is (not (sel/sw/project::ignored-path-p
            "./src/foo" :ignore-paths '("etc/*")))))

(deftest only-paths-are-only ()
  (is (not (sel/sw/project::ignored-path-p
            "README" :only-paths '("README"))))
  (is (sel/sw/project::ignored-path-p
       "Makefile" :only-paths '("README")))
  (is (not (sel/sw/project::ignored-path-p
            "etc/foo" :only-paths '("etc/*"))))
  (is (not (sel/sw/project::ignored-path-p
            "./etc/foo" :only-paths '("etc/*"))))
  (is (not (sel/sw/project::ignored-path-p
            "./etc/foo/bar/baz" :only-paths '("etc/**/*"))))
  (is (sel/sw/project::ignored-path-p
       "Makefile" :only-paths '("etc/*")))
  (is (sel/sw/project::ignored-path-p
       "src/foo" :only-paths '("etc/*")))
  (is (sel/sw/project::ignored-path-p
       "./src/foo" :only-paths '("etc/*"))))

(deftest (project-copy-preserves-permissions :long-running) ()
  ;; Ensure `to-file' preserves permissions on executable files.
  (nest
   (with-fixture grep-project)
   (with-temp-file (dir-path))
   (let ((dir (pathname-directory (pathname-as-directory dir-path))))
     (is (project-dir *project*))
     (to-file *project* dir-path)
     (is (member :user-exec
                 (file-permissions
                  (make-pathname :name "test"
                                 :type "sh"
                                 :directory (append dir (list "support")))))))))

(deftest (clang-project-test :long-running) ()
  (with-fixture grep-project
    (is (equal "make grep" (build-command *project*)))
    (is (equalp '("grep") (artifacts *project*)))
    (is (equal 1 (length (evolve-files *project*))))
    (is (equal "grep.c" (car (first (evolve-files *project*)))))
    (is (equal "cc" (compiler (cdr (first (evolve-files *project*))))))
    ;; Don't include binaries in `other-files'.
    (is (not (member "support/inputs/grepBinary"
                     (mapcar #'car (other-files *project*)) :test #'equalp)))
    (is (equal (namestring (make-pathname :directory +grep-prj-dir+))
               (second (member "-I" (flags (cdr (car (evolve-files *project*))))
                               :test #'equal))))
    (is (equal (namestring (make-pathname :directory +grep-prj-dir+))
               (namestring (project-dir *project*))))))

(deftest (apply-mutations-to-project-unique-test :long-running) ()
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

(deftest (apply-picked-mutations-to-project-unique-test :long-running) ()
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

(deftest clang-project-compilation-database-flags-test ()
  (is (equal (list "-DDIR='\"/tmp\"'" "-DIN" "\"-D_U_=a\"")
             (sel/sw/clang-project::compilation-db-entry-flags
               `((:command .
                  "cc -DDIR=\\\"/tmp\\\" -DIN \"-D_U_=a\"")))))
  (is (equal (list "-DDIR1='\"/tmp1\"'" "-DDIR2='\"/tmp2\"'")
             (sel/sw/clang-project::compilation-db-entry-flags
               `((:command .
                  "cc -DDIR1=\\\"/tmp1\\\" -DDIR2=\\\"/tmp2\\\"")))))
  (is (equal (list "-DDIR='\"\"'")
             (sel/sw/clang-project::compilation-db-entry-flags
               `((:command .
                  "cc -DDIR=\\\"\\\""))))))


;;;; Tests that require bear.
(defsuite bear-tests "Clang representation."
            (lambda () (zerop (nth-value 2 (shell "which bear")))))

(deftest (able-to-create-a-bear-project :long-running) ()
  (with-fixture grep-bear-project
    (is (equal "make grep" (build-command *project*)))
    (is (equalp '("grep") (artifacts *project*)))
    (is (not (zerop (length (genome *project*)))))))

(deftest (able-to-copy-a-bear-project :long-running) ()
  (with-fixture grep-bear-project
    (is (copy *project*)
        "Able to copy a project built with bear.")))


;;;; Condition synthesis tests.
(defsuite condition-synthesis "Condition synthesis tests."
  (clang-mutate-available-p))

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
                     (->> (get-immediate-children repaired-prog stmt)
                          (first)
                          (source-text)
                          (peel-bananas)))))))))



;;;; Selection tests.
(defsuite selection "Selection tests.")


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
    (is (lexicase-better-p '(1 2 0) a b))
    (is (not (lexicase-better-p '(1 2 0) b a)))
    (is (lexicase-better-p '(1 0 2) b a))
    (is (not (lexicase-better-p '(1 0 2) a b)))))

(deftest lexicase-better-p-tie ()
  (let ((a #(0 1 1))
        (b #(0 1 1)))
    (is (not (lexicase-better-p '(0 1 2) a b)))
    (is (not (lexicase-better-p '(0 1 2) b a)))))

(deftest dominates-all-trivial-test ()
  (is (dominates-all (list #'>)
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
  (is (equal 1.0 (multi-objective-scalar '(0.1 0.4 0.5))))
  (is (equal 1.0 (multi-objective-scalar '(0.1 0.4 #(1 0))))))

(deftest crowding-distance-works ()
  (let ((*population* (list (make-instance 'simple :fitness '(0.5 0.5 0.5))
                            (make-instance 'simple :fitness '(0 0 1))
                            (make-instance 'simple :fitness '(0.6 1 0.1)))))

    (is (equal (+ 0.6 1 0.9)
               (crowding-distance (first *population*))))
    (is (equal infinity
               (crowding-distance (second *population*))))
    (is (equal infinity
               (crowding-distance (third *population*))))))

(deftest crowding-distance-lexicase-works ()
  (let ((*population*
         (list (make-instance 'simple :fitness '(#(1 0) #(1 1 0 0)))
               (make-instance 'simple :fitness '(#(0.5 0.5) #(0.5 0.5 0.5 0.5)))
               (make-instance 'simple :fitness '(#(0 1) #(0 0 1 1))))))

    (is (equal infinity
               (crowding-distance (first *population*))))
    (is (equal 2
               (crowding-distance (second *population*))))
    (is (equal infinity
               (crowding-distance (third *population*))))))

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
(defsuite style-features "Style features tests."
            (clang-mutate-available-p))


(deftest uni-grams-ht-test ()
  (let* ((sentence (list "the" "quick" "brown" "fox"
                         "the" "lazy" "brown" "dog"
                         "the" "quick" "hare"))
         (ht (uni-grams sentence)))
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
         (ht (uni-grams sentence))
         (fv (to-feature-vector ht (list "the" "quick" "brown" "fox"
                                              "lazy" "dog" "hare"))))
    (is (= 7 (length fv)))
    (is (equalp fv (vector 3 2 2 1 1 1 1)))))

(deftest function-ast-depths-are-zero ()
  (with-fixture variety-clang
    (let ((asts (functions *variety*)))
      (is (= 4 (length asts)))
      ;; depths of all function asts are 0 (all top-level)
      (is (every #'zerop (mapcar {ast-depth *variety*} asts))))))

(deftest max-depth-ast-functions-is-0 ()
  (with-fixture variety-clang
    (is (zerop (max-depth-ast *variety* (functions *variety*))))))

(deftest max-depth-ret-stmts-is-2 ()
  (with-fixture variety-clang
    (let ((return-stmts (remove-if-not [{eql :ReturnStmt} #'ast-class]
                                       (asts *variety*))))
      (is (= 2 (max-depth-ast *variety* return-stmts))))))

(deftest (merge-max-picks-larger :long-running) ()
  (bind (((:values vec1 meta1) (with-fixture variety-clang
                                 (max-depth-ast-extractor *variety*)))
         ((:values vec2 meta2) (with-fixture gcd-clang
                                 (max-depth-ast-extractor *gcd*)))
         ((:values vecr _) (merge-max vec1 meta1 vec2 meta2)))
    (is (= 1 (length vec1)))
    (is (= 1 (length vec2)))
    (is (= 1 (length vecr)))
    (is (= (elt vecr 0)
           (max (elt vec1 0)
                (elt vec2 0))))))

(deftest avg-depth-ast-node-type-function-is-0 ()
  (with-fixture variety-clang
    (is (zerop (ast-node-type-avg-depth *variety* :Function)))))

(deftest avg-depth-ast-node-type-return-stmts ()
  (with-fixture variety-clang
    (is (= 2 (ast-node-type-avg-depth *variety* :ReturnStmt)))))

(deftest node-type-counts ()
  (with-fixture variety-clang
    ;; list of (ast-class, occurrences)
    (bind (((:values vec denom) (ast-node-type-tf-extractor *variety*))
           (ast-counts (mapcar #'cons
                               *clang-c-ast-classes*
                               (mapcar {* denom} (coerce vec 'list)))))
      ;; for each ast-class, verify occurrence count is correct
      (iter (for (type . count) in ast-counts)
            (is (= count
                   (count-if [{equal type} #'ast-class]
                             (asts *variety*))))
            (finally (return t))))))

(deftest (ast-keywords-auto-counts :long-running) ()
  (with-fixture variety-clang
    (let ((auto-counts
           (iter (for keyword in *clang-c-keywords*)
                 (collect
                  (cons (reduce #'+ (mapcar {auto-count-keyword keyword}
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
           (iter (for keyword in *clang-c-keywords*)
                 (collect
                  (cons (reduce #'+ (mapcar {search-keyword *variety*
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
    (let ((ls-count (-<>> (ast-keyword-tf-extractor *variety*)
                      (coerce <> 'list)
                      (mapcar {* 44}))))
      (is
       (equal
        (mapcar #'cons
                ls-count
                *clang-c-keywords*)
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
         (bi-grams (bi-grams ls :key #'identity))
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
           (bi-grams (bi-grams functions :key #'ast-class))
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
             (bi-grams (bi-grams asts :key #'ast-class))
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

(deftest extract-style-features-no-asts ()
  (is (-> (from-string (make-instance (if *new-clang?*
                                          'new-clang-styleable-test-class
                                          'clang-styleable-test-class))
                       "")
          (extract-baseline-features))
      "extract-baseline-features should not throw an error on empty software"))


;;;; Clang syntactic contexts.
(defsuite clang-syntactic-contexts "Clang syntactic contexts."
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
    (apply-mutation-ops *contexts*
                             `((:cut (:stmt1 . ,(stmt-with-text
                                                 *contexts* "int x = 0;")))))
    (is (eq 0
            (count-matching-chars-in-stmt
             #\;
             (find-function *contexts* "full_stmt"))))))

(deftest insert-full-stmt-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;")))
      (apply-mutation-ops *contexts*
                               `((:insert (:stmt1 . ,target)
                                          (:value1 . ,target)))))
    (is (eq 2 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest insert-braced-full-stmt-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;"))
          (inserted (function-body *contexts*
                                   (find-function *contexts*
                                                  "list"))))
      (apply-mutation-ops *contexts*
                               `((:insert (:stmt1 . ,target)
                                          (:value1 . ,inserted)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest replace-full-stmt-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;"))
          (replacement (stmt-with-text *contexts* "int x = 1")))
      (apply-mutation-ops *contexts*
                               `((:set (:stmt1 . ,target)
                                       (:value1 . ,replacement)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest replace-full-stmt-with-braced-removes-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;"))
          (replacement (function-body *contexts*
                                      (find-function *contexts*
                                                     "list"))))
      (apply-mutation-ops *contexts*
                               `((:set (:stmt1 . ,target)
                                       (:value1 . ,replacement)))))
    (is (eq 0 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest insert-non-full-stmt-into-fullstmt-context-makes-full ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x"))
          (location (stmt-starting-with-text *contexts* "if (1)")))
      (apply-mutation-ops *contexts*
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
      (apply-mutation-ops *contexts*
                               `((:cut (:stmt1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a,  int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest insert-list-elt-adds-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b")))
      (apply-mutation-ops *contexts*
                               `((:insert (:stmt1 . ,target)
                                          (:value1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b,int b, int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-list-elt-keeps-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b"))
          (replacement (stmt-with-text *contexts* "int a")))
      (apply-mutation-ops *contexts*
                               `((:set (:stmt1 . ,target)
                                       (:value1 . ,replacement)))))
    (is (starts-with-subseq
         "void list(int a, int a, int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest cut-final-list-elt-removes-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c")))
      (apply-mutation-ops *contexts*
                               `((:cut (:stmt1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b)"
         (source-text (find-function *contexts* "list"))))))

(deftest insert-final-list-elt-adds-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c")))
      (apply-mutation-ops *contexts*
                               `((:insert (:stmt1 . ,target)
                                          (:value1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b, int c,int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-final-list-elt-keeps-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c"))
          (replacement (stmt-with-text *contexts* "int a")))
      (apply-mutation-ops *contexts*
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
      (apply-mutation-ops *contexts*
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

(deftest (cut-unbraced-body-adds-nullstmt :long-running) ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2;")))
      (apply-mutation-ops *contexts*
			  `((:cut (:stmt1 . ,target)))))
    ;; Note -- this is no longer a good mutation, since there's a ; missing
    ;; Cutting statements from non-compound statement should introduce
    ;; a semicolon (or this should be fixed up)
    (is (eq 0 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "unbraced_body"))))
    (is (eq :NullStmt
            (some->> (stmt-starting-with-text *contexts* "if (2)")
                     (get-immediate-children *contexts*)
                     (second)
                     (ast-class))))))

(deftest cut-braced-body-adds-nullstmt ()
  (with-fixture contexts
    (let ((target (->> (stmt-with-text *contexts* "int x = 1;")
                       (get-parent-ast *contexts*))))
      (apply-mutation-ops *contexts*
                               `((:cut (:stmt1 . ,target)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "braced_body"))))
    (is (eq :NullStmt
            (some->> (stmt-starting-with-text *contexts* "if (1)")
                     (get-immediate-children *contexts*)
                     (second)
                     (ast-class))))))

(deftest replace-unbraced-body-keeps-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2;")))
      (apply-mutation-ops *contexts*
                               `((:set (:stmt1 . ,target)
                                       (:value1 . ,target)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "unbraced_body"))))))

(deftest replace-unbraced-body-with-braced ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2;"))
          (replacement (function-body *contexts*
                                      (find-function *contexts*
                                                     "full_stmt"))))
      (apply-mutation-ops *contexts*
                               `((:set (:stmt1 . ,target)
                                       (:value1 . ,replacement)))))
    (let ((function (find-function *contexts* "unbraced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      (is (eq 1 (count-matching-chars-in-stmt #\; function))))))

(deftest insert-after-full-stmt-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;")))
      (apply-mutation-ops *contexts*
                               `((:insert-after (:stmt1 . ,target)
                                                (:value1 . ,target)))))
    (let ((function (find-function *contexts* "full_stmt")))
      (is (eq 2 (count-matching-chars-in-stmt #\; function)))
      (is (eq 2 (count-if [{string= "int x = 0;"} #'source-text]
                          (asts *contexts*)))))))

(deftest insert-after-braced-body-adds-trailing-semicolon ()
  (with-fixture contexts
    (let ((target (->> (stmt-with-text *contexts* "int x = 1;")
                       (get-parent-ast *contexts*)))
          (inserted (stmt-with-text *contexts* "int x = 0;")))
      (apply-mutation-ops *contexts*
                               `((:insert-after (:stmt1 . ,target)
                                                (:value1 . ,inserted)))))
    (let ((function (find-function *contexts* "braced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\; function)))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      (is (eq 2 (count-if [{string= "int x = 0;"} #'source-text]
                          (asts *contexts*)))))))

(deftest insert-braced-after-braced-body-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (->> (stmt-with-text *contexts* "int x = 1;")
                       (get-parent-ast *contexts*))))
      (apply-mutation-ops *contexts*
                               `((:insert-after (:stmt1 . ,target)
                                                (:value1 . ,target)))))
    (let ((function (find-function *contexts* "braced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\; function)))
      (is (eq 3 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 3 (count-matching-chars-in-stmt #\} function)))
      (is (eq 2 (count-if [{string= "int x = 1"} #'source-text]
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
;;       (apply-mutation-ops *contexts*
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
      (apply-mutation-ops *contexts*
                               `((:cut (:stmt1 . ,target)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 2
              (count-matching-chars-in-stmt #\; struct))))))

(deftest insert-field-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;")))
      (apply-mutation-ops *contexts*
                               `((:insert (:stmt1 . ,target)
                                          (:value1 . ,target)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 4
              (count-matching-chars-in-stmt #\; struct))))))

(deftest replace-field-keeps-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;"))
          (replacement (stmt-with-text *contexts* "int f2;")))
      (apply-mutation-ops *contexts*
                               `((:set (:stmt1 . ,target)
                                       (:value1 . ,replacement)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 3
              (count-matching-chars-in-stmt #\; struct))))))

(deftest insert-toplevel-adds-semicolon ()
  (with-fixture contexts
    (let ((location (stmt-starting-with-text *contexts* "struct"))
          (inserted (stmt-with-text *contexts* "int x = 0;"))
          (semicolons (count-if {eq #\;} (genome *contexts*))))
      (apply-mutation-ops *contexts*
                               `((:insert (:stmt1 . ,location)
                                          (:value1 . ,inserted))))
      (is (eq (1+ semicolons)
              (count-if {eq #\;} (genome *contexts*)))))))

(deftest insert-toplevel-braced ()
  (with-fixture contexts
    (let ((location (stmt-starting-with-text *contexts* "struct"))
          (inserted (stmt-starting-with-text *contexts* "void list"))
          (semicolons (count-if {eq #\;} (genome *contexts*))))
      (apply-mutation-ops *contexts*
                               `((:insert (:stmt1 . ,location)
                                          (:value1 . ,inserted))))
      (is (eq semicolons
              (count-if {eq #\;} (genome *contexts*)))))))

(deftest splice-asts-and-text ()
  (with-fixture contexts
    (let ((location (stmt-with-text *contexts* "int x = 0;"))
          (inserted (list (format nil "/*comment 1*/~%")
                          (stmt-starting-with-text *contexts*
                                                   "int x = 1")
                          (format nil ";~%/*comment 2*/~%"))))
      (apply-mutation-ops *contexts*
                               `((:splice (:stmt1 . ,location)
                                          (:value1 . ,inserted))))

      (is (not (stmt-with-text *contexts* "int x = 0;" :no-error t)))
      (is (stmt-with-text *contexts* "int x = 1;" :no-error t))
      (is (eq 1
              (->> (stmt-starting-with-text *contexts* "void full_stmt")
                   (function-body *contexts*)
                   (get-immediate-children *contexts*)
                   (remove-if-not #'ast-full-stmt)
                   (length))))
      (is (search "comment 1" (genome *contexts*)))
      (is (search "comment 2" (genome *contexts*))))))

(deftest cut-initialization-list-preserves-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "{ 1, 2, 3 }")))
      (apply-mutation-ops *contexts*
                               `((:cut (:stmt1 . ,target)))))
    (is (eq 1 (->> (find-function *contexts* "initialization_list")
                   (count-matching-chars-in-stmt #\;))))))

(deftest replace-removes-trailing-semicolon-with-whitespace ()
  (with-fixture contexts
    (let ((location (stmt-starting-with-text *contexts* "MACRO"))
          (replacement (->> (find-function *contexts* "unbraced_body")
                            (get-immediate-children *contexts*)
                            (first))))
      (apply-mutation-ops *contexts*
                          `((:cut (:stmt1 . ,location)
                                  (:value1 . ,replacement)))))
    (is (eq 0 (->> (find-function *contexts* "trailing_semi_with_whitespace")
                   (count-matching-chars-in-stmt #\;))))))


;;;; Clang scope and type tests.
(defsuite clang-scopes-and-types "Clang scope and type tests."
            (clang-mutate-available-p))


(defun compare-scopes (result expected)
  (is (equal (length result) (length expected)))
  (is (every (lambda (a b)
               (and (equal (length a) (length b))
                    (every #'name= a b)))
             (mapcar {mapcar {aget :name}} result)
             expected))
  (iter (for var-info in (apply #'append result))
        (is (aget :type var-info))
        (is (aget :decl var-info))
        (is (aget :scope var-info))))

(deftest scopes-are-correct ()
  (with-fixture scopes2-clang
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int b;"))
                    '(nil
                      ("a")
                      ("global")))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int c;"))
                    '(("b")
                      ("a")
                      ("global")))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "char d;"))
                    '(nil
                      ("c" "b")
                      ("a")
                      ("global")))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "return;"))
                    '(("c" "b")
                      ("a")
                      ("global")))))

(deftest cxx-method-scopes-are-correct ()
  (with-fixture scopes-cxx-clang
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int y;"))
                    '(nil
                      ("x")
                      nil))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int z;"))
                    '(nil
                      ("y")
                      ("x")
                      nil))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "y = 0;"))
                    '(("z")
                      ("y")
                      ("x")
                      nil))))

(deftest types-are-correct ()
  (with-fixture scopes2-clang
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-with-text *scopes*
                                                      "int global;")))
               '("int")))
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-with-text *scopes*
                                                      "char d;")))
               '("char")))
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-starting-with-text *scopes*
                                                               "void foo")))
               '("char" "int")))))

(deftest unbound-vals-are-correct ()
  (flet
      ((compare-vals (result expected)
         (is (null (set-exclusive-or (mapcar {aget :name} result)
                                     expected
                                     :test #'name=)))
         (iter (for var-info in result)
               (is (aget :type var-info))
               (is (aget :decl var-info))
               (is (aget :scope var-info)))))

    (with-fixture scopes2-clang
      (is (null (get-unbound-vals *scopes*
                                  (stmt-with-text *scopes* "int global;"))))
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

(defun unbound-funs-equal (result expected)
  (and (= (length result) (length expected))
       (every
        (lambda (r e)
          (and (consp r) (consp e)
               (equal (cdr r) (cdr e))
               (name= (car r) (if *new-clang?*
                                  (peel-bananas (car e))
                                  (car e)))))
        result expected)))

(deftest unbound-funs-are-correct ()
  (with-fixture scopes2-clang
    (is (null (get-unbound-funs *scopes*
                                (stmt-with-text *scopes* "int global;"))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
                           (stmt-with-text *scopes* "foo(0);"))
         '(("(|foo|)" t nil 1))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
                           (stmt-with-text *scopes* "bar();"))
         '(("(|bar|)" t nil 0))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
                           (stmt-starting-with-text *scopes* "void bar"))
         '(("(|foo|)" t nil 1)
           ("(|bar|)" t nil 0))))))

(deftest unbound-fun-not-defined ()
  (with-fixture unbound-fun-clang
    (is (unbound-funs-equal
         (get-unbound-funs *soft*
                           (stmt-with-text *soft* "g();"))
         '(("(|g|)" nil nil 0))))))

(deftest scopes-type-field-is-correct ()
  (with-fixture scopes-type-field-clang
    (is (equal "char"
               (->> (scopes *scopes* (stmt-with-text *scopes* "return 0;"))
                    (lastcar)
                    (first)
                    (aget :type)
                    (find-type *scopes*)
                    (type-name)))
        "char variable should have been found at the global scope")))

(deftest get-vars-in-scope-keep-globals-flag ()
  (with-fixture scopes-type-field-clang
    (is (name= "time_args"
               (->> (get-vars-in-scope *scopes*
                                       (stmt-with-text *scopes* "return 0;")
                                       t)
                    (first)
                    (aget :name)))
        "time_args variable should have been found at the global scope")
    (is (equal nil (get-vars-in-scope *scopes*
                     (stmt-with-text *scopes* "return 0;")
                     nil))
        "no variables should have been found outside the global scope")))

(deftest move-statement-updates-scopes ()
  (with-fixture scopes2-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (apply-mutation *scopes*
        `(clang-swap (:stmt1 . ,(stmt-with-text *scopes* "int c;"))
                     (:stmt2 . ,(stmt-with-text *scopes* "b = 0;")))))
    (compare-scopes (scopes *scopes* (stmt-starting-with-text *scopes* "b ="))
                    '(("b")
                      ("a")
                      ("global")))))

(deftest cut-decl-updates-scopes ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
      `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "int global;"))))
    (compare-scopes (scopes *scopes* (stmt-starting-with-text *scopes* "b ="))
                    '(("c" "b")
                      ("a")
                      nil))))

(deftest insert-decl-updates-types ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
      `(clang-insert (:stmt1 . ,(stmt-with-text *scopes* "foo(0);"))
                     (:stmt2 . ,(stmt-with-text *scopes* "int b;"))))
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-starting-with-text *scopes*
                                                               "void bar")))
               '("int")))))

(deftest cut-statement-updates-unbound-funs ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
      `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "foo(0);"))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
                           (stmt-starting-with-text *scopes* "void bar"))
         '(("(|bar|)" t nil 0))))))

(deftest insert-statement-updates-unbound-funs ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
      `(clang-insert (:stmt1 . ,(stmt-with-text *scopes* "int b;"))
                     (:stmt2 . ,(stmt-with-text *scopes*
                                                "bar();"))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
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
      `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "int b;"))))
    (let ((unbound (get-unbound-vals *scopes*
                                     (stmt-starting-with-text *scopes*
                                                              "void foo"))))
      (is (fully-every #'name= (mapcar {aget :name} unbound) '("global" "b")))
      (is (aget :decl (find-if [{name= "global"} {aget :name}] unbound)))
      ;; b is now undeclared
      (is (not (aget :decl (find-if [{name= "b"} {aget :name}] unbound)))))))

(deftest insert-statement-updates-unbound-vals ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
      `(clang-insert (:stmt1 . ,(stmt-with-text *scopes*
                                                "foo(0);"))
                     (:stmt2 . ,(stmt-with-text *scopes*
                                                "b = 0;"))))
    ;; "b" is not defined in this context so it will be rebound
    (let ((unbound (get-unbound-vals *scopes*
                                     (stmt-starting-with-text *scopes*
                                                              "void bar"))))
      (is (eq 1 (length unbound)))
      (is (name= "global" (aget :name (car unbound))))
      (is (equalp (stmt-with-text *scopes* "int global;")
                  (aget :decl (car unbound))))
      (is (eq (ast-root *scopes*) (aget :scope (car unbound))))
      (is (aget :type (car unbound))))))


;;;; Types and traces tests.
(defsuite type-traces-tests "Types and traces tests."
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
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*[]int")
              (make-clang-type :name "int"
                               :pointer t
                               :array "[]"
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*[5]int")
              (make-clang-type :name "int"
                               :pointer t
                               :array "[5]"
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
(defsuite clang-tokenizer "Clang tokenizer tests."
            (clang-mutate-available-p))


(deftest (case-tokens :long-running) ()
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

(deftest (do-while-tokens :long-running) ()
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

(deftest (function-tokens :long-running) ()
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

(deftest (mixed-tokens :long-running) ()
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

(deftest (memberexpr-tokens :long-running) ()
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

(deftest (parenexpr-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((roots (remove-if-not {eq :ParenExpr}
                                 (asts *variety*)
                                 :key #'ast-class))
           (token-lists (mapcar [{tokens *variety*} #'list] roots)))
      (is (= 1 (length token-lists)))
      (is (every [{<= 3} #'length] token-lists))
      (is (every [{eql (make-keyword "(")} #'first] token-lists))
      (is (every [{eql (make-keyword ")")} #'lastcar] token-lists)))))

(deftest (parmvar-tokens :long-running) ()
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

(deftest (record-tokens :long-running) ()
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

(deftest (while-tokens :long-running) ()
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
              (phenome instrumented :bin bin)
            (declare (ignorable stdout src))

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
              (is (equal bad-stmts gold-set-prefix)))))))))

(defsuite clang-super-mutants "Super mutants of clang objects."
            (clang-mutate-available-p))

(deftest (super-mutant-genome-works :long-running) ()
  (with-fixture fib-clang
    (let* ((mutant-a (copy *fib*))
           (mutant-b (copy *fib*))
           (*matching-free-var-retains-name-bias* 1.0))
      (apply-mutation mutant-a
        `(clang-cut (:stmt1 . ,(stmt-with-text mutant-a
                                               "x = x + y;"))))
      (apply-mutation mutant-b
        `(clang-cut (:stmt1 . ,(stmt-with-text mutant-b
                                               "y = t;"))))

      (let ((super (make-instance 'super-mutant
                     :mutants (list mutant-a mutant-b
                                    (copy mutant-b)))))
        (is (genome super))
        (is (phenome-p super))))))

(deftest (super-mutant-genome-preserves-unvarying-functions :long-running) ()
  "Switch should be omitted in functions which are the same across all mutants."
  (with-fixture huf-clang
    (let ((mutant-a (copy *huf*))
          (mutant-b (copy *huf*))
          (mutant-c (copy *huf*)))
      (apply-mutation mutant-a
        `(clang-cut (:stmt1 . ,(stmt-with-text mutant-a
                                               "h->n = 0;"))))
      (apply-mutation mutant-b
        `(clang-cut (:stmt1 . ,(stmt-with-text mutant-b
                                               "free(heap);"))))
      (apply-mutation mutant-c
        `(clang-cut (:stmt1 . ,(stmt-with-text mutant-b
                                               "heap->n--;"))))

      (let* ((super (make-instance 'super-mutant
                      :mutants (list mutant-a mutant-b
                                     mutant-c)))
             (obj (super-soft super)))
        (is (genome super))
        (is (phenome-p super))
        (mapcar (lambda (fun)
                  (is (eq (if (member (ast-name fun)
                                      '("_heap_create" "_heap_destroy"
                                        "_heap_remove")
                                      :test #'string=)
                              1
                              0)
                          (count-if [{eq :SwitchStmt} #'ast-class]
                                    (->> (function-body obj fun)
                                         (get-immediate-children obj))))))
                (functions obj))))))

(deftest (super-mutant-genome-has-union-of-global-decls :long-running) ()
  (with-fixture gcd-clang
    (let* ((mutant-a (->>
                      `(clang-insert (:stmt1 . ,(car (asts *gcd*)))
                                     (:stmt2 . ,(stmt-with-text *gcd*
                                                                "double a")))
                      (apply-mutation (copy *gcd*))))
           (mutant-b (copy mutant-a))
           (mutant-c (copy mutant-b)))
      (apply-mutation mutant-b
        `(clang-insert (:stmt1 . ,(second (roots mutant-b)))
                       (:stmt2 . ,(stmt-with-text mutant-b
                                                  "double b"))))
      (apply-mutation mutant-c
        `(clang-insert (:stmt1 . ,(second (roots mutant-c)))
                       (:stmt2 . ,(stmt-with-text mutant-c
                                                  "double c"))))
      (apply-mutation mutant-c
        `(clang-insert (:stmt1 . ,(second (roots mutant-c)))
                       (:stmt2 . ,(stmt-with-text mutant-c
                                                  "double r1"))))
      (let* ((super (make-instance 'super-mutant
                      :mutants (list mutant-a mutant-b
                                     mutant-c)))
             (obj (super-soft super)))
        (is (genome super))
        (is (phenome-p super))
        (let ((decls (mapcar #'source-text
                             (remove-if #'function-decl-p (roots obj)))))
          ;; Ordering between b and (c r1) is arbitrary, but a must
          ;; come first.
          (is (or (equal decls
                         '("double a" "double c" "double r1" "double b"))
                  (equal decls
                         '("double a" "double b" "double c" "double r1")))))))))

(deftest (super-mutant-genome-has-union-of-functions :long-running) ()
  (with-fixture huf-clang
    (let* ((mutant-a (copy *huf*))
           (mutant-b (copy *huf*))
           (mutant-c (copy *huf*)))
      (->> `(clang-cut (:stmt1 . ,(find-if [{string= "_heap_add"}
                                            #'ast-name]
                                           (functions mutant-a))))
           (apply-mutation mutant-a))
      (->> `(clang-cut (:stmt1 . ,(find-if [{string= "_heap_remove"}
                                            #'ast-name]
                                           (functions mutant-a))))
           (apply-mutation mutant-a))
      (->> `(clang-cut (:stmt1 . ,(find-if [{string= "_heap_add"}
                                            #'ast-name]
                                           (functions mutant-b))))
           (apply-mutation mutant-b))
      (->> `(clang-cut (:stmt1 . ,(find-if [{string= "_heap_remove"}
                                            #'ast-name]
                                           (functions mutant-c))))
           (apply-mutation mutant-c))

      (let* ((super (make-instance 'super-mutant
                      :mutants (list mutant-a mutant-b
                                     mutant-c)))
             (obj (super-soft super)))
        (is (genome super))
        (is (phenome-p super))
        (let ((functions (->> (functions obj)
                              (take 5)
                              (mapcar #'ast-name))))
          ;; Ordering between _heap_sort and _heap_destroy is
          ;; arbitrary, but _heap_create must come first.
          (is (or (equal functions
                         '("_heap_create" "_heap_destroy" "_heap_sort"
                           "_heap_add" "_heap_remove"))
                  (equal functions
                         '("_heap_create" "_heap_destroy" "_heap_sort"
                           "_heap_remove" "_heap_add")))))))))

(deftest (super-mutant-genome-can-insert-merged-function :long-running) ()
  (with-fixture huf-clang
    (let* ((mutant-a (copy *huf*))
           (mutant-b (copy *huf*))
           (mutant-c (copy *huf*))
           (*matching-free-var-retains-name-bias* 1.0))
      (->> `(clang-cut (:stmt1 . ,(find-if [{string= "_heap_add"}
                                            #'ast-name]
                                           (functions mutant-a))))
           (apply-mutation mutant-a))
      (->> `(clang-insert (:stmt1 . ,(stmt-with-text mutant-b
                                                     "_heap_sort(heap);"))
                          (:value1 . ,(stmt-with-text mutant-b
                                                      "heap->n++;")))
           (apply-mutation mutant-b))
      (->> `(clang-insert (:stmt1 . ,(stmt-with-text mutant-c
                                                     "_heap_sort(heap);"))
                          (:value1 . ,(stmt-with-text mutant-c
                                                      "heap->h[heap->n] = c;")))
           (apply-mutation mutant-c))



      (let* ((super (make-instance 'super-mutant
                      :mutants (list mutant-a mutant-b
                                     mutant-c)))
             (obj (super-soft super))
             (heap-add (find-if [{string= "_heap_add"} #'ast-name]
                                (functions obj)))
             (stmts (apply #'subseq (asts obj) (stmt-range obj heap-add))))
        (is (genome super))
        (is (phenome-p super))
        (is heap-add)
        (mapcar (lambda (fun)
                  (is (eq (if (eq heap-add fun)
                              1
                              0)
                          (count-if [{eq :SwitchStmt} #'ast-class]
                                    (->> (function-body obj fun)
                                         (get-immediate-children obj))))))
                (functions obj))
        (is (eq 1 (count-if [{eq :DefaultStmt} #'ast-class] stmts))
            "Super-function contains default statement.")
        (is (eq 2 (count-if [{eq :CaseStmt} #'ast-class] stmts))
            "Super-function contains correct number of case statements.")))))

(deftest super-mutant-genome-handles-function-prototypes ()
  (let ((mutant (from-string (make-clang) "int foo();")))
    (is (genome (make-instance 'super-mutant
                  :mutants (list (copy mutant) (copy mutant)))))))

(deftest (super-mutant-genome-detects-incompatible-functions :long-running) ()
  ;; These all fail because ast-args is set by clang-mutate and does not
  ;; update upon mutation
  #+(or )
  (let* ((base (from-string (make-clang)
                            "void foo(int a, int b) {}"))
         (remove-arg (copy base))
         (change-arg-type (copy base))
         (change-arg-name (copy base)))
    ;; Different argument count
    (apply-mutation remove-arg
                    `(clang-cut (:stmt1 . ,(stmt-with-text remove-arg
                                                           "int b"))))
    (signals mutate
      (genome (make-instance 'super-mutant
                :mutants (list base remove-arg))))
    ;; Different argument type
    (apply-mutation change-arg-type
                    `(clang-replace (:stmt1 . ,(stmt-with-text change-arg-type
                                                               "int b"))
                                    (:value1 . ,(make-statement :ParmVar
                                                                :finallistelt
                                                                '("char b")))))
    (signals mutate
      (genome (make-instance 'super-mutant
                :mutants (list base change-arg-type))))
    ;; Different argument name
    (apply-mutation change-arg-name
                    `(clang-replace (:stmt1 . ,(stmt-with-text change-arg-name
                                                               "int b"))
                                    (:value1 . ,(make-statement :ParmVar
                                                                :finallistelt
                                                                '("int c")))))
    (signals mutate
      (genome (make-instance 'super-mutant
                :mutants (list base change-arg-name)))))

  ;; Different return types
  (signals mutate
           (genome (make-instance 'super-mutant
                     :mutants
                     (list (from-string (make-clang)
                                        "void foo() {}")
                           (from-string (make-clang)
                                 "int foo() { return 1; }")))))

  ;; Prototype vs. complete function
  (signals mutate
           (genome (make-instance 'super-mutant
                     :mutants
                     (list (from-string (make-clang)
                                        "void foo() {}")
                           (from-string (make-clang)
                                        "void foo();"))))))

(deftest super-mutant-genome-detects-mismatched-globals ()
  (let* ((base (from-string (make-clang)
                            "int a; int b; int c;"))
         (variant (copy base)))
    (apply-mutation variant
                    `(clang-replace (:stmt1 . ,(stmt-with-text variant
                                                               "int b;"))
                                    (:value1 . ,(->> (find-or-add-type variant
                                                                       "char")
                                                     (make-var-decl "b")))))
    (signals mutate
      (genome (make-instance 'super-mutant
                :mutants (list base variant))))))

(deftest super-mutant-genome-detects-delete-function-body ()
  (let* ((base (from-string (make-clang)
                            "void foo() {}"))
         (variant (copy base)))
    ;; This is a useless mutation but it happens sometimes. Ensure
    ;; that it leads to a mutation error.
    (apply-mutation variant
      `(clang-cut (:stmt1 . ,(stmt-with-text variant "{}"))))
    (signals mutate
      (genome (make-instance 'super-mutant
                :mutants (list base variant))))))

(deftest collate-ast-variants-test ()
  ;; This function is intended to be called on asts, but it only
  ;; relies on EQUAL comparison of the keys so we can test it with
  ;; artificial data.

  ;; Simple case: all top-level decls line up
  (is (equal (collate-ast-variants '(((1 . a1) (2 . a2) (3 . a3))
                                     ((1 . b1) (2 . b2) (3 . b3))))
             '((a1 b1) (a2 b2) (a3 b3))))

  ;; Deleted AST
  (is (equal (collate-ast-variants '(((1 . a1) (2 . a2) (3 . a3))
                                     ((1 . b1) (3 . b3))))
             '((a1 b1) (a2 nil) (a3 b3))))

  ;; Inserted AST
  (is (equal (collate-ast-variants '(((1 . a1) (3 . a3))
                                     ((1 . b1) (2 . b2) (3 . b3))))
             '((a1 b1) (nil b2) (a3 b3))))

  ;; Deleted at beginning
  (is (equal (collate-ast-variants '(((1 . a1) (2 . a2) (3 . a3))
                                     ((2 . b2) (3 . b3))))
             '((a1 nil) (a2 b2) (a3 b3))))

  ;; Deleted at end
  (is (equal (collate-ast-variants '(((1 . a1) (2 . a2) (3 . a3))
                                     ((1 . b1) (2 . b2))))
             '((a1 b1) (a2 b2) (a3 nil))))

  ;; Inserted at beginning
  (is (equal (collate-ast-variants '(((2 . a2) (3 . a3))
                                     ((1 . b1) (2 . b2) (3 . b3))))
             '((nil b1) (a2 b2) (a3 b3))))

  ;; Inserted at end
  (is (equal (collate-ast-variants '(((1 . a1) (2 . a2))
                                     ((1 . b1) (2 . b2) (3 . b3))))
             '((a1 b1) (a2 b2) (nil b3))))

  ;; Multiple inserted ASTs
  (is (equal (collate-ast-variants '(((1 . a1) (3 . a3))
                                     ((1 . b1) (2 . b2) (4 . b4)
                                      (5 . b5) (3 . b3))))
             '((a1 b1) (nil b2) (nil b4) (nil b5) (a3 b3))))

  ;; 3 variants
  (is (equal (collate-ast-variants '(((1 . a1) (2 . a2) (3 . a3))
                                     ((1 . b1) (2 . b2) (3 . b3))
                                     ((1 . c1) (2 . c2) (3 . c3))))
             '((a1 b1 c1) (a2 b2 c2) (a3 b3 c3))))

  ;; 3 variants with inserts and deletes
  (is (equal (collate-ast-variants '(((1 . a1) (2 . a2) (3 . a3))
                                     ((1 . b1) (3 . b3))
                                     ((1 . c1) (2 . c2) (4 . c4)
                                      (3 . c3))))
             '((a1 b1 c1) (a2 nil c2) (nil nil c4) (a3 b3 c3))))


  ;; Swapped ASTs are not merged correctly. This is a known
  ;; limitation.
  (is (equal (collate-ast-variants '(((1 . a1) (2 . a2) (3 . a3))
                                     ((2 . b2) (1 . b1) (3 . b3))))
             '((a1 nil) (a2 b2) (nil b1) (a3 b3)))))

(deftest super-evolve-handles-mutation-failure ()
  (let* ((obj (from-string (make-instance 'mutation-failure-tester)
                           "int main() { return 0; }"))
         (*population* (list obj))
         (*fitness-evals* 0)
         (*cross-chance* 0)
         (*target-fitness-p* (lambda (fit) (declare (ignorable fit)) t)))
    (setf (fitness obj) 0)
    ;; Ensure the software objects raise mutation errors as expected
    (signals mutate
      (evolve (lambda (obj) (declare (ignorable obj)) 1)
              :max-evals 10
              :super-mutant-count 4))
    (handler-bind ((mutate (lambda (err)
                             (declare (ignorable err))
                             (invoke-restart 'ignore-failed-mutation))))
      ;; This should exit after evaluating the first super-mutant,
      ;; because *target-fitness-p* is trivially true.
      (evolve (lambda (obj) (declare (ignorable obj)) 1)
              :max-evals 10
              :super-mutant-count 4))
    ;; Despite errors, the first super-mutant should accumulate the
    ;; desired number of variants and evaluate all of them.
    (is (eq *fitness-evals* 4))))

(deftest (super-mutant-evaluate-works :long-running) ()
  (let* ((template "#include <stdio.h>
int main() { puts(\"~d\"); return 0; }
")
         (mutants (mapcar (lambda (i)
                            (from-string (make-clang)
                                         (format nil template i)))
                          '(1 2 3 4)))
         (super (make-instance 'super-mutant :mutants mutants)))
    (evaluate (lambda (obj)
                ;; Proxies are the same type as mutants
                (is (typep obj 'clang-base))
                (cons (some->> (phenome obj)
                               (shell)
                               (parse-integer))
                      (genome obj)))
              super)
    ;; Each variant printed the appropriate number
    (is (equal '(1 2 3 4) (mapcar [#'car #'fitness] mutants)))
    ;; Each proxy had genome identical to the corresponding mutant
    (is (equal (mapcar #'genome mutants)
               (mapcar [#'cdr #'fitness] mutants)))))


;;; Test SerAPI (low-level Coq interaction)
(defun serapi-available-p ()
  (set-serapi-paths)
  (zerop (nth-value 2 (shell "which ~a" *sertop-path*))))

(setf *serapi-timeout* 10)

(defsuite test-serapi "Coq SerAPI interaction." (serapi-available-p))

(in-readtable :serapi-readtable)

(defixture serapi
  (:setup (sleep 0.1)
          (setf *serapi-process* (make-serapi))
          (handler-bind ((serapi-timeout-error
                          (lambda (c)
                            (declare (ignorable c))
                            (invoke-restart 'use-empty-response))))
            ;; read two-line prologue
            (sel/cp/serapi-io::read-with-timeout *serapi-process* 2 0.1)
            (sel/cp/serapi-io::read-with-timeout *serapi-process* 2 0.1)))
  (:teardown
   (kill-serapi *serapi-process*)
   (setf *serapi-process* nil)))

(deftest can-start-end-serapi ()
  (is *sertop-path*)
  (let ((serapi (make-serapi)))
    (is serapi)
    (is (process-running-p serapi))
    (kill-serapi serapi)
    (sleep 0.1)
    (is (not (process-running-p serapi)))))

(deftest (with-serapi-creates-new-process :long-running) ()
  (is (not *serapi-process*))
  (with-serapi ()
    ;; locally binds `*serapi-process*'
    (write-to-serapi *serapi-process*
                     #!`((Test1 (Query () (Vernac "Print nat.")))))
    (is (member (mapcar {intern _ :sel/cp/serapi-io}
                        (list "Answer" "Test1" "Ack"))
                (read-serapi-response *serapi-process*)
                :test #'equal)))
  ;; `*serapi-process*' goes out of scope at end
  (is (not *serapi-process*)))

(deftest (with-serapi-can-capture-process :long-running) ()
  (with-fixture serapi
    (is (process-running-p *serapi-process*))
    (let ((serproc (make-serapi)))
      ;; serproc is a different process than *serapi-process*
      (is (not (eq serproc *serapi-process*)))
      (with-serapi (serproc)
        ;; *serapi-process is rebound to serproc inside with-serapi
        (is (eq serproc *serapi-process*))
        (write-to-serapi *serapi-process*
                         #!'((Test1 (Query () (Vernac "Print nat.")))))
        ;; writing to *serapi-process* also writes to serproc
        (is (member (mapcar {intern _ :sel/cp/serapi-io}
                            (list "Answer" "Test1" "Ack"))
                    (read-serapi-response serproc)
                    :test #'equal)))
      ;; serproc isn't killed after with-serapi ends
      (is (process-running-p serproc))
      (kill-serapi serproc))
    ;; *serapi-process isn't killed after with-serapi ends
    (is (process-running-p *serapi-process*))))

(deftest serapi-special-character-handling ()
  (let* ((src0 "[[\\\"expr\\\" ::== \\\"coords\\\" \\\\n || \\\"coords\\\" \\\\n \\\"expr\\\" <{< fun x _ y => Row x y >}>]] ;;.")
         (sanitized0 (sanitize-process-string src0))
         ;; a /\\n b /\n c should differentiate \n from an extraneous n
         ;; preceded by backslashes
         (src1 "a /\\\\\\n b /\\\\n c")
         (sanitized1 (sanitize-process-string src1)))
    (is (equal sanitized0
               "[[\\\\\\\"expr\\\\\\\" ::== \\\\\\\"coords\\\\\\\" \\\\\\\\n || \\\\\\\"coords\\\\\\\" \\\\\\\\n \\\\\\\"expr\\\\\\\" <{< fun x _ y => Row x y >}>]] ;;."))
    (is (equal (sel/cp/serapi-io::unescape-coq-string sanitized0)
               "[[\\\"expr\\\" ::== \\\"coords\\\" \\\\n || \\\"coords\\\" \\\\n \\\"expr\\\" <{< fun x _ y => Row x y >}>]] ;;."))
    (is (equal sanitized0 (sel/cp/serapi-io::escape-coq-string sanitized0)))
    (is (equal sanitized1 "a /\\\\\\\\  b /\\\\\\\\n c"))
    (is (equal sanitized1 (sel/cp/serapi-io::escape-coq-string sanitized1)))
    (is (equal (sel/cp/serapi-io::unescape-coq-string sanitized1)
               "a /\\\\  b /\\\\n c"))))

(deftest can-read-write-serapi ()
  (with-fixture serapi
    ;; write basic "Print nat." query and read response
    (write-to-serapi *serapi-process*
                     #!`((TestQ (Query () (Vernac "Print nat.")))))
    (let ((response (read-serapi-response *serapi-process*)))
      (is response)
      (is (= 5 (length response)))
      (is (member (mapcar {intern _ :sel/cp/serapi-io}
                          (list "Answer" "TestQ" "Ack"))
                  response
                  :test #'equal))
      (is (member (mapcar {intern _ :sel/cp/serapi-io}
                          (list "Answer" "TestQ" "Completed"))
                  response
                  :test #'equal)))))

(deftest can-run-coq-vernacular ()
  (with-fixture serapi
    (let ((vernac "Print nat."))
      (write-to-serapi *serapi-process*
                       #!`((TestQ (Query () (Vernac ,VERNAC)))))
      (let ((resp1 (read-serapi-response *serapi-process*))
            (resp2 (run-coq-vernacular vernac :qtag #!'TestQ)))
        (is (equal resp1 resp2))))))

(deftest serapi-is-type-works ()
  (with-fixture serapi
    (let* ((resp (run-coq-vernacular "Print nat." :qtag #!'TestQ))
           ;; (Answer TestQ Ack)
           (ack (nth 0 resp))
           ;; (Feedback (id 1) (route 0) (contents Processed))
           (feedback (nth 1 resp))
           (answer-sym (intern "Answer" :sel/cp/serapi-io))
           (feedback-sym (intern "Feedback" :sel/cp/serapi-io)))
      (is (sel/cp/serapi-io::is-type answer-sym ack))
      (is (sel/cp/serapi-io::is-type feedback-sym feedback))
      (is (not (sel/cp/serapi-io::is-type answer-sym feedback)))
      (is (not (sel/cp/serapi-io::is-type feedback-sym ack))))))

(deftest serapi-feedback-parsing-works ()
  (with-fixture serapi
    (let* ((resp (run-coq-vernacular "Print nat." :qtag #!'TestQ))
           (ack (nth 0 resp))
           (feedback (nth 1 resp)))
      (is (not (sel/cp/serapi-io::feedback-id ack)))
      (is (not (sel/cp/serapi-io::feedback-route ack)))
      (is (not (sel/cp/serapi-io::feedback-contents ack)))
      (is (eql 1 (sel/cp/serapi-io::feedback-id feedback)))
      (is (eql 0 (sel/cp/serapi-io::feedback-route feedback)))
      (is (eql (intern "Processed" :sel/cp/serapi-io)
               (sel/cp/serapi-io::feedback-contents feedback))))))

(deftest serapi-message-content-works ()
  (let ((*package* (find-package 'sel/cp/serapi-io)))
    (let ((resp1
            (list (intern "Feedback")
                  (list (list (intern "id") 1)
                        (list (intern "route") 0)
                        (list (intern "contents")
                              (list (intern "Message")
                                    (intern "Notice")
                                    ()
                                    (list (intern "Some")
                                          (list (intern "AST")
                                                (list (intern "tree")))
                                          (intern "here")))))))
          (resp2 (mapcar #'intern (list "Answer" "TestQ" "Ack"))))
      (is (equal (list (intern "Some")
                       (list (intern "AST")
                             (list (intern "tree")))
                       (intern "here"))
                 (sel/cp/serapi-io::message-content
                  (sel/cp/serapi-io::feedback-contents resp1))))
      (is (not (sel/cp/serapi-io::message-content
                (sel/cp/serapi-io::feedback-contents resp2)))))))

(deftest serapi-message-level-works ()
  (with-fixture serapi
    (let* ((resp (run-coq-vernacular "Print nat." :qtag #!'TestQ))
           ;; (Answer TestQ Ack)
           (ack (nth 0 resp))
           ;; (Feedback (id 1) (route 0) (contents (Message Notice () ...)))
           (message (nth 2 resp)))
      (is (eql (intern "Notice" :sel/cp/serapi-io)
               (sel/cp/serapi-io::message-level
                (sel/cp/serapi-io::feedback-contents message))))
      (is (not (sel/cp/serapi-io::message-level
                (sel/cp/serapi-io::feedback-contents ack)))))))

(deftest (serapi-answer-parsing-works :long-running) ()
  (with-fixture serapi
    (let ((ids (add-coq-string "Definition test := true.")))
      (write-to-serapi *serapi-process*
                       #!`((TestQ (Query () (Ast ,(FIRST IDS))))))
      (let* ((resp1 (mapcar #'tag-loc-info
                            (read-serapi-response *serapi-process*)))
             ;; (Answer TestQ Ack)
             (ack (nth 0 resp1))
             ;; (Answer TestQ (ObjList ((CoqAst ...))))
             (objlist-ast (nth 1 resp1))
             (resp2 (run-coq-vernacular "Check test." :qtag #!'TestC))
             ;; (Feedback (id 2) (route 0) (contents Processed))
             (feedback (nth 1 resp2))
             (none (write-to-serapi *serapi-process*
                         #!`((TestP (Query ((pp ((pp_format PpStr))))
                                           (Ast ,(FIRST IDS)))))))
             (resp3 (read-serapi-response *serapi-process*))
             ;; (Answer TestP (ObjList ((CoqString Definition test := true.))))
             (objlist-string (nth 1 resp3)))
        (declare (ignorable none))
        ;; verify answer-content
        (is (equal (list (lastcar ack) (lastcar objlist-ast) nil
                         (lastcar objlist-string))
                   (mapcar #'sel/cp/serapi-io::answer-content
                           (list ack objlist-ast feedback objlist-string))))
        ;; verify answer-string
        (is (equal (list nil nil nil "Definition test := true.")
                   (mapcar [#'sel/cp/serapi-io::answer-string
                               #' sel/cp/serapi-io::answer-content]
                           (list ack objlist-ast feedback objlist-string))))
        ;; verify answer-ast is nil for non-AST answers
        (is (equal (list nil nil nil)
                   (mapcar [#'sel/cp/serapi-io::answer-ast
                            #'sel/cp/serapi-io::answer-content]
                           (list ack feedback objlist-string))))
        ;; verify answer-ast is a VernacDefinition
        (is (sel/cp/serapi-io::is-type
             (intern "VernacDefinition" :sel/cp/serapi-io)
             (nth 1 (sel/cp/serapi-io::answer-ast
                     (sel/cp/serapi-io::answer-content objlist-ast)))))))))

(deftest serapi-end-of-response-parsing-works ()
  (let ((resp1 (mapcar {intern _ :sel/cp/serapi-io}
                       (list "Answer" "TestQ" "Completed")))
        (resp2 (list (intern "Sexplib.Conv.Of_sexp_error" :sel/cp/serapi-io)
                     (list (intern "Failure" :sel/cp/serapi-io)
                           "Failure message")
                     (intern "etc" :sel/cp/serapi-io)))
        (resp3 (mapcar {intern _ :sel/cp/serapi-io}
                       (list "Answer" "TestQ" "Ack"))))
    (is (equal (list t t nil)
               (mapcar #'is-terminating (list resp1 resp2 resp3))))
    (is (equal (list nil t nil)
               (mapcar #'is-error (list resp1 resp2 resp3))))))

(deftest (can-add-and-lookup-coq-string :long-running) ()
  (with-fixture serapi
    (let* ((add-str "Inductive test :=   | T1 : test   | T2 : test.")
           (id (add-coq-string add-str)))
      (is (= 1 (length id)))
      (is (integerp (first id)))
      (let ((lookup-str (lookup-coq-string (first id))))
        (is (equal add-str lookup-str))))))

(deftest (can-convert-coq-ast-to-string :long-running) ()
  (with-fixture serapi
    (let* ((add-str "Inductive test :=   | T1 : test   | T2 : test.")
           (id (add-coq-string add-str)))
      (is (= 1 (length id)))
      (is (integerp (first id)))
      (let* ((lookup-ast (lookup-coq-ast (first id)))
             (ast-str (lookup-coq-string lookup-ast)))
        (is (equal add-str ast-str))))))

(deftest (can-lookup-coq-pretty-printed-repr-1 :long-running) ()
  (with-fixture serapi
    (let ((add-str "Inductive test :=   | T1 : test   | T2 : test."))
      (add-coq-string add-str)
      (write-to-serapi *serapi-process* #!`((Query () (Vernac "Print test."))))
      (let ((resp1 (read-serapi-response *serapi-process*))
            (resp2 (lookup-coq-pp "test")))
        (is (equal resp2 (coq-message-contents resp1)))
        (is (some {eql (intern "Notice" :sel/cp/serapi-io) }
                  (coq-message-levels resp1)))))))

(deftest (can-load-coq-file :long-running) ()
  (with-fixture serapi
    (let ((ids (load-coq-file (coq-test-dir "NatBinop.v"))))
      (is (equal '(2 3 4) ids)))))

(deftest can-tokenize-coq-types ()
  (let ((test1 "test1 : nat -> bool")
        (test2 "test2 : (bool -> (bool -> bool)) -> (bool -> nat) -> nat)")
        (test3 "test3 : a -> ((b -> (c -> d) -> e) -> f) -> g"))
    (is (equal (tokenize-coq-type test1)
               '("test1" :COLON "nat" :-> "bool")))
    (is (equal (tokenize-coq-type test2)
               '("test2" :COLON ("bool" :-> ("bool" :-> "bool")) :->
                 ("bool" :-> "nat") :-> "nat")))
    (is (equal (tokenize-coq-type test3)
               '("test3" :COLON "a" :->
                 (("b" :-> ("c" :-> "d") :-> "e") :-> "f")
                 :-> "g")))))

(deftest (can-lookup-coq-types :long-running) ()
  (with-fixture serapi
    ;; Look up some built-in Coq types
    (is (equal (check-coq-type "True") '("True" :COLON "Prop")))
    (is (equal (check-coq-type "true") '("true" :COLON "bool")))
    (is (equal (check-coq-type "nat")  '("nat" :COLON "Set")))
    (is (equal (check-coq-type "7")    '("7" :COLON "nat")))
    (is (equal (check-coq-type "negb") '("negb" :COLON "bool" :-> "bool")))
    (is (equal (check-coq-type "plus")
               '("Nat.add" :COLON "nat" :-> "nat" :-> "nat")))))

(deftest (can-search-coq-types :long-running) ()
  ;; NOTE: may need to change if Coq version or default load libraries change.
  (with-fixture serapi
    (let* ((fns (search-coq-type "nat -> nat -> nat -> nat"))
           (fn-names (mapcar #'car fns)))
      (is (= 3 (length fns)))
      (is (every {member _ '("Nat.log2_iter" "Nat.sqrt_iter" "Nat.bitwise")
                         :test #'equal}
                 fn-names)))))

(deftest (can-create-coq-expressions-1 :long-running) ()
  (with-fixture serapi
    ;; Nat.add 5 3
    (let ((e-ast1 (make-coq-application (make-coq-var-reference #!'plus)
                                        (make-coq-integer 5)
                                        (make-coq-integer 3)))
          (e-ast2 (make-coq-match
                   #!'RegularStyle
                   (make-coq-application (make-coq-var-reference #!'S)
                                         (make-coq-var-reference #!'O))
                   (make-coq-case-pattern "CPatAtom" #!'O)
                   (make-coq-integer 0)
                   (make-coq-case-pattern
                    "CPatCstr"
                    (make-coq-ident #!'S)
                    (make-coq-case-pattern "CPatAtom" #!'_))
                   (make-coq-integer 1))))
      (let ((e-str1 (lookup-coq-string e-ast1 :input-format #!'CoqExpr))
            (e-str2 (lookup-coq-string e-ast2 :input-format #!'CoqExpr)))
        (is (equal e-str1 "plus 5 3"))
        (is (equal e-str2 "match S O with | O => 0 | S _ => 1 end"))))))


;;; Test Coq software objects
(defsuite test-coq "Coq software object tests." (serapi-available-p))

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

;;; conflict asts
(defsuite conflict-ast-tests "Conflict ast tests.")

(deftest conflict-ast.1 ()
  (let ((c1 (make-conflict-ast
             :child-alist '((1 a) (2 b))
             :default-children '(c)))
        (c2 (make-conflict-ast
             :child-alist '((2 d) (3 e))
             :default-children '(f))))
    (let ((c (combine-conflict-asts c1 c2)))
      (is (equalp (conflict-ast-child-alist c)
                  '((1 a f) (2 b d) (3 c e)))
          "conflict ast alists are merged")
      (is (equalp (conflict-ast-default-children c) '(c f))
          "conflict ast defaults are merged"))))

;;; cpp-scan

(defun is-comma (c) (eql c #\,))

(defsuite cpp-scan-tests "Tests of CPP-SCAN")

(deftest cpp-scan-basic ()
  (is (null (cpp-scan "" #'is-comma)))
  (is (eql (cpp-scan "," #'is-comma) 0))
  (is (eql (cpp-scan "," #'is-comma :start 0) 0))
  (is (null (cpp-scan ", " #'is-comma :start 1)))
  (is (null (cpp-scan " , " #'is-comma :end 1))))

(deftest cpp-scan-simple-parens ()
  (is (eql (cpp-scan " , " #'is-comma) 1))
  (is (null (cpp-scan "()" #'is-comma)))
  (is (null (cpp-scan "(,)" #'is-comma)))
  (is (eql (cpp-scan "(,)," #'is-comma) 3))
  (is (eql (cpp-scan "()," #'is-comma) 2))
  (is (null (cpp-scan "[,]" #'is-comma)))
  (is (eql (cpp-scan "[,]," #'is-comma) 3))
  (is (eql (cpp-scan "[]," #'is-comma) 2))
  (is (null (cpp-scan "{,}" #'is-comma)))
  (is (eql (cpp-scan "{,}," #'is-comma) 3))
  (is (eql (cpp-scan "{}," #'is-comma) 2)))

(deftest cpp-scan-literals ()
  (is (null (cpp-scan "\",\"" #'is-comma)))
  (is (null (cpp-scan "\"\\\",\"" #'is-comma)))
  (is (eql (cpp-scan "\"\\\",\" , " #'is-comma) 6))
  (is (eql (cpp-scan " ,\",\"" #'is-comma) 1))
  (is (null (cpp-scan "','" #'is-comma)))
  (is (eql (cpp-scan "',' , " #'is-comma) 4))
  (is (eql (cpp-scan "'\\n'," #'is-comma) 4)))

(deftest cpp-scan-comments ()
  (is (null (cpp-scan "/* , , , */" #'is-comma)))
  (is (eql (cpp-scan "/**/," #'is-comma) 4))
  (is (eql (cpp-scan "/***/," #'is-comma) 5))
  (is (eql (cpp-scan "/* // */ ," #'is-comma) 9)))

(deftest cpp-scan-partials ()
  (is (null (cpp-scan "/" #'is-comma)))
  (is (null (cpp-scan "\"" #'is-comma)))
  (is (null (cpp-scan "'" #'is-comma)))
  (is (null (cpp-scan "'," #'is-comma)))
  (is (null (cpp-scan "\"" #'is-comma)))
  (is (null (cpp-scan "\"," #'is-comma)))
  (is (eql (cpp-scan "/," #'is-comma) 1)))


(deftest cpp-scan-line-comments ()
  (is (null (cpp-scan " // jsajd ,adas" #'is-comma)))
  (is (eql (cpp-scan (format nil " // . ~% , ") #'is-comma) 8)))

(deftest cpp-scan-nested-comments ()
  (is (eql (cpp-scan "( /* ( */ ) , " #'is-comma) 12))
  (is (eql (cpp-scan "( /* ) */ ) , " #'is-comma) 12))
  (is (eql (cpp-scan "( /* [ */ ) , " #'is-comma) 12))
  (is (eql (cpp-scan "( /* ] */ ) , " #'is-comma) 12))
  (is (eql (cpp-scan "( /* { */ ) , " #'is-comma) 12))
  (is (eql (cpp-scan "( /* } */ ) , " #'is-comma) 12))
  (is (eql (cpp-scan "[ /* ( */ ] , " #'is-comma) 12))
  (is (eql (cpp-scan "[ /* ) */ ] , " #'is-comma) 12))
  (is (eql (cpp-scan "[ /* [ */ ] , " #'is-comma) 12))
  (is (eql (cpp-scan "[ /* ] */ ] , " #'is-comma) 12))
  (is (eql (cpp-scan "[ /* { */ ] , " #'is-comma) 12))
  (is (eql (cpp-scan "[ /* } */ ] , " #'is-comma) 12))
  (is (eql (cpp-scan "{ /* ( */ } , " #'is-comma) 12))
  (is (eql (cpp-scan "{ /* ) */ } , " #'is-comma) 12))
  (is (eql (cpp-scan "{ /* [ */ } , " #'is-comma) 12))
  (is (eql (cpp-scan "{ /* ] */ } , " #'is-comma) 12))
  (is (eql (cpp-scan "{ /* { */ } , " #'is-comma) 12))
  (is (eql (cpp-scan "{ /* } */ } , " #'is-comma) 12)))

(deftest cpp-scan-nested-parens ()
  (is (null (cpp-scan "(())" #'is-comma)))
  (is (null (cpp-scan "((,))" #'is-comma)))
  (is (null (cpp-scan "(,())" #'is-comma)))
  (is (null (cpp-scan "((),)" #'is-comma)))
  (is (null (cpp-scan "([])" #'is-comma)))
  (is (null (cpp-scan "(,[])" #'is-comma)))
  (is (null (cpp-scan "([,])" #'is-comma)))
  (is (null (cpp-scan "([],)" #'is-comma)))
  (is (null (cpp-scan "({})" #'is-comma)))
  (is (null (cpp-scan "(,{})" #'is-comma)))
  (is (null (cpp-scan "({,})" #'is-comma)))
  (is (null (cpp-scan "({},)" #'is-comma)))

  (is (null (cpp-scan "[()]" #'is-comma)))
  (is (null (cpp-scan "[(,)]" #'is-comma)))
  (is (null (cpp-scan "[,()]" #'is-comma)))
  (is (null (cpp-scan "[(),]" #'is-comma)))
  (is (null (cpp-scan "[[]]" #'is-comma)))
  (is (null (cpp-scan "[,[]]" #'is-comma)))
  (is (null (cpp-scan "[[,]]" #'is-comma)))
  (is (null (cpp-scan "[[],]" #'is-comma)))
  (is (null (cpp-scan "[{}]" #'is-comma)))
  (is (null (cpp-scan "[,{}]" #'is-comma)))
  (is (null (cpp-scan "[{,}]" #'is-comma)))
  (is (null (cpp-scan "[{},]" #'is-comma)))

  (is (null (cpp-scan "{()}" #'is-comma)))
  (is (null (cpp-scan "{(,)}" #'is-comma)))
  (is (null (cpp-scan "{,()}" #'is-comma)))
  (is (null (cpp-scan "{(),}" #'is-comma)))
  (is (null (cpp-scan "{[]}" #'is-comma)))
  (is (null (cpp-scan "{,[]}" #'is-comma)))
  (is (null (cpp-scan "{[,]}" #'is-comma)))
  (is (null (cpp-scan "{[],}" #'is-comma)))
  (is (null (cpp-scan "{{}}" #'is-comma)))
  (is (null (cpp-scan "{,{}}" #'is-comma)))
  (is (null (cpp-scan "{{,}}" #'is-comma)))
  (is (null (cpp-scan "{{},}" #'is-comma))))

(deftest cpp-scan-angle-brackets ()
  (is (null (cpp-scan "" #'is-comma :angle-brackets t)))
  (is (null (cpp-scan "<,>" #'is-comma :angle-brackets t)))
  (is (eql (cpp-scan "," #'is-comma :angle-brackets t) 0))
  (is (eql (cpp-scan "<>," #'is-comma :angle-brackets t) 2)))

(deftest cpp-scan-skip-first ()
  (is (eql (cpp-scan "()(" (lambda (c) (eql c #\())) 0))
  (is (eql (cpp-scan "()(" (lambda (c) (eql c #\()) :skip-first t) 2))
  (is (eql (cpp-scan "(())(" (lambda (c) (eql c #\()) :skip-first t) 4))
  (is (eql (cpp-scan "(()(" (lambda (c) (eql c #\()) :start 1 :skip-first t)
           3))
  (is (eql (cpp-scan " ()(" (lambda (c) (eql c #\()) :start 1 :skip-first t)
           3)))
