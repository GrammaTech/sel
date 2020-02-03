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
   #-windows :drakma
   #+gt :testbot
   #-windows :trace-db
   :uiop
   ;; TODO: Remove those which aren't actually needed for testing.
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/command-line
   #-windows :software-evolution-library/command-line-rest
   #-windows :software-evolution-library/rest
   :software-evolution-library/components/instrument
   #-windows :software-evolution-library/components/clang-instrument
   :software-evolution-library/components/clang-tokens
   #-windows :software-evolution-library/components/condition-synthesis
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
   #-windows :software-evolution-library/components/serapi-io
   :software-evolution-library/components/test-suite
   #-windows :software-evolution-library/components/traceable
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
   #-windows  :software-evolution-library/software/csurf-asm
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
   :software-evolution-library/software/git-project
   :software-evolution-library/software/simple
   :software-evolution-library/software/source
   :software-evolution-library/software/styleable
   :software-evolution-library/software/with-exe
   :software-evolution-library/stefil-plus)
  #-windows (:import-from :hunchentoot)
  #-windows (:import-from :osicat :file-permissions :pathname-as-directory)
  (:shadowing-import-from :common-lisp :type)
  (:shadowing-import-from :software-evolution-library :size)
  (:shadowing-import-from :clack :stop)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:shadowing-import-from :uiop :getenv :quit #-windows :parameter-error )
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
(defvar *project*     nil "Software used in project fixtures.")
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

#-windows
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

(defixture asm-super-rip-test-att
  (:setup
   (setf *soft* (from-file (make-instance 'asm-super-mutant)
                           (asm-test-dir "re_set_syntax.s.att"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*)
         (asm-test-dir "re_set_syntax.io"))
   (target-function-name *soft* "re_set_syntax"))

  (:teardown (setf *soft* nil)))

(defixture asm-super-dead-stack-test
  (:setup
   (setf *soft* (from-file (make-instance 'asm-super-mutant)
                           (asm-test-dir "re_set_syntax.s.att"))
         (fitness-harness *soft*) (software-dir "asm-super-mutant-fitness.c"))
   (setf (sel/sw/asm-super-mutant::io-file *soft*)
         (asm-test-dir "re_set_syntax.io"))
   (target-function-name *soft* "re_set_syntax"))

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

#-windows
(defixture rest-server
  (:setup (unless *clack* (setf *clack* (initialize-clack))))
  (:teardown (clack:stop *clack*)(setf *clack* nil)(setf *rest-client* nil)))

#-windows
(let (old-standard-out old-error-out)
  (defixture fact-rest-server
    (:setup
     (setf old-standard-out *standard-output*
           old-error-out *error-output*
           *standard-output* (make-broadcast-stream)
           *error-output* (make-broadcast-stream))
     (define-command-async-rest (fact-entry :environment (*population*))
         ((n integer) &spec +common-command-line-options+)
       "Test that canonical REST endpoints work. Computes factorial."
       #.(format nil
                 "~%Built from SEL ~a, and ~a ~a.~%"
                 +software-evolution-library-version+
                 (lisp-implementation-type) (lisp-implementation-version))
       (declare (ignorable quiet verbose language))
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

#-windows
(defixture gcd-clang
  (:setup
   (setf *gcd*
         (from-file (make-clang :compiler "clang")
                    (gcd-dir "gcd.c"))))
  (:teardown
   (setf *gcd* nil)))

#+windows
(defixture gcd-clang
  (:setup
   (setf *gcd*
         (from-file (make-clang :compiler "clang")
                    (gcd-dir "gcd.windows.c")))
   (setf (sel/sw/new-clang::include-dirs *gcd*)
         (split ";" (uiop:getenv "INCLUDE"))))
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
(defsuite asm "ASM representation." #+windows nil)


;;; ASM-SUPER-MUTANT representation.
(defsuite asm-super-mutant
    "ASM-SUPER-MUTANT representation."
  ;; Only run these tests if we found libpapi.so.
  *lib-papi*)

;;; Command Line tests
(defsuite cl "Command Line tool tests.")


;;; REST tests
#-windows
(defsuite rest "REST API tests.")


;;; CSURF-ASM representation.
#-windows
(progn

  (defsuite csurf-asm "CSURF-ASM representation."))


;;;
;;; Test the TASK-RUNNER modules.
;;;
(defsuite task-runner "TASK-RUNNER tests.")


;;; ELF representation.
;;;
;;; NOTE: Currently failing because not populating .text section.
;;;
(defsuite elf "ELF representation." :silent)

(defsuite clang "Clang representation." (clang-mutate-available-p))


;;; Detailed clang mutation tests
;;;
;;; These all run the entire mutate method, rather that just
;;; apply-mutation, adjusting the good and bad picks to get
;;; predictable results. And they check the results of each mutation
;;; in as much detail as possible.
(defsuite clang-mutations "Detailed clang mutation tests."


;;;; Clang w/ mutation fodder representation.
  (defsuite clang-w-fodder "Clang w/ mutation fodder representation."


;;;; Clang utility methods.
(defsuite clang-utility "Clang utility methods."

  (defsuite java "JAVA representation."

  (defsuite javascript "Javascript representation." (acorn-available-p))

(defsuite javascript-project "Javascript project."


;;;; Range representation.
(defsuite range-representation "Range representation.")


;;;; Mutation analysis and statistics collection tests.
(defsuite mutation-analysis
            "Mutation analysis and statistics collection tests."


;;;; Ancestry tests.
(defsuite clang-ancestry "Ancestry tests."

(defsuite csurf-asm-ancestry "Ancestry tests.")


;;;; Diff tests.
(defsuite diff "Diff tests.")


;;;; Population tests.
(defsuite population "Population tests."


;;;; Fix compilation tests.
  (defsuite fix-compilation "Fix compilation tests."


;;;; Crossover tests.
(defsuite clang-crossover "Crossover tests."


;;;; Misc. mutation tests.
(defsuite misc-mutations "Misc. mutation tests."


;;;; Adaptive-mutation tests.
  (defsuite adaptive-mutation "Adaptive-mutation tests.")


;;;; Database tests.
(defsuite database "Database tests.")


;;;; Instrumentation tests.
(defsuite instrumentation "Instrumentation tests."

  (defsuite clang-instrumentation "Tests for Clang instrumentation."

  (defsuite java-instrumentation "Tests for Java instrumentation."
  ;; (zerop (nth-value 2 (shell "which java-mutator")))
  :silent)


;;;; Traceable tests.
(defsuite traceable "Traceable tests."


;;;; Tests of declaration and type databases on clang objects.
(defsuite declaration-type-databases
            "Tests of declaration and type databases on clang objects."


;;;; Lisp representation.
  (defsuite sexp "Sexp representation.")


;;;; Mutations and evaluation of clang expressions in Lisp form.
(defsuite clang-expression
            "Mutation and evaluation of clang expressions in Lisp form."


;;;; Utility tests.
  (defsuite utility "Utility tests.")


;;;; Command-line tests.
(defsuite command-line "Command line tests")


;;;; Project tests.
(defsuite clang-project "Project tests."


;;;; Tests that require bear.
  (defsuite bear "Clang representation."


#-windows
(progn
;;;; Condition synthesis tests.
(defsuite condition-synthesis "Condition synthesis tests."


;;;; Selection tests.
(defsuite selection "Selection tests.")


;;;; Style features tests.
(defsuite style-features "Style features tests."


;;;; Clang syntactic contexts.
(defsuite clang-syntactic-contexts "Clang syntactic contexts."


;;;; Clang scope and type tests.
(defsuite clang-scopes-and-types "Clang scope and type tests."
            (clang-mutate-available-p))


;;;; Types and traces tests.
(defsuite type-traces "Types and traces tests."


;;;; Clang tokenizer tests.
(defsuite clang-tokenizer "Clang tokenizer tests."

(defsuite clang-super-mutants "Super mutants of clang objects."

(defsuite test-serapi "Coq SerAPI interaction." (serapi-available-p))


;;; Test Coq software objects
(defsuite test-coq "Coq software object tests." (serapi-available-p))



;;; conflict asts
(defsuite conflict-ast "Conflict ast tests.")

(defsuite cpp-scan "Tests of CPP-SCAN")

