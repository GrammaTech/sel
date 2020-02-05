(defpackage :software-evolution-library/test/util
  (:nicknames :sel/test/util)
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/stefil-plus
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable)
  (:export :test
           :+etc-dir+
           :+gcd-dir+
           :+grep-prj-dir+
           :+multi-file-dir+
           ;; Other functions
           :stmt-with-text
           :stmt-starting-with-text
           ;; Directory functions
           :gcd-dir
           :fib-dir
           ;; Variables referenced in tests
           :*tfos*
           :*soft*
           :*gcd*
           :*binary-search*
           :*fib*
           :*test*
           :*project*
           ;; Fixtures.
           :soft
           :range
           :double-range
           :gcd-elf))
(in-package :software-evolution-library/test/util)
(in-readtable :curry-compose-reader-macros)

(defvar *tfos* nil "Another software used in tests.")
(defvar *soft* nil "Software used in tests.")
(defvar *gcd* nil "Holds the gcd software object.")
(defvar *binary-search* nil "Holds the binary_search software object.")
(defvar *fib* nil "Holds the fibonacci software object.")
(defvar *test* nil "Variable to hold evaluation function for tests.")
(defvar *project* nil "Software used in project fixtures.")

(defroot test)

(define-constant +etc-dir+
    (append (pathname-directory
             #.(or *compile-file-truename*
                   *load-truename*
                   *default-pathname-defaults*))
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

(define-constant +clang-format-dir+ (append +etc-dir+ (list "clang-format"))
  :test #'equalp
  :documentation "Location of the clang-format example directory")

(define-constant +expand-arithmatic-op-dir+
    (append +etc-dir+ (list "expand-arithmatic-op"))
  :test #'equalp
  :documentation "Location of the expand arithmatic op example dir")

(define-constant +explode-for-loop-dir+
    (append +etc-dir+ (list "explode-for-loop"))
  :test #'equalp
  :documentation "Location of the explode for loop example dir")

(define-constant +fib-dir+
    (append +etc-dir+ (list "fib"))
  :test #'equalp
  :documentation "Location of the fib example dir")

(define-constant +condition-synthesis-dir+
    (append +etc-dir+ (list "condition-synthesis"))
  :test #'equalp
  :documentation "Path to condition synthesis examples.")

(define-constant +javascript-dir+ (append +etc-dir+ (list "javascript"))
  :test #'equalp
  :documentation "Path to directory holding Javascript test programs.")


;;; Functions on constants.
(defun gcd-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +gcd-dir+))

(defun clang-format-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +clang-format-dir+))

(defun expand-arithmatic-op-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +expand-arithmatic-op-dir+))

(defun explode-for-loop-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +explode-for-loop-dir+))

(defun fib-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +fib-dir+))

(defun javascript-dir (path)
  (merge-pathnames-as-file (make-pathname :directory +javascript-dir+)
                           path))


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


;;; Software.
(define-software soft (software)
  ((genome :initarg :genome :accessor genome :initform nil)))

(defvar *soft-mutate-errors* nil
  "Control when mutations on soft objects throw errors.")


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
