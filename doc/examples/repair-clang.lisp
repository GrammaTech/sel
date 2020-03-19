;;; repair-clang.lisp --- repair C program
;;; Repair C program, displaying progress using the viewer
(defpackage :software-evolution-library/doc/examples/repair-clang
  (:use :gt/full
        :uiop/image
        :software-evolution-library
        :software-evolution-library/components/test-suite
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang
        :software-evolution-library/utility/debug
        :software-evolution-library/view)
  (:import-from :uiop/image :*command-line-arguments*)
  (:export :repair-clang))
(in-package :software-evolution-library/doc/examples/repair-clang)
(in-readtable :curry-compose-reader-macros)


;;; Configuration Fitness and Runtime
(defvar *orig*       nil         "Original version of the program to be run.")
(defvar *path*       nil         "Path to C source file.")
(defvar *evals*      (expt 2 18) "Maximum number of test evaluations.")
(defvar *num-tests*  nil         "Number of tests run in test suite.")
(defvar *test-suite* nil         "Test suite.")
(defvar *script*     nil         "The shell script fitness function")

(defvar *res-dir* (directory-namestring (truename "."))
  "Directory in which to save results.")

(defun configure-view (&key run)
  (setf *view-application-name* "Example Repair")
  ;; set name of individual run
  (when run (setf *view-run-name* run))
  ;; maximum view width
  (setf *view-length* 75)
  (setf *view-max-note-lines* 10)
  (setf *view-max-mutations* 5)
  ;; number of lines of the best candidate to show
  (setf *view-max-best-lines* 32))

(defun run (obj)
  "Compile software object OBJ and return number of tests passed."
  (with-temporary-file (:pathname bin)
    (phenome obj :bin bin)
    (loop :for test :in (test-cases *test-suite*)
       ;; `evaluate' runs test and returns 1 if the exit code is 0 (success)
       ;; and returns 0 on failure
       :summing (evaluate bin test))))


;;; Command line repair driver
(defun run-repair-clang ()
  (repair-clang *command-line-arguments*))

(defun repair-clang (args)
  (in-package :repair-clang)
  (let ((help "Usage: repair-clang ORIGINAL TEST-SCRIPT TESTS [OPTIONS...]
 Repair a C file, ORIGINAL, using the shell script TEST-SCRIPT to run unit
tests. TESTS indicates the number of tests in the test suite. The test
script should accept two arguments: the executable under test and a number
from 0 up to tests - 1 indicating which test case to run.

Options:
 -h,--help ------------- show this help message
   ,--view ------------- display a view of evolution progress
 -v,--verbose NUM ------ verbosity level 0-4
 -V,--version ---------- print version and exit")
        (version
         (format nil
          #+ccl "repair version ~a using Clozure Common Lisp (CCL)~%"
          #+sbcl "repair version ~a using Steel Bank Common Lisp (SBCL)~%"
          #+ecl "repair version ~a using Embeddable Common Lisp (ECL)~%"
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (let ((raw (shell "git describe --always")))
              (subseq raw 0 (1- (length raw)))))))
        show-view)

    ;; check command line arguments (help/version info)
    (when (or (< (length args) 3)
           (and (>= (length args) 3)
                (string= (subseq (car args) 0 2) "-h")
                (string= (subseq (car args) 0 3) "--h")
                (string= (car args) "-V")
                (string= (car args) "--version")))
      (if (or (string= (car args) "-V")
              (string= (car args) "--version"))
          (progn (format t version) (quit))
          (format t help))
      (quit))

    ;; mandatory arguments
    (setf *path* (pop args))
    (setf *script* (pop args))
    (setf *num-tests* (parse-integer (pop args)))

    ;; process optional command line flags
    (getopts (args)
      ("-v" "--verbose"
            (let ((lvl (parse-integer (pop args))))
              (when (>= lvl 4) (setf *shell-debug* t))
              (setf *note-level* lvl)))
      ("" "--view" (setf show-view t)))

    ;; write out version information
    (note 1 version)

    ;; configurations
    (setf *max-population-size* (expt 2 9)
          *fitness-predicate* #'>
          *cross-chance* 2/3
          *tournament-size* 2
          *tournament-eviction-size* 2)

    ;; write out configuration parameters
    (note 1 "Parameters:~%~S~%"
          (mapcar (lambda (param)
                    (cons param (eval param)))
                  '(*path* *script* *num-tests* *max-population-size*
                    *cross-chance* *tournament-size*
                    *tournament-eviction-size*)))

    ;; fitness testing variables
    (setf *target-fitness-p*
          ;; fitness is the number of passing tests
          ;; target fitness is `*num-tests*', i.e., a candidate that passes
          ;; every test
          (lambda (obj)
            (or (= *num-tests* (fitness obj))
                (funcall *fitness-predicate*
                         (fitness obj) *num-tests*)))

          ;; set up a test suite to run each unit test
          *test-suite*
          (make-instance
           'test-suite
           :test-cases
           (loop :for i :below *num-tests*
              ;; each test case runs `*script*' with arguments
              ;; `:bin' (replaced with the path to a compiled variant)
              ;; and `i', the test number
              :collecting (make-instance
                           'test-case
                           :program-name *script*
                           :program-args (list :bin (write-to-string i))))))

    ;; load software object from file
    (unless *orig*
      (setf *orig*
            (from-file (make-instance 'clang) *path*)))

    ;; sanity check: make sure the starting fitness isn't 0
    ;; there's no inherent reason to disallow this, but for this example
    ;; it probably indicates an issue
    (setf (fitness *orig*) (run *orig*))
    (when (= (fitness *orig*) 0)
      (format *error-output* "Original program has bad fitness!")
      (quit))

    ;; initialize the population
    (unless *population* ;; don't re-populate an existing population
      (note 1 "Building the population.")
      (setf *population* (loop :for n :below *max-population-size*
                            :collect (copy *orig*))))

    ;; start the viewer
    (when show-view
      (note 1 "Starting interactive viewer.")
      (configure-view)
      (view-start))

    ;; run repair
    (note 0 "Beginning repair.")
    (handler-bind
        ;; it's normal for occasional errors during mutations or crossover,
        ;; so the restarts 'try-another-mutation and 'ignore-failed-mutation
        ;; are provided to help with recovery
        ((no-mutation-targets
          (lambda (e)
            (declare (ignorable e))
            (invoke-restart 'try-another-mutation)))
         (t (lambda (e)
              (cond
                ((find-restart 'ignore-failed-mutation)
                 (invoke-restart 'ignore-failed-mutation))
                ((find-restart 'try-another-mutation)
                 (invoke-restart 'try-another-mutation))
                ;; an unexpected error, so fail/exit
                (t e)))))
      (evolve #'run :max-evals *evals*))

    ;; finish up
    (note 1 "Done after ~a fitness evaluations." *fitness-evals*)
    ;; find the candidate with the highest fitness and save to file
    (let ((best (extremum *population* *fitness-predicate*
                          :key #'fitness)))
      (to-file best (make-pathname
                     :directory *res-dir*
                     :name (format nil "~a-repair" (pathname-name *path*))
                     :type (pathname-type *path*))))))
