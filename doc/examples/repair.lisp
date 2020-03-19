;;; repair.lisp --- repair software
(defpackage :software-evolution-library/doc/examples/repair
  (:use :gt/full
        :cl-store
        :uiop/image
        :software-evolution-library
        :software-evolution-library/software/asm
        :software-evolution-library/software/elf
        :software-evolution-library/software/cil
        :software-evolution-library/utility/debug)
  (:export :main))
(in-package :software-evolution-library/doc/examples/repair)
(in-readtable :curry-compose-reader-macros)


;;; Configuration Fitness and Runtime
(defvar *orig*    nil         "Original version of the program to be run.")
(defvar *evals*   (expt 2 18) "Maximum number of test evaluations.")
(defvar *path*    nil         "Path to Assembly file.")
(defvar *rep*     'range      "Program representation to use.")
(defvar *res-dir* nil         "Directory in which to save results.")
(defvar *script*  nil         "The shell script fitness function")
(setf *max-population-size* (expt 2 9)
      *fitness-predicate* #'>
      *cross-chance* 2/3
      *tournament-size* 2
      *tournament-eviction-size* 2)

(defun run (src)
  (with-temporary-file-of (:pathname src :type "s") (genome-string src)
    (multiple-value-bind (stdout stderr errno)
	(shell *script* src)
      (declare (ignorable stderr) (ignorable stdout))
      errno)))


;;; Command line repair driver
(defun main (args)
  (in-package :repair)
  (let ((help "Usage: repair ORIGINAL TEST-SCRIPT [OPTIONS...]
 Repair an assembly file.

ORIGINAL:
  A text file of assembler code or (if using the \".store\"
  extension) a serialized assembly software object.

Options:
 -h,--help ------------- show this help message
 -v,--verbose NUM ------ verbosity level 0-4
 -V,--version ---------- print version and exit
 -t,--target-fitness N - set target fitness to N~%")
        (version
         (format nil
          #+ccl "repair version ~a using Clozure Common Lisp (CCL)~%"
          #+sbcl "repair version ~a using Steel Bank Common Lisp (SBCL)~%"
          #+ecl "repair version ~a using Embeddable Common Lisp (ECL)~%"
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (let ((raw (shell "git describe --always")))
              (subseq raw 0 (1- (length raw))))))))
    (setf *note-level* 1)

    ;; process command line options
    (getopts (args)
      ("-v" "--verbose"   (let ((lvl (parse-integer (pop args))))
                            (when (>= lvl 4) (setf *shell-debug* t))
                            (setf *note-level* lvl)))
      ("-t" "--target"    (let ((target (parse-number (pop args))))
                            (setf *target-fitness-p*
                                  (lambda (obj)
                                    (or (= target (fitness obj))
                                        (funcall *fitness-predicate*
                                                 (fitness obj) target)))))))

    ;; check command line arguments
    (when (or (<= (length args) 2)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h")
              (string= (car args) "-V")
              (string= (car args) "--version"))
      (if (or (string= (car args) "-V")
              (string= (car args) "--version"))
          (progn (format t version) (quit))
          (format t help))
      (quit))

    ;; process mandatory command line arguments
    (setf *path*   (pop args))
    (setf *script* (pop args))
    (setf *script* (format nil "~a ~~a" *script*))

    (unless *orig*
      (setf *orig*
            (from-file (make-instance
                           (case (intern (string-upcase (pathname-type *path*)))
                             (s 'asm)
                             (c 'cil)
                             (t 'elf)))
                       *path*)))

    (when (string= (pathname-type (pathname *path*)) "store")
      (setf *orig* (restore *path*)))

    ;; write out version information
    (note 1 version)

    ;; write out configuration parameters
    (note 1 "Parameters:~%~S~%"
          (mapcar (lambda (param)
                    (cons param (eval param)))
                  '(*path*)))

    ;; sanity check
    (setf (fitness *orig*) (run *orig*))
    (when (= (fitness *orig*) 0) ; 0 should work, but it may be better
                                 ; to parse the expected number of
                                 ; passing tests and error out if
                                 ; fitness is less than expected
                                 ; passsing
      (format *error-output* "Original program has bad fitness!")
      (quit))

    ;; save the original
    (store *orig* (make-pathname :directory *res-dir*
                                 :name "original"
                                 :type "store"))

    ;; populate population
    (unless *population* ;; don't re-populate an existing population
      (note 1 "Building the Population")
      (setf *population* (loop :for n :below *max-population-size*
                            :collect (copy *orig*))))

    ;; run repair
    (evolve #'run :max-evals *evals*)

    ;; finish up
    (note 1 "done after ~a fitness evaluations~%" *fitness-evals*)
    (close (pop *note-out*))))
