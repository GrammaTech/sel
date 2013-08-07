;;; repair.lisp --- repair software

;; Copyright (C) 2013  Eric Schulte

;;; Commentary:

;;; Code:
(in-package :repair)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))


;;; Configuration Fitness and Runtime
(defvar *orig* nil "Original version of the program to be run.")
(setf *max-population-size* (expt 2 9)
      *fitness-predicate* #'<
      *cross-chance* 2/3
      *tournament-size* 2
      *tournament-eviction-size* 2)

(defun run (asm)
  (with-temp-file (bin)
    (loop :for positive :below positive-test-numb :collect
       (multiple-value-bind (stdout stderr errno) (shell "test.sh ~a ~a" bin)
         (declare (ignorable stderr) (ignorable stdout))
         (if (zerop errno) 1 0)))))


;;; Shell script helpers
(defun throw-error (&rest args)
  (apply #'note 0 args)
  (quit))

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (and ,short (string= ,arg ,short))
                            (and ,long  (string= ,arg ,long)))
                        ,@body))
                    forms)))))


;;; Command line optimization driver
(defun repair (&optional (args *arguments*))
  (in-package :repair)
  (let ((help "Usage: opt ASM-FILE [OPTIONS...]
 Repair an assembly file.

ASM-FILE:
  A text file of assembler code or (if using the \".store\"
  extension) a serialized assembly software object.

Options:
 -v,--verbose NUM ------ verbosity level 0-4
 -V,--version ---------- print version and exit
 -w,--work-dir DIR ----- use an sh-runner/work directory~%")
        (version
         (format nil
          #+ccl "repair version ~a using Clozure Common Lisp (CCL)~%"
          #+sbcl "repair version ~a using Steel Bank Common Lisp (SBCL)~%"
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (let ((raw (shell "git describe --always")))
              (subseq raw 0 (1- (length raw))))))))
    (setf *note-level* 1)

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
    (setf *path* (pop args))

    (when (string= (pathname-type (pathname *path*)) "store")
      (setf *orig* (restore *path*)))

    ;; process command line options
    (getopts
     ("-v" "--verbose"   (let ((lvl (parse-integer (pop args))))
                           (when (>= lvl 4) (setf *shell-debug* t))
                           (setf *note-level* lvl)))
     ("-w" "--work-dir"  (setf *work-dir* (pop args))))
    (unless *orig*
      (setf *orig* (from-file (make-instance (case *rep*
                                               (asm 'asm-perf)
                                               (light 'asm-light)
                                               (range 'asm-range)))
                              *path*)))
    (when linker (setf (linker *orig*) linker))
    (when flags  (setf (flags  *orig*) flags))

    ;; write out version information
    (note 1 version)

    ;; write out configuration parameters
    (note 1 "Parameters:~%~S~%"
          (mapcar (lambda (param)
                    (cons param (eval param)))
                  '(*path*)))

    ;; sanity check
    (when (= (fitness *orig*) #| TODO: fitness of the original |#)
      (throw-error "Original program has bad fitness!"))

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
    (evolve #'run #| TODO: implement (see software-optimization::evolve) |#)

    ;; finish up
    (note 1 "done after ~a fitness evaluations~%" *fitness-evals*)
    (close (pop *note-out*))))
