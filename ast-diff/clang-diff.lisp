;;; clang-diff.lisp --- render ast diffs to html
(defpackage :software-evolution-library/clang-diff
  (:nicknames :sel/clang-diff)
  (:use :common-lisp
        :alexandria
        :cl-arrowz
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :uiop
        :software-evolution-library/utility
        :software-evolution-library/ast-diff
        :software-evolution-library)
  (:shadowing-import-from
   :uiop :getenv :directory-exists-p :copy-file :appendf :parse-body
   :ensure-list :simple-style-warning :ensure-gethash :ensure-function
   :if-let :emptyp :featurep)
  (:export :diff-to-html))
(in-package :software-evolution-library/clang-diff)

(defun run-clang-diff (&aux (self (argv0)) (args *command-line-arguments*)
                         unified)
  "Run `clang-instrument' on *COMMAND-LINE-ARGUMENTS*."
  (declare (ignorable unified))
  (when (or (not args)
            (< (length args) 1)
            (string= (subseq (car args) 0 (min 2 (length (car args))))
                     "-h")
            (string= (subseq (car args) 0 (min 3 (length (car args))))
                     "--h"))
    (format t "Usage: ~a [OPTION]... FILES
Compare FILES line by line.

Options:
 -U NUM, --unified [NUM]   output NUM (default 3) lines of unified context

Built with SEL version ~a, and ~a version ~a.~%"
            self +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
    (quit))
  ;; Argument handling and checking.
  (getopts (args :unknown :return)
    ("-U" "--unified" (setf unified (parse-number (pop args)))))
  (when (= (length args) 1)
    (format *error-output* "~a: missing operand after '~a'~%" self (car args))
    (format *error-output* "~a: Try '~a --help' for more information." self self)
    (quit 2))
  (when (> (length args) 2)
    (format *error-output* "~a: extra operand '~a'~%" self (third args))
    (format *error-output* "~a: Try '~a --help' for more information." self self)
    (quit 2))
  (when (some #'identity
              (mapcar (lambda (file)
                        (unless (probe-file file)
                          (format *error-output*
                                  "~a: ~a: No such file or directory~%"
                                  self (third args))
                          t))
                      args))
    (quit 2))
  ;; Run the diff
  (let ((diff (diff-software (from-file (make-instance 'clang) (first args))
                             (from-file (make-instance 'clang) (second args)))))
    (pprint diff)))
