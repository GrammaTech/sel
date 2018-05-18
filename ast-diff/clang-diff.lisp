;;; clang-diff.lisp --- render ast diffs to html
;;
;; TODO: Add instructions for setting up git-difftool to use.
;;       - This can be done using the "-x" command to git-difftool
;;       - This can be done with configuration
;;         difftool.<tool>.path = $SEL/bin/clang-diff
;;         difftool.<tool>.cmd = clang-diff $LOCAL $REMOTE|colordiff
;;
(defpackage :software-evolution-library/clang-diff
  (:nicknames :sel/clang-diff)
  (:use :common-lisp
        :alexandria
        :cl-arrowz
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
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
(in-readtable :curry-compose-reader-macros)

(defun print-diff (diff &optional (stream *standard-output*))
  (let ((*print-escape* nil))
    (labels ((text (content)
               (etypecase content
                 (string content)
                 (clang-ast (ast-text content))
                 (list (mapconcat #'text content "")))))
      (mapc (lambda-bind ((type . content))
              (ecase type
                (:same (write (text content) :stream stream))
                (:delete (write "[-" :stream stream)
                         (write (text content) :stream stream)
                         (write "-]" :stream stream))
                (:insert (write "{+" :stream stream)
                         (write (text content) :stream stream)
                         (write "+}" :stream stream))
                (:recurse (print-diff content stream))))
            diff))))

(defun run-clang-diff (&aux (self (argv0)) (args *command-line-arguments*)
                         raw)
  "Run `clang-instrument' on *COMMAND-LINE-ARGUMENTS*."
  (flet ((report (fmt &rest args)
           (apply #'format *error-output* (concatenate 'string "~a: " fmt)
                  self args)))
    (when (or (not args)
              (< (length args) 1)
              (string= (subseq (car args) 0 (min 2 (length (car args))))
                       "-h")
              (string= (subseq (car args) 0 (min 3 (length (car args))))
                       "--h"))
      (format t "Usage: ~a [OPTION]... FILES
Compare FILES line by line.

Options:
 -r, --raw                 output diff in raw Sexp (default is as text)

Built with SEL version ~a, and ~a version ~a.~%"
              self +software-evolution-library-version+
              (lisp-implementation-type) (lisp-implementation-version))
      (quit))
    ;; Argument handling and checking.
    (getopts (args :unknown :return)
      ("-r" "--raw" (setf raw t)))
    (when (= (length args) 1)
      (report "missing operand after '~a'~%" (car args))
      (report "Try '~a --help' for more information." self)
      (quit 2))
    (when (> (length args) 2)
      (report "extra operand '~a'~%" (third args))
      (report "Try '~a --help' for more information." self)
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
    ;; Create the diff.
    (let ((diff
           (ast-diff (from-file (make-instance 'clang) (first args))
                     (from-file (make-instance 'clang) (second args)))))
      ;; Print according to the RAW option.
      (if raw
          (writeln (ast-diff-elide-same diff) :readably t)
          (print-diff diff))
      ;; Only exit with 0 if the two inputs match.
      (quit (if (every [{eql :same} #'car] diff) 0 1)))))
