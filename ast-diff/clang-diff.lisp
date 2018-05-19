;;; clang-diff.lisp --- render ast diffs to html
;;;
;;; 
;;; The following git configuration will register clang-diff as a tool
;;; to be used with @code{git difftool}.
;;;
;;;     # Set clang-diff as the default difftool.
;;;     [diff]
;;;     	tool = clang-diff
;;;
;;;     # Command-line to use with clang-diff.  Piping through
;;;     # colordiff is optional but nice to highlight diffs.
;;;     [difftool "clang-diff"]
;;;     	cmd = "clang-diff $LOCAL $REMOTE|colordiff"
;;;
;;;     # Optionally don't prompt.
;;;     [difftool]
;;;     	prompt = false
;;;
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

(setf *note-out* *error-output*)
(defun handle-set-verbose-argument (level)
  (when (>= level 4) (setf *shell-debug* t))
  (setf *note-level* level))

(defun run-clang-diff (&aux (self (argv0)) (args *command-line-arguments*)
                         raw flags
                         (on-parse-error 'error)
                         (comp-db (probe-file "compile_commands.json")))
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
 -C, --comp-db [PATH]      path to clang compilation database
 -v, --verbose [NUM]       verbosity level 0-4
 -e, --errors [OPT]        how to handle parse errors 0-2
                            ignore---silently ignore
                            warn-----warn and ignore
                            error----error and quit (default)

Built with SEL version ~a, and ~a version ~a.~%"
              self +software-evolution-library-version+
              (lisp-implementation-type) (lisp-implementation-version))
      (quit))
    ;; Argument handling and checking.
    (getopts (args :unknown :return)
      ("-r" "--raw" (setf raw t))
      ("-C" "--comp-db" (setf comp-db (pop args)))
      ("-v" "--verbose" (handle-set-verbose-argument
                         (parse-integer (pop args))))
      ("-e" "--errors" (setf on-parse-error (intern (pop args) :clang-diff))))
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
    ;; Setup clang-mutate options.
    (setf flags (list "-I" (pwd)))
    (when comp-db
      (push (list "-C" comp-db)
            *clang-mutate-additional-args*))
    ;; Create the diff.
    (let ((diff
           (handler-bind ((mutate ; Ignore clang-mutate errors.
                            (lambda (e)
                              (ecase on-parse-error
                                (ignore (invoke-restart 'keep-partial-asts))
                                (warn (warn "Parse error: ~a" e)
                                      (invoke-restart 'keep-partial-asts))
                                (error (error e))))))
             (ast-diff
              (from-file (make-instance 'clang :flags flags) (first args))
              (from-file (make-instance 'clang :flags flags) (second args))))))
      ;; Print according to the RAW option.
      (if raw
          (writeln (ast-diff-elide-same diff) :readably t)
          (print-diff diff))
      ;; Only exit with 0 if the two inputs match.
      (quit (if (every [{eql :same} #'car] diff) 0 1)))))
