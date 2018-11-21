;;; clang-diff.lisp --- Calculate and render Lisp AST diffs at the command line
;;;
;;;
;;; The following git configuration will register clang-diff as a tool
;;; to be used with @code{git difftool} (see
;;; @url{https://git-scm.com/docs/git-difftool}).
;;;
;;;     # Set clang-diff as the default difftool.
;;;     [diff]
;;;     	tool = clang-diff
;;;
;;;     # Command-line to use with clang-diff.
;;;     [difftool "clang-diff"]
;;;     	cmd = "clang-diff $LOCAL $REMOTE
;;;
;;;     # Optionally don't prompt.
;;;     [difftool]
;;;     	prompt = false
;;;
;;; To help clang resolve includes it may be necessary to add include
;;; paths to the invocation of clang-diff.  E.g., putting the
;;; following in the .git/config of a particular repo with headers in
;;; a "src/" subdirectory will ensure clang can find those headers.
;;; (By default -I takes "." passing the working directory to clang.)
;;;
;;;     [difftool "clang-diff"]
;;;     	cmd = "clang-diff -I .,src $LOCAL $REMOTE"
;;;
;;; @texi{clang-diff}
(defpackage :software-evolution-library/ast-diff/clang
  (:nicknames :sel/ast-diff/clang)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :uiop
        :split-sequence
        :software-evolution-library/utility
        :software-evolution-library/ast-diff/ast-diff
        :software-evolution-library/software/clang
        :software-evolution-library)
  (:shadowing-import-from
   :uiop :getenv :directory-exists-p :copy-file :appendf :parse-body
   :ensure-list :simple-style-warning :ensure-gethash :ensure-function
   :if-let :emptyp :featurep)
  (:shadowing-import-from :software-evolution-library/view
                          +color-RED+ +color-GRN+ +color-RST+)
  (:export :run-clang-diff))
(in-package :software-evolution-library/ast-diff/clang)
(in-readtable :curry-compose-reader-macros)
;;; TODO:
;;;  - Implement a string diff to recurse into strings.
;;;    (This should have a configurable separator.)
;;;  - Work on clang-diff tool's git configuration (or maybe this has
;;;    to be implemented in clang-diff itself) to limit application by
;;;    extension.

(setf *note-out* *error-output*)
(defun handle-set-verbose-argument (level)
  (when (>= level 4) (setf *shell-debug* t))
  (setf *note-level* level))

(defun run-clang-diff (&aux (self (argv0)) (args *command-line-arguments*)
                         raw flags (includes '(".")) (colorp t)
                         (on-parse-error 'error)
                         (comp-db (probe-file "compile_commands.json")))
  "Run a clang diff on *COMMAND-LINE-ARGUMENTS*."
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
Compare C/C++ source files AST by AST.

Options:
 -r, --raw                 output diff in raw Sexp (default is as text)
 -c, --comp-db [PATH]      path to clang compilation database
 -C, --no-color            inhibit color printing
 -I [DIRS]                 include ,-delimited DIRS in flags to clang
                           default \".\" includes current working directory
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
      ("-c" "--comp-db" (setf comp-db (pop args)))
      ("-C" "--no-color" (setf colorp nil))
      ("-I" "-I" (setf includes (split-sequence #\, (pop args)
                                                :remove-empty-subseqs t)))
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
    (setf flags
          (mappend [{list "-I"} {concatenate 'string (pwd) "/"}] includes))
    (when comp-db
      (push (cons :build-path comp-db)
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
          (if colorp
              (print-diff diff *standard-output*
                          (format nil "~a[-" +color-RED+)
                          (format nil "-]~a" +color-RST+)
                          (format nil "~a{+" +color-GRN+)
                          (format nil "+}~a" +color-RST+))
              (print-diff diff)))
      ;; Only exit with 0 if the two inputs match.
      (quit (if (every [{eql :same} #'car] diff) 0 1)))))
