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
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/ast-diff/ast-diff
        :software-evolution-library/software/clang)
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

(defun handle-errors-option (error-option)
  (intern error-option :software-evolution-library/ast-diff/clang))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append +common-command-line-options+
            (remove-if {equalp "compiler"}
                       +clang-command-line-options+
                       :key #'caar)
            `((("errors" #\e) :type string :initial-value "error"
               :action #'handle-errors-option
               :documentation
               "how to handle parse errors (ignore, warn, error)")
              (("includes" #\I) :type string :initial-value "."
               :action #'handle-comma-delimited-argument
               :documentation "include ,-delimited DIRS in flags to clang")
              (("compilation-database" #\C) :type string :initial-value "."
               :documentation "path to a clang compilation database")
              (("raw" #\r) :type boolean :optional t
               :documentation "output diff in raw Sexp (default is as text)")
              (("no-color" #\C) :type boolean :optional t
               :documentation "inhibit color printing")))))

(define-command clang-diff (file1 file2 &spec +command-line-options+)
  "Compare C/C++ source in FILE1 and FILE2 by AST."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (when help (show-help-for-clang-diff))
  (unless (every #'resolve-file (list file1 file2))
    (exit-command clang-diff 2 (error "Missing file.")))
  ;; Setup clang-mutate options.
  (setf flags
        (mappend [{list "-I"} {concatenate 'string (namestring (pwd)) "/"}]
                 includes))
  (when compilation-database
    (push (cons :build-path compilation-database)
          *clang-mutate-additional-args*))
  ;; Create the diff.
  (let ((diff
         (handler-bind ((mutate ; Ignore clang-mutate errors.
                         (lambda (e)
                           (ecase errors
                             (ignore (invoke-restart 'keep-partial-asts))
                             (warn (warn "Parse error: ~a" e)
                                   (invoke-restart 'keep-partial-asts))
                             (error (error e))))))
           (ast-diff
            (from-file (make-instance 'clang :flags flags) file1)
            (from-file (make-instance 'clang :flags flags) file2)))))
    ;; Print according to the RAW option.
    (if raw
        (writeln (ast-diff-elide-same diff) :readably t)
        (if no-color
            (print-diff diff :no-color t)
            (print-diff diff)))
    ;; Only exit with 0 if the two inputs match.
    (if uiop/image:*lisp-interaction*
        (quit (if (every [{eql :same} #'car] diff) 0 1))
        diff)))
