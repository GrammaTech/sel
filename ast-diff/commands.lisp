;;; commands.lisp --- Calculate and render AST diffs at the command line
;;;
;;; The following git configuration will register ast-diff as a tool
;;; to be used with @code{git difftool} (see
;;; @url{https://git-scm.com/docs/git-difftool}).
;;;
;;;     # Set ast-diff as the default difftool.
;;;     [diff]
;;;     	tool = ast-diff
;;;
;;;     # Command-line to use with ast-diff.  Piping through
;;;     # colordiff is optional but nice to highlight diffs.
;;;     [difftool "ast-diff"]
;;;     	cmd = "ast-diff $LOCAL $REMOTE|colordiff"
;;;
;;;     # Optionally don't prompt.
;;;     [difftool]
;;;     	prompt = false
;;;
;;; For clang language differences and merges, to help clang resolve
;;; includes, it may be necessary to add include paths to the
;;; invocation of ast-diff.  E.g., putting the following in the
;;; .git/config of a particular repo with headers in a "src/"
;;; subdirectory will ensure clang can find those headers.  (By
;;; default -I takes "." passing the working directory to clang.)
;;;
;;;     [difftool "ast-diff"]
;;;     	cmd = "ast-diff -I .,src $LOCAL $REMOTE"
;;;
;;; @texi{ast-diff-commands}
(defpackage :software-evolution-library/ast-diff/commands
  (:nicknames :sel/ast-diff/commands
              :software-evolution-library/ast-diff/commands)
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/ast-diff/ast-diff
        :software-evolution-library/software/ast
        :software-evolution-library/software/simple
        :software-evolution-library/software/project
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/java
        :software-evolution-library/software/java-project
        :software-evolution-library/software/javascript
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/json
        :software-evolution-library/software/lisp)
  (:import-from :uiop :writeln :truenamize :nest)
  (:shadow :merge :ast-diff)
  (:export :ast-diff :ast-merge))
(in-package :software-evolution-library/ast-diff/commands)
(in-readtable :curry-compose-reader-macros)
;;; TODO: Work on clang-diff tool's git configuration (or maybe this
;;;       has to be implemented in clang-diff itself) to limit
;;;       application by extension.


;;; Command-line interface to ast differencing.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append +common-command-line-options+
            +interactive-command-line-options+
            +clang-command-line-options+
            +project-command-line-options+
            +clang-project-command-line-options+
            `((("language" #\L) :type string :optional t
               :documentation "language to use for source")
              (("raw" #\r) :type boolean :optional t
               :documentation "output diff as raw ASTs (default is as text)")
              (("no-color" #\C) :type boolean :optional t
               :documentation "inhibit color printing")))))

(define-command ast-diff (source1 source2 &spec +command-line-options+)
  "Compare source code in SOURCE1 and SOURCE2 by AST."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (when help (show-help-for-ast-diff))
  (setf *note-out* (list *error-output*))
  (unless (every #'resolve-file (list source1 source2))
    (exit-command ast-diff 2 (error "Missing source.")))
  (unless language
    (setf language (guess-language source1 source2)))
  ;; Create the diff.
  (let ((diff
         (apply #'sel/ast-diff/ast-diff:ast-diff
                (mapcar
                 {create-software _
                                  :language language
                                  :compiler compiler
                                  :flags flags
                                  :build-command build-command
                                  :artifacts artifacts
                                  :compilation-database compilation-database}
                 (list source1 source2)))))
    ;; Print according to the RAW option.
    (if raw
        (writeln (ast-diff-elide-same diff) :readably t)
        (print-diff diff :no-color no-color))
    ;; Only exit with 0 if the two inputs match.
    (wait-on-manual manual)
    (exit-command ast-diff
                  (if (every [{eql :same} #'car] diff) 0 1)
                  diff)))

(nest
 (eval-when (:compile-toplevel :load-toplevel :execute))
 (defparameter +ast-merge-command-line-options+)
 (append +command-line-options+)
 ;; Triples (base, left, right) of relevant options.
 (mappend
  (lambda (arg-spec)
    (destructuring-bind
          ((long short) &key type optional initial-value action documentation)
        arg-spec
      (declare (ignorable short))
      (mapcar (lambda (which)
                (append
                 (list (list (concatenate 'string which "-" long)))
                 (when type (list :type type))
                 (when optional (list :optional optional))
                 (when action (list :action action))
                 (when initial-value (list :initial-value initial-value))
                 (nest
                  (when documentation)
                  (list :documentation)
                  (concatenate 'string documentation " for " which " file"))))
              '("my" "old" "your")))))
 (append +clang-command-line-options+
         +project-command-line-options+
         +clang-project-command-line-options+))

(define-command ast-merge (my-file old-file your-file
                                   &spec +ast-merge-command-line-options+)
  "Merge changes from old-file->my-file and old-file->your-file."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose raw no-color))
  (when help (show-help-for-ast-merge))
  (setf *note-out* (list *error-output*))
  (unless (every #'resolve-file (list old-file my-file your-file))
    (exit-command ast-merge 2 (error "Missing source.")))
  (setf old-file (truenamize old-file)
	my-file (truenamize my-file)
	your-file (truenamize your-file)
        language (or language (guess-language old-file my-file your-file)))
  (macrolet
      ((expand-options-for-which-files (language which)
         "Expand the options for WHICH calling `create-software' appropriately."
         `(create-software
           ,@(let ((pairs (mapcar
                           «list [#'intern {concatenate 'string which "-"}
                                           #'symbol-name]
                                 #'identity»
                           '(file compiler flags build-command
                             artifacts compilation-database))))
               (list* (caar pairs)
                      :language language
                      (mappend «list [#'make-keyword #'second] {cons 'or}»
                               (cdr pairs)))))))
    ;; Force OUT-DIR when running as a command line utility and merging
    ;; whole directories.  We can't write multiple files to STDOUT.
    (when (and (not uiop/image:*lisp-interaction*)
               (not out-dir)
               (directory-p old-file))
      (setf out-dir (resolve-out-dir-from-source old-file))
      (note 0 "Merging directories, out-dir set to ~a." out-dir))
    ;; Don't write notes when we're writing merge results to STDOUT.
    (unless out-dir (setf *note-level* 0))

    (multiple-value-bind (new-merged unstable)
        (converge
         (expand-options-for-which-files language "OLD")
         (expand-options-for-which-files language "MY")
         (expand-options-for-which-files language "YOUR"))
      ;; Write the result, either to out-dir or to STDOUT.
      (if out-dir
          (if (directory-p old-file)
              (to-file new-merged
                       (make-pathname :directory (append out-dir (list "merged"))))
	      (to-file new-merged
                       (resolve-store-path-from-out-dir-and-name
                        out-dir
                        (pathname-name old-file) "merged"
                        (pathname-type old-file))))
          (genome-string new-merged *standard-output*))

      (wait-on-manual manual)
      (exit-command ast-merge (if unstable 1 0) new-merged))))
