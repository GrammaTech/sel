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
        :software-evolution-library/software/lisp)
  (:import-from :uiop :writeln :truenamize)
  (:shadow :merge :ast-diff)
  (:export :save-to))
(in-package :software-evolution-library/ast-diff/commands)
(in-readtable :curry-compose-reader-macros)
;;; TODO: Work on clang-diff tool's git configuration (or maybe this
;;;       has to be implemented in clang-diff itself) to limit
;;;       application by extension.

;;; TODO: I don't see why `save-to' is needed instead of just using `to-file'.
(defgeneric save-to (soft out-dir sub))

(defmethod save-to ((soft t) out-dir sub)
  (let ((dest (make-pathname :directory (append out-dir (list sub)))))
    (unless (probe-file dest)
      (to-file (copy soft) dest))))

(defun guess-language-from (&rest sources)
  (flet ((%guess-language-from (source)
           (unless (directory-p source)
             (second (find-if (lambda (pair)
                                (member (pathname-type source) (car pair)
                                        :test #'equalp))
                              '((("lisp") lisp)
                                (("java") java)
                                (("js") javascript)
                                (("c" "cpp" "h" "hpp" "cc" "cxx" "hxx")
                                 clang)))))))
    (let ((guesses (mapcar #'%guess-language-from sources)))
      (when (= 1 (length (remove-duplicates guesses)))
        (car guesses)))))


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

(defun create-software-from
    (path language
     &optional flags compiler compilation-database build-command)
  "Build software object of type LANGUAGE from PATH."
  (from-file (apply
              #'make-instance
              language
              (append
               (when flags
                 (list :flags flags))
               (when (and compiler (member language '(clang #| clang-project |#)))
                 (list :compiler compiler))
               (when compilation-database
                 (list :compilation-database compilation-database))
               (when (and compiler (subtypep language 'project))
                 (list :build-command build-command))))
             path))

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
    (setf language (guess-language-from source1 source2)))
  ;; Create the diff.
  (let ((diff
         (apply #'sel/ast-diff/ast-diff:ast-diff
                (mapcar
                 {create-software-from
                  _ language flags compiler compilation-database build-command}
                 (list source1 source2)))))
    ;; Print according to the RAW option.
    (if raw
        (writeln (ast-diff-elide-same diff) :readably t)
        (if no-color
            (print-diff diff :no-color t)
            (print-diff diff)))
    ;; Only exit with 0 if the two inputs match.
    (wait-on-manual manual)
    (exit-command ast-diff
                  (if (every [{eql :same} #'car] diff) 0 1)
                  diff)))

(define-command ast-merge (my-source old-source your-source
                               &spec +command-line-options+)
  "Merge changes from old-source->my-source and old-source->your-source."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose raw no-color))
  (when help (show-help-for-ast-merge))
  (setf *note-out* (list *error-output*))
  (unless (every #'resolve-file (list old-source my-source your-source))
    (exit-command ast-merge 2 (error "Missing source.")))
  (setf out-dir (or out-dir (resolve-out-dir-from-source old-source))
        old-source (truenamize old-source)
	my-source (truenamize my-source)
	your-source (truenamize your-source)
        language (or language
                     (guess-language-from old-source my-source your-source)))

  (note 3 "Parameters:~%~S~%"
        `((old-source . ,old-source)
          (my-source . ,my-source)
          (your-source . ,your-source)
          (out-dir . ,out-dir)
          (*note-level* . ,*note-level*)))

  (note 3 "Creating software objects")

  ;; TODO: clang-merge had a step "Create syled versions of the input files."
  ;;       which has been removed here.  Not sure why it was necessary.

  (note 3 "Performing merge")
  (multiple-value-bind (new-merged unstable)
      (apply #'converge
	     (mapcar
	      { create-software-from
	      _ language flags compiler compilation-database build-command}
	      (list old-source my-source your-source)))
    (if (directory-p old-source)
	(save-to new-merged out-dir "merge")
	(to-file new-merged (make-pathname :directory out-dir
					   :name "merge")))
    (if (not unstable)
	(note 1 "No merge conflicts")
	(note 0 "Merge conflicts:~%~a~%" unstable))
    (wait-on-manual manual)
    (exit-command ast-merge (if unstable 1 0) new-merged)))
