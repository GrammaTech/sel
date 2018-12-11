;;; lisp-diff.lisp --- Calculate and render Lisp AST diffs at the command line
;;;
;;; The following git configuration will register lisp-diff as a tool
;;; to be used with @code{git difftool} (see
;;; @url{https://git-scm.com/docs/git-difftool}).
;;;
;;;     # Set lisp-diff as the default difftool.
;;;     [diff]
;;;     	tool = lisp-diff
;;;
;;;     # Command-line to use with lisp-diff.  Piping through
;;;     # colordiff is optional but nice to highlight diffs.
;;;     [difftool "lisp-diff"]
;;;     	cmd = "lisp-diff $LOCAL $REMOTE|colordiff"
;;;
;;;     # Optionally don't prompt.
;;;     [difftool]
;;;     	prompt = false
;;;
;;; @texi{lisp-diff}
(defpackage :software-evolution-library/ast-diff/lisp
  (:nicknames :sel/ast-diff/lisp)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :uiop
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/utility
        :software-evolution-library/ast-diff/ast-diff
        :software-evolution-library/ast-diff/merge
        :software-evolution-library/software/lisp)
  (:import-from :software-evolution-library/software/ast :ast-hash :ast-text)
  (:shadowing-import-from
   :uiop :getenv :directory-exists-p :copy-file :appendf :parse-body
   :ensure-list :simple-style-warning :ensure-gethash :ensure-function
   :if-let :emptyp :featurep)
  (:shadowing-import-from :software-evolution-library/view
                          +color-RED+ +color-GRN+ +color-RST+))
(in-package :software-evolution-library/ast-diff/lisp)
(in-readtable :curry-compose-reader-macros)


;;; Command-line interface to lisp differencing.
(setf *note-out* *error-output*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append +common-command-line-options+
            `((("raw" #\r) :type boolean :optional t
               :documentation "output diff in raw Sexp (default is as text)")
              (("no-color" #\C) :type boolean :optional t
               :documentation "inhibit color printing")))))

(define-command lisp-diff (file1 file2 &spec +command-line-options+)
  "Compare Lisp source in FILE1 and FILE2 by AST."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (when help (show-help-for-lisp-diff))
  (unless (every #'resolve-file (list file1 file2))
    (exit-command lisp-diff 2 (error "Missing file.")))
  ;; Create the diff.
  (let ((diff (ast-diff (from-file (make-instance 'lisp) file1)
                        (from-file (make-instance 'lisp) file2))))
    ;; Print according to the RAW option.
    (if raw
        (writeln (ast-diff-elide-same diff) :readably t)
        (if no-color
            (print-diff diff :no-color t)
            (print-diff diff)))
    ;; Only exit with 0 if the two inputs match.
    (unless uiop/image:*lisp-interaction*
      (quit (if (every [{eql :same} #'car] diff) 0 1)))
    diff))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +merge-command-line-options+
    (append +common-command-line-options+
	    `())))

(define-command lisp-merge (file1 file2 file3 output-file
				  &spec +merge-command-line-options+)
  "Merge changes from file1->file2 and file1->file3 to produce output-file"
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (when help (show-help-for-lisp-merge))
  (unless (every #'resolve-file (list file1 file2 file3))
    (exit-command lisp-merge 2 (error "Missing file.")))
  (multiple-value-bind (new-merged unstable)
      (converge (from-file (make-instance 'lisp) file1)
                (from-file (make-instance 'lisp) file2)
                (from-file (make-instance 'lisp) file3))
    (save-to new-merged out-dir "merged")
    (if (not unstable)
	(note 1 "No merge conflicts")
	(note 0 "Merge conflicts:~%~a~%" unstable))
    (exit-command lisp-merge (if unstable 1 0) new-merged)))
