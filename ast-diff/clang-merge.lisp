;;;; clang-merge.lsip -- command line interface for three way merging
(defpackage :software-evolution-library/ast-diff/clang-merge
  (:nicknames :sel/ast-diff/clang-merge)
  (:documentation "Command line interface for three way merging of software")
  (:use :common-lisp
	:alexandria
        :metabang-bind
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
	:split-sequence
	:uiop
        :bordeaux-threads
        :uiop/filesystem
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/components/formatting
	:software-evolution-library/ast-diff/ast-diff
        :software-evolution-library
        :software-evolution-library/software/project
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-project)
  (:shadowing-import-from
   :uiop :getenv :directory-exists-p :copy-file :appendf :parse-body
   :ensure-list :simple-style-warning :ensure-gethash :ensure-function
   :if-let :emptyp :featurep :truenamize)
  (:export :clang-merge :run-clang-merge))
(in-package :sel/ast-diff/clang-merge)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append +common-command-line-options+
            +interactive-command-line-options+
            +clang-command-line-options+
            +project-command-line-options+
            +clang-project-command-line-options+)))

(define-command clang-merge
    (original version1 version2
              &spec +command-line-options+
              &aux original-soft version1-soft version2-soft)
  "Merge VERSION1 and VERSION2 sharing ancestor BASE."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose compiler))
  (when help (show-help-for-clang-merge))
  (mapc (lambda (file)
	  (unless (probe-file file)
	    (format *error-output*
		    "~a: No such file or directory~%"
		    file)
	    (return-from clang-merge 3))
	  file) (list original version1 version2))
  (setf out-dir (or out-dir (resolve-out-dir-from-source original))
        original (truenamize original)
	version1 (truenamize version1)
	version2 (truenamize version2))

  (note 1 "Parameters:~%~S~%"
        `((original . ,original)
          (version1 . ,version1)
          (version2 . ,version2)
          (out-dir . ,out-dir)
          (*note-level* . ,*note-level*)))

  (note 3 "Creating software objects")
  (flet ((%create (s)
	   (from-file (apply
                       #'make-instance
                       'clang-project
                       :project-dir s
                       (append
                        (when flags
                          (list :flags flags))
                        (when compilation-database
                          (list :compilation-database compilation-database))
                        (when build-command
                          (list :build-command build-command))))
                      s)))
    (setf original-soft (%create original))
    (setf version1-soft (%create version1))
    (setf version2-soft (%create version2)))

  ;; Create styled versions of the input files
  (save-to original-soft out-dir "original")
  (save-to version1-soft out-dir "v1")
  (save-to version2-soft out-dir "v2")

  ;; Perform merge
  (note 3 "Performing merge")
  (multiple-value-bind (new-merged unstable)
      (converge original-soft version1-soft version2-soft)
    (save-to new-merged out-dir "merged")
    (if (not unstable)
	(note 1 "No merge conflicts")
	(note 0 "Merge conflicts:~%~a~%" unstable))
    (wait-on-manual manual)
    ;; exit with 0 if no merge conflicts, 1 otherwise
    (return-from clang-merge (if unstable 1 0))))

(defgeneric save-to (soft out-dir sub))

(defmethod save-to ((soft project) out-dir sub)
  (let ((dest (make-pathname :directory (append out-dir (list sub)))))
    (unless (probe-file dest)
      (to-file (copy soft) dest))))
