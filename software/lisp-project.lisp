;;; lisp-project.lisp --- Projects composed of Lisp objects
(defpackage :software-evolution-library/software/lisp-project
  (:nicknames :sel/software/lisp-project
              :sel/sw/lisp-project)
  (:use :common-lisp
        :cl-json
        :named-readtables
        :curry-compose-reader-macros
        :uiop/pathname
        :software-evolution-library
        :software-evolution-library/software/lisp
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/utility)
  (:export :lisp-project))
(in-package :software-evolution-library/software/lisp-project)
(in-readtable :curry-compose-reader-macros)

(define-software lisp-project (parseable-project)
    ()
  (:documentation "Project specialization for lisp software objects."))

(defmethod collect-evolve-files ((obj lisp-project) &aux result)
  (with-cwd ((project-dir obj))
    (walk-directory (project-dir obj)
      (lambda (file)
        (push (cons (namestring (pathname-relativize (project-dir obj) file))
                    (from-file (make-instance (component-class obj)) file))
              result))
      :test [{member _ '("lisp" "asd") :test #'equal} #'pathname-type]))
  result)

(defmethod phenome ((obj lisp-project) &key (bin (temp-file-name)))
  "Create a phenotype of the LISP-PROJECT.
As lisp is not a compiled language do nothing."
  (declare (ignorable obj))
  (values bin 0 nil nil nil))
