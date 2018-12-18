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

(defmethod initialize-instance :after ((lisp-project lisp-project) &key)
  (setf (component-class lisp-project)
        (or (component-class lisp-project) 'lisp)))

(defmethod collect-evolve-files ((obj lisp-project) &aux result)
  (assert (project-dir obj) (obj) "directory-dir must be set on ~S" obj)
  (with-cwd ((project-dir obj))
    (walk-directory (project-dir obj)
      (lambda (file)
        (push (cons (namestring (pathname-relativize (project-dir obj) file))
                    (from-file (make-instance (component-class obj)) file))
              result))
      :test «and [{member _ '("lisp" "asd") :test #'equal} #'pathname-type]
                 [#'not {ignored-path-p obj}]»))
  result)

(defmethod phenome ((obj lisp-project) &key (bin (temp-file-name)))
  "Create a phenotype of the LISP-PROJECT.
As lisp is not a compiled language do nothing."
  (declare (ignorable obj))
  (values bin 0 nil nil nil))
