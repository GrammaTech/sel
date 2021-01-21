;;; python-project.lisp --- Projects composed of python objects.
;;;
;;; Implements the core functionality of the software-evolution-library
;;; for python projects.  This includes identifying files in a
;;; project containing python source code, and overloading several
;;; software-evolution-library methods for python.
;;;
;;; As this class is dependendent on @code{python} software objects,
;;; dependencies for these objects are also required.
;;;
;;; @texi{python-project}
(defpackage :software-evolution-library/software/python-project
  (:nicknames :sel/software/python-project :sel/sw/python-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/tree-sitter)
  #-windows (:shadowing-import-from :osicat :file-permissions)
  (:export :python-project))
(in-package :software-evolution-library/software/python-project)
(in-readtable :curry-compose-reader-macros)

(define-software python-project (parseable-project) ()
  (:documentation "Project specialization for python software objects."))

(defmethod initialize-instance :after ((project python-project) &key)
  (setf (slot-value project 'component-class)
        (or (component-class project) 'python)))

(defmethod collect-evolve-files ((project python-project) &aux result)
  (walk-directory
    (project-dir project)
    (lambda (file)
      (push (cons (pathname-relativize (project-dir project) file)
                  (from-file (make-instance (component-class project))
                             file))
            result))
    :test (lambda (file)
            (let ((rel-path (pathname-relativize (project-dir project) file)))
              (and (not (ignored-evolve-path-p project rel-path))
                   (or (equal "py" (pathname-type file))
                       (and (intersection '(:user-exec :group-exec :other-exec)
                                          (file-permissions file))
                            (member (file-mime-type file)
                                    '((:text :x-python)
                                      (:text :x-script.python))
                                    :test #'set-equal)))))))
  result)

(defmethod phenome ((obj python-project) &key (bin (temp-file-name)))
  "Create a phenotype of the PYTHON-PROJECT.  In this case,
as python is not a compiled language, override the phenome method
to return where the genome of OBJ is written to - BIN.

OBJ object to create a phenome for
BIN location where the phenome will be created on the filesystem"
  (to-file obj bin)
  (values bin 0 nil nil nil))
