;;; lisp-project.lisp --- Projects composed of Lisp objects
(defpackage :software-evolution-library/software/lisp-project
  (:nicknames :sel/software/lisp-project
              :sel/sw/lisp-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/lisp
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/software/directory)
  (:export :lisp-project))
(in-package :software-evolution-library/software/lisp-project)
(in-readtable :curry-compose-reader-macros)

(define-software lisp-project (parseable-project)
    ()
  (:documentation "Project specialization for lisp software objects."))

(defmethod initialize-instance :after ((lisp-project lisp-project) &key)
  (setf (component-class lisp-project)
        (or (component-class lisp-project) 'lisp)))

(defmethod collect-evolve-files ((obj lisp-project))
  (collect-evolve-files* obj :extensions '("lisp" "asd")))

(defmethod phenome ((obj lisp-project) &key (bin (temp-file-name)))
  "Create a phenotype of the LISP-PROJECT.
As lisp is not a compiled language do nothing."
  (declare (ignorable obj))
  (values bin 0 nil nil nil))
