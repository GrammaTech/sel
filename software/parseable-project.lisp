;;; parseable-project.lisp --- project with ASTs
;;;
;;; This abstract class represents a project which may be parsed into ASTs.
;;;
;;; @texi{parseable-project}
(defpackage :software-evolution-library/software/parseable-project
  (:nicknames :sel/software/parseable-project :sel/sw/parseable-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project)
  (:import-from :functional-trees/attrs :attrs-root)
  (:export :parseable-project))
(in-package :software-evolution-library/software/parseable-project)
(in-readtable :curry-compose-reader-macros)

(define-software parseable-project (project attrs-root) ()
  (:documentation "Abstract project specialization for parseable software
objects."))

(defmethod asts ((obj parseable-project))
  "Return a list of all ASTs in the project OBJ."
  (apply #'append (mapcar [#'asts #'cdr] (evolve-files obj))))
