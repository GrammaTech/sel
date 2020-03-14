;;; coq-project.lisp --- Coq project software representation
;;;
;;; A `coq-project' object extends the main SEL `project' object. In addition to
;;; the SEL `project' fields, it includes an additional `project-file' field for
;;; storing the path to a _CoqProject file. (This is intentionally duplicated on
;;; `coq' objects, because having it stored at the project-level makes more
;;; sense for projects, while having it on individual `coq' objects allows them
;;; to function well outside of projects.)
;;;
;;; @texi{coq-project}
(defpackage :software-evolution-library/software/coq-project
  (:nicknames :sel/software/coq-project :sel/sw/coq-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/project
        :software-evolution-library/software/coq
        :software-evolution-library/components/serapi-io)
  (:export :coq-project))
(in-package :software-evolution-library/software/coq-project)
(in-readtable :serapi-readtable)

(define-software coq-project (project)
  ((project-file :initarg :project-file :accessor project-file :initform nil
                 :copier :direct
                 :documentation "Path to _CoqProject file, if it exists."))
  (:documentation "Coq project software object."))

(defmethod reset-and-load-imports ((project coq-project) &key imports)
  "Reset the SerAPI process and load IMPORTS for each Coq object in PROJECT.
If IMPORTS is NIL, it defaults to the list of `imports' for each Coq object in
PROJECT."
  (iter (for (file . obj) in (all-files project))
        (declare (ignorable file))
        (if imports
            (reset-and-load-imports obj :imports imports)
            (reset-and-load-imports obj))
        ;; insert reset point so subsequent iterations don't undo the import
        (insert-reset-point)))

