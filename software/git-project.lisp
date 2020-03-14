;;; git.lisp --- Git backed software projects
(defpackage :software-evolution-library/software/git-project
  (:nicknames :sel/software/git-project :sel/sw/git-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/project)
  (:export :git-project :git-repo))
(in-package :software-evolution-library/software/git-project)
(in-readtable :curry-compose-reader-macros)

(define-software git-project (project)
  ((git-repo :initarg :git-repo :accessor git-repo
             :initform nil
             :documentation "A valid, permissive git repo URL.
This is used to create the project and potentially push changes."))
  (:documentation "A software project backed by a git repo.
The additional field(s) enable various git-specific operations."))
