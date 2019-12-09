;;; git.lisp --- Git backed software projects
(defpackage :software-evolution-library/software/git-project
  (:nicknames :sel/software/git-project :sel/sw/git-project)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :uiop
        :software-evolution-library
        :software-evolution-library/software/project
        :software-evolution-library/utility)
  (:shadowing-import-from :uiop/run-program :run-program)
  (:shadowing-import-from :uiop :quit)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
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
