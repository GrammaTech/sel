;;;; ancestral-git.lisp --- ancestral git representation.
(defpackage :software-evolution-library/test/ancestral-git
  (:nicknames :sel/test/ancestral-git)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/ancestral-git
   :software-evolution-library/software/c
   :software-evolution-library/software/c-project)
  (:import-from :trivial-garbage
                :finalize)
  (:export :test-ancestral-git))
(in-package :software-evolution-library/test/ancestral-git)
(in-readtable :curry-compose-reader-macros)
(defsuite test-ancestral-git "Ancestral git representation."
  (c-tree-sitter-available-p))


;;; Utility

(define-software c-git (ancestral-git c-project)
  ())

(defmacro with-c-git-cleanup ((project-name project-path) &body body)
  "Create a new c-git project and ensure that its temporary directories have been
removed."
  (with-gensyms (path)
    `(progn
       (let (,path )
         (let ((,project-name (from-file 'c-git ,project-path)))
           (setf ,path (repository-path ,project-name))
           ,@body)
         ;; Invoke worktree finalizers
         (tg:gc :full t)
         (finalize/remove-local-repo ,path)))))


;;; Tests
;;; TODO: make all of this :long
(deftest ancestral-git-clones-local-repo ()
  "The project repository is cloned to /tmp/ by default."
  ;; NOTE: this prevents all of the branches used for evolution from being stored
  ;;       in the primary repository.

  ;; TODO: use a proper path for this.
  (with-c-git-cleanup (project "~/Programs/benchmark/meta/grep-single-file/")
    (let* ((path (repository-path project))
           (directory-path (pathname-directory path)))
      ;; TODO: not convinced this is a good assertion.
      (is (equal (second directory-path) "tmp"))
      (is (probe-file path)))))

(deftest ancestral-git-creates-work-tree ()
  "A new worktree is created on project creation."
  ;; TODO: use a proper path for this.
  (with-c-git-cleanup (project "~/Programs/benchmark/meta/grep-single-file/")
    (let* ((path (worktree-path project))
           (directory-path (pathname-directory path)))
      ;; TODO: not convinced this is a good assertion.
      (is (equal (second directory-path) "tmp"))
      (is (probe-file path)))))

(deftest ancestral-git-cloned-creates-new-work-tree ()
  "A new worktree is created when a project is copied."
  ;; TODO: use a proper path for this.
  (with-c-git-cleanup
      (original-project "~/Programs/benchmark/meta/grep-single-file/")
    (let* ((original-path (worktree-path original-project))
           (copy-project (copy original-project))
           (copy-path (worktree-path copy-project)))
      (is (not (equal original-path copy-path)))
      (is (probe-file copy-path)))))
