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
   :software-evolution-library/software/c-project
   :software-evolution-library/software/directory
   :cmd)
  (:import-from :trivial-garbage
                :finalize)
  (:import-from :uiop
                :delete-directory-tree)
  (:export :test-ancestral-git))
(in-package :software-evolution-library/test/ancestral-git)
(in-readtable :curry-compose-reader-macros)
(defsuite test-ancestral-git "Ancestral git representation."
  (c-tree-sitter-available-p))


;;; Utility

(define-software c-git (ancestral-git c-project)
  ())

(defun initialize-repository (path)
  (lret ((new-path #p"/tmp/ancestral-git-test/"))
    (cmd "cp  -r" path new-path :&> nil)
    (cmd :in new-path "git init" :&> nil)
    (cmd :in new-path "git add ." :&> nil)
    (cmd :in new-path "git commit -m" (list "Initial commit") :&> nil)))

(defmacro with-git-repository ((path-name project-path) &body body) ;
  `(let ((,path-name (initialize-repository ,project-path)))
     (unwind-protect (progn ,@body)
       (delete-directory-tree ,path-name :validate t))))

(defmacro with-c-git-cleanup ((project-name project-path) &body body) ;
  "Create a new c-git project and ensure that its temporary directories have been
removed."
  ;; TODO: add repo creation here. It likely won't be too bad
  (with-gensyms (path git-path)
    `(with-git-repository (,git-path ,project-path)
       (let (,path )
         (unwind-protect
              (let ((,project-name (from-file 'c-git ,git-path)))
                (setf ,path (repository-path ,project-name))
                (setf (fitness ,project-name) 1)
                ,@body)
           ;; Invoke worktree finalizers
           (tg:gc :full t)
           (when ,path
             (finalize/remove-local-repo ,path)))))))

(defun has-commit-p (ancestral-git &rest words)
  "Return T if the latest commit in the repository for ANCESTRAL-GIT contains all
of WORDS in it."
  (let ((commit-message ($cmd :in (worktree-path ancestral-git)
                              "git log 'HEAD^..HEAD'")))
    (iter
      (for word in words)
      (always (scan word commit-message)))))

;;; TODO: use a proper path for this.
(defconst +project-path+ "/home/nberch/quicklisp/local-projects/sel/test/etc/c-symbol-table-project/")


;;; Tests

;;; TODO: make all of this :long
(deftest ancestral-git-clones-local-repo ()
  "The project repository is cloned to /tmp/ by default."
  ;; NOTE: this prevents all of the branches used for evolution from being stored
  ;;       in the primary repository.

  (with-c-git-cleanup (project +project-path+)
    (let* ((path (repository-path project))
           (directory-path (pathname-directory path)))
      (is (equal (second directory-path) "tmp"))
      (is (probe-file path)))))

(deftest ancestral-git-creates-work-tree ()
  "A new worktree is created on project creation."
  (with-c-git-cleanup (project +project-path+)
    (let* ((path (worktree-path project))
           (directory-path (pathname-directory path)))
      (is (probe-file path)))))

(deftest ancestral-git-cloned-creates-new-work-tree ()
  "A new worktree is created when a project is copied."
  (with-c-git-cleanup
      (original-project +project-path+)
    (let* ((original-path (worktree-path original-project))
           (copy-project (copy original-project))
           (copy-path (worktree-path copy-project)))
      (is (not (equal original-path copy-path)))
      (is (probe-file copy-path)))))

(deftest ancestral-git-worktree-creation-commit ()
  "A commit is created when a worktree is created."
  (with-c-git-cleanup
      (original-project +project-path+)
    (let ((copy-project (copy original-project)))
      (is (has-commit-p copy-project "Create Worktree")))))

;;; TODO: create an actual repo in test/etc to use.
(deftest ancestral-git-worktree-evolve-mutation-commit ()
  (with-c-git-cleanup
      (original-project +project-path+)
    (let* (;; NOTE: ensure the population is large enough such that the mutated
           ;;       member isn't evicted.
           (*max-population-size* 10)
           (*population* (list original-project))
           (*fitness-evals* 0)
           (*tree-sitter-mutation-types* '((tree-sitter-cut . 1))))
      (evolve (constantly 1) :max-evals 1)
      (is (iter
            (for variant in *population*)
            (thereis (has-commit-p
                      variant
                      "MUTATION" "FILE-PATH" "TARGETS")))))))

;;; TODO: need to test for crossover.
