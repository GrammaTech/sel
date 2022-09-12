;;; ancestral-git.lisp --- class adding ancestry tracking via git to software
;;;
;;; Add documentation here explaining usage and how to analyze logs.
;;;
;;; @texi{ancestral-git}
(defpackage :software-evolution-library/software/ancestral-git
  (:nicknames :sel/software/ancestral-git :sel/sw/ancestral-git)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/components/file
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/directory
        :cmd)
  (:import-from :trivial-garbage
                :finalize)
  (:export :ancestral-git
           :worktree-path
           :worktree-identifier
           :repository-path
           :finalize/remove-local-repo
           :ensure-ancestral-git))
(in-package :software-evolution-library/software/ancestral-git)
(in-readtable :curry-compose-reader-macros)



;;; Ancestral Git

;;; TODO: it is important that the versions of the tree-sitter library and SEL
;;;       are stored as a commit in the cloned repo. This is to allow for
;;;       "replaying" the evolutionary loop if desired. Technically git patches
;;;       could be applied for the same effect, but having all necessary
;;;       information doesn't hurt.

(define-software ancestral-git ()
  ((repository-path :initarg :repository-path
                    :initform nil
                    :reader repository-path
                    :documentation "Path to the main git repository.")
   (worktree-path :initarg :worktree-path
                  :initform nil
                  :reader worktree-path
                  :documentation "Path to the git worktree.")
   (worktree-identifier :initarg :worktree-identifier
                        :initform nil
                        :reader worktree-identifier
                        :documentation "Unique identifier for the worktree."))
  (:documentation "Class adding ancestry tracking via git to software."))

(defmethod print-object ((obj ancestral-git) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a:~a" (original-path obj) (worktree-identifier obj)))
  obj)

(defparameter *evolutionary-worktree-prefix* "EVOLUTIONARY-WORKTREE"
  "A prefix used to generate branch names for a worktree.")

(defgeneric commit (project message &key)
  (:documentation "Add MESSAGE as a commit to PROJECT. EMPTY-COMMIT can be set
to T to allow for a commit that has no source changes associated with it.")
  (:method ((project ancestral-git) (message string) &key)
    (with-slots (worktree-path) project
      (cmd :in worktree-path
           "git add -A"
           #-dbg :&> #-dbg nil)
      (cmd :in worktree-path
           "git commit -m" (list message) "--allow-empty" "--allow-empty-message"
           #-dbg :&> #-dbg nil))))

(defun update-tree-paths (project)
  "Update any paths in PROJECT that don't match PROJECT's worktree."
  (with-slots (genome) project
    (let* ((root genome)
           (first-directory-ast (find-if (of-type 'directory-ast) root)))
      (setf genome
            (with root
                  first-directory-ast
                  (copy first-directory-ast
                        :name (string (worktree-identifier project))))))))

(defun finalize/remove-local-repo (path)
  (delete-directory-tree path :validate t))

(defun remove-local-repo (ancestral-git)
  "Remove the local repo associated with ANCESTRAL-GIT."
  (when-let ((path (repository-path ancestral-git)))
    (delete-directory-tree path :validate t)))

(defun clone-local-repo-p (ancestral-git)
  "Return T if ANCESTRAL-GIT should clone a local repo of its original path."
  (equal (pathname (original-path ancestral-git))
         (pathname (repository-path ancestral-git))))

(defun clone-local-repo (ancestral-git)
  "Clone a new repo for ANCESTRAL-GIT if the original-path and the
repository-path are identical."
  (let* ((path (pathname (original-path ancestral-git)))
         (local-name (gensym (lastcar (pathname-directory path))))
         (repository-path (pathname (format nil "/tmp/~a/" local-name))))
    (cmd :in #p"/tmp/"
         "git clone --no-hardlinks " path (string local-name)
         #-dbg :&> #-dbg nil)
    (cmd :in repository-path
         "git remote remove origin"
         #-dbg :&> #-dbg nil)
    (setf (slot-value ancestral-git 'repository-path) repository-path)))

(defun finalize/remove-worktree (repository-path worktree-path)
  ;; NOTE: if the repository path isn't available, just remove the directory.
  (tagbody
     (handler-bind
         ((cmd-error
            (lambda (cmd-error)
              (declare (ignorable cmd-error))
              (go clean-up))))
       (cmd :in (pathname repository-path)
            "git worktree remove" worktree-path
            #-dbg :&> #-dbg nil))
   clean-up
     (when (probe-file worktree-path)
       (delete-directory-tree worktree-path :validate t))))

(defun remove-worktree (project)
  "Remove the git worktree associated with PROJECT."
  (check-type project ancestral-git)
  (check-type (slot-value project 'worktree-path) string)
  (finalize/remove-worktree (repository-path project) (worktree-path project))
  (setf (slot-value project 'worktree-path) nil
        (slot-value project 'worktree-identifier) nil))

;;; NOTE: currently we are not checking if a branch already exists. To get around
;;;       this, clone the repo to /tmp. Use worktrees off of that repo.
;;;       Assume a unique prefix and nuke anything that already exists.
(defun add-worktree
    (project &aux (worktree-id (gensym *evolutionary-worktree-prefix*))
             ;; NOTE: git worktree doesn't like a trailing '/'.
             ;;       Add it back later since pathnames rely on having it to
             ;;       identify as a directory.
               (worktree-path (pathname (format nil "/tmp/~a" worktree-id)))
               (worktree-directory-path (pathname (format nil "~a/" worktree-path))))
  "Create a new git worktree for PROJECT."
  (labels ((ensure-repository-path-exists (project)
             (with-slots (repository-path) project
               (unless repository-path
                 (setf repository-path (pathname (original-path project))))))
           (ensure-local-repository-exists (project)
             (when (clone-local-repo-p project)
               (clone-local-repo project))))
    (check-type project ancestral-git)
    (ensure-repository-path-exists project)
    (ensure-local-repository-exists project)
    (cond
      ((directory-exists-p worktree-path)
       (add-worktree project))
      (t
       (cmd :in (pathname (repository-path project))
            "git worktree add" worktree-path
            #-dbg :&> #-dbg nil)
       (setf (slot-value project 'worktree-path) worktree-directory-path
             (slot-value project 'worktree-identifier) worktree-id
             ;; NOTE: set original path here to prevent breaking
             ;;       certain things.
             (slot-value project 'project-dir) worktree-directory-path)
       (finalize
        project
        {finalize/remove-worktree (repository-path project)
                                  worktree-directory-path})
       (update-tree-paths project)
       (commit project (format nil "Create Worktree '~a'" worktree-id))
       project))))

(defmethod copy :around ((obj ancestral-git)
                         &key (repository-path nil repository-path-supplied-p)
                           (worktree-path nil worktree-path-supplied-p)
                           (worktree-identifier nil worktree-identifier-supplied-p)
                         &allow-other-keys)
  (let ((copy (call-next-method)))
    (if repository-path-supplied-p
        (setf (slot-value copy 'repository-path) repository-path)
        (setf (slot-value copy 'repository-path) (repository-path obj)))
    (if worktree-identifier-supplied-p
        (setf (slot-value copy 'worktree-identifier) worktree-identifier)
        (setf (slot-value copy 'worktree-identifier) (worktree-identifier obj)))
    (if worktree-path-supplied-p
        (setf (slot-value copy 'worktree-path) worktree-path)
        ;; TODO: do we actually want to create a new work tree every time this
        ;;       is copied? Ideally, we don't, but since the ASTs are immutable
        ;;       in practice, is there any way to determine which copy should
        ;;       keep the previous worktree? If this was changed, we'd also
        ;;       need to log specific commits when creating worktrees.
        ;; NOTE: this sets the worktree-identifier as well.
        (add-worktree copy))
    copy))


(defmethod initialize-instance :after ((project ancestral-git) &key)
  ;; TODO: this assumes a git directory exists. This may not be the case.
  (when (and (original-path project)
             (not (worktree-path project)))
    ;; NOTE: only add a worktree if one doesn't already exist.
    (add-worktree project)))

(defmethod from-file :around ((project ancestral-git) path)
  ;; TODO: this assumes a git directory exists. This may not be the case.
  (call-next-method)
  (add-worktree project)
  project)

(defmethod log-message ((mutation mutation)
                        &rest rest
                        &key root &allow-other-keys)
  (assert root)
  (with-slots (targets) mutation
    (format nil "MUTATION: '~a'
~a~&~
TARGETS:
~{~a~^~%~}"
            (class-of mutation)
            (apply #'log-message (object mutation) rest)
            (mapcar (op (apply #'log-message _ rest))
                    (ensure-list (targets mutation))))))

(defmethod log-new-individuals ((variant ancestral-git) mutation-info &key
                                &aux (genome (genome variant)))
  ;; NOTE:  including the source ranges is currently inefficient.
  (destructuring-bind (mutation a a-point crossed b b-point) mutation-info
    (declare (ignorable crossed))
    ;; TODO: this is likely inefficient. Ideally, ASTs should be marked as
    ;;       dirty if they have changed since the previous iteration.

    ;; Write the project out so that the changed files can be committed.
    (to-file variant (project-dir variant))
    (let ((mutation
            (make-instance (cadr mutation)
                           :targets (cddr mutation)
                           :object (aget (car mutation)
                                         (evolve-files variant)))))
      (commit variant
              (cond
                ((and a-point b-point)
                 ;; Crossover logging.
                 (string+ (format nil "CROSSOVER A:'~a'~%B:'~a'~%AT~%A:~%~a~&B:~a"
                                  (worktree-identifier a)
                                  (worktree-identifier b)
                                  (log-message a-point :root genome)
                                  (log-message b-point :root genome))
                          (log-message mutation :root genome)))
                (t
                 ;; Mutant logging.
                 (log-message mutation :root genome)))))))


;;; Commandline

(defun ensure-ancestral-git
    (&aux (project-class (find-class 'project))
       (ancestral-git-class (find-class 'ancestral-git))
       (project-super-classes (class-direct-superclasses project-class)))
  "Modifies the project class definition to include ancestral-git as a
superclass."
  (unless (member ancestral-git-class project-super-classes)
    (ensure-class-using-class
     project-class 'project
     :direct-superclasses (cons ancestral-git-class project-super-classes))))
