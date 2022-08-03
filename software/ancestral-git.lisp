;;; ancestral-git.lisp --- class adding ancestry tracking via git to software
;;;
;;; DOCFIXME Need a page or so introduction to ancestral-git software mixin.
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
        ;; TODO: remove this
        :software-evolution-library/software/c-project
        :cmd)
  (:import-from :trivial-garbage
                :finalize)
  (:export :ancestral-git
           :worktree-path
           :worktree-identifier
           :repository-path
           :finalize/remove-local-repo))
(in-package :software-evolution-library/software/ancestral-git)
(in-readtable :curry-compose-reader-macros)


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

(defparameter *evolutionary-worktree-prefix* "EVOLUTIONARY-WORKTREE"
  "A prefix used to generate branch names for a worktree.")


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

;; TODO: maybe remove or move to a util.
(defun nuke-worktrees+branches (path)
  "Nuke the worktrees and branches that start with *EVOLUTIONARY-WORKTREE-PREFIX*
at the repo at PATH."
  (shell
   (string+
    (format nil "cd ~a; " path)
    "for worktree in $(git branch | grep evolutionary | awk '{print $1}'); do "
    "git worktree remove $worktree; "
    "git branch -D $worktree; "
    "done")))

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
         "git clone --no-hardlinks " path (string local-name))
    (setf (slot-value ancestral-git 'repository-path) repository-path)))

(defun finalize/remove-worktree (repository-path worktree-path)
  ;; NOTE: if the repository path isn't available, just remove the directory.
  (delete-directory-tree worktree-path :validate t)
  ;; TODO: fix this!
  #+nil
  (if (probe-file repository-path)
      (cmd :in (pathname repository-path)
           "git worktree remove" worktree-path
           :&> nil)
      (delete-directory-tree worktree-path :validate t)))

(defun remove-worktree (project)
  "Remove the git worktree associated with PROJECT."
  (check-type project ancestral-git)
  (check-type (slot-value project 'worktree-path) string)
  (finalize/remove-worktree (repository-path project) (worktree-path project))
  (setf (slot-value project 'worktree-path) nil
        (slot-value project 'worktree-identifier) nil))

;;; TODO: NOTE: currently we are not checking if a branch already exists. This
;;;             can be especially problematic if we overwrite previous work for
;;;             any reason. To get around this, clone the repo to /tmp.
;;;             Use worktrees off of that repo.
;;;             Assume a unique prefix and nuke anything that already exists.
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
            :&> nil)
       (setf (slot-value project 'worktree-path) worktree-directory-path
             (slot-value project 'worktree-identifier) worktree-id
             ;; NOTE: set original path here to prevent breaking
             ;;       certain things.
             (slot-value project 'project-dir) worktree-path)
       (finalize
        project
        {finalize/remove-worktree (repository-path project)
                                  worktree-directory-path})
       (update-tree-paths project)
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
     (if worktree-path-supplied-p
         (setf (slot-value copy 'worktree-path) worktree-path)
         (setf (slot-value copy 'worktree-path) (worktree-path
                                                 (add-worktree obj))))
     (if worktree-identifier-supplied-p
         (setf (slot-value copy 'worktree-identifier) worktree-identifier)
         (setf (slot-value copy 'worktree-identifier) (worktree-identifier obj)))
     copy))


;;; TODO: consider not creating the git worktree immediately--do it lazily.
;;;       This would save time.

;;; TODO: we may also want to clone the local directory and put the branches on
;;;       the clone. This would help with polluting the branch namespace of the
;;;       project.
(defmethod initialize-instance :after ((project ancestral-git) &key)
  ;; TODO: this assumes a git directory exists. This may not be the case.
  (when (and (original-path project)
             (not (worktree-path project)))
    ;; NOTE: only add a worktree if one doesn't already exist.
    (add-worktree project)))

(defmethod from-file :around ((project ancestral-git) path)
  (call-next-method)
  (add-worktree project)
  project)
