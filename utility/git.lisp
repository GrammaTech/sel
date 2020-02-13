(uiop/package:define-package :software-evolution-library/utility/git
    (:use :gt))
(in-package :software-evolution-library/utilitygit)

(define-condition git-error (error)
  ((description :initarg :description :initform nil :reader description))
  (:report (lambda (condition stream)
             (format stream "Git failed: ~a" (description condition)))))

(defclass git ()
  ((git-dir :initarg :git-dir :accessor git-dir
            :initform nil :type (or null pathname))
   (work-tree :initarg :work-tree :accessor work-tree
              :initform nil :type (or null pathname))
   (ssh-key :initarg :ssh-key :accessor ssh-key
            :initform nil :type (or null string)))
  (:documentation "An object to represent a git repository."))

(defgeneric run (git command &rest arguments)
  (:documentation "Run the git command COMMAND in the git repository GIT.")
  (:method ((git git) (command string) &rest arguments)
    (multiple-value-bind (stdout stderr errno)
        (with-slots (git-dir work-tree ssh-key) git
          (let ((prefix (format nil "GIT_WORK_TREE=~a GIT_DIR=~a"
                                (namestring work-tree)
                                (namestring git-dir))))
            (when ssh-key
              (setf prefix
                    (concatenate 'string prefix
                                 " GIT_SSH_COMMAND='ssh -i ~a -F /dev/null'")))
            (shell "~a git ~{~a~^ ~}"
                   prefix (cons command arguments))))
      (unless (zerop errno)
        (error (make-instance 'git-error :description (format nil
                                                              "stdout: ~a ~%stderr: ~a"
                                                              stdout
                                                              stderr))))
      stdout)))

(defun git-from-directory (directory) ; NOTE: Currently an internal function.
  "Return first parent directory of DIRECTORY that is a git repository.
Return a second value which is the base git repository, the GIT_WORK_TREE.
Raise an error if no such parent exists."
  (labels ((git-directory- (d)
             (when (< (length d) 2)
               (error
                (make-condition 'git-error
                  :description (format nil "~s is not in a git repository."
                                       directory))))
             (handler-case
                 (let ((gd (make-pathname :directory (append d (list ".git")))))
                   (when (probe-file gd)
                     (if (directory-exists-p gd)
                         ;; Actual .git/ directory.
                         (return-from git-directory-
                           (values gd (make-pathname :directory d)))
                         ;; For submodules we have a file pointing to directory.
                         (let ((line (split-sequence #\Space
                                       (with-open-file (in gd)
                                         (read-line in)))))
                           (when (string= "gitdir:" (first line))
                             (return-from git-directory-
                               (values (merge-pathnames
                                (ensure-directory-pathname (second line))
                                (ensure-directory-pathname
                                 (make-pathname :directory d)))
                                       (make-pathname :directory d))))))))
               (error (e)
                 (error
                  (make-condition 'git-error
                    :description (format nil "~s finding git directory." e)))))
             (git-directory- (butlast d))))
    (git-directory-
     (if (listp directory)
         directory
         (pathname-directory (ensure-directory-pathname directory))))))

(defmethod initialize-instance :after ((git git) &key)
  (when (and (work-tree git) (not (git-dir git)))
    (setf (git-dir git) (git-from-directory (work-tree git)))))

(defun make-git (local-directory &key remote ssh-key clone-args)
  "Make a git object in LOCAL-DIRECTORY.
If REMOTE is specified clone from REMOTE into LOCAL-DIRECTORY.  If
LOCAL-DIRECTORY exists it should be part of or under a git repository.
If LOCAL-DIRECTORY does not exist it will be created and a fresh git
repository will be initialized therein.  A username and password may
be specified as part of the REMOTE URL.  E.g. as
'https://USERNAME:PASSWORD@github.com/path/to/repo.git'."
  (nest
   (multiple-value-call
       (lambda (git-dir work-tree)
         (make-instance 'git
           :git-dir git-dir :work-tree work-tree :ssh-key ssh-key)))
   (git-from-directory)
   (if (directory-exists-p (ensure-directory-pathname local-directory))
       local-directory)
   (if (not remote)
       (error (make-instance 'git-error
                :description
                "local-directory does not exist and no remote was specified")))
   (let ((clone-cmd (format nil "git clone ~{~a~^ ~} ~a ~a"
                            clone-args remote local-directory)))
     (when ssh-key
       (setf clone-cmd (format nil "GIT_SSH_COMMAND='ssh -i ~a -F /dev/null' ~a"
                               ssh-key clone-cmd)))
     (note 2 "cloning git repo: ~a" clone-cmd)
     (multiple-value-bind (stdout stderr errno) (shell clone-cmd)
       (declare (ignorable stdout))
       (if (not (zerop errno))
           (error (make-instance 'git-error :description stderr))
           local-directory)))))

(defgeneric current-git-status (directory)
  (:documentation
   "Return the git status of DIRECTORY as a list of lists of (status file).
Return nil if there are no modified, untracked, or deleted files.")
  (:method ((dir list))
    (current-git-status (make-pathname :directory dir)))
  (:method ((dir string))
    (current-git-status (pathname dir)))
  (:method ((dir pathname))
    (multiple-value-bind (git-dir work-tree)
        (git-from-directory dir)
      (current-git-status (make-instance 'git :git-dir git-dir
                                         :work-tree work-tree))))
  (:method ((git git))
    (multiple-value-bind (stdout stderr errno)
        (with-slots (git-dir work-tree) git
          (shell "GIT_WORK_TREE=~a GIT_DIR=~a git status --porcelain"
                 (namestring work-tree)
                 (namestring git-dir)))
      (declare (ignorable stderr errno))
      (mapcar (lambda (line)
                (multiple-value-bind (status point)
                    (read-from-string line nil)
                  (list (make-keyword status) (subseq line point))))
              (split-sequence #\Newline stdout :remove-empty-subseqs t)))))

(defmacro define-direct-git-command (data (arg) &body body)
  "Define a git command which reads directly from the file system.
These commands don't actually have to invoke git commands directly."
  (let ((name (intern (concatenate 'string "CURRENT-GIT-" (symbol-name data))
                      *package*)))
    `(defgeneric ,name (repository)
       (:documentation ,(format nil "Return the current git ~a by directly ~
                                         reading git data on disk."
                                (string-downcase (symbol-name data))))
       (:method ((dir list))
         (,name (make-instance 'git :work-tree (make-pathname :directory dir))))
       (:method ((dir string)) (,name (make-instance 'git :work-tree dir)))
       (:method ((dir pathname))
         (,name (pathname-directory (ensure-directory-pathname dir))))
       (:method ((git git)) (let ((,arg (git-dir git))) ,@body)))))

(define-direct-git-command commit (git-dir)
  (with-open-file (git-head-in (merge-pathnames "HEAD" git-dir))
    (let ((git-head (read-line git-head-in)))
      (if (scan "ref:" git-head)
          (with-open-file (ref-in (merge-pathnames
                                   (second (split-sequence #\Space git-head))
                                   git-dir))
            (subseq (read-line ref-in) 0 7)) ; attached head
          (subseq git-head 0 7)))))          ; detached head

(define-direct-git-command branch (git-dir)
  (with-open-file (git-head-in (merge-pathnames "HEAD" git-dir))
    (lastcar (split-sequence #\/ (read-line git-head-in)))))

(defun git-url-p (url)
  "Return nil if URL does not look like a URL to a git valid remote."
  (let ((url-str (if (typep url 'pathname)
                     (namestring url)
                     url)))
    (or (scan "\\.git$" url-str)
        (scan "^git://" url-str)
        (scan "^https://git\\." url-str))))
