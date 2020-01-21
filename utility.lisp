;;;; utility.lisp --- utility functions
;;;
;;; Variables, functions, and macros used throughout the software
;;; evolution library.  This package provides simple utilities for a
;;; number of mundane and generally necessary tasks from working with
;;; files and directories, to representing and manipulating ranges of
;;; source code.
;;;
;;; @texi{utility}

;;; Code:
(defpackage :software-evolution-library/utility
  (:nicknames :sel/utility)
  (:use
   :common-lisp
   :alexandria
   :arrow-macros
   :closer-mop
   :uiop
   :asdf-encodings
   :metabang-bind
   :named-readtables
   :curry-compose-reader-macros
   :bordeaux-threads
   :iterate
   :split-sequence
   :cl-ppcre
   :cl-store
   :cl-dot
   :diff
   :flexi-streams
   :string-case)
  (:shadow :read)
  (:import-from :cffi
                :defcstruct
                :define-foreign-type
                :defcfun
                :with-foreign-object
                :with-foreign-slots
                :null-pointer
                :null-pointer-p)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:shadowing-import-from :asdf-encodings :encoding-external-format)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:shadowing-import-from :uiop/run-program :run-program)
  (:shadowing-import-from :uiop/os :os-unix-p)
  (:shadowing-import-from :uiop :quit)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  #-windows (:shadowing-import-from :osicat :file-permissions :pathname-as-directory)
  #+sbcl
  (:shadowing-import-from :sb-sprof :call-graph :node-count :call-graph-nsamples
                          :node-name :*samples* :node-callers :trace-start
                          :call-graph-vertices :with-output-to-string
                          :lookup-node :with-lookup-tables)
  #+sbcl
  (:shadowing-import-from :sb-introspect :function-lambda-list)
  #+sbcl
  (:shadowing-import-from
   :sb-posix
   :tcgetattr
   :termios-lflag
   :termios-lflag
   :icanon
   :echo
   :echoe
   :echok
   :echonl
   :echo
   :tcsetattr
   :tcsanow)
  (:export
   :infinity
   ;; OS
   :file-mime-type
   :file-to-string
   :use-encoding
   :file-to-bytes
   :file-access
   :file-access-operation
   :file-access-path
   :set-file-writable
   :string-to-file
   :bytes-to-file
   :stream-to-string
   :getenv
   :quit
   :git-error
   :git
   :git-dir
   :work-tree
   :ssh-key
   :make-git
   :run
   :current-git-commit
   :current-git-branch
   :current-git-status
   :git-url-p
   :*temp-dir*
   :temp-file-name
   :with-temp-file
   :with-temp-fifo
   :with-temp-file-of
   :with-temp-file-of-bytes
   :with-temp-files
   :with-temp-dir
   :with-temp-dir-of
   :with-cwd
   :with-temp-cwd-of
   :pwd
   :cd
   :pathname-relativize
   :in-directory
   :directory-p
   :canonical-pathname
   :merge-pathnames-as-directory
   :merge-pathnames-as-file
   :truenamestring
   :list-directory
   :walk-directory
   ;; Process wrapper
   :process
   :os-process
   :process-id
   :process-input-stream
   :process-output-stream
   :process-error-stream
   :process-exit-code
   :process-running-p
   :kill-process
   ;; Shell execution
   :*shell-debug*
   :*shell-error-codes*
   :*shell-non-error-codes*
   :ignore-shell-error
   :shell-command-failed
   :shell
   :write-shell
   :read-shell
   :write-shell-file
   :read-shell-file
   :xz-pipe
   :parse-number
   :parse-numbers
   :whitespacep
   :trim-whitespace
   :trim-right-whitespace
   :trim-left-whitespace
   :normalize-whitespace
   :split-quoted
   :+whitespace-chars+
   :escape-string
   :unescape-string
   :make-terminal-raw
   :make-terminal-unraw
   :ioctl
   :term-size
   :getopts
   :which
   ;; forensic
   :arglist
   :show-it
   :equal-it
   :count-cons
   :tree-right-length
   :tree-right-walk
   ;; simple utility
   :*uninteresting-conditions*
   :with-quiet-compilation
   :if-let*
   :repeatedly
   :indexed
   :different-it
   :mapt
   :plist-get
   :plist-keys
   :plist-drop-if
   :plist-drop
   :plist-merge
   :counts
   :proportional-pick
   :position-extremum
   :position-extremum-rand
   :partition
   :random-bool
   :random-elt-with-decay
   :random-hash-table-key
   :uniform-probability
   :normalize-probabilities
   :cumulative-distribution
   :un-cumulative-distribution
   :random-pick
   :random-subseq
   :random-sample-with-replacement
   :random-sample-without-replacement
   :apply-replacements
   :peel-bananas
   :peel-bananas-or-same
   :unpeel-bananas
   :replace-all
   :aget
   :areplace
   :alist
   :alist-merge
   :adrop
   :alist-filter
   :getter
   :transpose
   :interleave
   :mapconcat
   :drop
   :drop-while
   :drop-until
   :take
   :take-while
   :take-until
   :pad
   :chunks
   :cartesian
   :cartesian-without-duplicates
   :binary-search
   :tails
   :pairs
   :filter-subtrees
   :make-thread-safe-hash-table
   ;;; symbols
   :symbol-cat
   :symbol-cat-in-package
   ;;; Source and binary locations and ranges
   :source-location
   :line
   :column
   :source-range
   :range
   :begin
   :end
   :source-<
   :source-<=
   :source->
   :source->=
   :contains
   :intersects
   :levenshtein-distance
   :unlines
   :keep-lines-after-matching
   :resolve-function-includes
   ;; debugging
   :*compile-w/tracing*
   :traced
   :*note-level*
   :*note-out*
   :replace-stdout-in-note-targets
   :note
   :with-warnings-as-notes
   :trace-memory
   :*shell-count*
   ;; diff computing
   :diff-scalar
   ;; gdb functions
   :gdb-disassemble
   :addrs
   :function-lines
   :calculate-addr-map
   ;; oprofile
   :samples-from-oprofile-file
   :samples-from-tracer-file
   ;; iterate helpers
   :concatenating
   ;; Profiling helpers
   :*profile-dot-min-ratio*
   :profile-to-dot-graph
   :profile-to-flame-graph
   :*profile-flame-graph*
   :enhanced-copy-seq
   ;; jobs
   :task-runner
   :*task-runner*
   :task-runner-jobs
   :task-runner-workers
   :task-runner-workers-count
   :task-runner-results
   :task-runner-completed-jobs
   :task-runner-completed-tasks
   :task-runner-remaining-jobs
   :task-runner-init-jobs
   :task-runner-stop-jobs
   :task-runner-add-job
   :task-runner-create-worker
   :task
   :task-job
   :process-task
   :task-object
   :task-save-result
   :run-task
   :run-as-task
   :task-map
   :run-task-and-block
   :some-task
   :some-task-pred
   :some-test-task
   :convert-jsown-tree
   :string-case-to-keywords
   :with-prof
   :without-compiler-notes))
(in-package :software-evolution-library/utility)
#-windows (cffi:load-foreign-library :libosicat)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-preserving-case (stream char n)
    (declare (ignorable char) (ignorable n))
    (let ((*readtable* (copy-readtable nil)))
      (setf (readtable-case *readtable*) :preserve)
      (cl-user::read stream t nil t)))
  (handler-bind ((style-warning #'muffle-warning))
    (defreadtable :sel-readtable
      ;; Define an SEL readtable which combines CCRM with a #! reader
      ;; macro for preserving case reads.  Both can be useful generally.
      (:merge :curry-compose-reader-macros)
      (:dispatch-macro-char #\# #\! #'read-preserving-case)))

  (in-readtable :sel-readtable))

(defvar infinity
  #+sbcl
  sb-ext:double-float-positive-infinity
  #+ccl
  ccl::double-float-positive-infinity
  #+allegro
  excl:*infinity-double*
  #+ecl
  ext:long-float-positive-infinity
  #-(or ecl sbcl ccl allegro)
  (error "must specify a positive infinity value"))

(defmethod print-object ((obj (eql infinity)) stream)
  (if *print-readably* (call-next-method) (format stream "infinity")))


;;;; Files and Directories, Temporary and Git
;;;
;;; Functions for working with file and directories and some special
;;; handling for git repositories.  Includes functions for efficient
;;; serialization to/from files as well as functions for creating and
;;; using temporary files and directories.
;;;
;;; @texi{file-directory}
(define-condition git-error (error)
  ((description :initarg :description :initform nil :reader description))
  (:report (lambda (condition stream)
             (format stream "Git failed: ~a" (description condition)))))

(defvar *available-git-commands* nil)

(defun initialize-available-git-commands ()
  (unless *available-git-commands*
    (setf *available-git-commands*
          (list "add" "branch" "checkout" "clone" "commit" "diff" "fetch" "log" "merge" "rebase" "reset" "rm" "submodule" "tag"))))

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
  (:method :before ((git git) (command string) &rest arguments)
           (declare (ignorable arguments))
           (initialize-available-git-commands)
           (unless (member command *available-git-commands* :test #'string=)
             (error
              (make-instance 'git-error
                :description (format nil "Unknown git command ~S" command)))))
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
    (git-directory- directory)))

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
   (pathname-directory)
   (ensure-directory-pathname)
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

(defmacro define-direct-git-command (data (arg) &body body)
  (with-gensyms (implementation)
    `(flet ((,implementation (,arg) ,@body))
       (defgeneric
           ,(intern (concatenate 'string "CURRENT-GIT-" (symbol-name data)))
           (repository)
         (:documentation ,(format nil "Return the current git ~a by directly ~
                                      reading git data on disk."
                                  (string-downcase (symbol-name data))))
         (:method ((dir list)) (,implementation (git-from-directory dir)))
         (:method ((dir string))
           (,implementation (git-from-directory (pathname-directory (ensure-directory-pathname dir)))))
         (:method ((git git)) (,implementation (dir git)))))))

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

(defgeneric current-git-status (repository)
  (:documentation
   "Return the git status of REPOSITORY as a list of lists of (status file).
Return nil if there are no modified, untracked, or deleted files.")
  (:method ((git git))
    (mapcar (lambda (line)
              (multiple-value-bind (status point)
                  (read-from-string line nil)
                (list (make-keyword status) (subseq line point))))
            (run git "status" "--porcelain")))
  (:method ((directory string))
    (current-git-status (make-git directory))))

(defun git-url-p (url)
  "Return nil if URL does not look like a URL to a git valid remote."
  (let ((url-str (if (typep url 'pathname)
                     (namestring url)
                     url)))
    (or (scan "\\.git$" url-str)
        (scan "^git://" url-str)
        (scan "^https://git\\." url-str))))

(defmacro without-compiler-notes (&body body)
  "Suppress compiler notes from BODY"
  #+sbcl
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body)
  #-sbcl
  `(let () ,@body))

#+sbcl
(without-compiler-notes
  (sb-alien:define-alien-routine (#-win32 "tempnam" #+win32 "_tempnam" tempnam)
      sb-alien:c-string
    (dir sb-alien:c-string)
    (prefix sb-alien:c-string)))

#+(and ccl linux)
(defun tempnam (dir prefix)
  (ccl:with-filename-cstrs ((base dir) (prefix (or prefix "")))
    (ccl:get-foreign-namestring
     (ccl:external-call "tempnam" :address base :address prefix :address))))

#+(and ccl windows)
(defun tempnam (dir prefix)
  (ccl:with-filename-cstrs ((base (or dir "")) (prefix (or prefix "")))
    (ccl:%get-cstring
     (ccl:external-call "_tempnam" :address
                        base :address
                        prefix :address))))

#+ecl
(defun tempnam (dir &optional prefix)
  (let ((dir-name (uiop:ensure-directory-pathname
                   (or dir *temporary-directory*))))
    (ext:mkstemp (if prefix
                     (uiop::merge-pathnames dir-name prefix)
                     dir-name))))

(defun file-mime-type (path)
  "Return the mime type of PATH as a list of two symbols.
The Unix `file' command is used, specifically \"file -b --mime-type PATH\"."
  (assert (probe-file path) (path) "No file or directory at ~S" path)
  (nest (mapcar #'intern) (split-sequence #\/) (string-upcase) (trim-whitespace)
        (shell "file -b --mime-type ~a" (namestring path))))

(defun file-to-string
    (pathname &key (external-format
                    (encoding-external-format (detect-encoding pathname))))
  #+ccl (declare (ignorable external-format))
  (labels ((run-read ()
             (let (#+sbcl (sb-impl::*default-external-format* external-format)
                          #+ecl (ext:*default-external-format* external-format)
                          (element-type (case external-format
                                          (:ascii 'base-char)
                                          (t 'character))))
               (with-open-file (in pathname :element-type element-type)
                 (let* ((file-bytes (file-length in))
                        (seq (make-string file-bytes :element-type element-type))
                        (file-chars (read-sequence seq in)))
                   (if (= file-bytes file-chars)
                       seq
                       ;; Truncate the unused tail of seq.  It is possible
                       ;; for read-sequence to read less than file-length
                       ;; when the file has multi-byte UTF-8 characters.
                       (subseq seq 0 file-chars)))))))

    (restart-case
        (if (member external-format '(:utf8 :utf-8))
            (run-read)
            (handler-case
                (run-read)
              (stream-error (c)
                ;; Try utf-8 as a default fallback.
                (declare (ignorable c))
                (setf external-format :utf-8)
                (run-read))))
      (use-encoding (encoding)
        :report "Specify another encoding."
        (setf external-format encoding)
        (run-read)))))

(defun file-to-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      seq)))

(defun stream-to-string (stream)
  (when stream
    (with-open-stream (in stream)
      (iter (for char = (read-char in nil nil))
            (while char)
            (collect char into chars)
            (finally (return (coerce chars 'string)))))))

(define-condition file-access (error)
  ((file-access-path :initarg :path :initform nil
                     :reader file-access-path)
   (file-access-operation :initarg :operation :initform nil
                          :reader file-access-operation))
  (:report (lambda (condition stream)
             (format stream "Unable to ~a file ~a"
                     (file-access-operation condition)
                     (file-access-path condition)))))

(defun string-to-file (string path &key
                                     (if-exists :supersede)
                                     (external-format :default))
  "Write STRING to PATH.
Restarts available to handle cases where PATH is not writable,
SET-FILE-WRITABLE, and where the appropriate encoding is not used,
USE-ENCODING. "
  (labels ((run-write ()
             (ensure-directories-exist path)
             (with-open-file (out path :direction :output
                                  :if-exists if-exists
                                  :external-format external-format)
               (format out "~a" string))))

    (when (and (file-exists-p path)
               (not (member :user-write (file-permissions path))))
      (restart-case
          (error (make-condition 'file-access
                                 :path path
                                 :operation :write))
        (set-file-writable ()
          (format nil "Forcefully set ~a to be writable" path)
          (push :user-write (file-permissions path)))))

    (restart-case
        (if (member external-format '(:utf8 :utf-8))
            (run-write)
            (handler-case
                (run-write)
              (stream-error (c)
                ;; Try utf-8 as a default fallback.
                (declare (ignorable c))
                (setf external-format :utf-8)
                (run-write))))
      (use-encoding (encoding)
        :report "Specify another encoding."
        (setf external-format encoding)
        (run-write))))
  path)

(defun bytes-to-file (bytes path &key (if-exists :supersede))
  (with-open-file (out path :element-type '(unsigned-byte 8)
                       :direction :output :if-exists if-exists)
    (write-sequence bytes out)))

(defvar *temp-dir* nil
  "Set to non-nil for a custom temporary directory.")

(defun temp-file-name (&optional type)
  (let ((base #+clisp
          (let ((stream (gensym)))
            (eval `(with-open-stream
                       (,stream (ext:mkstemp
                                 (if *temp-dir*
                                     (namestring (make-pathname
                                                  :directory *temp-dir*
                                                  :name "XXXXXX"))
                                     nil)))
                     (pathname ,stream))))
          #+(or sbcl ccl ecl)
          (tempnam *temp-dir* nil)
          #+allegro
          (system:make-temp-file-name nil *temp-dir*)
          #-(or sbcl clisp ccl allegro ecl)
          (error "no temporary file backend for this lisp.")))
    ;; NOTE:  code smell -- two branches of these ifs are dead in SBCL
    (without-compiler-notes
        (if type
            (if (pathnamep base)
                (namestring (make-pathname :directory (pathname-directory base)
                                           :name (pathname-name base)
                                           :type type))
                (concatenate 'string base "." type))
            (if (pathname base)
                (namestring base)
                base)))))

(defun ensure-temp-file-free (path)
  "Delete anything at PATH."
  (let ((probe (probe-file path)))
    (when probe
      (if (equal (directory-namestring probe)
                 (namestring probe))
          (progn
            #+sbcl (sb-ext:delete-directory probe :recursive t)
            #+ccl (ccl:delete-directory probe)
            #-(or sbcl ccl)
            (uiop/filesystem:delete-directory-tree
             (uiop:ensure-directory-pathname probe)
             :validate t))
          (delete-file path)))))

(defmacro with-temp-file (spec &rest body)
  "SPEC holds the variable used to reference the file w/optional extension.
After BODY is executed the temporary file is removed."
  (with-gensyms (v)
    `(let* ((,v ,(second spec))
            (,(car spec) (temp-file-name ,v)))
       (unwind-protect (progn ,@body) (ensure-temp-file-free ,(car spec))))))

(defmacro with-temp-fifo (spec &rest body)
  `(with-temp-file ,spec
     (ensure-temp-file-free ,(car spec))
     #-windows (osicat-posix:mkfifo ,(car spec)
                          (logior osicat-posix:s-iwusr osicat-posix:s-irusr))
     ,@body))

(defmacro with-temp-dir (spec &rest body)
  `(with-temp-file ,spec
     (ensure-directories-exist (ensure-directory-pathname ,(car spec)))
     ,@body))

(defmacro with-cwd ((dir) &rest body)
  "Change the current working directory to dir and execute body.
WARNING: This function is not thread safe.  Execution in a threaded
environment may causes execution outside of the intended directory or
may lose the original working directory."
  (with-gensyms (orig)
    `(let ((,orig (getcwd)))
       (unwind-protect
         (progn (cd ,dir) ,@body)
         (cd ,orig)))))

(defmacro with-temp-cwd-of (spec dir &rest body)
  "Copy DIR into a temporary directory, the path to which is stored in SPEC,
and execute BODY within this temporary directory."
  `(with-temp-dir-of ,spec ,dir
     (with-cwd ,spec
       ,@body)))

(defun pwd ()
  (getcwd))

(defun cd (directory)
  (let ((pathname (probe-file directory)))
    (unless pathname
      (error "Directory ~S does not exist." directory))
    (unless (directory-exists-p (ensure-directory-pathname pathname))
      (error "Directory ~S is not a directory." directory))
    (setf *default-pathname-defaults* pathname)
    (chdir pathname)))

(defmacro with-temp-file-of ((variable &optional type) string &rest body)
  "Execute BODY with STRING in a temporary file whose path is in VARIABLE."
  `(let ((,variable (temp-file-name ,type)))
     (unwind-protect (progn (string-to-file ,string ,variable) ,@body)
       (when (probe-file ,variable) (delete-file ,variable)))))

(defmacro with-temp-file-of-bytes (spec bytes &rest body)
  "SPEC should be a list of the variable used to reference the file
and an optional extension."
  `(let ((,(car spec) (temp-file-name ,(second spec))))
     (unwind-protect (progn (bytes-to-file ,bytes ,(car spec)) ,@body)
       (when (probe-file ,(car spec)) (delete-file ,(car spec))))))

(defmacro with-temp-dir-of (spec dir &rest body)
  "Populate SPEC with the path to a temporary directory with the contents
of DIR and execute BODY"
  `(with-temp-dir ,spec
     (shell "cp -pr ~a/. ~a" (namestring ,dir) (namestring ,(car spec)))
     ,@body))

(defmacro with-temp-files (specs &rest body)
  (labels ((expander (specs body)
             (let ((s (car specs)))
               `(let ((,(car s) (temp-file-name ,(second s))))
                  (unwind-protect
                       ,(if (cdr specs)
                            (expander (cdr specs) body)
                            `(progn ,@body))
                    (when (probe-file ,(car s)) (delete-file ,(car s))))))))
    (expander (mapcar (lambda (s)
                        (if (listp s) s (list s)))
                      specs) body)))

(defun pathname-relativize (root-path path)
  "Return PATH relative to ROOT-PATH."
  (replace-all
   (namestring (canonical-pathname path))
   (namestring (canonical-pathname (ensure-directory-pathname root-path)))
   ""))

(defun truenamestring (path)
  (namestring (truename path)))

;;; TODO: Refactor `in-directory'.  This should probably be combined
;;; with `merge-pathnames-as-file' and `merge-pathnames-as-directory'.
;;; I believe the behavior of this function (to call
;;; `ensure-directory' on all but the last argument) is probably what
;;; most users expect of the other two functions.
(defun in-directory (directory path)
  "Return PATH based in DIRECTORY.
Uses `ensure-directory-pathname' to force DIRECTORY to be a directory
pathname (i.e., ending in a \"/\")."
  (let ((directory (ensure-directory-pathname directory)))
    (make-pathname
     :host (pathname-host directory)
     :device (pathname-device directory)
     :directory (append (pathname-directory directory)
                        (cdr (pathname-directory path)))
     :name (pathname-name path)
     :type (pathname-type path)
     :version (pathname-version path))))

#+windows
(defun pathname-as-directory (pathname)
  pathname)

(defun directory-p (pathname)
  "Return a directory version of PATHNAME if it indicates a directory."
  (cond
    ((directory-pathname-p pathname) pathname)
    ;; Wild pathnames are not directory pathnames.
    ;; Also, `directory-exists-p' faults on wild pathnames.
    ((wild-pathname-p pathname) nil)
    ;; `directory-exists-p' (like this function) returns the pathname.
    (t (directory-exists-p (pathname-as-directory pathname)))))


;;;; Utilities from cl-fad.
;;;
;;; The functions `merge-pathnames-as-file', `merge-pathnames-as-directory',
;;; `canonical-pathname', `directory-wildcard', `list-directory', and
;;; `walk-directory' were adapted from cl-fad.  The CL-FAD license applies
;;; to the source for these functions and is included below:
;;;
;;; Copyright (c) 2004, Peter Seibel.  All rights reserved.
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
(defun canonical-pathname (path)
  "Remove redundant information from PATH."
  (iter (with full-dir = (or (pathname-directory (pathname path))
                             (list :relative)))
        (with canon-dir = (if (member (first full-dir) '(:relative :absolute))
                              (list (pop full-dir))
                              (list :relative)))
        (while full-dir)
        (cond ((string= "." (first full-dir))
               (pop full-dir))
              ((eql :back (second full-dir))
               (pop full-dir)
               (pop full-dir))
              (t (push (pop full-dir) canon-dir)))
        (finally (return (make-pathname :defaults (pathname path)
                                        :directory (nreverse canon-dir))))))

(defun merge-pathnames-as-directory (&rest pathnames)
  "Given a list of pathnames, this returns a single
directory pathname containing the logical concatenation of them all."
  (if (null pathnames)
      (make-pathname)
      (let* ((pathnames (mapcar #'pathname pathnames))
             (defaults (first pathnames))
             (dir (pathname-directory defaults)))
        (make-pathname
          :defaults defaults
          :directory (iter (for pathname in (rest pathnames))
                           (for directory = (pathname-directory pathname))
                           (cond ((eq :absolute (first directory))
                                  (setf dir directory))
                                 ((eq :relative (first directory))
                                  (setf dir (append dir (rest directory)))))
                           (finally (return dir)))
          :name nil :type nil))))

(defun merge-pathnames-as-file (&rest pathnames)
  "Given a list of pathnames returns a single filename pathname
containing the logical concatenation of them all."
  (if (null pathnames)
      (make-pathname)
      (let ((file-name (first (last pathnames))))
        (make-pathname :defaults (apply #'merge-pathnames-as-directory pathnames)
                       :name (pathname-name file-name)
                       :type (pathname-type file-name)
                       :version (pathname-version file-name)))))

(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
the directory named by the non-wild pathname designator DIRNAME."
  (when (wild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                       #+:clisp nil
                       #+:cormanlisp "*"
                 :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname &key (follow-symlinks t))
  "Returns a fresh list of pathnames corresponding to all files within
the directory named by the non-wild pathname designator DIRNAME.
The pathnames of sub-directories are returned in directory form -
see PATHNAME-AS-DIRECTORY.

  If FOLLOW-SYMLINKS is true, then the returned list contains
truenames (symlinks will be resolved) which essentially means that it
might also return files from *outside* the directory.  This works on
all platforms.

  When FOLLOW-SYMLINKS is NIL, it should return the actual directory
contents, which might include symlinks.  Currently this works on SBCL
and CCL."
  (declare (ignorable follow-symlinks))
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #+(or :ecl :clasp)
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (directory (merge-pathnames (pathname "*/") dir))
                 (directory (merge-pathnames (pathname "*.*") dir))))
  #-(or :ecl :clasp)
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname)
    #+:sbcl (directory wildcard :resolve-symlinks follow-symlinks)
    #+(or :cmu :scl :lispworks) (directory wildcard)
    #+(or :openmcl :digitool) (directory wildcard :directories t
                                                  :follow-links follow-symlinks)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp (nconc (directory wildcard :if-does-not-exist :keep)
                    (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp
        :cormanlisp :ecl :abcl :digitool :clasp)
  (error "LIST-DIRECTORY not implemented"))

(defun walk-directory (dirname fn &key directories
                                       (if-does-not-exist :error)
                                       (test (constantly t))
                                       (follow-symlinks t))
  "Recursively applies the function FN to all files within the
directory named by the non-wild pathname designator DIRNAME and all of
its sub-directories.  FN will only be applied to files for which the
function TEST returns a true value.  If DIRECTORIES is not NIL, FN and
TEST are applied to directories as well.  If DIRECTORIES
is :DEPTH-FIRST, FN will be applied to the directory's contents first.
If DIRECTORIES is :BREADTH-FIRST and TEST returns NIL, the directory's
content will be skipped. IF-DOES-NOT-EXIST must be one of :ERROR
or :IGNORE where :ERROR means that an error will be signaled if the
directory DIRNAME does not exist.  If FOLLOW-SYMLINKS is T, then your
callback will receive truenames.  Otherwise you should get the actual
directory contents, which might include symlinks.  This might not be
supported on all platforms.  See LIST-DIRECTORY."
  (labels ((walk (name)
             (cond
               ((directory-pathname-p name)
                ;; the code is written in a slightly awkward way for
                ;; backward compatibility
                (cond ((not directories)
                       (dolist (file (list-directory name
                                       :follow-symlinks follow-symlinks))
                         (walk file)))
                      ((eql directories :breadth-first)
                       (when (funcall test name)
                         (funcall fn name)
                         (dolist (file (list-directory name
                                         :follow-symlinks follow-symlinks))
                           (walk file))))
                      ;; :DEPTH-FIRST is implicit
                      (t (dolist (file (list-directory name
                                         :follow-symlinks follow-symlinks))
                           (walk file))
                         (when (funcall test name)
                           (funcall fn name)))))
               ((funcall test name)
                (funcall fn name)))))
    (let ((pathname-as-directory (pathname-as-directory dirname)))
      (case if-does-not-exist
        ((:error)
         (cond ((not (directory-exists-p pathname-as-directory))
                (error "File ~S does not exist."
                       pathname-as-directory))
               (t (walk pathname-as-directory))))
        ((:ignore)
         (when (directory-exists-p pathname-as-directory)
           (walk pathname-as-directory)))
        (otherwise
         (error "IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE."))))
    (values)))


;;;; Process wrapping
;;;
;;; TODO: What is the benefit of this wrapper layer?  Just interop
;;;       between lisps implementations?  Does nothing else already
;;;       provide this?
;;;
;;; @texi{process}
(defclass process ()
  ((os-process
    :initarg :os-process :initform nil :reader os-process
    :documentation "The underlying process object (compiler-specific).
This field will not usually need to be accessed directly: use methods
`process-input-stream', `process-output-stream',
`process-error-stream', `process-error-code', `process-status',
`signal-process' to interact with processes."))
  (:documentation "Object representing an external process.
Wraps around SBCL- or CCL-specific representations of external processes."))

(defgeneric process-id (process)
  (:documentation "Return the process id for PROCESS"))

(defmethod process-id ((process process))
  "Return the process id for PROCESS."
  (process-info-pid (os-process process)))

(defgeneric process-input-stream (process)
  (:documentation "Return the input stream for PROCESS."))

(defmethod process-input-stream ((process process))
  "Return the input stream for PROCESS."
  (process-info-input (os-process process)))

(defgeneric process-output-stream (process)
  (:documentation "Return the output stream for PROCESS."))

(defmethod process-output-stream ((process process))
  "Return the output stream for PROCESS."
  (process-info-output (os-process process)))

(defgeneric process-error-stream (process)
  (:documentation "Return the error stream for PROCESS."))

(defmethod process-error-stream ((process process))
  "Return the error stream for PROCESS."
  (process-info-error-output (os-process process)))

(defgeneric process-exit-code (process)
  (:documentation
   "Return the exit code for PROCESS, or nil if PROCESS has not exited."))

(defmethod process-exit-code ((process process))
  "Return the exit code for PROCESS, or nil if PROCESS has not exited."
  (and (not (process-running-p process))
       (wait-process (os-process process))))

(defgeneric process-running-p (process)
  (:documentation "Return T if PROCESS is running, NIL otherwise."))

(defmethod process-running-p ((process process))
  "Return T if PROCESS is running, NIL otherwise."
  (process-alive-p (os-process process)))

(defgeneric kill-process (process &key urgent children)
  (:documentation
   "Send a kill signal to PROCESS. If URGENT is T, send SIGKILL.
If CHILDREN is T, also kill all processes below PROCESS."))

(defmethod kill-process (process &key urgent children)
  (if (not children)
      (terminate-process (os-process process) :urgent urgent)
      (if (os-unix-p)
          (zerop (nth-value 2 (shell "kill -~d -$(ps -o pgid= ~d | tr -d ' ')"
                                     (if urgent 9 15) (process-id process))))
          (error "Killing all children not implemented on this platform"))))


;;;; Shell and system command helpers
;;;
;;; Wrappers for evaluating shell commands and returning the STDOUT,
;;; STDERR, and ERRNO as values.  Includes the special `*shell-debug*'
;;; variable which may be set to non-nil to dump all system and shell
;;; executions and results for diagnostics.
;;;
;;; The `write-shell', `read-shell', `write-shell-file',
;;; `read-shell-file' and `xz-pipe' functions provide for running
;;; shell commands and common lisp streams (in some cases flowing from
;;; or into files on disk).
;;;
;;;@texi{shell}
(defvar *shell-debug* nil
  "Set to true to print shell invocations.  If a list, print
shell cmd if :CMD is a membe, input if :INPUT is a member, and
print the shell outputs if :OUTPUT is a member.")

(defvar *shell-error-codes* '(126 127)
  "Raise a condition on these exit codes.")

(defvar *shell-non-error-codes* nil
  "Raise a condition on any but these exit codes.")

(define-condition shell-command-failed (error)
  ((commmand :initarg :command :initform nil :reader command)
   (exit-code :initarg :exit-code :initform nil :reader exit-code)
   (stderr :initarg :stderr :initform nil :reader stderr))
  (:report (lambda (condition stream)
             (format stream "Shell command ~S failed with [~A]:~%~S~&"
                     (command condition)
                     (exit-code condition)
                     (stderr condition)))))

(defun shell (control-string &rest format-arguments &aux input)
  "Apply CONTROL-STRING to FORMAT-ARGUMENTS and execute the result with a shell.
Return (values stdout stderr errno).  FORMAT-ARGUMENTS up to the first
keyword are passed to `format' with CONTROL-STRING to construct the
shell command.  All subsequent elements of FORMAT-ARGUMENTS are passed
through as keyword arguments to `uiop:run-program'.

Raise a `shell-command-failed' exception depending on the combination
of errno with `*shell-error-codes*' and `*shell-non-error-codes*'.

Optionally print debug information if `*shell-debug*' is non-nil."
  (let ((format-arguments (take-until #'keywordp format-arguments))
        (run-program-arguments (drop-until #'keywordp format-arguments))
        (debug *shell-debug*))
    ;; Manual handling of an :input keyword argument.
    (when-let ((input-arg (plist-get :input run-program-arguments)))
      (setq input
            (if (stringp input-arg)
                (make-string-input-stream input-arg)
                input-arg)))
    (setq run-program-arguments (plist-drop :input run-program-arguments))
    ;; Manual handling of :bash keyword argument.
    (when (plist-get :bash run-program-arguments)
      ;; Use bash instead of /bin/sh, this means setting bash -c "<command>"
      ;; with appropriate string escaping.  Use a formatter function instead
      ;; of a control-string.
      (if input
          (setf control-string
                (let ((cs control-string))
                  (lambda (stream &rest args)
                    (format stream "~a"
                            (concatenate 'string "bash -c \""
                                         (escape-chars "$\\\""
                                                       (apply #'format nil cs args))
                                         "\"")))))
          ;; When there is no input, send the command directly to bash
          (setf
           input (make-string-input-stream
                  (apply #'format nil control-string format-arguments))
           control-string "bash")))
    (setq run-program-arguments (plist-drop :bash run-program-arguments))
    (let ((cmd (apply #'format (list* nil control-string format-arguments)))
          (stdout-str nil)
          (stderr-str nil)
          (errno nil))
      (when (or (not (listp debug)) (member :cmd debug))
        (format t "  cmd: ~a~%" cmd))
      (when (and input (or (not (listp debug))
                           (member :input debug)))
        (format t "  input: ~a~%" input))

      ;; Direct shell execution with `uiop/run-program:run-program'.
      #+(and (not ccl) (not windows))
      (progn
        (setf stdout-str (make-array '(0)
                                     :element-type
                                     #+sbcl 'extended-char
                                     #-sbcl 'character
                                     :fill-pointer 0 :adjustable t))
        (setf stderr-str (make-array '(0)
                                     :element-type
                                     #+sbcl 'extended-char
                                     #-sbcl 'character
                                     :fill-pointer 0 :adjustable t))
        (with-output-to-string (stderr stderr-str)
          (with-output-to-string (stdout stdout-str)
            (setf errno (nth-value 2 (apply #'run-program
                                            cmd
                                            :force-shell t
                                            :ignore-error-status t
                                            :input input
                                            :output stdout
                                            :error-output stderr
                                            run-program-arguments))))))
      #+windows
      (multiple-value-setq (stdout-str stderr-str errno)
        (apply #'run-program cmd :force-shell nil
               :ignore-error-status t
               :input input
               :output :string
               :error-output :string
               run-program-arguments))

      #+(and ccl (not windows))
      (progn
        (with-temp-file (stdout-file)
          (with-temp-file (stderr-file)
            (setf errno (nth-value 2 (apply #'run-program
                                            (format nil "~a 1>~a 2>~a"
                                                    cmd stdout-file stderr-file)
                                            :force-shell t
                                            :ignore-error-status t
                                            :input input
                                            run-program-arguments)))
            (setf stdout-str (if (probe-file stdout-file)
                                 (file-to-string stdout-file)
                                 ""))
            (setf stderr-str (if (probe-file stderr-file)
                                 (file-to-string stderr-file)
                                 "")))))
      (when (or (not (listp debug)) (member :output debug))
        (format t "~&stdout:~a~%stderr:~a~%errno:~a"
                stdout-str stderr-str errno))
      (when (or (and *shell-non-error-codes*
                     (not (find errno *shell-non-error-codes*)))
                (find errno *shell-error-codes*))
        (restart-case (error (make-condition 'shell-command-failed
                               :command cmd
                               :exit-code errno
                               :stderr stderr-str))
          (ignore-shell-error () "Ignore error and continue")))
      (values stdout-str stderr-str errno))))

#-windows  ; IO-SHELL not yet supported on Windows
(defmacro io-shell ((io stream-var shell &rest args) &rest body)
  "Executes BODY with STREAM-VAR holding the input or output of SHELL.
ARGS (including keyword arguments) are passed through to `uiop:launch-program'."
  (assert (member io '(:input :output)) (io)
          "first argument ~a to `io-shell' is not one of :INPUT or :OUTPUT" io)
  (let ((proc-sym (gensym)))
    `(let* ((,proc-sym (uiop:launch-program ,shell ,@args
                                            ,io :stream
                                            :wait nil
                                            :element-type '(unsigned-byte 8))))
       (with-open-stream
           (,stream-var (make-flexi-stream
                         ,(ecase io
                            (:input `(process-info-input ,proc-sym))
                            (:output `(process-info-output ,proc-sym)))))
         ,@body))))

(defmacro write-shell ((stream-var shell &rest args) &rest body)
  "Executes BODY with STREAM-VAR passing the input to SHELL.
ARGS (including keyword arguments) are passed through to `uiop:launch-program'."
  `(io-shell (:input ,stream-var ,shell ,@args) ,@body))

(defmacro read-shell ((stream-var shell &rest args) &rest body)
  "Executes BODY with STREAM-VAR holding the output of SHELL.
ARGS (including keyword arguments) are passed through to `uiop:launch-program'."
  `(io-shell (:output ,stream-var ,shell ,@args) ,@body))

(defmacro write-shell-file ((stream-var file shell &rest args) &rest body)
  "Executes BODY with STREAM-VAR passing through SHELL to FILE.
ARGS (including keyword arguments) are passed through to `uiop:launch-program'."
  `(io-shell (:input ,stream-var ,shell ,@args :output ,file) ,@body))

(defmacro read-shell-file ((stream-var file shell &rest args) &rest body)
  "Executes BODY with STREAM-VAR passing through SHELL from FILE.
ARGS (including keyword arguments) are passed through to `uiop:launch-program'"
  `(io-shell (:output ,stream-var ,shell ,@args :input ,file) ,@body))

(defmacro xz-pipe ((in-stream in-file) (out-stream out-file) &rest body)
  "Execute BODY with IN-STREAM and OUT-STREAM read/writing data from xz files."
  `(read-shell-file (,in-stream ,in-file "unxz")
     (write-shell-file (,out-stream ,out-file "xz")
       ,@body)))

(define-condition parse-number (error)
  ((text :initarg :text :initform nil :reader text))
  (:report (lambda (condition stream)
             (format stream "Can't parse ~a as a number" (text condition)))))

(defun parse-number (string)
  "Parse the number located at the front of STRING or return an error."
  (let ((number-str
         (or (multiple-value-bind (whole matches)
                 (scan-to-strings
                  "^(-?.?[0-9]+(/[-e0-9]+|\\.[-e0-9]+)?)([^\\./A-Xa-x_-]$|$)"
                  string)
               (declare (ignorable whole))
               (when matches (aref matches 0)))
             (multiple-value-bind (whole matches)
                 (scan-to-strings "0([xX][0-9A-Fa-f]+)([^./]|$)"
                                  string)
               (declare (ignorable whole))
               (when matches (concatenate 'string "#" (aref matches 0)))))))
    (unless number-str
      (make-condition 'parse-number :text string))
    (read-from-string number-str)))

(defun parse-numbers (string &key (radix 10) (delim #\Space))
  (mapcar #'(lambda (num) (parse-integer num :radix radix))
          (split-sequence delim string :remove-empty-subseqs t)))

;;; This duplicates the function hu.dwim.utils:string-trim-whitespace

(define-constant +whitespace-chars+
    (remove-duplicates
     (loop for s in '("Space" "Tab" "Newline" "Return"
                      "Linefeed" "Page")
        when (name-char s)
        collect it))
  :test #'equal)

(defun whitespacep (c)
  (find c +whitespace-chars+))

(defun trim-whitespace (str)
  (string-trim +whitespace-chars+ str))

(defun trim-right-whitespace (str)
  (string-right-trim +whitespace-chars+ str))

(defun trim-left-whitespace (str)
  (string-left-trim +whitespace-chars+ str))

(defun normalize-whitespace (str)
  "Trims leading and trailing whitespace, and reduces all remaining
sequences of one or more whitespace characters to a single space"
  (flet ((%whitespacep (c) (member c +whitespace-chars+)))
    (let ((str (trim-whitespace str))
          (element-type (array-element-type str)))
      (let* ((len (length str)))
        (if (= len 0)
            (make-array '(0) :element-type element-type)
            (let ((result (make-array (list len)
                                      :element-type element-type
                                      :initial-element #\Space))
                  (out 0)
                  (w-start nil))
              (loop for cursor from 0 below len
                 do (let ((c (aref str cursor)))
                      (if (%whitespacep c)
                          (unless w-start
                            (psetf (aref result out) #\Space
                                   out (1+ out)
                                   w-start cursor))
                          (setf (aref result out) c
                                out (1+ out)
                                w-start nil))))
              (subseq result 0 out)))))))

(defun escape-chars (chars str)
  "Returns a fresh string that is the same as str, except that
every character that occurs in CHARS is preceded by a backslash."
  (declare (type string str))
  (with-output-to-string (s)
    (map nil (lambda (c)
               (if (find c chars)
                   (format s "\\~a" c)
                   (format s "~a" c)))
         str)))

(defun split-quoted (str)
  "Split STR at spaces except when the spaces are escaped or within quotes.
Return a list of substrings with empty strings elided."
  (let ((subseqs nil)
        (in-single-quote-p nil)
        (in-double-quote-p nil)
        (prev 0)
        (pos 0)
        (len (length str)))
    (iter (while (< pos len))
          (let ((c (elt str pos)))
            (case c
              (#\Space
               (when (and (< prev pos)
                          (not in-single-quote-p)
                          (not in-double-quote-p))
                 (push (subseq str prev pos) subseqs)
                 (setf prev (1+ pos))))
              (#\\
               (incf pos)
               (when (>= pos len) (return)))
              (#\'
               (setf in-single-quote-p (not in-single-quote-p)))
              (#\"
               (setf in-double-quote-p (not in-double-quote-p))))
            (incf pos)))
    (assert (= len pos))
    (when (< prev pos)
      (push (subseq str prev pos) subseqs))
    (reverse subseqs)))

(defun escape-string (str)
  "Return a copy of STR with special characters escaped before output to SerAPI.
Control characters for whitespace (\\n, \\t, \\b, \\r in Lisp) should be
preceded by four backslashes, and double quotes should be preceded by 2.
Additionally, ~ must be escaped as ~~ so that the string can be formatted.
See also `unescape-string'."
  ;; Please be intimidated by the number of backslashes here, use *extreme*
  ;; caution if editing, and see the CL-PPCRE note on why backslashes in
  ;; regex-replace are confusing prior to thinking about editing this.
  (-<> str
       ;; replace all \\n with \\\\n unless already escaped (also other WS)
       ;; in regex \\\\ ==> \\ in Lisp string (which is \ in "real life")
       ;; (replace-all "\\" "\\\\")
       (regex-replace-all "(?<!\\\\)\\\\(n\|t\|b\|r)" <> "\\\\\\\\\\1")

       ;; replace all \" with \\" unless already escaped
       ;; in regex, \\\" ==> \" in Lisp string
       ;; (replace-all "\"" "\\\"")
       (regex-replace-all "(?<!\\\\)\\\"" <> "\\\\\"")

       ;; replace all ~ with ~~
       (regex-replace-all "~" <> "~~")))

(defun unescape-string (str)
  "Remove extra escape characters from STR prior to writing to screen or file.
Control characters for whitespace (\\n, \\t, \\b, \\r) and double quotes (\")
are preceded by an extra pair of backslashes. See also `escape-string'."
  (-<> str
       ;; change \\\\foo to \\foo
       (regex-replace-all "\\\\\\\\(n\|t\|b\|r)" <> "\\\\\\1")
       ;; change \\\" to \"
       (regex-replace-all "\\\\\\\"" <> "\"")))

(defun make-terminal-raw ()
  "Place the terminal into 'raw' mode, no echo or delete.
This allows characters to be read directly without waiting for a newline.
See 'man 3 termios' for more information."
  #+win32 (error "`make-terminal-raw' not implemented for windows.")
  #-sbcl (error "`make-terminal-raw' not implemented for non-SBCL.")
  #+sbcl
  (let ((options (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag options)
          (logand (sb-posix:termios-lflag options)
                  (lognot (logior sb-posix:icanon
                                  sb-posix:echo
                                  sb-posix:echoe
                                  sb-posix:echok
                                  sb-posix:echonl))))
    (sb-posix:tcsetattr 0 sb-posix:TCSANOW options)))

(defun make-terminal-unraw ()
  "Place the terminal out of 'raw' mode, with echo and delete.
This allows characters to be read directly without waiting for a newline.
See 'man 3 termios' for more information."
  #+win32 (error "`make-terminal-raw' not implemented for windows.")
  #-sbcl (error "`make-terminal-raw' not implemented for non-SBCL.")
  #+sbcl
  (let ((options (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag options)
          (logior (sb-posix:termios-lflag options)
                  sb-posix:icanon
                  sb-posix:echo
                  sb-posix:echoe
                  sb-posix:echok
                  sb-posix:echonl))
    (sb-posix:tcsetattr 0 sb-posix:TCSANOW options)))


;;; Terminal size with CFFI and ioctl.
;;; Adapted from:
;;; https://github.com/cffi/cffi/blob/master/examples/gettimeofday.lisp
#-windows
(defcstruct winsize (row :short) (col :short) (xpixel :short) (ypixel :short))

#-windows
(define-foreign-type null-pointer-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser null-pointer))

#-windows
(defgeneric translate-to-foreign (value type)
  (:method (value (type null-pointer-type))
    (cond
      ((null value) (null-pointer))
      ((null-pointer-p value) value)
      (t (error "~A is not a null pointer." value)))))

#-windows
(define-foreign-type ioctl-result-type ()
  ()
  (:actual-type :int)
  (:simple-parser ioctl-result))

#-windows
(define-condition ioctl (error)
  ((ret :initarg :ret :initform nil :reader ret))
  (:report (lambda (condition stream)
             (format stream "IOCTL call failed with return value ~d"
                     (ret condition)))))
#-windows
(defgeneric translate-from-foreign (value type)
  (:method (value (type ioctl-result-type))
    (if (minusp value)
        (make-condition 'ioctl :ret value)
        value)))

#-windows
(defcfun ("ioctl" %ioctl) ioctl-result
  (fd :int)
  (request :int)
  (winsz :pointer))

#-windows
(defun term-size ()
  "Return terminal size information.
The following are returned as separate values; rows, columns,
x-pixels, y-pixels.  Note, this may throw an error when called from
SLIME."
  (with-foreign-object (wnsz '(:struct winsize))
    ;; 0 == STDIN_FILENO
    ;; 21523 == TIOCGWINSZ
    (%ioctl 0 21523 wnsz)
    (with-foreign-slots ((row col xpixel ypixel) wnsz (:struct winsize))
      (values row col xpixel ypixel))))


;;;; Shell and command line functions.
#-windows
(defun which (file &key (path (getenv "PATH")))
  (iterate (for dir in (split-sequence #\: path))
           (let ((fullpath (merge-pathnames file
                                            (make-pathname :directory dir))))
             (when (probe-file fullpath)
               (return fullpath)))))
#+windows
(defun convert-backslash-to-slash (str)
  (let ((new (copy-sequence 'string str)))
    (dotimes (i (length new) new)
      (if (char= (aref new i) #\\)
          (setf (aref new i) #\/)))))

#+windows
(defun ensure-slash (dir)
  "Make sure the directory name ends with a slash (or backslash)"
  (if (member (char dir (- (length dir) 1)) (list #\/ #\\))
      dir
      (concatenate 'string dir "\\")))

#+windows
(defun which (file &key (path (convert-backslash-to-slash (getenv "PATH"))))
  (iterate (for dir in (remove "" (split-sequence #\; path) :test 'equal))
           (let ((fullpath (merge-pathnames file (ensure-slash dir))))
             (when (probe-file fullpath)
               (return fullpath)))))

(defmacro getopts (args-and-opts &body forms)
  "Collect command-line options from ARGS in an executable.

For usage see the definition of `clang-instrument'.  E.g.,

    (getopts
      (\"-c\" \"--compiler\" (setf (compiler original) (pop args)))
      (\"-e\" \"--exit\" (setf instrument-exit t))
      (\"-F\" \"--flags\" (setf (flags original) (split-sequence #\, (pop args))))
      #| ... |#)
"
  (let ((arg (gensym))
        (getopts-block (gensym))
        (unknown (or (plist-get :unknown (cdr args-and-opts)) :error)))
    `(block ,getopts-block
       (loop :for ,arg = (pop ,(car args-and-opts)) :while ,arg :do
          (cond
            ,@(mapcar (lambda-bind ((short long . body))
                        `((or (string= ,arg ,short) (string= ,arg ,long))
                          ,@body))
                      forms)
            (:otherwise
             ,(case unknown
               (:error `(error "Unrecognized argument:~a" ,arg))
               (:return `(progn (push ,arg ,(car args-and-opts))
                                (return-from ,getopts-block))))))))))


;;;; generic forensic functions over arbitrary objects
(defun arglist (fname)
  "Return the argument list of FNAME."
  ;; Taken from swank/backend:arglist.
  #+sbcl
  (function-lambda-list fname)
  ;; NOTE: The following is similar, but may return 0 for nil args.
  ;; (sb-kernel:%simple-fun-arglist fname)
  #+ecl
  (multiple-value-bind (arglist foundp)
      (ext:function-lambda-list name)
    (if foundp arglist :not-available))
  #+ccl
  (multiple-value-bind (arglist binding) (let ((*break-on-signals* nil))
                                           (ccl:arglist fname))
    (if binding
        arglist
        :not-available))
  #-(or ecl sbcl ccl)
  (error "Only ECL, SBCL, and CCL."))

(defun show-it (hd &optional out)
  "Print the fields of a elf, section or program header.
Optional argument OUT specifies an output stream."
  (format (or out t) "~&")
  (mapcar
   (lambda (slot)
     (let ((val (slot-value hd slot)))
       (format (or out t) "~s:~a " slot val)
       (list slot val)))
   (mapcar #'slot-definition-name (class-slots (class-of hd)))))

(defun equal-it (obj1 obj2 &optional trace inhibit-slots)
  "Equal over objects and lists."
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((and (listp obj1) (not (listp (cdr obj1)))
            (listp obj2) (not (listp (cdr obj2))))
       (and (equal-it (car obj1) (car obj2))
            (equal-it (cdr obj1) (cdr obj2))))
      ((or (and (listp obj1) (listp obj2)) (and (vectorp obj1) (vectorp obj2)))
       (and (equal (length obj1) (length obj2))
            (reduce (lambda (acc pair)
                      (and acc (equal-it (car pair) (cdr pair) trace1)))
                    (if (vectorp obj1)
                        (mapcar #'cons (coerce obj1 'list) (coerce obj2 'list))
                        (mapcar #'cons obj1 obj2))
                    :initial-value t)))
      ((class-slots (class-of obj1))
       (reduce
        (lambda (acc slot)
          (and acc (equal-it (slot-value obj1 slot) (slot-value obj2 slot)
                             trace1)))
        (remove-if [{member _ inhibit-slots :test #'string= :key #'symbol-name}
                    #'symbol-name]
                   (mapcar #'slot-definition-name
                           (class-slots (class-of obj1))))
        :initial-value t))
      (t (equal obj1 obj2)))))

(defvar *uninteresting-conditions* nil
  "Additional uninteresting conditions for `with-quiet-compilation' to stifle.")

(defmacro with-quiet-compilation (&body body)
  `(let ((*load-verbose* nil)
         (*compile-verbose* nil)
         (*load-print* nil)
         (*compile-print* nil)
         (uiop/lisp-build:*uninteresting-conditions*
          (append *uninteresting-conditions*
                  uiop/lisp-build:*usual-uninteresting-conditions*)))
     ,@body))

(defmacro if-let* (bindings &body (then-form &optional else-form))
  "Creates new bindings, and conditionally executes THEN-FORM or ELSE-FORM.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each INITIAL-FORM is executed in turn, and the variable bound to the
corresponding value. INITIAL-FORM expressions can refer to variables
previously bound by the IF-LET*.

Execution of IF-LET* transitions to ELSE-FORM immediately if any
INITIAL-FORM evaluates to NIL.  No bindings are present if ELSE-FORM
is evaluated.  If all INITIAL-FORMs evaluate to true, then THEN-BODY
is executed."
  ;; NOTE: Largely adapted form Alexandria's `when-let*'.
  (with-gensyms  (if-block)
    (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                            (list bindings)
                            bindings)))
      (labels ((bind (bindings body)
                 (if bindings
                     `((let (,(car bindings))
                         (when ,(caar bindings)
                           ,@(bind (cdr bindings) body))))
                     `((return-from ,if-block ,body)))))
        `(block ,if-block
           (let (,(car binding-list))
             (when ,(caar binding-list)
               ,@(bind (cdr binding-list) then-form)))
           ,else-form)))))

(defmacro repeatedly (times &rest body)
  (let ((ignored (gensym)))
    `(loop :for ,ignored :below ,times :collect ,@body)))

(defun indexed (list)
  (loop :for element :in list :as i :from 0 :collect (list i element)))

(defun different-it (obj1 obj2 &optional trace)
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((or (and (vectorp obj1) (vectorp obj2))
           (and (proper-list-p obj1) (proper-list-p obj2)))
       (and (or (equal (length obj1) (length obj2))
                (format t "~&different lengths ~a!=~a"
                        (length obj1) (length obj2)))
            (reduce (lambda-bind (acc (i (a b)))
                      (and acc (or (different-it a b trace1)
                                   (format t "~& at ~d ~a!=~a" i a b))))
                    (indexed
                     (if (vectorp obj1)
                         (mapcar #'list (coerce obj1 'list) (coerce obj2 'list))
                         (mapcar #'list obj1 obj2)))
                    :initial-value t)))
      ((and (consp obj1) (consp obj2))
       (and (different-it (car obj1) (car obj2))
            (different-it (cdr obj1) (cdr obj2))))
      ((class-slots (class-of obj1))
       (reduce (lambda (acc slot)
                 (and acc (or (different-it
                               (slot-value obj1 slot) (slot-value obj2 slot)
                               trace1)
                              (format t "~&  ~a" slot))))
               (mapcar #'slot-definition-name
                       (class-slots (class-of obj1)))
               :initial-value t))
      (t (or (equal obj1 obj2) (format t "~&~a!=~a" obj1 obj2))))))

(defun count-cons (cons-cell)
  "Count and return the number of cons cells used in CONS-CELL."
  ;; TODO: extend to map over the fields in an object.
  (the fixnum (if (consp cons-cell)
                  (+ (count-cons (car cons-cell))
                     (count-cons (cdr cons-cell)))
                  1)))

(defun tree-right-length (tree &aux (size 1))
  "Return the length of the right spine of TREE."
  (declare (optimize speed))
  (iter (while (consp tree))
        (setf tree (cdr tree))
        (incf (the fixnum size)))
  (the fixnum size))

(defun tree-right-walk (tree)
  "Return the right spine of TREE as a list."
  (declare (optimize speed))
  (if tree
      (if (consp tree)
          (cons (car tree) (tree-right-walk (cdr tree)))
          (list tree))
      nil))


;;;; Generic utility functions
;;;
;;; Generic utility functions on cons tree, association lists, plists,
;;; and sequences.  Includes some random and statistical functions as
;;; well as string manipulation functions..
;;;
;;; DOXFIXME: for each of the above briefly describe the important
;;; functions with links to the index.
;;;
;;; @texi{generic-utility}
(defun mapt (function tree)
  "Like `mapcar' but TREE is a cons tree instead of a proper list."
  (if (consp tree)
      (cons (mapt function (car tree))
            (mapt function (cdr tree)))
      (funcall function tree)))

(defun plist-get (item list &key (test #'eql) &aux last)
  (loop :for element :in list :do
     (cond
       (last (return element))
       ((funcall test item element) (setf last t)))))

(defun plist-keys (plist)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (iter (for (key value) on plist by #'cddr)
        (declare (ignorable value))
        (collect key)))

(defun plist-drop-if (predicate list &aux last)
  (nreverse (reduce (lambda (acc element)
                      (cond
                        (last (setf last nil) acc)
                        ((funcall predicate element) (setf last t) acc)
                        (t (cons element acc))))
                    list :initial-value '())))

(defun plist-drop (item list &key (test #'eql))
  (plist-drop-if {funcall test item} list))

(defun plist-merge (plist-1 plist-2)
  "Merge arguments into a single plist with unique keys, prefer PLIST-1 items."
  (append plist-1 (plist-drop-if {member _ (plist-keys plist-1)} plist-2)))

(defun counts (list &key (test #'eql) key frac &aux totals)
  "Return an alist keyed by the unique elements of list holding their counts.
Keyword argument FRAC will return fractions instead of raw counts."
  (mapc (lambda (el)
          (if-let (place (assoc el totals :key key :test test))
            (incf (cdr place))
            (push (cons el 1) totals)))
        list)
  (if frac
      (let ((total (reduce #'+ (mapcar #'cdr totals))))
        (mapcar (lambda-bind ((obj . cnt)) (cons obj (/ cnt total))) totals))
      totals))

(defun proportional-pick (list key)
  (let ((raw (reduce (lambda (acc el) (cons (+ el (car acc)) acc))
                     (mapcar key list) :initial-value '(0))))
    (position-if {<= (random (first raw))} (cdr (reverse raw)))))

(defun position-extremum (list predicate key)
  "Returns the position in LIST of the element maximizing KEY."
  (car (extremum (indexed list) predicate :key [key #'second])))

(defun position-extremum-rand (list predicate key)
  "Randomly returns one of position in LIST maximizing KEY."
  (declare (ignorable predicate))
  (warn "`position-extremum-rand' not finished: doesn't use all parameters")
  (let ((scores (mapcar key list)))
    (random-elt (mapcar #'car (remove-if-not [{= (apply #'max scores)} #'second]
                                             (indexed scores))))))

(defun partition (test list)
  "Return a list of lists of elements of LIST which do and do not satisfy TEST.
The first list holds elements of LIST which satisfy TEST, the second
holds those which do not."
  (loop :for x :in list
     :if (funcall test x) :collect x :into yes
     :else :collect x :into no
     :finally (return (list yes no))))

(defun random-bool (&optional bias)
  (> (or bias 0.5) (random 1.0)))

(defun uniform-probability (list)
  (mapcar {cons _ (/ 1.0 (length list))} list))

(defun normalize-probabilities (alist)
  "Normalize ALIST so sum of second elements is equal to 1."
  (let ((total-prob (reduce #'+ (mapcar #'cdr alist))))
    (mapcar (lambda-bind ((key . prob)) (cons key (/ prob total-prob))) alist)))

(defun cumulative-distribution (alist)
  "Cumulative distribution function.
Return an updated version of ALIST in which the cdr of each element is
transformed from an instant to a cumulative probability."
  (nreverse
   (reduce (lambda-bind (acc (value . prob)) (acons value (+ (cdar acc) prob) acc))
           (cdr alist) :initial-value (list (car alist)))))

(defun un-cumulative-distribution (alist)
  "Undo the `cumulative-distribution' function."
  (let ((last 0))
    (mapcar (lambda-bind ((value . prob))
              (prog1 (cons value (- prob last)) (setf last prob)))
            alist)))

(defun random-pick (cdf)
  (car (find-if {<= (random 1.0)} cdf :key #'cdr)))

(defun random-elt-with-decay (orig-list decay-rate)
  (if (null orig-list)
      nil
      (labels ((pick-from (list)
                 (if (null list)
                     (pick-from orig-list)
                     (if (< (random 1.0) decay-rate)
                         (car list)
                         (pick-from (cdr list))))))
        (pick-from orig-list))))

(defun random-subseq (list &optional (size (1+ (if (null list) 0
                                                   (random (length list))))))
  (if (null list)
      nil
      (subseq (shuffle list) 0 size)))

(declaim (inline random-sample-with-replacement))
(defun random-sample-with-replacement
    (range size &aux (result (make-array size :element-type 'fixnum)))
  "Return a random sample of SIZE numbers in RANGE with replacement."
  (declare (optimize speed))
  (declare (type fixnum size))
  (declare (type fixnum range))
  (dotimes (n size (coerce result 'list))
    (setf (aref result n) (random range))))

(declaim (inline random-sample-without-replacement))
(defun random-sample-without-replacement (range size)
  (declare (optimize speed))
  (declare (type fixnum size))
  (declare (type fixnum range))
  "Return a random sample of SIZE numbers in RANGE without replacement."
  (cond
    ((> size range)
     (error "Can't sample ~a numbers from [0,~a] without replacement"
            size range))
    ((= size range)
     (let ((result (make-array size :element-type 'fixnum)))
       (dotimes (n range (coerce result 'list))
         (setf (aref result n) n))))
    (t
     ;; TODO: For faster collection implement a skip-list which
     ;;       increments the value being stored as it passes might be
     ;;       a better data structure.
     (labels ((sorted-insert (list value)
                (declare (type fixnum value))
                (cond
                  ((null list) (cons value nil))
                  ((< value (the fixnum (car list))) (cons value list))
                  (t (cons (car list) (sorted-insert (cdr list) (1+ value)))))))
       (let (sorted)
         (dotimes (n size sorted)
           (setf sorted (sorted-insert sorted (random (- range n))))))))))

(defun find-hashtable-element (hash-tbl n)
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (when (= n 0) (return-from find-hashtable-element k))
     (decf n))
   hash-tbl))

(defun random-hash-table-key (hash-tbl)
  "Return a random key in a hash table"
  (let ((size (hash-table-count hash-tbl)))
    (unless (zerop size)
      (find-hashtable-element hash-tbl (random size)))))

(defun mapcar-improper-list (fn list)
  "Apply FN to the elements of a possibly improper list LIST,
including the final non-nil tail element, if any.  Return a fresh
list composed of the value returned by each application.  Does
not work on circular lists."
  (let* ((head (list nil))
         (tail head))
    (iter (while (consp list))
          (setf tail (setf (cdr tail)
                           (list (funcall fn (pop list))))))
    (when list
      (setf (cdr tail) (funcall fn list)))
    (cdr head)))

;; From the Common Lisp Cookbook
(defgeneric replace-all (string part replacement &key test)
  (:documentation "Returns a new string in which all the
occurences of the part is replaced with replacement."))

(defmethod replace-all ((string string) (part string)
                        (replacement string) &key (test #'char=))
  (with-output-to-string (out)
    (loop :with part-length := (length part)
       :for old-pos := 0 :then (+ pos part-length)
       :for pos := (search part string
                           :start2 old-pos
                           :test test)
       :do (write-string string out
                         :start old-pos
                         :end (or pos (length string)))
       :when pos :do (write-string replacement out)
       :while pos)))

;; Specialization to base strings, which are more space
;; efficient
(defmethod replace-all ((string base-string) (part base-string)
                        (replacement base-string) &key (test #'char=))
  (coerce
   (with-output-to-string (out)
     (loop :with part-length := (length part)
        :for old-pos := 0 :then (+ pos part-length)
        :for pos := (search part string
                            :start2 old-pos
                            :test test)
        :do (write-string string out
                          :start old-pos
                          :end (or pos (length string)))
        :when pos :do (write-string replacement out)
        :while pos))
   'base-string))

(defun apply-replacements (list str)
  (if (null list)
      str
      (let ((new-str
             ;; If (caar list) is null then `replace-all' can fall
             ;; into an infinite loop.
             (if (and (caar list) (cdar list))
                 (replace-all str (caar list) (cdar list))
                 str)))
        (apply-replacements (cdr list) new-str))))

;;  Helper function for removing tags identifying DeclRefs
;;  from a code snippet.
(defgeneric peel-bananas (text)
  (:documentation  "Helper function for removing tags identifying DeclRefs
from a code snippet.")
  (:method ((text string))
    (apply-replacements '(("(|" . "") ("|)" . "")) text))
  (:method (text) text))

(defun peel-bananas-or-same (text)
  (let ((new (peel-bananas text)))
    (if (equal text new) text new)))

(defgeneric unpeel-bananas (text)
  (:method ((text string)) (concatenate 'string "(|" text "|)"))
  (:method (text) text))

(defun aget (item list &key (test #'eql))
  "Get KEY from association list LIST."
  (cdr (assoc item list :test test)))

(define-compiler-macro aget (&whole whole item list &key (test '#'eql test-p))
  (if (constantp item)
      (if test-p
          `(cdr (assoc ,item ,list :test ,test))
          `(cdr (assoc ,item ,list)))
      whole))

(define-setf-expander aget (item list &key (test ''eql) &environment env)
  (multiple-value-bind (dummies vals stores store-form access-form)
      (get-setf-expansion list env)
    (declare (ignorable stores store-form))
    (let ((store (gensym))
          (cons-sym (gensym)))
      (values dummies
              vals
              `(,store)
              `(let ((,cons-sym (assoc ,item ,access-form :test ,test)))
                 (if ,cons-sym
                     (setf (cdr ,cons-sym) ,store)
                     (prog1 ,store
                       (setf ,access-form (acons ,item ,store ,access-form)))))
              `(aget ,item ,access-form :test ,test)))))

(defun areplace (key val alist &key (test #'eql))
  "Replace the value of KEY in the association list ALIST with VAL."
  (cons (cons key val) (remove key alist :key #'car :test test)))

(defun adrop (drop-keys alist)
  "Remove all keys in DROP-KEYS from alist."
  (remove-if [{member _ drop-keys} #'car] alist))

(defun alist-filter (keep-keys alist)
  "Remove all keys from ALIST except those in KEEP-KEYS."
  (remove-if-not [{member _ keep-keys} #'car] alist))

(defun getter (key)
  "Return a function which gets KEY from an association list."
  (lambda (it) (aget key it)))

(defun transpose (matrix)
  "Simple matrix transposition."
  (apply #'map 'list #'list matrix))

(defun interleave (list sep &optional rest)
  (cond
    ((cdr list) (interleave (cdr list) sep (cons sep (cons (car list) rest))))
    (list (reverse (cons (car list) rest)))
    (t nil)))

(defun mapconcat (func list sep)
  (apply #'concatenate 'string (interleave (mapcar func list) sep)))

(defun drop (n seq)
  "Return SEQ less the first N items."
  (if (> n (length seq))
      nil
      (subseq seq (min n (length seq)))))

(defun drop-while (pred seq)
  (if (and (not (null seq)) (funcall pred (car seq)))
      (drop-while pred (cdr seq))
      seq))

(defun drop-until (pred seq)
  (drop-while (complement pred) seq))

(defun take (n seq)
  "Return the first N items of SEQ."
  (subseq seq 0 (min n (length seq))))

(defun take-while (pred seq)
  (if (and (not (null seq)) (funcall pred (car seq)))
      (cons (car seq) (take-while pred (cdr seq)))
      '()))

(defun take-until (pred seq)
  (take-while (complement pred) seq))

(defun pad (list n &optional (elem nil))
  "Pad LIST to a length of N with ELEM"
  (if (>= (length list) n)
      list
      (append list (make-list (- n (length list))
                              :initial-element elem))))

(defun chunks (list size &optional include-remainder-p)
  "Return subsequent chunks of LIST of size SIZE."
  (loop :for i :to (if include-remainder-p
                       (length list)
                       (- (length list) size))
     :by size :collect (subseq list i (min (+ i size) (length list)))))

(defun cartesian (lists)
  "Cartesian product of a set of lists."
  (cartesian-without-duplicates lists :test (constantly nil)))

(defun cartesian-without-duplicates (lists &key (test #'eql))
  "Cartesian product of a set of lists, without sets containing duplicates."
  (labels ((cartesian-nil-duplicates (lists)
             (if (car lists)
                 (mappend (lambda (inner)
                            (mapcar (lambda (outer)
                                      (if (not (member outer inner :test test))
                                          (cons outer inner)
                                          nil))
                                    (car lists)))
                          (cartesian-nil-duplicates (cdr lists)))
                 (list nil))))
    (remove-if [{> (length lists)} #'length] (cartesian-nil-duplicates lists))))

(defun binary-search (value array &key (low 0)
                                       (high (1- (length array)))
                                       (test (lambda (v)
                                                (cond ((< v value) -1)
                                                      ((> v value) 1)
                                                      (t 0)))))
  "Perform a binary search for VALUE on a sorted ARRAY.
Optional keyword parameters:
LOW:  Lower bound
HIGH: Higher bound
TEST: Test for the binary search algorithm taking on arg.
Return -1 if arg is less than value, 1 if arg is greater than value,
and 0 otherwise."
  (if (< high low)
      nil
      (let ((middle (floor (/ (+ low high) 2))))

        (cond ((< 0 (funcall test (aref array middle)))
               (binary-search value array :low low
                                          :high (1- middle)
                                          :test test))

              ((> 0 (funcall test (aref array middle)))
               (binary-search value array :low (1+ middle)
                                          :high high
                                          :test test))

              (t middle)))))

(defun tails (lst)
  "Return all final segments of the LST, longest first.

For example (tails '(a b c)) => ('(a b c) '(b c) '(c))
"
  (when lst (cons lst (tails (cdr lst)))))

(defun pairs (lst)
  "Return all pairs of elements in LST.

For example (pairs '(a b c)) => ('(a . b) '(a . c) '(b . c))
"
  (iter (for (a . rest) in (tails lst))
        (appending (iter (for b in rest)
                         (collecting (cons a b))))))

(defgeneric filter-subtrees (predicate tree)
  (:documentation "Return a list of subtrees of TREE satisfying PREDICATE."))

(defmethod filter-subtrees (predicate (tree list))
  "Return a list of subtrees of TREE satisfying PREDICATE."
  (when (and tree (listp tree))
    (append
     (when (funcall predicate tree) (list tree))
     (when (listp (car tree))
       (filter-subtrees predicate (car tree)))
     (when (listp (cdr tree))
       (filter-subtrees predicate (cdr tree))))))

(defun make-thread-safe-hash-table (&rest args)
  "Create a thread safe hash table with the given ARGS"
  #+(or sbcl ecl)
  (apply #'make-hash-table :synchronized t args)
  #+ccl
  (apply #'make-hash-table :shared :lock-free args)
  #-(or ccl sbcl ecl)
  (error "unsupported implementation for thread-safe hashtables"))


;;;; Symbol-related functions (useful for macros)
(defun symbol-cat (&rest symbols)
  "Return a symbol concatenation of SYMBOLS."
  (intern (string-upcase (mapconcat #'symbol-name symbols "-"))))

(defun symbol-cat-in-package (package &rest symbols)
  "Return a symbol concatenation of SYMBOLS in PACKAGE."
  (intern (string-upcase (mapconcat #'symbol-name symbols "-"))
          package))


;;;; Source and binary locations and ranges.
;;;
;;; Classes for representing locations and ranges in sequences
;;; (typically text source code files or binary executable bytes).
;;; Functions for comparing locations and ranges, as well as
;;; determining "contains" and "intersects" relationships between
;;; locations and ranges.
;;;
;;; @texi{locations-and-ranges}
(defclass source-location ()
  ((line :initarg :line :accessor line :type 'fixnum)
   (column :initarg :column :accessor column :type 'fixnum)))

(defclass source-range ()
  ((begin :initarg :begin :accessor begin :type 'source-location)
   (end   :initarg :end   :accessor end   :type 'source-location)))

(defclass range ()
  ((begin :initarg :begin :accessor begin :type 'fixnum)
   (end   :initarg :end   :accessor end   :type 'fixnum)))

(defmethod print-object ((obj source-location) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (line obj) stream)
    (format stream ":")
    (prin1 (column obj) stream)))

(defmethod print-object ((obj source-range) stream)
  (flet ((p1-range (range)
           (prin1 (line range) stream)
           (format stream ":")
           (prin1 (column range) stream)))
    (print-unreadable-object (obj stream :type t)
      (p1-range (begin obj))
      (format stream " to ")
      (p1-range (end obj)))))

(defmethod print-object ((obj range) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (begin obj) stream)
    (format stream " to ")
    (prin1 (end obj) stream)))

(defgeneric source-< (a b)
  (:documentation "Return true if source location A comes strictly before B.")
  (:method ((a source-location) (b source-location))
    (or (< (line a) (line b))
        (and (= (line a) (line b))
             (< (column a) (column b))))))

(defgeneric source-<= (a b)
  (:documentation "Return true if source location A is equal to or comes
before B.")
  (:method ((a source-location) (b source-location))
    (or (< (line a) (line b))
        (and (= (line a) (line b))
             (<= (column a) (column b))))))

(defgeneric source-> (a b)
  (:documentation "Return true if source location A comes strictly after B.")
  (:method ((a source-location) (b source-location))
    (or (> (line a) (line b))
        (and (= (line a) (line b))
             (> (column a) (column b))))))

(defgeneric source->= (a b)
  (:documentation "Return true if source location A is equal to or comes
after B.")
  (:method ((a source-location) (b source-location))
    (or (> (line a) (line b))
        (and (= (line a) (line b))
             (>= (column a) (column b))))))

(defgeneric contains (range location)
  (:documentation "Return true if RANGE fully subsumes LOCATION.")
  (:method ((range source-range) (location source-location))
    (and (source-<= (begin range) location)
         (source->= (end range) location)))
  (:method ((a-range source-range) (b-range source-range))
    (and (source-<= (begin a-range) (begin b-range))
         (source->= (end a-range) (end b-range))))
  (:method ((range range) point)
    (and (<= (begin range) point) (>= (end range) point)))
  (:method ((a-range range) (b-range range))
    (and (<= (begin a-range) (begin b-range))
         (>= (end a-range) (end b-range)))))

(defgeneric intersects (a-range b-range)
  (:documentation "Return true if A-RANGE and B-RANGE intersect.")
  (:method ((a-range source-range) (b-range source-range))
    (and (source-< (begin a-range) (end b-range))
         (source-> (end a-range) (begin b-range))))
  (:method ((a-range range) (b-range range))
    (and (< (begin a-range) (end b-range))
         (> (end a-range) (begin b-range)))))


;;; Functions to run multiple tasks (such as mutations and fitness
;;; tests) on multiple threads.
;;;
;;; This module makes use of Bordeaux Threads. See the documentation
;;; here:
;;; https://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation
;;;
;;; * Job: a function which returns a series of Tasks, one Task each
;;;        time it is called. When the series is exhausted, it will
;;;        return NIL.
;;;
;;; * Task: a thin wrapper over a Lisp object (typically a SOFTWARE
;;;         instance, but could be anything).  The task can be used to
;;;         customize how to process the object after fitness testing
;;;         (basically a completion routine) and to customize how it
;;;         spins off child Jobs.
;;;
;;; * Worker: one or more Worker threads can be created to process
;;;           Jobs.  When all jobs are finished, all the Worker
;;;           threads will exit.  Therefore you create Jobs first,
;;;           then the Workers.
;;;
;;; @subsection Description
;;;
;;; A TASK is an operation to be performed by the multi-threaded
;;; TASK-RUNNER. A TASK can be customized by the client to generate
;;; a job (child series of tasks) by implementing the TASK-JOB method,
;;; and the code to be performed when processing a TASK is defined by the
;;; PROCESS-TASK method.
;;;
;;; A job is a Lisp function, which takes no arguments, and which will
;;; produce a TASK each time it is called, or NIL when all of its tasks are
;;; complete. Think of a job as a lazy sequence of TASK.
;;;
;;; TASK-RUNNER-JOBS is a stack of jobs. Worker threads will call the
;;; first job on the stack, and process the task returned.
;;;
;;; A task may add 1 or more jobs to the top of the stack, causing worker
;;; threads to immediately start processing those jobs since they are now
;;; higher on the stack and therefore have priority over other tasks.
;;;
;;; When the JOBS stack is empty/NIL, then all worker threads will exit.
;;;
;;; @subsection Example use
;;;
;;;     (setf *runner* (run-task (make-instance 'single-cut-all :object *orig*)
;;;                              10))
;;;      ;; When (task-runner-worker-count *runner*) = 0,
;;;      ;; it means all threads are finished.
;;;
;;; @subsection Complex Example use
;;;
;;;     (defmacro task-map (num-threads function sequence)
;;;       "Run FUNCTION over SEQUENCE using a `simple-job' `task-job'."
;;;       (with-gensyms (task-map task-item)
;;;         `(if (<= ,num-threads 1)
;;;              (mapcar ,function ,sequence) ; No threading.
;;;              (progn                     ; Multi-threaded implementation.
;;;                (defclass ,task-map (task) ())  ; Task to map over SEQUENCE.
;;;                (defclass ,task-item (task) ()) ; Task to process elements.
;;;                (defmethod task-job ((task ,task-map) runner)
;;;                  (declare (ignore runner))
;;;                  (let ((objs (task-object task))) ; Enclose SEQUENCE for fn.
;;;                    (lambda () (when objs ; Return nil when SEQUENCE is empty.
;;;                                ;; Return a task-item whose task-object run
;;;                                ;; FUNCTION on the next element of SEQUENCE.
;;;                            (make-instance ',task-item :object
;;;                                           (curry ,function (pop objs)))))))
;;;                (defmethod process-task ((task ,task-item) runner)
;;;                  ;; Evaluate the task-object for this item as created in
;;;                  ;; the task-job method above.  Save the results.
;;;                  (task-save-result runner (funcall (task-object task))))
;;;                (task-runner-results ; Return results from the results obj.
;;;                 ;; Create the task-map object, and run until exhausted.
;;;                 (run-task-and-block
;;;                  (make-instance ',task-map :object ,sequence)
;;;                  ,num-threads))))))
;;;
;;; The above example uses the tasks API to implement a simple
;;; parallel map spread across a configurable number of workers.  When
;;; more than one worker thread is requested the following objects and
;;; methods are created to implement the parallel map.
;;;
;;; * A TASK-MAP TASK is created to hold the sequence.
;;;
;;; * The TASK-JOB method is defined for this TASK-MAP.  This method
;;;   returns a function which has access to the SEQUENCE in a
;;;   closure.  The function will continually pop the first element
;;;   off the top of the sequence and wrap it in a TASK-ITEM object to
;;;   be returned until the SEQUENCE is empty at which point the
;;;   function returns nil causing all worker threads to exit.
;;;
;;; * The TASK-ITEM TASK is created to hold tasks for every item in
;;;   the sequence.
;;;
;;; * The PROCESS-TASK method is defined for this TASK-ITEM.  This
;;;   method evaluates the function stored in the TASK-OBJECT of this
;;;   TASK-ITEM and saves the result into the task runner's results.
;;;
;;; Finally, with the above objects and methods defined, the TASK-MAP
;;; macro wraps the sequence into a TASK-MAP TASK object and passes
;;; this to the RUN-TASK-AND-BLOCK function yielding a runner and the
;;; contents of that runner are extracted and returned using the
;;; TASK-RUNNER-RESULTS accessor.
;;;
;;; See the actual implementation of TASK-MAP in the SEL/UTILITY
;;; package for a more efficient implementation which doesn't use a
;;; macro or require new objects and methods to be defined on the fly.
;;;
;;; @texi{task}

(defstruct task-runner
"The state needed to run multi-threaded tasks and associated jobs.
* jobs:         stack of current jobs to execute
* workers:      list running worker threads
* results:      result objects collected by worker threads
* jobs-lock:    (internal) used to synchronize jobs stack
* results-lock: (internal) used to synchronnize results list
* workers-lock: (internal) used to synchronize worker list
* completed-jobs: number of jobs that have been executed and finished
* completed-tasks: number of tasks that have finished"
  (jobs nil)
  (workers nil)
  (results nil)
  (jobs-lock (bt:make-recursive-lock "task-runner-jobs"))
  (results-lock (bt:make-lock "task-runner-results"))
  (workers-lock (bt:make-lock "task-runner-workers"))
  (completed-jobs 0)
  (completed-tasks 0))

(defparameter *task-runner* nil
  "Bind *TASK-RUNNER* for worker threads")

(defclass task ()
  ((object :initarg :object :accessor task-object))
  (:documentation "Base class for all task classes."))

;;;
;;; A class derived from the TASK class will override the method
;;; TASK-JOB to customize how the job's tasks are created and ordered.
;;; This method should return a Job, which is a function, which, when executed,
;;; will return the next TASK in the series. A Job returns NIL when no more
;;; tasks remain.
;;;
(defgeneric task-job (task runner)
 (:documentation  "Return a job for the *jobs* stack. This is a function which,
 when called, returns the next task in the series."))

;;; default method gives error if you try to create a job and there is no
;;; overridden method to do it
(defmethod task-job (obj runner)
  (declare (ignore obj runner))
  (error "Cannot create a task from the object"))

;;;
;;; Process task and task object, including evaluate fitness, push new Jobs,
;;; and store interesting results..
;;; PROCESS-TASK may add one ore more jobs based on the variant qualities
;;; and the fitness. The added jobs get pushed onto the TASK-RUNNER-JOBS stack
;;; and will be the next jobs to run.
;;;
;;; Also, PROCESS-TASK may save any interesting software variants on
;;; the TASK-RUNNER-RESULTS list.
;;;

(defgeneric process-task (task runner)
  (:documentation
   "Process the object, including evaluate fitness, push new Jobs, and
 store interesting results"))

(defmethod process-task (obj runner)
  (declare (ignore obj runner))
  ;; default does nothing
  nil)

;;;
;;; increment completed task counter after each task is processed
;;;
(defmethod process-task :after (obj runner)
  (declare (ignore obj))
  (bt:with-recursive-lock-held ((task-runner-jobs-lock runner))
    (incf (task-runner-completed-tasks runner))
    nil))

;;;
;;; Returns the next mutated software task, or NIL if no more tasks
;;;
(defun get-next-task (runner)
  (bt:with-recursive-lock-held ((task-runner-jobs-lock runner))
    (if (consp (task-runner-jobs runner))
        (let ((task (funcall (car (task-runner-jobs runner)))))
          (if (null task)                       ;; if no more tasks in that job
              (progn
                (pop (task-runner-jobs runner)) ;; pop the job
                (incf (task-runner-completed-jobs runner))
                (get-next-task runner)) ;; and recurse (until no more jobs)
              task)))))

;;;
;;; Add a Job to the JOBS stack.
;;;
(defun task-runner-add-job (runner job)
  (bt:with-recursive-lock-held ((task-runner-jobs-lock runner))
    (push job (task-runner-jobs runner))))

;;;
;;; exit-worker
;;; Do any processing necessary when a worker thread exits
;;; For now, just decrement the *WORKER-COUNT* special variable.
;;;
(defun exit-worker (runner)
  (bt:with-lock-held ((task-runner-workers-lock runner))
    (setf (task-runner-workers runner)
          (remove (current-thread) (task-runner-workers runner) :test 'equal))))

;;;
;;; The task executed by each worker thread.
;;; It simply executes a loop:
;;;     (a) get next task (GET-NEXT-TASK)
;;;     (b) process the task object (PROCESS-TASK)
;;; until no more tasks are found. Then the thread exits.
;;;
(defun worker-thread-task (runner)
  (do* ((task (get-next-task runner)(get-next-task runner)))
       ((null task)(exit-worker runner))
    (process-task task runner)))

;;; start-worker
;;; Do any processing when a worker thread is started.
;;;
(defun start-worker ()
  (let ((runner *task-runner*))  ;; get the special variable binding
    (worker-thread-task runner)))  ;; begin worker loop

(defun task-save-result (runner obj)
  "Save a result object."
  (bt:with-lock-held ((task-runner-results-lock runner))
    (push obj (task-runner-results runner))))

(let ((worker-id -1))
  (defun task-runner-create-worker (runner)
    "Create a new worker thread."
    (let ((*default-special-bindings* (acons '*task-runner* runner nil)))
      (with-lock-held ((task-runner-workers-lock runner))
        (push (make-thread 'start-worker
                :name (format nil "~A-~D" "software-mutator"
                              (incf worker-id)))
              (task-runner-workers runner))))))

;;
;; Re-initialize, overwrite any previous results.
;;
(defun task-runner-init-jobs (runner)
  (setf (task-runner-jobs runner) nil
        (task-runner-workers runner) nil
        (task-runner-results runner) nil))

;;
;; Remove all jobs from the jobs stack. This will cause
;; all the worker threads to finish and exit.
;;
(defun task-runner-stop-jobs (runner)
  (setf (task-runner-jobs runner) nil))

;;
;; Returns the TASK-RUNNER.
;; Client should save this result for further status updates.
;;
(defun run-task (task &optional (num-workers 1))
  "Create a TASK-RUNNER, using the specified task as the first job."
  (let ((runner (make-task-runner)))
    (task-runner-add-job runner (task-job task runner))
    (dotimes (i num-workers)
      (task-runner-create-worker runner))
    runner))

(defun run-task-and-block (task &optional (num-workers 1))
  "Create a TASK-RUNNER, using the specified task as the first job,
blocking until completion"
  (let ((runner (run-task task num-workers)))
    (mapcar #'join-thread (task-runner-workers runner))
    runner))

(defun task-runner-remaining-jobs (runner)
  "Returns the number of jobs remaining."
  (length (task-runner-jobs runner)))

(defun task-runner-workers-count (runner)
  "Returns the number of running threads."
  (length (task-runner-workers runner)))

;; Implementation of `some-task': mimics the behavior of `some'
;; except that results are stored as a list (due to implementation of
;; `task-save-result').
(defclass some-task (task)
  ((pred :initarg :pred :accessor some-task-pred
         :documentation "Predicate used by `some'."))
  (:documentation "Task for applying `some' in parallel.
The OBJECT field is a list on whose elements SOME-TASK-PRED is applied."))

(defclass some-test-task (task)
  ((pred :initarg :pred :accessor some-task-pred
         :documentation "Predicate used by `some'."))
  (:documentation "Task to apply predicate SOME-TASK-PRED to OBJECT."))

(defmethod task-job ((task some-task) runner)
  "Return the generating function for `some-task'.
Create new subtasks for each item in the list until either applying the
predicate SOME-TASK-PRED in TASK succeeds or there are no more items in the
list."
  (let ((ls (task-object task)))
    (lambda ()
      ;; Stop creating new tasks after a result is found or no elements remain.
      (when (and ls (not (task-runner-results runner)))
        (prog1
            (make-instance 'some-test-task
                           :object (car ls)
                           :pred (some-task-pred task))
          (setf ls (cdr ls)))))))

(defmethod process-task ((task some-test-task) runner)
  "Process a single TASK by applying SOME-TASK-PRED to the OBJECT in TASK.
NOTE: Since `task-save-result' pushes results to a list, it's possible for up to
N results to be saved (where N is the number of running threads), so `first'
should be used to retrieve one result. Additionally, due to differences in
timing, it's possible that the result won't match that of `some', since `some'
promises to find the first while `some-task' may return any element satisfying
`some-task-pred'."
  (when (not (task-runner-results runner))
    (when-let ((result (funcall (some-task-pred task) (task-object task))))
      (task-save-result runner result))))


;;;
;;; A simple way to just run a single task as a one-off
;;; Example:
;;;   (run-as-task (task1 runner1)
;;;     (with-output-to-string (s)
;;;       (dotimes (i 10)(format s "~A~%" i))
;;;       (task-save-result runner1 (get-output-stream-string s))))
;;;

(defclass simple-job (task) ())
(defmethod task-job ((task simple-job) runner)
  (declare (ignore runner))
  (let ((index 0))
    (lambda ()
      (if (<= (incf index) 1)
          (task-object task)))))

(defmacro run-as-task ((task runner) &body body)
  "Run the body code as a one-off task, which can access task and runner by
name. The supplied names may be any available symbols. Returns the TASK-RUNNER
object."
  (with-gensyms (task-type)
    `(progn
       (defclass ,task-type (task) ())
       (defmethod process-task ((,task ,task-type) ,runner)
         ,@body)
      (run-task
        (make-instance 'simple-job
          :object (make-instance ',task-type :object nil))))))

(defclass task-map (task)
  ((task-function :initarg :task-function :accessor task-function))
  (:documentation
   "Task object used to map a function over a sequence using workers."))

(defclass task-item (task) ()
  (:documentation
   "Task object used to execute a function on an element of a sequence.
See the `task-job' method on `task-map' objects."))

(defmethod task-job ((task task-map) runner)
  "Return a function which will spawn jobs for all of TASK's objects."
  (declare (ignore runner))
  (let ((objs (task-object task)))
    (lambda () (when objs
                 ;; Return a task-item whose task-object run
                 ;; FUNCTION on the next element of OBJECTS.
                 (make-instance 'task-item :object
                                (curry (task-function task) (pop objs)))))))

(defmethod process-task ((task task-item) runner)
  "Evaluate the TASK saving the result in the runner."
  (task-save-result runner (funcall (task-object task))))

(defun task-map (num-threads function objects)
  "Run FUNCTION over OBJECTS using a `simple-job' `task-job'."
  (if (<= num-threads 1)
      (mapcar function objects)   ; No threading when num-threads <= 1.
      (task-runner-results  ; Return the results from the results obj.
       ;; Create the task-map object, and run until exhausted.
       (run-task-and-block (make-instance 'task-map
                             :object objects
                             :task-function function)
                           num-threads))))

(defun task-map-async (num-threads func objects)
  "Run FUNC over OBJECTS using a `simple-job' `task-job'."
  ;; Create the task-map object, and run until exhausted.
  (run-task (make-instance 'task-map
              :object objects
              :task-function func)
            num-threads))

(defun simple-task-async-runner (num-threads func arguments)
  "Run FUNCTION with ARGUMENTS as a `simple-job' `task-job'."
  ;; Create the task-map object, and run until exhausted.
  (task-map-async num-threads (lambda (args) (apply func args)) arguments))


;;;; Debugging helpers
;;;
;;; Functions useful for debugging lisp code.  Of particular note are
;;; the `note' functions and associated `*note-level*' and
;;; `*note-out*' variables which provide a basic logging framework.
;;;
;;; @texi{debugging}
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *compile-w/tracing* nil
    "Controls compilation of tracing information with the `traced' macro."))

(defmacro traced ((fn &rest args))
  "Trace wrapped function call when `*compile-w/tracing*' is non-nil.
This is useful for `flet' and `labels' functions which can't be traced
with `cl-user:trace'."
  (if *compile-w/tracing*
      (let ((result-sym (gensym)))
        `(progn (format t "  X: ~S ~S~%" ',fn (list ,@args))
                (let ((,result-sym (,fn ,@args)))
                  (format t ,(format nil "  X: ~a returned~~%      ~~S~~%" fn)
                          ,result-sym)
                  ,result-sym)))
      `(,fn ,@args)))

(defvar *note-level* 0 "Enables execution notes.")
(defvar *note-out* '(t) "Targets of notation.")

(defun replace-stdout-in-note-targets (&optional (targets *note-out*))
  "Replace `t' which is a place holder for `*standard-output*'.
Ideally we would like to set the value of `*note-out*' to a list
holding `*standard-output*', however in compiled binaries the value of
`*standard-output*' changes each time the binary is launched.  So
instead we use `t' as a place-holder, and provide this function for
performing the replacement on the fly when `note' is called.  To
specify a particular value for `*standard-output*' the user may
replace `t' in the `*note-out*' list."
  (mapcar (lambda (s) (if (eq s t) *standard-output* s)) targets))

(defun print-time (&optional (out t))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignorable day-of-week dst-p tz))
    (format out "~d.~2,'0d.~2,'0d.~2,'0d.~2,'0d.~2,'0d"
            year month date hour minute second)))

(defun note (level &rest format-args)
  (when (>= *note-level* level)
    (let ((*print-pretty* nil))
      (mapcar
       #'finish-output
       (mapc
        {write-sequence
         (concatenate 'string ";;" (print-time nil) ": "
                      (apply #'format nil format-args)
                      (list #\Newline))}
        (replace-stdout-in-note-targets)))))
  ;; Always return nil.
  nil)

(defmacro with-warnings-as-notes (note-level &body forms)
  `(handler-bind ((warning (lambda (c)
                             (note ,note-level "~&~A~%" c)
                             (invoke-restart 'muffle-warning))))
     ,@forms))

#+sbcl
(defun trace-memory ()
  (when (>= *note-level* 2)
    (let ((percentage-used (/ (sb-vm::dynamic-usage)
                              (sb-ext::dynamic-space-size))))
      (if (>= *note-level* 4)
        (note 4 "~a ~,2f~%" (second (sb-debug:list-backtrace))
                            percentage-used)
        (when (>= percentage-used 0.5)
          (note 2 "~a ~,2f~%" (second (sb-debug:list-backtrace))
                              percentage-used))))))

;; adopted from a public domain lisp implementation copied from the

;; scheme implementation given at
;; http://en.wikipedia.org/wiki/Levenshtein_distance
(defun levenshtein-distance (s1 s2 &key (test #'char=) (key #'identity))
  (let* ((width (1+ (length s1)))
         (height (1+ (length s2)))
         (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
        (setf (aref d (1+ y) (1+ x))
              (min (1+ (aref d y (1+ x)))
                   (1+ (aref d (1+ y) x))
                   (+ (aref d y x)
                      (if (funcall test
                                   (funcall key (aref s1 x))
                                   (funcall key (aref s2 y)))
                          0
                          1))))))
    (aref d (1- height) (1- width))))

;;; Diff computing
(defun diff-scalar (original-seq modified-seq)
  "Return an integer representing the diff size of two sequences
Sum O + |O - M| over each diff region.  O is the length of the
original diff region and M is the length of the modified diff
region."
  (reduce (lambda (acc region)
            (+ acc
               (ecase (type-of region)
                 (common-diff-region 0)
                 (modified-diff-region
                   (+ (original-length region)
                      (abs (- (original-length region)
                              (modified-length region))))))))
          (diff:compute-raw-seq-diff original-seq modified-seq)
          :initial-value 0))

;;; memory mapping, address -> LOC
(defun gdb-disassemble (phenome function)
  "Return the raw gdb disassembled code of FUNCTION in PHENOME."
  (shell "gdb --batch --eval-command=\"disassemble ~s\" ~s 2>/dev/null"
         function phenome))

(defun addrs (phenome function)
  "Return the numerical addresses of the lines (in order) of FUNCTION."
  (remove nil
    (mapcar
     (lambda (line)
       (multiple-value-bind (matchp strings)
           (scan-to-strings "[\\s]*0x([\\S]+)[\\s]*<([\\S]+)>:.*" line)
         (when matchp (parse-integer (aref strings 0) :radix 16))))
     (split-sequence #\Newline (gdb-disassemble phenome function)))))

(defun function-lines (lines)
  "Return the line numbers of the lines (in order) of FUNCTION.
LINES should be the output of the `lines' function on an ASM object."
  (loop :for line :in lines :as counter :from 0
     :for function = (register-groups-bind
                         (line-function) ("^\\$*([^\\.][\\S]+):" line)
                       line-function)
     :collect (or function counter)))

(defun calculate-addr-map (lines phenome genome)
  "Calculate a map of memory address to offsets in LINES.
LINES should be the output of the `lines' function on an ASM object,
PHENOME should be the phenome of an ASM object and GENOME should be
the genome of an ASM object."
  (let ((flines (function-lines lines))
        (genome (coerce genome 'vector))
        (map (make-hash-table)))
    (loop
       :for addrs :in (mapcar (lambda (func) (addrs phenome func))
                              (remove-if-not #'stringp flines))
       :for lines :in (cdr (mapcar
                            {remove-if
                             [{scan "^[\\s]*\\."} {aget :code} {aref genome}]}
                            (split-sequence-if #'stringp flines)))
       :do (mapc (lambda (addr line) (setf (gethash addr map) line))
                 addrs lines))
    map))


;;;; Oprofile functions
;;;
;;; Functions for working with oprofile, a formerly popular Linux
;;; statistical profiler which has subsequently been superseded by
;;; Linux Perf.  See @url{http://oprofile.sourceforge.net/news/}.
;;;
;;; @texi{oprofile}
(defun samples-from-oprofile-file (path)
  (with-open-file (in path)
    (remove nil
      (iter (for line = (read-line in nil :eof))
            (until (eq line :eof))
            (collect (register-groups-bind (c a)
                         ("^ *(\\d+).+: +([\\dabcdef]+):" line)
                       (cons (parse-integer (or a "") :radix 16)
                             (parse-integer (or c "")))))))))

(defun samples-from-tracer-file (path &aux samples)
  (with-open-file (in path)
    (loop :for line := (read-line in nil)
       :while line
       :do (let ((addr (parse-integer line)))
             (if (assoc addr samples)
                 (setf (cdr (assoc addr samples))
                       (1+ (cdr (assoc addr samples))))
                 (setf samples (cons (cons addr 0) samples)))))
    samples))

(defvar *resolved-header-files* (make-hash-table :test 'equal)
  "A map from function name to a list of headers where
that function may be declared.")

(defun headers-in-manpage (section name)
  (multiple-value-bind (stdout stderr errno)
      (shell
       "man -P cat ~a ~a | sed -n \"/DESCRIPTION/q;p\" | grep \"#include\" | cut -d'<' -f 2 | cut -d'>' -f 1"
       section name)
    (declare (ignorable stderr errno))
    (split-sequence #\Newline stdout :remove-empty-subseqs t)))

(defun resolve-function-includes (func)
  (let ((headers (gethash func *resolved-header-files* 'not-found)))
    (mapcar {format nil "<~a>"}
            (if (eq headers 'not-found)
                (setf (gethash func *resolved-header-files*)
                      (or (headers-in-manpage 3 func)
                          (headers-in-manpage 2 func)))
                headers))))

(defun unlines (lines)
  (format nil "~{~a~^~%~}" lines))

;; Just a little sed-ish thing: find the first line that
;; contains the substring needle, and return the lines
;; after the one that matched.
(defun keep-lines-after-matching (needle haystack)
  (labels ((keep-after (lines)
             (if (null lines)
                 '()
                 (if (search needle (car lines))
                     (cdr lines)
                     (keep-after (cdr lines))))))
    (unlines (keep-after (split-sequence '#\Newline haystack)))))



;;;; Enhanced COPY-SEQ functionality
;;;

(defun sel-copy-array (array)
  (let* ((element-type (array-element-type array))
         (fill-pointer (and (array-has-fill-pointer-p array)(fill-pointer array)))
         (adjustable (adjustable-array-p array))
         (new (make-array (array-dimensions array)
                          :element-type element-type
                          :adjustable adjustable
                          :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array) new)
      (setf (row-major-aref new i)(row-major-aref array i)))))

(defun enhanced-copy-seq (sequence)
  "Copies any type of array (except :displaced-to) and lists. Otherwise returns NIL."
  (if (arrayp sequence)
      (sel-copy-array sequence)
      (if (listp sequence)
          (copy-list sequence))))


;;;; Iteration helpers
(defmacro-clause (CONCATENATING expr &optional INTO var INITIAL-VALUE (val ""))
  ;; Use of this helper is *NOT* recommended for potentially large
  ;; lists of strings as the repeated concatenation continually copies
  ;; the string leading to very bad performance.  A better option
  ;; would be the following:
  ;;
  ;;     (with-output-to-string (out)
  ;;       (iter #|...foo...|#
  ;;         (write-string #|...bar|# out)))
  `(reducing ,expr by {concatenate 'string} into ,var initial-value ,val))


;;;; Profiling
;;;
;;; Tools for profiling lisp code.  Specifically functionality for
;;; dumping profiling information into a format usably for the
;;; generation of flame graphs (see
;;; @url{http://oprofile.sourceforge.net/news/}).
;;;
;;; @texi{profiling}

;; Dot implementation from
;; https://techfak.uni-bielefeld.de/~jmoringe/call-graph.html.
(defvar *profile-dot-min-ratio* 1/200
  "Minimum percentage ratio to include a node in the profile dot graph.")

#+sbcl
(defmethod cl-dot:graph-object-node
    ((graph sb-sprof::call-graph) (object sb-sprof::node))
  (flet ((ratio->color (ratio)
           (let ((red   (floor 255))
                 (green (floor (alexandria:lerp ratio 255 0)))
                 (blue  (floor (alexandria:lerp ratio 255 0))))
             (logior (ash red 16) (ash green 8) (ash blue 0)))))
    (let ((ratio (/ (sb-sprof::node-count object)
                    (sb-sprof::call-graph-nsamples graph))))
      (make-instance 'cl-dot:node
        :attributes `(:label ,(format nil "~A\\n~,2,2F %"
                                      (sb-sprof::node-name object) ratio)
                             :shape     :box
                             :style     :filled
                             :fillcolor ,(format nil "#~6,'0X"
                                                 (ratio->color ratio)))))))

#+sbcl
(defmethod cl-dot:graph-object-pointed-to-by
    ((graph sb-sprof::call-graph) (object sb-sprof::node))
  (sb-sprof::node-callers object))

#+sbcl
(defun profile-to-dot-graph (stream)
  "Write profile to STREAM."
  (progn
    (unless sb-sprof::*samples*
      (warn "; `profile-to-dot-graph': No samples to report.")
      (return-from profile-to-dot-graph))
    (let ((call-graph (sb-sprof::make-call-graph most-positive-fixnum)))
      (cl-dot:print-graph
       (cl-dot:generate-graph-from-roots
        call-graph
        (remove-if [{> *profile-dot-min-ratio*}
                    {/ _ (sb-sprof::call-graph-nsamples call-graph)}
                    #'sb-sprof::node-count]
                   (sb-sprof::call-graph-vertices call-graph)))
       :stream stream))))

#-sbcl
(defun profile-to-dot-graph (&rest args)
  (declare (ignorable args))
  (error "`PROFILE-TO-DOT-GRAPH' unimplemented for non-SBCL lisps."))

;; FlameGraph implementation from
;; http://paste.lisp.org/display/326901.
#+sbcl
(defun profile-to-flame-graph (stream)
  "Write FlameGraph profile data to STREAM.
The resulting file may be fed directly to the flamegraph tool as follows.

    REPL> (sb-sprof:start-profiling)

       ...do some work...

    REPL> (with-open-file (out \"profile.data\"
                               :direction :output
                               :if-exists :supersede)
            (profile-to-flame-graph out))

    shell$ cat profile.data|flamegraph > profile.svg

See http://www.brendangregg.com/FlameGraphs/cpuflamegraphs.html."
  (progn
    (unless sb-sprof::*samples*
      (warn "; `profile-to-flame-graph': No samples to report.")
      (return-from profile-to-flame-graph))
    (let ((samples (sb-sprof::samples-vector sb-sprof::*samples*))
          (counts (make-hash-table :test #'equal)))

      (sb-sprof::with-lookup-tables ()
        (loop :for start = 0 :then end
           :while (< start (length samples))
           :for end = (or (position 'sb-sprof::trace-start samples
                                    :start (1+ start)
                                    :key (lambda (it) (and (listp it) (car it))))
                          (return))
           :do (let ((key
                      (sb-sprof::with-output-to-string (stream)
                        (loop :for i :from (- end 2) :downto (+ start 2) :by 2
                           :for node = (sb-sprof::lookup-node
                                        (aref samples i))
                           :when node
                           :do (let ((*print-pretty* nil))
                                 (format stream "~A;"
                                         (sb-sprof::node-name node)))))))
                 (incf (gethash key counts 0)))))

      (maphash (lambda (trace count)
                 (format stream "~A ~D~%" trace count))
               counts))))

#-sbcl
(defun profile-to-flame-graph (&rest args)
  (declare (ignorable args))
  (error "`PROFILE-TO-FLAME-GRAPH' unimplemented for non-SBCL lisps."))

;;; Utilities associated with json processing

(defun convert-jsown-tree (jt &optional (key-fn (lambda (s)
                                                  (intern (string-upcase s)
                                                          :keyword))))
  "Converts the tree representation from JSOWN into something similar to
output from CL-JSON.  KEY-FN, if present, maps keyword strings to keywords."
  (labels ((%convert (jt)
             (typecase jt
               ((cons (eql :obj) t)
                (%convert-obj (cdr jt)))
               (cons
                (mapcar-improper-list #'%convert jt))
               (t jt)))
           (%convert-obj (key-alist)
             (iter (for (key . val) in key-alist)
                   (collect (cons (funcall key-fn key)
                                  (%convert val))))))
    (%convert jt)))

(defun strings-to-string-cases (strings)
  (iter (for n in strings)
        (collect (list n (intern (string-upcase n)
                                 :keyword)))))

(defun string-case-to-keyword-body (strings s)
  `(string-case (,s) ,@(strings-to-string-cases strings)
                (t (intern (string-upcase ,s) :keyword))))

(defmacro string-case-to-keywords (strings str)
  "Macro to convert a string to a keyword, using string-case to
accelerate the common cases given by STRINGS."
  (unless (and (listp strings)
               (every #'stringp strings))
    (error "Usage: (string-case-to-keywords <list of string constants> form)"))
  (let ((v (gensym "STR")))
    `(let ((,v ,str))
       (etypecase ,v
         (simple-base-string
          ,(string-case-to-keyword-body strings `(the simple-base-string ,v)))
         #-ccl
         ((and simple-string (vector character))
          ,(string-case-to-keyword-body
            strings `(the (and simple-string (vector character)) ,v)))
         (string (intern (string-upcase ,v) :keyword))))))

;;; Profiling

(defvar *profile-flame-graph* nil
  "Write report with `profile-to-flame-graph' from `with-prof'.")

(defmacro with-prof (profile-path &rest body)
  "Execute BODY with profiling enables.  Write the profile report
to a file at PROFILE-PATH.  Currently only works in SBCL."
  #+sbcl
  ;; Profiler settings
  `(if ,profile-path
       (sb-sprof:with-profiling (:sample-interval .01
                                                  :max-samples 10000000
                                                  :mode :cpu
                                                  :loop nil)
         ,@body
         (if sel/utility:*profile-flame-graph*
             (with-open-file (out ,profile-path
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :direction :output)
               (sel/utility:profile-to-flame-graph out))
             (with-output-to-file (out ,profile-path
                                       :if-exists :overwrite
                                       :if-does-not-exist :create)
               (sb-sprof:report :stream out))))
       (progn ,@body))
  #-sbcl `(progn
            (when ,profile-path
              (with-output-to-file (out ,profile-path)
                (format out "Not profiling.~%")))
            ,@body))
