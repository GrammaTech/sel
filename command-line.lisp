(defpackage :software-evolution-library/command-line
  (:nicknames :sel/command-line)
  (:documentation "Command line interface for three way merging of software")
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :command-line-arguments
        :split-sequence
        :software-evolution-library/utility)
  (:import-from :bordeaux-threads :all-threads :thread-name :join-thread)
  (:import-from :cl-ppcre :scan)
  (:import-from :swank :create-server)
  (:import-from :cl-json :decode-json-from-source)
  (:import-from :uiop/utility :nest)
  (:import-from :uiop/image :*lisp-interaction*)
  (:import-from :uiop/stream :detect-encoding)
  (:import-from :uiop/pathname
                :ensure-directory-pathname
                :pathname-directory-pathname
                :pathname-parent-directory-pathname)
  (:shadowing-import-from :uiop/filesystem
                          :file-exists-p
                          :directory-exists-p)
  (:shadowing-import-from :asdf-encodings :encoding-external-format)
  (:export :define-command
           ;; Functions to handle command line options and arguments.
           :read-compilation-database
           :handle-comma-delimited-argument
           :handle-set-interactive-argument
           :handle-swank-port-argument
           :handle-load
           :handle-eval
           :handle-out-dir-argument
           :handle-read-random-state-from-path-argument
           :handle-save-random-state-to-path-argument
           :handle-set-quiet-argument
           :handle-set-verbose-argument
           :handle-store-traces-argument
           :handle-load-traces-argument
           :resolve-file
           :resolve-out-dir-from-source
           :resolve-name-from-source
           :resolve-test-dir-from-source
           :resolve-store-path-from-out-dir-and-name
           :resolve-test-script-from-test-script
           :resolve-num-tests-from-num-tests
           :wait-on-manual
           :exit-command
           ;; Common sets of command-line-arguments options.
           :+common-command-line-options+
           :+interactive-command-line-options+
           :+clang-command-line-options+
           :+project-command-line-options+
           :+clang-project-command-line-options+))
(in-package :software-evolution-library/command-line)
(in-readtable :curry-compose-reader-macros)

(defun read-compilation-database (file)
  "Read a Clang compilation database from FILE.

* FILE holds a JSON compilation database as generated by the bear utility."
  (with-open-file (in file :direction :input)
    (remove-duplicates (decode-json-from-source in)
                       :test #'equalp :key {aget :file} :from-end t)))


;;;; Functions to handle command line options and arguments.

(defun handle-comma-delimited-argument (argument)
  (split-sequence #\, argument :remove-empty-subseqs t))

(defun handle-set-interactive-argument (interactivep)
  (setf *lisp-interaction* interactivep))

(defun handle-swank-port-argument (port)
  (create-server :port port :style :spawn :dont-close t))

(defun handle-load (path)
  (load path :external-format (encoding-external-format (detect-encoding path))))

(defun handle-eval (string)
  (eval (read-from-string string)))

(defun handle-out-dir-argument (path)
  (let ((out-dir (nest (pathname-directory)
                       (canonical-pathname)
                       (merge-pathnames
                        (ensure-directory-pathname path)
                        (truename ".")))))
    (assert (probe-file (make-pathname :directory (butlast out-dir)))
            (path)
            "Output directory ~a does not exist" path)
    out-dir))

(defun handle-read-random-state-from-path-argument (path)
  (setf *random-state*
        (with-open-file (in path :direction :input) (read in))))

(defun handle-save-random-state-to-path-argument (path)
  (with-open-file (out path :direction :output
                       :if-exists :supersede)
    (format out "~S" *random-state*)))

(defun handle-set-quiet-argument (arg)
  (declare (ignorable arg))
  (setf *note-level* 0))

(defun handle-set-verbose-argument (level)
  (when (>= level 4) (setf *shell-debug* t))
  (setf *note-level* level))

(defun handle-store-traces-argument (path)
  "Ensure PATH is a valid argument to store-traces."
  (let ((parent-parent-dir (pathname-parent-directory-pathname
                            (pathname-directory-pathname path))))
    (when (pathname-directory parent-parent-dir)
      (assert (directory-exists-p parent-parent-dir)
              (parent-parent-dir)
              "~a does not exist" parent-parent-dir)))
  path)

(defun handle-load-traces-argument (path)
  "Ensure PATH is a valid argument to load-traces."
  (assert (file-exists-p path)
          (path)
          "~a does not exist" path)
  path)

(defun resolve-file (file)
  "Ensure file is an actual file that exists on the filesystem."
  (if (probe-file file)
      file
      (format *error-output*
	      "~a: No such file or directory~%"
	      file)))

(defun resolve-out-dir-from-source (source)
  "Select a reasonable output directory based on SOURCE."
  (if-let ((as-dir (directory-p source)))
    ;; SOURCE is a directory, default out-dir to the parent directory
    ;; because we can't copy a project dir into a subdir of itself.
    (butlast (pathname-directory as-dir))
    (pathname-directory source)))

(defun resolve-name-from-source (source)
  "Select a reasonable name based on SOURCE."
  (if-let ((as-dir (directory-p source)))
    (lastcar (pathname-directory as-dir))
    (pathname-name source)))

(defun resolve-test-dir-from-source (source)
  "Select a reasonable test directory based on SOURCE."
  (or (directory-p source)
      (pathname-directory-pathname source)))

(defun resolve-store-path-from-out-dir-and-name
    (out-dir name &optional description (type "store"))
  "Build a reasonable store path based on OUT-DIR and NAME.
Optional DESCRIPTION is added to the path."
  (namestring
   (make-pathname :directory out-dir
                  :name (if description
                            (concatenate 'string name "-" description)
                            name)
                  :type type)))

(defun resolve-test-script-from-test-script (test-script test-dir
					     &aux result)
  "Ensure that TEST-SCRIPT exists and is within TEST-DIR.

* TEST-SCRIPT FIXME
* TEST-DIR FIXME
* RESULT FIXME
"
  (let ((test-dir-path (probe-file (canonical-pathname test-dir)))
        (test-script-path
         (probe-file ; <- Required for canonical path w.r.t. symlinks.
          (canonical-pathname
           (merge-pathnames-as-file (ensure-directory-pathname test-dir)
                                    (car (split-sequence #\Space
                                           test-script)))))))
    (assert (probe-file test-script-path)
            (test-script)
            "Test script ~S does not exist." test-script-path)
    (assert (search (pathname-directory test-dir-path)
                    (pathname-directory test-script-path)
                    :test #'equal)
            (test-script-path)
            "Test script must be in a subdirectory of ~S" test-dir-path)

    (setf result (format nil "~{~a~^ ~}"
                         (append (list test-script-path)
                                 (cdr (split-sequence #\Space
                                                      test-script)))))
    (setf result (if (scan "~a" result)
                     result
                     (format nil "~a ~~a" result)))
    (setf result (if (scan "~d" result)
                     result
                     (format nil "~a ~~d" result)))))

(defun resolve-num-tests-from-num-tests (num-tests)
  "FIXME

* NUM-TESTS FIXME
"
  (etypecase num-tests
    (string (setf num-tests (parse-integer num-tests)))
    (integer nil))
  (assert (and (numberp num-tests) (>= num-tests 0)) (num-tests)
          "Must supply the positive number of tests to run.")
  num-tests)

(defun wait-on-manual (manual)
  "Wait to terminate until the swank server returns if MANUAL is non-nil."
  (when manual
    (note 1 "Waiting on swank server...")
    (join-thread
     (car (remove-if-not [{string= "Swank Sentinel"} #'thread-name]
                         (all-threads))))))

(defmacro exit-command (command-name errno &optional interactive-return-val)
  "Exit COMMAND-NAME with ERRNO (command line) or INTERACTIVE-RETURN-VAL (REPL).
COMMAND-NAME should be the name of the enclosing function defined with
`define-command'.  Command-line or interactive state is determined by
inspecting the value of `*lisp-interaction*'."
  `(if uiop/image:*lisp-interaction*
       (return-from ,command-name ,interactive-return-val)
       (quit ,errno)))


;;;; Common sets of command-line-arguments options.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +common-command-line-options+
    '((("help" #\h #\?) :type boolean :optional t
       :documentation "display help output")
      (("quiet" #\q) :type boolean :optional t
       :action #'handle-set-quiet-argument
       :documentation "set verbosity level to 0")
      (("verbose" #\V) :type integer :optional t :initial-value 2
       :action #'handle-set-verbose-argument
       :documentation "verbosity level 0-4")
      (("load" #\l) :type string :optional t
       :action #'handle-load
       :documentation "load FILE as lisp code")
      (("eval" #\e) :type string :optional t
       :action #'handle-eval
       :documentation "eval STRING as lisp code")
      (("out-dir" #\o) :type string :optional t
       :action #'handle-out-dir-argument
       :documentation "write final population into DIR")
      (("read-seed") :type string :optional t
       :action #'handle-read-random-state-from-path-argument
       :documentation "load random seed from FILE")
      (("save-seed") :type string :optional t
       :action #'handle-save-random-state-to-path-argument
       :documentation "save random seed to FILE")))
  (defparameter +interactive-command-line-options+
    '((("interactive") :type boolean :optional t
       :action #'handle-set-interactive-argument
       :documentation "run interactively")
      (("manual") :type boolean :optional t
       :documentation "Don't automatically evolve")
      (("swank" #\s) :type integer :optional t
       :action #'handle-swank-port-argument
       :documentation "start a swank listener on PORT")))
  (defparameter +clang-command-line-options+
    '((("compiler" #\c) :type string :initial-value "clang"
       :documentation "use CC as the C compiler")
      (("flags" #\F) :type string :optional t
       :action #'handle-comma-delimited-argument
       :documentation "comma-separated list of compiler flags")))
  (defparameter +project-command-line-options+
    '((("build-command" #\b) :type string :optional t :initial-value "make"
       :documentation "shell command to build project directory")))
  (defparameter +clang-project-command-line-options+
    '((("compilation-database" #\D) :type string :optional t
       :action #'read-compilation-database
       :documentation "path to clang compilation database"))))
