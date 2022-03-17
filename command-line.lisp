;;;; command-line.lisp --- General functionality for SEL command-line tools
;;;
;;; Command line utility functions and helpers for SEL-based
;;; command-line tools.
;;;
;;; This package extends the
;;; @url{https://github.com/fare/command-line-arguments,
;;; command-line-arguments} package with numerous option definitions
;;; and helper functions for parsing command line arguments and
;;; options which are specific to SEL.  See the appendix for a full
;;; list of the available options.
;;;
;;; @texi{command-line}
(uiop:define-package :software-evolution-library/command-line
  (:nicknames :sel/command-line)
  (:documentation
   "Generally useful functionality for SEL-based command-line tools.")
  (:use :gt/full
        :command-line-arguments
        :cl-store
        :software-evolution-library
        :software-evolution-library/utility/debug
        :software-evolution-library/utility/git
        ;; Software objects.
        :software-evolution-library/software/project
        :software-evolution-library/software/git-project
        :software-evolution-library/software/compilable
        :software-evolution-library/software/parseable
        :software-evolution-library/software/lisp
        :software-evolution-library/software/simple
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/all-tree-sitter
        :software-evolution-library/software/javascript
        :software-evolution-library/software/typescript
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/c-project
        :software-evolution-library/software/cpp-project
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/java-project
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/python-project
        :software-evolution-library/software/lisp-project
        ;; Components.
        :software-evolution-library/components/test-suite)
  (:import-from :swank :create-server)
  (:import-from :software-evolution-library/software/clang :clang)
  (:import-from :cl-json :decode-json-from-source)
  (:shadowing-import-from :asdf-encodings
                          :detect-file-encoding
                          :encoding-external-format)
  ;; The idea here is to be able to use tree-sitter symbols as
  ;; language names without having to actually load every language.
  ;; With `define-package', `:import-from' interns symbols in the
  ;; source package if not already present.
  (:import-from :software-evolution-library/software/tree-sitter
                :css
                :c-sharp
                :go
                :html
                :java
                :json
                :julia
                :php
                :ruby
                :rust
                :scala
                :typescript)
  (:export :define-command
           :interrupt-signal
           :*lisp-interaction*
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
           :handle-trace-file-argument
           :handle-pop-size-argument
           :handle-mut-rate-argument
           :handle-tournament-size-argument
           :resolve-file
           :resolve-out-dir-from-source
           :resolve-name-from-source
           :resolve-test-dir-from-source
           :resolve-store-path-from-out-dir-and-name
           :resolve-test-script-from-test-script
           :resolve-num-tests-from-num-tests
           :resolve-language-from-language-and-source
           :wait-on-manual
           :exit-command
           :guess-language
           :create-software
           :create-test
           :create-test-suite
           ;; Common sets of command-line-arguments options.
           :+common-command-line-options+
           :+interactive-command-line-options+
           :+git-project-command-line-options+
           :+clang-command-line-options+
           :+project-command-line-options+
           :+clang-project-command-line-options+
           :+evolutionary-command-line-options+
           ;; Projects including git urls.
           :clang-git-project
           :javascript-git-project
           :typescript-git-project
           :lisp-git-project))
(in-package :software-evolution-library/command-line)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar interrupt-signal
    #+sbcl 'sb-sys:interactive-interrupt
    #+ccl 'ccl:interrupt-signal-condition
    #+clisp 'system::simple-interrupt-condition
    #+ecl 'ext:interactive-interrupt
    #+allegro 'excl:interrupt-signal
    "Implementation specific signal thrown by a user's interrupt, C-c."))

(defun read-compilation-database (file)
  "Read a Clang compilation database from FILE.

* FILE holds a JSON compilation database as generated by the bear utility."
  (with-open-file (in file :direction :input)
    (remove-duplicates (decode-json-from-source in)
                       :test #'equalp :key {aget :file} :from-end t)))

(defclass clang-git-project (clang-project git-project) ())
(defclass javascript-git-project (javascript-project git-project) ())
(defclass typescript-git-project (typescript-project git-project) ())
(defclass java-git-project (javascript-project git-project) ())
(defclass lisp-git-project (lisp-project git-project) ())


;;;; Language-related utility

(-> alias-language (string-designator &rest t &key &allow-other-keys)
  (values symbol &optional))
(defun alias-language (alias &rest rest &key &allow-other-keys)
  (let ((language (apply #'language-alias->language-symbol alias rest)))
    (cond
      ((string= 'c language)
       (find-c))
      ((string= 'cpp language)
       (find-cpp))
      (t language))))

(-> pathname-language ((or string pathname)) symbol)
(defun pathname-language (p &aux (p (pathname p)))
  (alias-language (pathname-type p) :pathname p))


;;;; Functions to handle command line options and arguments.

(defun handle-comma-delimited-argument (argument)
  (split-sequence #\, argument :remove-empty-subseqs t))

(defun handle-set-interactive-argument (interactivep)
  (setf *lisp-interaction* interactivep))

(defun handle-swank-port-argument (port)
  (create-server :port port :style :spawn :dont-close t))

(defun handle-load (path)
  (nest (load path :external-format)
        (encoding-external-format)
        (detect-file-encoding path)))

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

(defun handle-trace-file-argument (path)
  "Ensure PATH is a valid argument to store (or potentially load) traces."
  (let ((parent-parent-dir (pathname-parent-directory-pathname
                            (pathname-directory-pathname path))))
    (when (pathname-directory parent-parent-dir)
      (assert (directory-exists-p parent-parent-dir)
              (parent-parent-dir)
              "~a does not exist" parent-parent-dir)))
  path)

(defun handle-pop-size-argument (pop-size)
  ;; Command-line argument handling ensures POP-SIZE is an Int.
  (setf *max-population-size* (the integer pop-size))
  (assert (> *max-population-size* 0) (*max-population-size*)
          "Must supply a positive population size"))

(defun handle-cross-chance-argument (cross-chance)
  ;; Command-line argument handling ensures CROSS-CHANCE is an int.
  (setf *cross-chance* (parse-number cross-chance))
  (assert (and (>= *cross-chance* 0) (<= *cross-chance* 1)) (*cross-chance*)
          "Crossover chance must be between 0 and 1"))

(defun handle-mut-rate-argument (mut-rate)
  ;; Command-line argument handling ensures MUT-RATE is an int.
  (setf *mut-rate* (parse-number mut-rate))
  (assert (> *mut-rate* 0) (*mut-rate*)
          "Must supply a positive mutation rate"))

(defun handle-tournament-size-argument (tournament-size)
  (setf *tournament-size* tournament-size)
  (assert (> *tournament-size* 1) (*tournament-size*)
          "Tournament size must be >1"))

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

(defun resolve-test-script-from-test-script
    (test-script test-dir &aux result)
  "Ensure that TEST-SCRIPT exists, add ~~a and ~~d arguments if missing."
  (let* ((test-dir (cond
                     ((null test-dir) ; Handle TEST-DIR as NIL, meaning '.'.
                      (make-pathname :directory '(:RELATIVE ".")))
                     ((listp test-dir) ; Handle raw directory TEST-DIR.
                      (make-pathname :directory test-dir))
                     (t test-dir)))
         (test-dir-path (canonical-pathname test-dir))
         (test-script-path
          (canonical-pathname
           (merge-pathnames-as-file (ensure-directory-pathname test-dir)
                                    (car (split-sequence #\Space
                                                         test-script))))))
    (assert (probe-file test-script-path)
            (test-script)
            "Test script ~S does not exist." test-script-path)
    ;; Required for canonical path w.r.t. symlinks.
    (setf test-dir-path (probe-file test-dir-path))
    (setf test-script-path (probe-file test-script-path))

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
  "Resolves the NUM-TESTS command line argument to a positive number.
NUM-TESTS should be either a string or integer. Raise an error if the
input is not positive."
  (etypecase num-tests
    (string (setf num-tests (parse-integer num-tests)))
    (integer nil))
  (assert (and (numberp num-tests) (>= num-tests 0)) (num-tests)
          "Must supply the positive number of tests to run.")
  num-tests)

(-> resolve-language-from-language-and-source
    (string &optional (or null string pathname))
    symbol)
(defun resolve-language-from-language-and-source (language &optional source)
  (let ((class (alias-language language)))
    (cond
      ((and source (git-url-p source))
       (intern (concatenate 'string (symbol-name class) "-GIT-PROJECT")
               :sel/command-line))
      ((and source (directory-p source))
       (intern (concatenate 'string (symbol-name class) "-PROJECT")
               :sel/command-line))
      (t class))))

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
  `(if *lisp-interaction*
       (return-from ,command-name ,interactive-return-val)
       (quit ,errno)))

(defgeneric language-to-project (language)
  (:documentation "The name of the project class associated
with a language.")
  (:method ((language symbol))
    (if (ends-with-subseq "-PROJECT" (symbol-name language))
        language
        (intern (concatenate 'string (symbol-name language) "-PROJECT")
                (find-package :sel/command-line)))))

(defun first-external-symbol-in-package (name-list package-list)
  "Look for name (string) as an external symbol in each of the packages
 in package-list (list of package-designators).
 i.e. look for first name in first package, 2nd name in second package,
 etc.
 Return the first symbol found, or NIL if none found."
  (iter (for name in name-list)
        (for package in package-list)
        (when-let* ((p (find-package package))
                    (sym (find-external-symbol name p)))
          (return sym))))

(defun find-c ()
  "Return the preferred C software object.
 If tree-sitter C class is loaded, return 'sel/sw/tree-sitter:c.
 If CLANG class is loaded, return 'sel/sw/clang:clang.
 Else error."
  (let ((sym (first-external-symbol-in-package
              '("C" "CLANG")
              '(:sel/sw/tree-sitter :sel/sw/clang))))
    (or sym (error "No available representation for C ASTs."))))

(defun find-cpp ()
  "Return the preferred C++ software object.
 If tree-sitter CPP class is loaded, return 'sel/sw/tree-sitter:cpp.
 If CLANG class is loaded, return 'sel/sw/clang/clang.
 Else error."
  (let ((sym (first-external-symbol-in-package
              '("CPP" "CLANG")
              '(:sel/sw/tree-sitter :sel/sw/clang))))
    (or sym (error "No available representation for C++ ASTs."))))

(-> guess-language (&rest (or string pathname)) symbol)
(defun guess-language (&rest sources)
  "Guess the SEL software object class that best matches SOURCES.
SOURCES should be a collection of paths.  The result is determined
based on heuristics based on whether SOURCES points to files or
directories and if files based on their extensions."
  (labels
      ((best-guess (guesses)
         (let ((frequencies (frequencies guesses)))
           ;; Inside of a project we remove all JSON and SIMPLE software
           ;; objects assuming that they may exist in any type of project.
           ;;
           ;; NOTE: We will have to add to this list of incidental
           ;; software types that don't determine the project type.
           (mapc (op (remhash _ frequencies))
                 '(nil json simple bash))
           ;; If a project contains both Javascript and TypeScript,
           ;; it's a TypeScript project.
           (when-let* ((js-count (gethash 'javascript frequencies))
                       (ts-count (gethash 'typescript frequencies)))
             (incf (gethash 'typescript frequencies) js-count)
             (remhash 'javascript frequencies))
           (when-let* ((js-count (gethash 'javascript-project frequencies))
                       (ts-count (gethash 'typescript-project frequencies)))
             (incf (gethash 'typescript-project frequencies) js-count)
             (remhash 'javascript-project frequencies))
           (car (extremum (hash-table-alist frequencies)
                          #'> :key #'cdr))))
       (guess-helper (sources project-p)
         (let ((guesses
                (mapcar (lambda (source)
                          (if (directory-p source)
                              (unless (intersection
                                       ;; If we descended into
                                       ;; node_modules we might end up
                                       ;; labelling a JS project as
                                       ;; TypeScript or v.v.
                                       '(".git" "node_modules")
                                       (pathname-directory source)
                                       :test #'equal)
                                (when-let ((guess (guess-helper
                                                   (list-directory source)
                                                   t)))
                                  (language-to-project guess)))
                              (pathname-language source)))
                        sources)))
           (cond
             (project-p
              (best-guess guesses))
             ((= 1 (length sources))
              ;; For a single file either return the guess, or return
              ;; SIMPLE if no language matched.
              (or (car guesses) 'simple))
             ((= 1 (length (remove-duplicates guesses)))
              ;; Multiple non-project files must all be equal to return a guess.
              (or (car guesses) 'simple))))))
    (guess-helper sources nil)))

(defun create-software (path &rest rest
                        &key ; NOTE: Maintain list of keyword arguments below.
                          (language (guess-language path) language-p)
                          compiler flags build-command artifacts
                          compilation-database store-path
                          git-sub-path git-ssh-key
                          &allow-other-keys)
  "Build a software object from a common superset of language-specific options.

Keyword arguments are as follows:
  LANGUAGE ------------- (optional) language of input file/directory
                         (default to result of `guess-language' on PATH)
                         String language is assumed to be a language name to be
                         resolved w/`resolve-language-from-language-and-source'.
                         Symbol language is assumed to be a class in sel/sw
  STORE-PATH ----------- path to the cached store file with the software
  COMPILER ------------- compiler for software object
  BUILD-COMMAND -------- shell command to build project directory
  ARTIFACTS ------------ build-command products
  COMPILATION-DATABASE - clang compilation database
Other keyword arguments are allowed and are passed through to `make-instance'."
  ;; Should any of the input parameters be set in the restored objects?
  (when (and store-path (probe-file store-path))
    (return-from create-software (restore store-path)))
  ;; If a SW named ends in ".store", the file will
  ;; be considered a store file.  Restore it instead
  ;; of creating a software object from source.
  (when (equal (pathname-type (pathname path)) "store")
    (return-from create-software (restore path)))
  ;; When `path` is a git repository, generate a new temp dir,
  ;; check out the repo, and set relevant variables
  (let* ((repo (when (git-url-p path)
                 (let ((remote path))
                   (prog1 remote
                     (make-git (setf path (temp-file-name))
                               :remote remote
                               :ssh-key git-ssh-key)
                     ;; Update path if working in sub-directory of repository.
                     (when git-sub-path
                       (setf path (make-pathname :directory path
                                                 :name git-sub-path)))
                     ;; Reset guessed language, now that repo is cloned.
                     (unless language-p
                       (setf language-p t ; Treat as explicitly set.
                             language
                             (ecase (guess-language path)
                               (clang-project 'clang-git-project)
                               (javascript-project 'javascript-git-project)
                               (typescript-project 'typescript-git-project)
                               (lisp-project 'lisp-git-project)
                               (simple 'simple))))))))
         (obj (from-file
               (nest
                ;; These options are interdependent.  Resolve any
                ;; dependencies and drop options which don't exist for
                ;; LANGUAGE in this `let*'.
                (let* ((language (cond
                                   ((and language-p (symbolp language))
                                    language)
                                   ((and language-p (stringp language))
                                    (resolve-language-from-language-and-source
                                     language path))
                                   (t language)))
                       (flags
                        (when (subtypep language 'compilable) flags))
                       (compiler
                        (when (subtypep language 'compilable) compiler))
                       (compilation-database
                        (when (eql language 'clang-project)
                          compilation-database))
                       (build-command
                        (when (subtypep language 'project)
                          (let* ((build-command-list
                                  (split-sequence #\Space build-command))
                                 (abs-cmd-name (nest
                                                (ignore-errors)
                                                (merge-pathnames-as-file path)
                                                (canonical-pathname
                                                 (car build-command-list)))))
                            ;; Remove any absolute path from the beginning of
                            ;; build-command *if* build-command is a file in the
                            ;; base of the project.
                            (if (file-exists-p abs-cmd-name)
                                (format nil "~a~{ ~a~}"
                                        (replace-all (namestring abs-cmd-name)
                                                     (namestring path)
                                                     "./")
                                        (cdr build-command-list))
                                build-command))))
                       (artifacts
                        (when (subtypep language 'project) artifacts))))
                (apply #'make-instance language)
                (apply
                 #'append
                 (plist-drop-if ; Other keyword arguments are passed through.
                  {member _ (list :language :compiler :flags :build-command
                                  :artifacts :git-repo :compilation-database
                                  :store-path)}
                  (copy-seq rest)))
                (remove-if-not #'second)
                `((:allow-other-keys t)
                  (:compiler ,compiler)
                  (:flags ,flags)
                  (:build-command ,build-command)
                  (:artifacts ,artifacts)
                  (:git-repo ,repo)
                  (:compilation-database ,compilation-database)))
               path)))
    obj))

(defgeneric create-test (script &rest other-keys &key &allow-other-keys)
  (:documentation "Return a test case of SCRIPT.")
  (:method ((script pathname) &rest other-keys &key &allow-other-keys)
    (apply 'make-instance 'test-case
           :program-name (namestring script)
           other-keys))
  (:method ((script string) &rest other-keys &key &allow-other-keys)
    (apply 'create-test (split-sequence #\Space script) other-keys))
  (:method ((script list) &rest other-keys &key &allow-other-keys)
    (apply
      'make-instance
      'test-case
      :program-name (car script)
      :program-args (mapcar (lambda (x) (if (string= x "~a") :bin x))
                            (cdr script))
      other-keys)))

(defgeneric create-test-suite (script num-tests
                               &rest other-keys &key &allow-other-keys)
  (:documentation "Return a test suite of SCRIPT and NUM-TESTS.
Replaces ~~a with the binary name and ~~d with NUM-TESTS if they occur
in SCRIPT.")
  (:method ((script pathname) (num-tests t)
            &rest other-keys &key &allow-other-keys)
    (apply 'create-test-suite (namestring script) num-tests other-keys))
  (:method ((script string) (num-tests t)
            &rest other-keys &key &allow-other-keys)
    (let ((cmd (split-sequence #\space script)))
      (flet ((replace-num (num)
               (cons (car cmd)
                     (mapcar (lambda (x)
                               (if (string= x "~d") (write-to-string num) x))
                             (cdr cmd)))))
        (make-instance 'test-suite
               :test-cases
               (mapcar (lambda (num)
                         (apply 'create-test
                                (replace-num num)
                                other-keys))
                       (iota num-tests)))))))


;;;; Common sets of command-line-arguments options.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +common-command-line-options+
    '((("help" #\h #\?) :type boolean :optional t
       :documentation "display help output")
      (("quiet" #\q) :type boolean :optional t
       :action #'handle-set-quiet-argument
       :documentation "set verbosity level to 0")
      (("verbose" #\V) :type integer :initial-value 2
       :action #'handle-set-verbose-argument
       :documentation "verbosity level 0-4")
      (("load" #\l) :type string
       :action #'handle-load
       :documentation "load FILE as lisp code")
      (("eval" #\e) :type string
       :action #'handle-eval
       :documentation "eval STRING as lisp code")
      (("out-dir" #\o) :type string
       :action #'handle-out-dir-argument
       :documentation "write final population into DIR")
      (("read-seed") :type string
       :action #'handle-read-random-state-from-path-argument
       :documentation "load random seed from FILE")
      (("save-seed") :type string
       :action #'handle-save-random-state-to-path-argument
       :documentation "save random seed to FILE")
      (("language" #\L) :type string
       :documentation
       "language of input files (e.g. c, c++, lisp, or javascript)")))
  (defparameter +interactive-command-line-options+
    '((("interactive") :type boolean :optional t
       :action #'handle-set-interactive-argument
       :documentation "run interactively")
      (("manual") :type boolean :optional t
       :documentation "Don't automatically evolve")
      (("swank" #\s) :type integer
       :action #'handle-swank-port-argument
       :documentation "start a swank listener on PORT")))
  (defparameter +git-project-command-line-options+
    '((("git-sub-path" #\p) :type string :initial-value nil
       :documentation "sub path to software, when using a git repo")
      (("git-ssh-key" #\k) :type string :initial-value nil
       :documentation "path to ssh key used for pushing a git repo")))
  (defparameter +clang-command-line-options+
    '((("compiler" #\c) :type string :initial-value "clang"
       :documentation "use CC as the C compiler")
      (("flags" #\F) :type string
       :action #'handle-comma-delimited-argument
       :documentation "comma-separated list of compiler flags")))
  (defparameter +project-command-line-options+
    '((("build-command" #\b) :type string :initial-value "make"
       :documentation "shell command to build project directory")))
  (defparameter +clang-project-command-line-options+
    '((("artifacts" #\a) :type string
       :action #'handle-comma-delimited-argument
       :documentation "build products")
      (("compilation-database" #\D) :type string
       :action #'read-compilation-database
       :documentation "path to clang compilation database")))
  (defparameter +evolutionary-command-line-options+
    '((("pop-size") :type integer :initial-value #.(expt 2 8)
       :action #'handle-pop-size-argument
       :documentation "size of evolution population")
      (("cross-chance") :type string :initial-value "2/3"
       :action #'handle-cross-chance-argument
       :documentation "fraction of new individuals crossed over")
      (("mut-rate") :type string :initial-value "1"
       :action #'handle-mut-rate-argument
       :documentation "mutations per evolutionary loop iteration")
      (("tournament-size") :type integer :initial-value 2
       :action #'handle-tournament-size-argument
       :documentation "mutations per evolutionary loop iteration")
      (("max-evals") :type integer
       :documentation "maximum number of evaluations to run in evolution")
      (("max-time") :type integer
       :documentation "maximum number of seconds to run evolution")))
  (defparameter +tree-sitter-command-line-options+
    '((("tree-sitter-language-dir") :type string
       :documentation "the directory that contains tree-sitter json files"))))
