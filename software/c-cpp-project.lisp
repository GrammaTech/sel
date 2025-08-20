(defpackage :software-evolution-library/software/c-cpp-project
  (:nicknames :sel/software/c-cpp-project :sel/sw/c-cpp-project)
  (:use :gt/full
        :functional-trees/attrs
        :software-evolution-library
        :software-evolution-library/components/compilation-database
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c-cpp
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/parseable
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/compilation-database-project
        :software-evolution-library/software/project
        :software-evolution-library/software/compilable
        :software-evolution-library/software/directory
        :software-evolution-library/components/file)
  (:import-from :software-evolution-library/software/tree-sitter
                :morally-noexcept-parent?)
  (:local-nicknames
   (:attrs :functional-trees/attrs)
   (:debug :software-evolution-library/utility/debug)
   (:dir :software-evolution-library/software/directory)
   (:file :software-evolution-library/components/file)
   (:task :software-evolution-library/utility/task)
   (:ts :software-evolution-library/software/tree-sitter))
  (:shadowing-import-from :serapeum :~>>)
  (:export :c/cpp-project
           :get-standard-path-header
           :header-name
           :system-headers
           :implicit-headers
           :c/cpp-root
           :include-not-found-warning
           :include-conflict-error
           :include-conflict-error.ast
           :include-conflict-error.candidates
           :get-implicit-header
           :command-implicit-header
           :project-dependency-tree
           :who-includes?
           :file-dependency-tree
           :find-include
           :*header-extensions*
           :*cpp-module-extensions*
           :*cpp-implementation-extensions*
           :*cpp-extensions*
           :evolve-files/dependency-order
           :included-headers
           :including-files
           :find-enclosing-software
           :*dependency-stack*
           :update-dependency-graph
           :find-symbol-table-from-include
           :skip-include
           :c/cpp-system-header))

(in-package :software-evolution-library/software/c-cpp-project)
(in-readtable :curry-compose-reader-macros)

(defvar *header-dirs* nil
  "The header search path of the current compilation unit.")

(defparameter *header-extensions*
  '("h" "hpp" "hh")
  "Header file extensions.")

(defparameter *stdinc-dirs*
  '("/usr/local/include/" "/usr/include/"))

(defparameter *cpp-module-extensions*
  '(;; Module unit extensions. Visual Studio uses .ixx, Clang
    ;; uses the extensions ending with -m.
    "ixx" "cppm" "ccm" "cxxm" "c++m"))

(defparameter *cpp-implementation-extensions*
  '("cpp" "cp" "cc" "cxx"))

(defparameter *cpp-extensions*
  (append *header-extensions*
          *cpp-implementation-extensions*
          *cpp-module-extensions*)
  "List of extensions we will consider for evolving.")

(defvar *dependency-stack* nil
  "Stack of include file names currently being processed during type
inference.  Used to prevent circular attr propagation.")

(defgeneric include-path (header)
  (:documentation "Extract the path from HEADER.")
  (:method ((file string))
    (pathname file))
  (:method ((path pathname))
    path)
  (:method ((path symbol))
    "\"Path\" for a standard header."
    (if (keywordp path)
        (pathname (string path))
        (call-next-method)))
  (:method ((file file-ast))
    (full-pathname file))
  (:method ((sw software))
    (original-path sw)))

(defun header-file? (file)
  (member (pathname-type (include-path file))
          *header-extensions*
          :test #'equal))

(defun module-file? (file)
  (member (pathname-type (include-path file))
          *cpp-module-extensions*
          :test #'equal))

(defclass c/cpp-project
    (directory-project compilation-database-project parseable-project compilable
     include-paths-mixin normal-scope)
  ()
  (:documentation "Mixin for common project functionality between C and C++."))

(define-node-class c/cpp-root (functional-tree-ast normal-scope-ast)
  ((system-headers
    :reader system-headers
    :initarg :system-headers)
   (system-headers/string->ast
    :accessor system-headers/string->ast
    :initarg :system-headers/string->ast)
   (implicit-headers
    :accessor implicit-headers
    :initarg :implicit-headers)
   (implicit-headers-table
    :reader implicit-headers-table
    :initarg :implicit-headers-table
    :type hash-table)
   (included-headers
    :documentation
    "Table from file paths to headers they directly include."
    :reader included-headers
    :initarg :included-headers
    :initform (dict)
    :type hash-table)
   (including-files
    :documentation
    "Table from headers to files that include them."
    :reader including-files
    :initarg :including-files
    :initform (dict)
    :type hash-table)
   (project-directory
    :accessor project-directory
    :initarg :project-directory
    :initform nil)
   (child-slots
    :initform '((project-directory . 1)
                (system-headers . 0)
                (implicit-headers . 0))
    :allocation :class))
  (:default-initargs
   :system-headers nil
   :implicit-headers nil
   :implicit-headers-table (dict)
   :system-headers/string->ast (dict))
  (:documentation "Node for c/cpp-project objects that allows for storing the
system-headers directly in the tree. Note that system headers are lazily added
by the symbol-table attribute."))

(define-node-class synthetic-header (functional-tree-ast root-ast)
  ((children :initarg :children
             :accessor children
             :initform nil)
   (child-slots :initform '((children . 0))
                :allocation :class))
  (:documentation "Superclass of synthetic headers, such as substitute system
headers and implicit headers for command-line preprocessor macros."))

(defmethod attrs:subroot? ((ast synthetic-header))
  nil)

(defmethod get-command-objects ((project c/cpp-project) (file synthetic-header))
  nil)

(defun trim-path-string (path-ast &aux (text (text path-ast)))
  "Return the text of PATH-AST with the quotes around it removed."
  ;; Spaces are included here for path ASTs that are preprocessor
  ;; macro expansions.
  (string-trim " <>\"" text))

(defun file-header-dirs (project ast &key (file (find-enclosing 'file-ast project ast)))
  "Get a header search path for FILE from PROJECT's compilation database."
  (when (and project file)
    (when-let ((co (command-object project file)))
      (command-header-dirs co))))

(define-node-class implicit-header (synthetic-header
                                    ;; Inherit symbol-table-union
                                    ;; specialization.
                                    c-like-syntax-ast)
  ((source-file
    :initarg :source-file
    :reader source-file))
  (:documentation "A fake header containing definitions for preprocessor macros
supplied at the command line."))

(defun clear-implicit-headers (project)
  "Clear cached implicit headers in PROJECT's genome.
For development."
  (synchronized (project)
    (setf (genome project)
          (copy (genome project)
                :implicit-headers nil
                :implicit-headers-table (dict))))
  (values))

(defmethod project-dependency-tree
    ((project c/cpp-project)
     &key allow-headers entry-points)
  (declare ((soft-list-of string) entry-points))
  (let* ((genome (genome project))
         (included-headers (included-headers genome))
         (files (mapcar #'car (evolve-files project)))
         (intern-table (dict)))
    ;; Populate the symbol table, recording header dependencies as a
    ;; side effect.
    (symbol-table project)
    (labels ((intern-leaf (leaf)
               "Intern duplicated subtrees (common with modules)."
               (ensure-gethash leaf intern-table leaf))
             (rec (path seen)
               (if (contains? seen path)
                   (list :circle path)
                   (let ((seen (with seen path)))
                     (intern-leaf
                      (cons path
                            (mapcar (op (rec _ seen))
                                    (gethash path included-headers))))))))
      (mapcar (op (rec _ (empty-set)))
              (or entry-points
                  (if allow-headers
                      files
                      (remove-if #'header-file? files)))))))

(defgeneric file-dependency-tree (project file)
  (:documentation "Return the tree of includes rooted at FILE in PROJECT.")
  (:method ((project t) (file software))
    (file-dependency-tree project
                       (pathname-relativize (project-dir project)
                                            (original-path file))))
  (:method ((project t) (file file-ast))
    (file-dependency-tree project
                          (namestring (full-pathname file))))
  (:method ((project t) (file ast))
    (if-let (enclosing (find-enclosing 'file-ast project file))
      (file-dependency-tree project enclosing)
      (error "No enclosing file for ~a in ~a" file project)))
  (:method ((project t) (file string))
    (aget file
          (project-dependency-tree project)
          :test #'equal)))

(defun who-includes? (project header)
  (let* ((genome (genome project))
         (including-files (including-files genome))
         (path (include-path header))
         (path (pathname-relativize (project-dir project) path)))
    (symbol-table project)
    (labels ((who-includes? (path)
               (let ((includes (href including-files path)))
                 (cons path
                       (mappend #'who-includes? includes)))))
      (cdr (who-includes? path)))))

(defun include-tree-constraints (include-tree)
  (labels ((generate-constraints (constraint-tree)
             (destructuring-bind (dependent &rest dependencies) constraint-tree
               (mapcar (op (list (car _) dependent))
                       (remove-if-not (op (stringp (car _))) dependencies)))))
    (mappend #'generate-constraints include-tree)))

(defun include-tree-dependency-order (include-tree)
  (let ((sort-function (toposort (include-tree-constraints include-tree)
                                 :test #'equal))
        (items (remove-duplicates (flatten include-tree) :test #'equal)))
    (remove-if-not (of-type 'string)
                   (stable-sort items sort-function))))

(defun include-tree-compilation-database-order
    (project &key (include-tree (project-dependency-tree project :allow-headers t))
               (comp-db (compilation-database project)))
  (labels ((relativized-path (project command-object)
             "Relative the path of COMMAND-OBJECT to PROJECT's path."
             (with-accessors ((directory command-directory)
                              (file command-file))
                 command-object
               (namestring
                (enough-pathname
                 (canonical-pathname (fmt "~a/~a" directory file))
                 (project-dir project)))))
           (grab-paths (include-tree &aux paths)
             "Grab all paths in INCLUDE-TREE."
             (assert include-tree)
             (walk-tree (lambda (child)
                          (and-let* (((consp child))
                                     (item (car child))
                                     ((stringp item)))
                            (push item paths)))
                        include-tree)
             (reverse paths)))
    (iter
      (iter:with include-table = (alist-hash-table include-tree :test #'equal))
      (for entry in (command-objects comp-db))
      (for filename = (relativized-path project entry))
      (for include-tree = (gethash filename include-table))
      (appending (cons filename (and include-tree (grab-paths include-tree)))
                 into result at beginning)
      (finally (return (reverse result))))))

(defun evolve-files/dependency-order
    (project &key (include-tree
                   (project-dependency-tree project :allow-headers t))
               (skip-unparsed t))
  "Return the evolve-files of PROJECT sorted in dependency order.
Skip files that have not been parsed."
  (labels ((unused-files (table &key (used-flag :visited))
             (iter
               (for (key value) in-hashtable table)
               (unless (eql value used-flag)
                 (collect (cons key value))))))
    (let ((dependency-order
            (if-let ((comp-db (compilation-database project)))
              (include-tree-compilation-database-order
               project :include-tree include-tree :comp-db comp-db)
              (include-tree-dependency-order include-tree))))
      (symbol-macrolet ((target-hash (gethash path evolve-files nil)))
        (iter
          (iter:with evolve-files = (alist-hash-table (evolve-files project)
                                                      :test #'equal))
          (for path in dependency-order)
          (for target = target-hash)
          (cond
            ;; NOTE: it is likely that INCLUDE-TREE has cycles, so the value is
            ;;       marked as :visited to prevent adding the same file more than
            ;;       once.
            ((eql target :visited))
            (target
             (setf target-hash :visited)
             (collect (cons path target) into ordered-files at beginning))
            (t
             (error "~a does not exist in evolve-files of ~a."
                    path project)))
          (finally
           (return
             (let ((ordered-files
                     ;; NOTE: add header files which haven't been
                     ;;       reached to the end. Anything added here
                     ;;       will not have any guarantees on whether
                     ;;       it's in the correct order.
                     (reverse
                      (append (unused-files evolve-files)
                              ordered-files))))
               (if skip-unparsed
                   (filter (op (typep (slot-value (cdr _) 'genome) 'ast))
                           ordered-files)
                   ordered-files)))))))))


#+(or :TREE-SITTER-C :TREE-SITTER-CPP)
(progn


;;; Symbol Table

  (defmethod symbol-table ((project c/cpp-project) &optional in)
    "Force computing symbol tables in compilation units.
We delay header files to the end. At this point, their symbol tables
should already have been computed as part of their compilation units."
    (mvlet* ((files
              (collect-if (of-type 'file-ast)
                          (genome project)))
             (headers non-headers
              (partition (lambda (file)
                           (let ((path (full-pathname file)))
                             (member (pathname-type path)
                                     *header-extensions*
                                     :test #'equal)))
                         files)))
      (dolist (file non-headers)
        (symbol-table file in))
      (dolist (file headers)
        (symbol-table file in))
      ;; Fill in symbol tables for directories.
      (call-next-method)))


;;; Implicit Headers

(defgeneric command-implicit-header (command lang)
  (:documentation "Synthesize an implicit header for COMMAND.
Implicit headers give us a place to inject code \"as if\" it had been
included in a header.")
  (:method ((co command-object) lang)
    (when-let (alist (command-preproc-defs co))
      (let* ((alist
              (if (eql lang 'cpp)
                  (cons '("__cplusplus" . "1") alist)
                  alist))
             (source
              (mapconcat (lambda (cons)
                           (preproc-macro-source (car cons) (cdr cons)))
                         (reverse alist)
                         ""))
             (children
              (collect-if
               (of-type '(or c/cpp-preproc-def c/cpp-preproc-function-def))
               (from-string lang source)))
             (root-class
              (ecase lang
                (c 'c-translation-unit)
                (cpp 'cpp-translation-unit))))
        (make 'implicit-header
              :source-file (command-file co)
              :children
              (list
               (make root-class :children children)))))))

(defgeneric get-implicit-header (project file)
  (:method ((project c/cpp-project) (file file-ast))
    (get-implicit-header project (full-pathname file)))
  (:method ((project c/cpp-project) (file software))
    (get-implicit-header project (original-path file)))
  (:method ((project c/cpp-project) (file string))
    (get-implicit-header project (pathname file)))
  (:method ((project c/cpp-project) (file pathname))
    (assert (relative-pathname-p file))
    (when-let ((co (command-object project file)))
      (let* ((genome (genome project))
             (table (implicit-headers-table genome))
             (lang (component-class project)))
        (synchronized (project)
          (ensure2 (gethash file table)
            (lret ((header (command-implicit-header co lang)))
              (when header
                (setf (genome project)
                      (copy (genome project)
                            :implicit-headers
                            (adjoin header (implicit-headers genome))))))))))))

(defun implicit-header-symbol-table (header)
  (with-attr-session (header :inherit nil)
    (symbol-table (first (children header)))))

(defmethod lookup ((obj c/cpp-root) (key string))
  ;; Enables the use of the `@' macro directly against projects.
  (lookup (project-directory obj) key))

(defmethod lookup :around ((obj c/cpp-project) (key string))
  "Compute the symbol table if we can't load a header."
  (or (call-next-method)
      (and (header-file? key)
           (unless (boundp 'attrs:*attrs*)
             (attrs:with-attr-table obj
               (debug:note :trace "Loading symbol table to find ~a" key)
               (symbol-table obj)))
           (call-next-method))))

(defmethod symbol-table :context ((ast c/cpp-translation-unit) &optional in)
  (let ((root (attrs-root*)))
    (or (and-let* (((typep root 'c/cpp-project))
                   ((compilation-database root))
                   (file (find-enclosing 'file-ast root ast))
                   (implicit-header (get-implicit-header root file)))
          (let ((augmented-table
                 (symbol-table-union ast
                                      in
                                      (implicit-header-symbol-table implicit-header))))
            (call-next-method ast augmented-table)))
        (call-next-method))))


;;; System Headers

(defvar *system-header-cache* (make-hash-table :size 0)
  "Store system headers that have already been parsed.
The cache has two levels, to prevent commingling headers for different
languages. There is a top-level table for the type of the
project (e.g. C vs. C++) and another table from the name to
the header.")

(define-node-class named-synthetic-header (synthetic-header)
  ((header-name :initarg :header-name
                :type string
                :accessor header-name))
  (:documentation "Node for representing named synthetic headers."))

(define-node-class c/cpp-system-header (named-synthetic-header)
  ()
  (:documentation "Node for representing synthetic system headers."))

(defparameter *morally-noexcept-headers*
  ;; See https://en.cppreference.com/w/cpp/header
  (let ((c-compatibility-headers
         '("assert"
           "ctype"
           "errno"
           "fenv"
           "float"
           "inttypes"
           "limits"
           "locale"
           "math"
           "setjmp"
           "signal"
           "stdarg"
           "stddef"
           "stdint"
           "stdio"
           "stdlib"
           "string"
           "time"
           "uchar"
           "wchar"
           "wctype"))
        (special-c-compatibility-headers
         '("stdatomic.h"))
        (empty-c-headers
         '("complex" "tgmath"))
        (meaningless-c-headers
         '("iso646" "stdalign" "stdbool"))
        (unsupported-c-headers
         '("stdnoreturn.h" "threads.h")))
    (set-hash-table
     (append unsupported-c-headers
             special-c-compatibility-headers
             (mappend (op (list (string+ "c"  _1)
                                (string+ _1 ".h")))
                      (append c-compatibility-headers
                              empty-c-headers
                              meaningless-c-headers)))
     :test #'equal))
  "Names of C++ headers to treat as always noexcept.
This is largely C compatibility headers for C++, which do not throw exceptions.")

(defun c-compatibility-header? (name)
  (declare (string name))
  (and (string$= ".h" name)
       (gethash name *morally-noexcept-headers*)))

(defmethod morally-noexcept-parent? ((self c/cpp-system-header))
  (gethash (header-name self) *morally-noexcept-headers*))

(define-node-class c/cpp-unknown-header (named-synthetic-header)
  ()
  (:documentation "Node for representing unknown system headers."))

(defgeneric make-unknown-header (name)
  (:method ((name string))
    (make 'c/cpp-unknown-header
          :header-name name
          :children nil))
  (:method ((name c/cpp-preproc-include))
    (make-unknown-header (include-ast-path-ast name)))
  (:method ((name c/cpp-system-lib-string))
    (make-unknown-header (trim-path-string name)))
  (:method ((name c/cpp-string-literal))
    (make-unknown-header (trim-path-string name)))
  (:method ((name c/cpp-preproc-arg))
    (make-unknown-header (trim-path-string name)))
  (:method ((name cpp-import-declaration))
    (make-unknown-header (cpp-name name))))

(defmethod original-path ((self c/cpp-system-header))
  (make-keyword (header-name self)))

(defmethod original-path ((self c/cpp-unknown-header))
  (make-symbol (header-name self)))

(defmethod print-object ((self named-synthetic-header) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (header-name self)))
  self)

(defgeneric get-standard-path-header (project system-header-string
                                      &key header-dirs)
  (:method ((project t) (system-header-string t) &key header-dirs)
    (declare (ignore header-dirs))
    nil)
  (:method ((project t) (system-header-ast c/cpp-system-lib-string) &key header-dirs)
    (get-standard-path-header project
                              (trim-path-string system-header-ast)
                              :header-dirs header-dirs))
  (:method ((project t) (system-header-ast c/cpp-string-literal) &key header-dirs)
    (get-standard-path-header project
                              (trim-path-string system-header-ast)
                              :header-dirs header-dirs))
  (:method ((project c/cpp-project)
            (system-header-ast c/cpp-preproc-arg)
            &key header-dirs)
    (get-standard-path-header project
                              (trim-path-string system-header-ast)
                              :header-dirs header-dirs))

  (:documentation "Get the system header indicated by SYSTEM-HEADER-STRING from
the standard path and add it to PROJECT."))

(defun cache-lookup (cache project path-string)
  (if-let (dict (gethash (type-of project) cache))
    (gethash path-string dict)
    (values nil nil)))

(defun (setf cache-lookup) (value cache project path-string)
  (let ((dict
         (ensure2 (gethash (type-of project) cache)
           (dict))))
    (setf (gethash path-string dict) value)))

(defun clear-cache ()
  (synchronized ('*system-header-cache*)
    (clrhash *system-header-cache*)))

(defun get-stdinc-header (project path-string)
  "Find PATH-STRING in standard include directories."
  (declare (project project)
           (string path-string))
  (flet ((header-children (file)
           ;; TODO Why is tree-copy necessary?
           (mapcar #'tree-copy
                   (children
                    (genome
                     (from-file
                      (component-class project)
                      file))))))
    (or (and-let* ((path (pathname path-string))
                   ((absolute-pathname-p path))
                   (file (file-exists-p path)))
          (header-children file))
        (some (lambda (dir)
                (when-let (file (file-exists-p (base-path-join dir path-string)))
                  (header-children file)))
              *stdinc-dirs*))))

(defun get-header-synopsis (project path-string)
  "Find the synopsis for the standard header in PATH-STRING."
  (nest (ensure-list)
        (parse-header-synopsis
         path-string
         :namespace
         (and (eql (component-class project) 'cpp)
              (not (c-compatibility-header? path-string))
              "std")
         :class-ast)
        (format-symbol :sel/sw/ts "~a-AST")
        (component-class project)))

(defmethod get-standard-path-header ((project c/cpp-project) (path-string string)
                                     &key header-dirs)
  (when (or (member :stdinc header-dirs)
            (no header-dirs))
    (synchronized ('*system-header-cache*)
      (symbol-macrolet ((header-hash (gethash
                                      path-string
                                      (system-headers/string->ast genome)))
                        (genome (genome project)))
        (labels ((get-header-by-name (path-string)
                   (or (get-header-synopsis project path-string)
                       (get-stdinc-header project path-string)))
                 (populate-header-entry (project path-string)
                   (symbol-macrolet ((cached-value
                                       (cache-lookup *system-header-cache*
                                                     project path-string)))
                     (or cached-value
                         (when-let (children (get-header-by-name path-string))
                           (debug:note :trace "Found header at ~a" path-string)
                           (setf cached-value
                                 (make 'c/cpp-system-header
                                       :header-name path-string
                                       :children children)))))))
          (lret ((header
                  (or header-hash
                      (when-let (header (populate-header-entry project path-string))
                        (setf header-hash header)))))
            (when header
              (synchronized (project)
                (let ((old-headers (system-headers genome)))
                  (unless (member header old-headers)
                    (debug:note :trace "Collecting system header ~a" header)
                    (setf genome
                          (copy genome
                                :system-headers
                                (cons header old-headers)))))))))))))

(defmethod lazy-path-p ((project c/cpp-project) path &key lazy-paths root)
  (declare (ignore lazy-paths root))
  (or (header-file? path)
      (call-next-method)))

(defmethod collect-evolve-files :context ((project c/cpp-project))
  "Remove non-header files not in compilation database."
  (flet ((filter-by-compilation-database (evolve-files)
           (mvlet* ((db-files other-files
                     (partition
                      (lambda (entry &aux (file (car entry)))
                        (or (header-file? file)
                            ;; NB This assumes we always want to
                            ;; eagerly load module files. (Probably
                            ;; but not necessarily true.)
                            (module-file? file)
                            (get-command-objects project file)))
                      evolve-files)))
             (debug:lazy-note
              :debug
              "Rejected ~a file~:p based on compilation database (~a kept)"
              (length other-files)
              (length db-files))
             (debug:note :trace "Rejected:~%~{~a~^~%~}" other-files)
             (debug:note :trace "Kept:~%~{~a~^~%~}" other-files)
             db-files)))
    (let ((evolve-files (call-next-method)))
      (if (not (compilation-database project))
          evolve-files
          (filter-by-compilation-database evolve-files)))))

(defun preload-headers (project)
  (debug:note :info "Populating headers")
  ;; Recursively populate headers, starting with .cpp files, adding
  ;; included headers, then adding transitively included headers with
  ;; depth 1, depth 2, etc. Note this will not handle includes where
  ;; the path is a preprocessor macro (they will be populated later
  ;; during symbol table construction).

  ;; The problem this solves is that symbol table computation is not
  ;; (currently) thread-safe, so when the symbol table analysis
  ;; discovers and inserts headers, it can only do so serially. We
  ;; do this approximate, parallel sweep over the headers to parse
  ;; as much as plausible in parallel in advance (but not
  ;; everything, as vendored dependencies can pull in huge numbers
  ;; of unwanted headers).
  (labels ((collect-find-include-arglists ()
             (iter outer
                   (for (nil . sw) in (parsed-evolve-files project))
                   (iter (for ast in-tree (genome sw))
                         (match ast
                           ((and include-ast (c/cpp-preproc-include))
                            (let* ((file-ast
                                     (find-enclosing 'file-ast
                                                     (attrs-root*)
                                                     include-ast)))
                              (in outer
                                  (collecting
                                    (list file-ast
                                          ;; TODO Project or
                                          ;; result?

                                          ;; TODO When including
                                          ;; headers from headers,
                                          ;; we don't have header
                                          ;; dirs. Could we carry
                                          ;; them forward? Or
                                          ;; leave to the symbol
                                          ;; table?
                                          (or (file-header-dirs project ast
                                                                :file file-ast)
                                              *default-header-dirs*)
                                          include-ast)))))))))
           (find-include-in-project (args)
             (destructuring-bind (file-ast header-dirs include-ast) args
               (with-thread-name (:name
                                  (fmt "Including ~a"
                                       (source-text
                                        (include-ast-path-ast include-ast))))
                 (find-include project file-ast include-ast
                               :global t
                               :header-dirs header-dirs)))))
    (iter (iter:with preload-count = 0)
          (iter:with last-evolve-files = (evolve-files project))
          (for pass-number from 0)
          (with-attr-table project
            (let* ((to-populate (collect-find-include-arglists))
                   (len (length to-populate)))
              (debug:note :info "Pass ~a: Preloading ~a header~:p"
                          pass-number len)
              (incf preload-count len)
              (task:task-map
               (parallel-parse-thread-count to-populate)
               (dynamic-closure '(*attrs*
                                  *standard-output*
                                  *trace-output*
                                  *error-output*)
                                #'find-include-in-project)
               to-populate)))
          ;; Stop once a fixed point has been reached.
          (until (eql (evolve-files project)
                      (shiftf last-evolve-files (evolve-files project))))
          (finally
           (debug:note :info "Preloaded ~a header~:p" preload-count)
           (return project)))))

(defmethod from-file :around ((project c/cpp-project) (dir t))
  (let ((project (call-next-method)))
    (setf (genome project)
          (make 'c/cpp-root
                :project-directory (genome project)))
    (preload-headers project)))


;;; Program Headers

(defun ensure-header-in-project-ast (project path software)
  "Ensure SOFTWARE is present in PROJECT's AST at PATH."
  (labels ((force-parse-genome (software)
             "If SOFTWARE isn't parsed yet, parse it. This can happen when a
              file is only lazy-loaded."
             (with-slots (genome) software
               (synchronized (software)
                 (unless (typep genome 'ast)
                   (with-thread-name (:name (fmt "Parsing ~a" path))
                     (genome software))))))
           (insert-software-into-project (project path software)
             (synchronized (project)
               (unless (lookup (genome project) path)
                 (debug:note :debug "~%Inserting ~a into tree~%"
                             path)
                 (let ((temp-project (with project path software)))
                   (setf (genome project)
                         (genome temp-project)
                         (evolve-files project)
                         (evolve-files temp-project))))
               project))
           (insert-program-header (project path software)
             (with-slots (genome) software
               (unless (and (typep genome 'ast)
                            (reachable? genome :from project))
                 (restart-case
                     (progn
                       (force-parse-genome software)
                       (insert-software-into-project
                        project path software))
                   (continue ()
                     :report (lambda (s) (format s "Skip inserting ~a" path))
                     (withf (project-parse-failures project) path)))))))
    (etypecase software
      (c/cpp-system-header project)
      (c/cpp-unknown-header project)
      (c/cpp (insert-program-header project path software)))))

(defun include-path-string (project file path-ast base)
  (let* ((project-dir (project-dir project))
         (file-path
           (make-pathname
            :name nil
            :type nil
            :directory
            (cons :relative
                  (mapcar #'sel/sw/directory::name
                          (cdr (reverse (get-parent-asts* project file)))))
            :defaults project-dir))
         (absolute-file-path (path-join project-dir file-path))
         (include-path (pathname (trim-path-string path-ast)))
         ;; CANONICAL-PATHNAME does not remove :BACK, so convert to :UP
         ;; Warn about this below if this happened.
         (tweaked-include-path-dir
           (substitute :up :back (pathname-directory include-path)))
         (tweaked-include-path
           (make-pathname :directory tweaked-include-path-dir
                          :defaults include-path))
         (absolute-include-path
          (path-join (or base absolute-file-path) tweaked-include-path))
         (relative-include-path
           (if base
               (enough-namestring absolute-include-path project-dir)
               (path-join file-path tweaked-include-path))))
    (unless (equal (pathname-directory include-path) tweaked-include-path-dir)
      (warn "~A may be interpreted incorrectly in the presence of symlinks"
            (namestring file-path)))
    (values (namestring (canonical-pathname relative-include-path))
            include-path
            tweaked-include-path
            absolute-include-path)))

(defun global-search-include (project file path-ast base)
  (receive (include-path-string
            include-path
            tweaked-include-path
            absolute-include-path)
      (include-path-string project file path-ast base)
    (declare (ignore include-path-string))
    (labels ((restrict-headers-by-name-and-type (evolve-files)
               (let ((include-name (pathname-name include-path))
                     (include-type (pathname-type include-path)))
                 (filter
                  ;; Search for the include file everywhere.
                  (op (let ((p (original-path (cdr _))))
                        (and (equal (pathname-name p) include-name)
                             (equal (pathname-type p) include-type))))
                  evolve-files)))
             (prefer-full-path-matches (evolve-files)
               "If there are matches to the whole tweaked include as a suffix,
                only use those. Otherwise return `evolve-files'."
               (or (and (pathname-directory include-path)
                        (filter
                         (lambda (cons)
                           (let ((p (original-path (cdr cons))))
                             (string$= (namestring tweaked-include-path)
                                       (namestring p))))
                         evolve-files))
                   evolve-files))
             (remove-duplicate-evolve-files (evolve-files)
               (let ((project-dir (project-dir project)))
                 (nub evolve-files
                      :test
                      (lambda (file1 file2)
                        (file= (path-join project-dir (car file1))
                               (path-join project-dir (car file2))))))))
      (debug:note :debug "Doing global search for ~a" absolute-include-path)
      (let* ((matches
               (remove-duplicate-evolve-files
                (prefer-full-path-matches
                 (restrict-headers-by-name-and-type
                  (evolve-files project))))))
        (if (null (cdr matches))
            (car matches)
            (or
             ;; If one of the matches is in the directory
             ;; targeted by the include relative to the
             ;; including file, choose it even if there are
             ;; files elsewhere with the same name.
             (find absolute-include-path matches
                   :key (op (original-path (cdr _)))
                   :test #'equal)
             (restart-case
                 (error 'include-conflict-error
                        :ast path-ast
                        :candidates matches)
               (ignore ()
                 :report "Ignore the conflict, return nil"
                 nil)
               (first ()
                 :report "Return the first candidate"
                 (car matches))
               (whichever ()
                 :report "Pick one at random"
                 (random-elt matches))
               (use-value (match)
                 :report "Specify the header to use"
                 (assert (member match matches))
                 match))))))))

(defmethod get-program-header ((project c/cpp-project)
                               file
                               include-ast
                               path-ast
                               &key global base)
  "Look for a header designated by a program include.
This does not search in standard directories.

When we have a search path \(from a compilation database) this is used
as a subroutine and called for every directory in the search path.

If we don't have a search path, this function does the entire search;
if GLOBAL is true, it searches through all files in the project, and
signals a restartable error if there are collisions."
  (let ((include-path-string
          (include-path-string project file path-ast base)))
    (or (assoc include-path-string
               (evolve-files project)
               :test #'equal)
        (and global
             (global-search-include
              project
              file
              path-ast
              base)))))


;;; Includes Symbol Table

(define-condition include-conflict-error (error)
  ((ast :initarg :ast :type ast :reader include-conflict-error.ast)
   (candidates :initarg :candidates :type (soft-list-of software)
               :reader include-conflict-error.candidates))
  (:documentation "Error for multiple headers matching an include")
  (:report (lambda (c s)
             (with-slots (ast candidates) c
               (format s "Clash in header candidate includes: ~a~%~{~a~^~%~}"
                       ast
                       (mapcar (op (original-path (cdr _))) candidates))))))

(define-condition include-not-found-warning (warning)
  ((include-ast
    :initarg :include-ast
    :reader include-not-found-warning.include-ast))
  (:documentation "Warning for an include whose header cannot be found")
  (:report (lambda (c s)
             (with-slots (include-ast) c
               (format s "Not found: ~a" (source-text include-ast))))))

(defun include-ast-path-ast (include-ast &key symbol-table)
  "Extract the path AST from INCLUDE-AST.
If SYMBOL-TABLE is supplied, use it to resolve macros in include
paths."
  (ematch include-ast
    ((c/cpp-preproc-include
      (c/cpp-path (and path-ast (c/cpp-string-literal))))
     path-ast)
    ((c/cpp-preproc-include
      (c/cpp-path (and path-ast (c/cpp-system-lib-string))))
     path-ast)
    ((cpp-import-declaration
      (cpp-name (and path-ast (c/cpp-system-lib-string))))
     path-ast)
    ;; E.g. `#include BOOST_ABI_PREFIX`.
    ((c/cpp-preproc-include
      (c/cpp-path (and id (identifier-ast))))
     (and-let* (((sel/sw/ts::macro-name? id))
                (symbol-table)
                (ns (lookup symbol-table :macro))
                (macro (car (lookup ns (source-text id))))
                (preproc-def (find-enclosing 'c/cpp-preproc-def (attrs-root*) macro)))
       (cpp-value preproc-def)))
    ;; E.g. `#include BOOST_PP_ITERATE()`.
    ((c/cpp-preproc-include
      (c/cpp-path (call-ast)))
     ;; TODO
     nil)))

(defun collect-potential-includes (project file
                                   include-ast
                                   path-ast
                                   header-dirs
                                   global)
  (declare
   (ast file include-ast path-ast)
   (list header-dirs))
  (labels ((search-header-dir (dir &key global)
             (econd
              ((eql dir :current)
               (let ((base
                       (and-let* (((typep file 'file-ast))
                                  (path (dir:full-pathname file)))
                         (pathname-directory-pathname path))))
                 (get-program-header project file include-ast
                                     path-ast
                                     :base base
                                     :global global)))
              ((member dir '(:always :system))
               nil)
              ((eql dir :stdinc)
               (get-standard-path-header
                project path-ast
                :header-dirs header-dirs))
              ((stringp dir)
               (debug:note :trace "Searching ~a for header ~a"
                           dir path-ast)
               (get-program-header
                project file include-ast path-ast
                :base dir
                :global global))))
           (search-header-dirs (&key global)
             (if (no header-dirs)
                 (progn
                   (debug:lazy-note :trace "No header dirs when searching for ~a"
                                    (source-text path-ast))
                   (get-standard-path-header project path-ast))
                 (filter-map (op (search-header-dir _ :global global))
                             header-dirs))))
    (search-header-dirs :global global)))

(defun find-include (project file include-ast
                     &key global
                       header-dirs
                       symbol-table)
  "Find an include in PROJECT. Ensure it is an evolve-file."
  (declare (functional-tree-ast file))
  (when-let ((path-ast
              (include-ast-path-ast
               include-ast
               :symbol-table symbol-table)))
    (labels ((make-unknown-header-with-proxy (include-ast)
               (lret ((unknown-header (make-unknown-header include-ast)))
                 ;; Make the unknown header proxy to the include.
                 (when (boundp '*attrs*)
                   (synchronized (project)
                     (setf (attr-proxy unknown-header) include-ast)))))
             (collect-potential-includes* (global)
               (collect-potential-includes
                project
                file
                include-ast
                path-ast
                header-dirs
                global))
             (find-include ()
               (or (first (collect-potential-includes* nil))
                   (and global (first (collect-potential-includes* t)))
                   (debug:lazy-note
                    :debug
                    "Unknown header: ~a"
                    (source-text include-ast))
                   (make-unknown-header-with-proxy include-ast))))
      (ematch (find-include)
        ((and include (or (c/cpp-unknown-header)
                          (c/cpp-system-header)))
         include)
        ((cons path software)
         (ensure-header-in-project-ast project path software)
         (debug:note :trace "Found include ~a for ~a"
                     software include-ast)
         software)))))

(defun find-enclosing-software (project ast &key file-ast)
  "Get the software object in PROJECT that contains AST."
  (when-let ((enclosing
              (or file-ast
                  (find-enclosing '(or file-ast synthetic-header)
                                  project ast))))
    (typecase enclosing
      (file-ast
       (evolve-files-ref project (namestring (full-pathname enclosing))))
      (synthetic-header enclosing))))

(defun relativize (project path)
  "Relative PATH relative to the root of PROJECT.
If PATH is a symbol, it is left unchanged."
  (assure dependency-tree-path
    (etypecase-of (or pathname dependency-tree-path) path
      ((or system-dependency unknown-dependency) path)
      ((or pathname dependency-path)
       (pathname-relativize (project-dir project) path)))))

(defun get-includer ()
  (car *dependency-stack*))

(defun update-dependency-graph (project includee)
  "Record dependencies and dependents for INCLUDEE based on the current
value of `*dependency-stack*'."
  (let ((includee-path (relativize project (original-path includee)))
        (includer (get-includer)))
    (when includer
      (let ((includer-path (relativize project (original-path includer))))
        (record-inclusion project includer-path includee-path)))))

(defun record-inclusion (project includer-path includee-path)
  "Return two values: who INCLUDER-PATH includes, and who INCLUDEE-PATH
is included by."
  (synchronized (project)
    (values
     (pushnew includee-path
              (href (included-headers (genome project))
                    includer-path)
              :test #'equal)
     (pushnew includer-path
              (href (including-files (genome project))
                    includee-path)
              :test #'equal))))

(defun find-symbol-table-from-include (project include-ast
                                       &key (in (empty-map))
                                         global
                                         (header-dirs nil header-dirs-supplied?))
  "Find the symbol table in PROJECT for the include file
included by INCLUDE-AST.  IN is the symbol table before entry
to the include-ast.  If GLOBAL is true, search for non-system
include files in all directories of the project."
  (declare (fset:map in)
           (ignore global))
  #+debug-fstfi
  (format t "Enter find-symbol-table-from-include on ~a~%"
          (source-text include-ast))
  (labels ((symbol-table* (header in)
             ;; TODO Currently we ignore input symbol tables to
             ;; headers, because we use the same representation of a
             ;; header no matter how many times it is included (or
             ;; what headers come before it). Beside that, we also
             ;; currently don't have a way of indicating that a
             ;; subroot's attributes depend on a prior sibling, not
             ;; just a parent.
             (declare (ignore in))
             (assert (attrs:reachable? (genome header)))
             (symbol-table (genome header) (empty-map)))
           (safe-symbol-table (software)
             "Extract a symbol table from SOFTWARE, guarding for circularity."
             (cond
               ((null software) nil)
               ((typep software 'c/cpp-unknown-header)
                (update-dependency-graph project software)
                (empty-map))
               ((member (original-path software)
                        *dependency-stack*
                        :key #'original-path
                        :test #'equal)
                ;; Just returning nil might still result in a global
                ;; search. We found the header, we just can't use it.
                (update-dependency-graph project software)
                (error 'circular-inclusion
                       :header software
                       :stack *dependency-stack*))
               (t
                (update-dependency-graph project software)
                (let ((*dependency-stack* (cons software *dependency-stack*)))
                  (symbol-table* software in))))))
    (let* ((file
            (or (find-enclosing '(or file-ast synthetic-header)
                                project include-ast)
                (error "No enclosing file for ~a" include-ast)))
           (*dependency-stack*
            ;; Initialize the stack with a top-level file, if needed.
            (or *dependency-stack*
                (when-let (sw
                           (find-enclosing-software project include-ast
                                                    :file-ast file))
                  (list sw))))
           (*header-dirs*
            (if header-dirs-supplied?
                header-dirs
                (or (file-header-dirs project include-ast :file file)
                    *header-dirs*
                    *default-header-dirs*))))
      (nlet retry ()
        (restart-case
            (handler-case
                (ematch include-ast
                  ((or (c/cpp-preproc-include)
                       (cpp-import-declaration))
                   (safe-symbol-table
                    (or
                     (find-include project file
                                   include-ast
                                   :header-dirs *header-dirs*
                                   :symbol-table in)
                     (error "Could not resolve ~a"
                            (source-text
                             (or (include-ast-path-ast include-ast :symbol-table in)
                                 include-ast)))))))
              (circular-inclusion ()
                nil))
          (skip-include ()
            :report (lambda (s)
                      (format s "Skip including ~a"
                              (source-text
                               (include-ast-path-ast include-ast
                                                     :symbol-table in))))
            (let ((path
                    (source-text
                     (include-ast-path-ast include-ast
                                           :symbol-table in))))
              (debug:note :trace "Skipping including ~a" path)
              (withf (project-parse-failures project) path)
              (return-from find-symbol-table-from-include
                in))))))))

(defmethod symbol-table ((node c/cpp-preproc-include) &optional (in (empty-map)))
  (debug:note :trace "Including symbol table for ~a" node)
  (let ((root (attrs-root *attrs*)))
    (if (typep root 'directory-project)
        (if-let (st (find-symbol-table-from-include root node :in in))
          ;; Return file's symbol table as the symbol table of the
          ;; include.
          (prog1 (symbol-table-union node in st)
            ;; Ensure the ASTs in the include have their own symbol
            ;; table.
            (call-next-method))
          (call-next-method))
        (call-next-method))))

(defmethod symbol-table ((node c/cpp-system-header) &optional in)
  (debug:note :trace "Computing symbol table for include ~a" node)
  (ts::propagate-declarations-down node in))

(defmethod symbol-table ((node c/cpp-unknown-header) &optional in)
  (declare (ignore in))
  (empty-map))

) ; #+(or :TREE-SITTER-C :TREE-SITTER-CPP)
