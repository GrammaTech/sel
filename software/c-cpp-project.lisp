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
   (:file :software-evolution-library/components/file)
   (:task :software-evolution-library/utility/task))
  (:shadowing-import-from :serapeum :~>>)
  (:export :c/cpp-project
           :get-standard-path-header
           :header-name
           :system-headers
           :implicit-headers
           :c/cpp-root
           :*global-search-for-include-files*
           :include-not-found-warning
           :include-conflict-error
           :include-conflict-error.ast
           :include-conflict-error.candidates
           :file-preproc-defs
           :get-implicit-header
           :command-implicit-header
           :project-dependency-tree
           :who-includes?
           :file-dependency-tree
           :find-include
           :*header-extensions*
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

(defgeneric header-path (header)
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
  (member (pathname-type (header-path file))
          *header-extensions*
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

(defmethod command-object ((project c/cpp-project) (file synthetic-header))
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
      (command-header-dirs (only-elt co)))))

(defun file-preproc-defs (project ast &key (file (find-enclosing 'file-ast project ast)))
  "Get preprocessor definitions for FILE from PROJECT's compilation database."
  (when (and project file)
    (when-let ((co (command-object project file)))
      (command-preproc-defs co))))

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
         (path (header-path header))
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
    (project &key (include-tree (project-dependency-tree project :allow-headers t)))
  "Return the evolve-files of PROJECT sorted in dependency order."
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
             ;; NOTE: add header files which haven't been reached to the end.
             ;;       Anything added here will not have any guarantees on whether
             ;;       it's in the correct order.
             (reverse
              (append (unused-files evolve-files)
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
  (:documentation "Synthesize an implicit header for COMMAND.")
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
    (when-let* ((cos (command-object project file))
                (co (only-elt cos)))
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
    (make-unknown-header (trim-path-string name))))

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
  (iter (for dir in *stdinc-dirs*)
        (when-let (file (file-exists-p (base-path-join dir path-string)))
          (return
            ;; TODO Why is tree-copy necessary?
            (mapcar #'tree-copy
                    (children (genome (from-file (component-class project) file))))))))

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
                           (setf cached-value
                                 (make-instance
                                  'c/cpp-system-header
                                  :header-name path-string
                                  :children children)))))))
          (lret ((header
                  (or header-hash
                      (when-let (header (populate-header-entry project path-string))
                        (setf header-hash header)))))
            (when header
              (synchronized (project)
                (setf genome
                      (copy genome
                            :system-headers (adjoin header
                                                    (system-headers genome))))))))))))

(defmethod from-file :around ((project c/cpp-project) (dir t))
  (let* ((result (call-next-method))
         (genome
          (make-instance 'c/cpp-root
                         :project-directory (genome result)))
         (last-evolve-files (evolve-files result)))
    (setf (genome result)
          (assure c/cpp-root genome))
    (debug:note 2 "Populating headers")
    ;; Recursively populate headers, starting with .cpp files, adding
    ;; included headers, then adding transitively included headers with
    ;; depth 1, depth 2, etc. Note this will not handle includes where
    ;; the path is a preprocessor macro (they will be populated later
    ;; during symbol table construction).
    (labels ((collect-find-include-arglists ()
               (iter outer
                     (for (nil . sw) in (evolve-files result))
                     (when (typep (slot-value sw 'genome) 'ast)
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
                                             (file-header-dirs project ast
                                                               :file file-ast)
                                             include-ast))))))))))
             (find-include-in-project (args)
               (destructuring-bind (file-ast header-dirs include-ast) args
                 (with-thread-name (:name
                                    (fmt "Including ~a"
                                         (source-text
                                          (include-ast-path-ast include-ast))))
                   (find-include project file-ast header-dirs include-ast)))))
      (iter (with-attr-table result
              (let ((to-populate (collect-find-include-arglists)))
                (debug:note 3 "Populating ~a header~:p" (length to-populate))
                (task:task-map
                 (parallel-parse-thread-count to-populate)
                 (dynamic-closure '(*attrs*)
                                  #'find-include-in-project)
                 to-populate)))
            ;; Stop once a fixed point has been reached.
            (until (eql (evolve-files result)
                        (shiftf last-evolve-files (evolve-files result))))))
    result))


;;; Program Headers

(defparameter *global-search-for-include-files* nil
  "When true, search for include files in the entire directory tree
of a project.  Used to set default value of the :GLOBAL keyword
arg of FIND-SYMBOL-TABLE-FROM-INCLUDE.  If there are duplicates
but the file is available locally, prioritize the local file.")

(defmethod get-program-header ((project c/cpp-project)
                               file
                               include-ast
                               path-ast
                               &key
                                 (global *global-search-for-include-files*)
                                 base)
  "Look for a header designated by a program include.
This does not search in standard directories.

When we have a search path \(from a compilation database) this is used
as a subroutine and called for every directory in the search path.

If we don't have a search path, this function does the entire search;
if GLOBAL is true, it searches through all files in the project, and
signals a restartable error if there are collisions."
  (let* ((project-dir (project-dir project))
         (file-path (make-pathname :name nil :type nil
                                   :directory
                                   (cons :relative
                                         (mapcar #'sel/sw/directory::name
                                                 (cdr (reverse (get-parent-asts* project file)))))
                                   :defaults project-dir))
         (absolute-file-path (path-join project-dir file-path))
         (include-path (pathname (trim-path-string path-ast)))
         ;; CANONICAL-PATHNAME does not remove :BACK, so convert to :UP
         ;; Warn about this below if this happened.
         (tweaked-include-path-dir (substitute :up :back (pathname-directory include-path)))
         (tweaked-include-path (make-pathname :directory tweaked-include-path-dir
                                              :defaults include-path))
         (absolute-include-path
          (path-join (or base absolute-file-path) tweaked-include-path))
         (relative-include-path
          (if base
              (enough-namestring absolute-include-path project-dir)
              (path-join file-path tweaked-include-path)))
         (include-path-string
          (namestring (canonical-pathname relative-include-path))))
    #+debug-fstfi
    (macrolet ((%d (v)
                 `(format t "~a = ~s~%" ',v ,v)))
      (%d project-dir) (%d file-path) (%d absolute-file-path)
      (%d include-path) (%d tweaked-include-path-dir)
      (%d absolute-include-path)
      (%d relative-include-path)
      (%d include-path-string))
    (unless (equal (pathname-directory include-path) tweaked-include-path-dir)
      (warn "~A in ~A may be interpreted incorrectly in the presence of symlinks"
            (remove #\Newline (source-text include-ast))
            (namestring file-path)))
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
               (nub evolve-files
                    :test
                    (lambda (file1 file2)
                      (file= (path-join project-dir (car file1))
                             (path-join project-dir (car file2))))))
             (global-search ()
               "Search for the include everywhere in the project.
This is used as a fallback when we have no header search path."
               (let* ((matches
                       (nest (remove-duplicate-evolve-files)
                             (prefer-full-path-matches)
                             (restrict-headers-by-name-and-type)
                             (evolve-files project))))
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
                          match))))))
             (simple-relative-search ()
               "Do a search relative to BASE, if supplied, or the location of
the including file."
               ;; Search only relative to the location of the file
               ;; containing the include-ast.  TODO: add a set
               ;; of paths to search in. TODO: Allow searching
               ;; up through grandparent directories (as some
               ;; compilers do).
               (assoc include-path-string
                      (evolve-files project)
                      :test #'equal))
             (force-parse-genome (path software)
               (with-slots (genome) software
                 (synchronized (software)
                   (unless (typep genome 'ast)
                     (with-thread-name (:name (fmt "Parsing ~a" path))
                       (genome software))))))
             (insert-software-into-project (path software)
               (synchronized (project)
                 (unless (lookup (genome project) path)
                   (debug:note 2 "~%Inserting ~a (~a) into tree~%"
                               path
                               include-path)
                   (let ((temp-project (with project path software)))
                     (setf (genome project)
                           (genome temp-project)
                           (evolve-files project)
                           (evolve-files temp-project))))))
             (ensure-result-in-project-ast (result)
               (destructuring-bind (path . software) result
                 (with-slots (genome) software
                   (unless (typep genome 'ast)
                     (force-parse-genome path software)
                     (insert-software-into-project path software))))))
      (when-let (result
                 (or (simple-relative-search)
                     (and global
                          (global-search))))
        (ensure-result-in-project-ast result)
        (cdr result)))))


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

(defparameter *dependency-stack* nil
  "Stack of include file names currently being processed during type
inference.  Used to prevent circular attr propagation.")

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

(defun find-include (project file header-dirs include-ast
                     &key (global *global-search-for-include-files*)
                       symbol-table)
  (declare (functional-tree-ast file))
  (when-let ((path-ast (include-ast-path-ast include-ast
                                             :symbol-table symbol-table)))
    (labels ((search-header-dirs (global)
               (if (no header-dirs)
                   (get-standard-path-header
                    project path-ast)
                   (some
                    (lambda (dir)
                      (econd ((eql dir :current)
                              (get-program-header project file include-ast
                                                  path-ast
                                                  :base nil
                                                  :global global))
                             ((member dir '(:always :system))
                              nil)
                             ((eql dir :stdinc)
                              (get-standard-path-header
                               project path-ast
                               :header-dirs header-dirs))
                             ((stringp dir)
                              (get-program-header
                               project file include-ast path-ast
                               :base dir
                               :global global))))
                    header-dirs)))
             (make-unknown-header* (include-ast)
               (lret ((unknown-header (make-unknown-header include-ast)))
                 (when (boundp '*attrs*)
                   (synchronized (project)
                     (setf (attr-proxy unknown-header) include-ast))))))
      (or (search-header-dirs nil)
          (and global (search-header-dirs :global))
          (make-unknown-header* include-ast)))))

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
                                         (global *global-search-for-include-files*)
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
             (symbol-table (genome header) in))
           (safe-symbol-table (software)
             "Extract a symbol table from SOFTWARE, guarding for circularity."
             (cond
               ((null software) nil)
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
                     (find-include project file *header-dirs*
                                   include-ast
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
            (return-from find-symbol-table-from-include
              in))
          (enable-global-search ()
            :report "Enable global (within project) search for include files"
            :test (lambda (c)
                    (declare (ignore c))
                    (null *global-search-for-include-files*))
            (setq *global-search-for-include-files* t)
            (retry)))))))

(defmethod symbol-table ((node c/cpp-preproc-include) &optional (in (empty-map)))
  (let ((root (attrs-root *attrs*)))
    (if (typep root 'directory-project)
        (if-let (st (find-symbol-table-from-include root node :in in))
          (symbol-table-union node in st)
          (call-next-method))
        (call-next-method))))

(defmethod symbol-table ((node c/cpp-system-header) &optional in)
  (if-let ((root-ast (car (children node))))
    (symbol-table root-ast in)
    (empty-map)))

(defmethod symbol-table ((node c/cpp-unknown-header) &optional in)
  (declare (ignore in))
  (empty-map))

) ; #+(or :TREE-SITTER-C :TREE-SITTER-CPP)
