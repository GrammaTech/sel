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
           :update-dependency-graph))

(in-package :software-evolution-library/software/c-cpp-project)
(in-readtable :curry-compose-reader-macros)

(defvar *header-dirs* nil
  "The header search path of the current compilation unit.")

(defparameter *header-extensions*
  '("h" "hpp" "hh")
  "Header file extensions.")

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

(define-node-class synthetic-header (functional-tree-ast)
  ((children :initarg :children
             :accessor children
             :initform nil)
   (child-slots :initform '((children . 0))
                :allocation :class))
  (:documentation "Superclass of synthetic headers, such as substitute system
headers and implicit headers for command-line preprocessor macros."))

(defmethod command-object ((project c/cpp-project) (file synthetic-header))
  nil)

(defun trim-path-string (path-ast &aux (text (text path-ast)))
  "Return the text of PATH-AST with the quotes around it removed."
  (string-trim "<>\"" text))

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
  (setf (genome project)
        (copy (genome project)
              :implicit-headers nil
              :implicit-headers-table (dict)))
  (values))

(deftype dependency-tree ()
  '(soft-list-of dependency-tree-entry))

(eval-always
 (defun symbol-package? (x)
   (and (symbolp x)
        (symbol-package x))))

(deftype uninterned-symbol ()
  '(and symbol (not (satisfies symbol-package?))))

(deftype dependency-tree-path ()
  '(or string keyword uninterned-symbol))

(deftype dependency-tree-entry ()
  '(cons dependency-tree-path list))

(defun project-dependency-tree (project &key allow-headers entry-points)
  "Dump the header graph of PROJECT as a cons tree.
The top level is a list of entries. Entries have the form (FILE .
INCLUDEES), where each of INCLUDEES is itself an entry, or the keyword
:CIRCLE for a circular inclusion.

In each entry, FILE is either a string (for a file) or a keyword (for
a standard include)."
  (declare ((soft-list-of string) entry-points))
  (assure dependency-tree
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
                 (if (find path seen :test #'equal)
                     (list :circle path)
                     (let ((seen (cons path seen)))
                       (intern-leaf
                        (cons path
                              (mapcar (op (rec _ seen))
                                      (gethash path included-headers))))))))
        (mapcar (op (rec _ nil))
                (or entry-points
                    (if allow-headers
                        files
                        (remove-if #'header-file? files))))))))

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
      (let* ((source
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
        (ensure2 (gethash file table)
          (lret ((header (command-implicit-header co lang)))
            (when header
              (setf (genome project)
                    (copy (genome project)
                          :implicit-headers
                          (adjoin header (implicit-headers genome)))))))))))

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
                  (symbol-table-union root
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
    (get-standard-path-header project (trim-path-string system-header-ast)
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

(defmethod get-standard-path-header ((project c/cpp-project) (path-string string)
                                     &key header-dirs)
  (when (or (no header-dirs)
            (member :stdinc header-dirs))
    (synchronized ('*system-header-cache*)
      (symbol-macrolet ((header-hash (gethash
                                      path-string
                                      (system-headers/string->ast genome)))
                        (genome (genome project)))
        (labels ((populate-header-entry (project path-string)
                   (symbol-macrolet ((cached-value
                                      (cache-lookup *system-header-cache*
                                                    project path-string)))
                     (or cached-value
                         (when-let (children
                                    (nest (ensure-list)
                                          (parse-header-synopsis
                                           path-string
                                           :namespace
                                           (when (eql (component-class project) 'cpp)
                                             "std")
                                           :class-ast)
                                          (format-symbol :sel/sw/ts "~a-AST")
                                          (component-class project)))
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
              (setf genome
                    (copy genome
                          :system-headers (adjoin header
                                                  (system-headers genome)))))))))))

(defmethod from-file :around ((project c/cpp-project) (dir t))
  (labels ((maybe-populate-header (ast)
             (match ast
               ((c/cpp-preproc-include
                 (c/cpp-path (and path (c/cpp-system-lib-string))))
                (get-standard-path-header project (trim-path-string path)
                                          :header-dirs
                                          (file-header-dirs project ast))))))
    (let ((result (call-next-method)))
      (setf #1=(genome result) (make-instance 'c/cpp-root
                                              :project-directory #1#))
      (mapc #'maybe-populate-header project)
      result)))


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
    (labels ((global-search ()
               "Search for the include everywhere in the project.
This is used as a fallback when we have no header search path."
               (let ((matches
                      (filter
                       ;; Search for the include file everywhere.
                       (op (let ((p (original-path (cdr _))))
                             (and (equal (pathname-name p)
                                         (pathname-name include-path))
                                  (equal (pathname-type p)
                                         (pathname-type include-path)))))
                       (evolve-files project))))
                 (if (null (cdr matches))
                     (cdar matches)
                     (if-let* ((m (find absolute-include-path matches
                                        :key (op (original-path (cdr _)))
                                        :test #'equal)))
                       ;; If one of the matches is in the directory
                       ;; targeted by the include relative to the including
                       ;; file, choose it even if there are files elsewhere
                       ;; with the same name.
                       (cdr m)
                       (restart-case
                           (error 'include-conflict-error
                                  :ast path-ast
                                  :candidates matches)
                         (ignore ()
                           :report "Ignore the conflict, return nil"
                           nil)
                         (first ()
                           :report "Return the first candidate"
                           (cdar matches))
                         (whichever ()
                           :report "Pick one at random"
                           (cdr (random-elt matches)))
                         (use-value (match)
                           :report "Specify the header to use"
                           (assert (member match matches))
                           (cdr match)))))))
             (simple-relative-search ()
               "Do a search relative to BASE, if supplied, or the location of
the including file."
               ;; Search only relative to the location of the file
               ;; containing the include-ast.  TODO: add a set
               ;; of paths to search in. TODO: Allow searching
               ;; up through grandparent directories (as some
               ;; compilers do).
               (aget include-path-string
                     (evolve-files project)
                     :test #'equal)))
      (or (simple-relative-search)
          (and global
               (global-search))))))


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

(defmethod symbol-table-union ((root c/cpp-project) table-1 table-2 &key)
  (multi-map-symbol-table-union
   table-1 table-2
   :allow-multiple (multi-declaration-keys root)))

(defparameter *dependency-stack* nil
  "Stack of include file names currently being processed during type
inference.  Used to prevent circular attr propagation.")

(define-condition circular-inclusion (error)
  ((header :initarg :header :reader header)
   (stack :initarg :stack :reader dependency-stack))
  (:default-initargs
   :stack *dependency-stack*)
  (:report (lambda (c s)
             (with-slots (header) c
               (format s "Circular inclusion of ~a" header)))))

(defun include-ast-path-ast (include-ast)
  (ematch include-ast
    ((c/cpp-preproc-include
      (c/cpp-path (and path-ast (c/cpp-string-literal))))
     path-ast)
    ((c/cpp-preproc-include
      (c/cpp-path (and path-ast (c/cpp-system-lib-string))))
     path-ast)))

(defun find-include (project file header-dirs include-ast
                     &key (global *global-search-for-include-files*))
  (assert file)
  (let ((path-ast (include-ast-path-ast include-ast)))
    (labels ((find-include (global)
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
                header-dirs))
             (make-unknown-header* (include-ast)
               (lret ((unknown-header (make-unknown-header include-ast)))
                 (when (boundp '*attrs*)
                   (setf (attr-proxy unknown-header) include-ast)))))
      (declare (dynamic-extent #'find-include))
      (or (find-include nil)
          (and global (find-include :global))
          (make-unknown-header* include-ast)))))

(defun find-enclosing-software (project ast &key file-ast)
  "Get the software object in PROJECT that contains AST."
  (when-let ((file-ast
              (or file-ast
                  (find-enclosing '(or file-ast synthetic-header)
                                  project ast))))
    (evolve-files-ref project (namestring (full-pathname file-ast)))))

(defun relativize (project path)
  "Relative PATH relative to the root of PROJECT.
If PATH is a symbol, it is left unchanged."
  (assure dependency-tree-path
    (etypecase-of (or pathname dependency-tree-path) path
      ((or keyword uninterned-symbol) path)
      ((or pathname string)
       (pathname-relativize (project-dir project) path)))))

(defun update-dependency-graph (project includee)
  "Record dependencies and dependents for INCLUDEE based on the current
value of `*dependency-stack*'."
  (let ((includee-path (relativize project (original-path includee)))
        (includer (car *dependency-stack*)))
    (when includer
      (let ((includer-path (relativize project (original-path includer))))
        (pushnew includee-path
                 (href (included-headers (genome project))
                       includer-path)
                 :test #'equal)
        (pushnew includer-path
                 (href (including-files (genome project))
                       includee-path)
                 :test #'equal)))))

(defun find-symbol-table-from-include (project include-ast
                                       &key (in (empty-map))
                                         (global *global-search-for-include-files*))
  "Find the symbol table in PROJECT for the include file
included by INCLUDE-AST.  IN is the symbol table before entry
to the include-ast.  If GLOBAL is true, search for non-system
include files in all directories of the project."
  (declare (fset:map in))
  #+debug-fstfi
  (format t "Enter find-symbol-table-from-include on ~a~%"
          (source-text include-ast))
  (labels ((symbol-table* (header in)
             (symbol-table (genome header) in))
           (safe-symbol-table (software)
             "Extract a symbol table from SOFTWARE, guarding for circularity."
             (cond
               ((null software) nil)
               ((member software *dependency-stack*)
                ;; Just returning nil might still result in a global
                ;; search. We found the header, we just can't use it.
                (update-dependency-graph project software)
                (error 'circular-inclusion :header software))
               (t
                (update-dependency-graph project software)
                (let ((*dependency-stack* (cons software *dependency-stack*)))
                  (symbol-table* software in))))))
    (let* ((file (find-enclosing '(or file-ast synthetic-header)
                                 project include-ast))
           (*dependency-stack*
            ;; Initialize the stack with a top-level file, if needed.
            (or *dependency-stack*
                (when-let (sw
                           (find-enclosing-software project include-ast
                                                    :file-ast file))
                  (list sw))))
           (*header-dirs*
            (or (file-header-dirs project include-ast :file file)
                *header-dirs*
                *default-header-dirs*)))
      (handler-case
          (ematch include-ast
            ((c/cpp-preproc-include)
             (safe-symbol-table
              (find-include project file *header-dirs*
                            include-ast))))
        (circular-inclusion ()
          nil)))))

(defmethod symbol-table ((node c/cpp-preproc-include) &optional (in (empty-map)))
  (let ((root (attrs-root *attrs*)))
    (if (typep root 'directory-project)
        (if-let (st (find-symbol-table-from-include root node :in in))
          (symbol-table-union root in st)
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
