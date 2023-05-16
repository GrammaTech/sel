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
           :project-include-tree
           :who-includes?
           :file-include-tree))

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

(defun trim-path-string (path-ast &aux (text (text path-ast)))
  "Return the text of PATH-AST with the quotes around it removed."
  (subseq text 1 (1- (length text))))

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

(deftype include-tree ()
  '(soft-list-of include-tree-entry))

(deftype include-tree-entry ()
  '(cons (or string keyword) list))

(defun project-include-tree (project)
  "Dump the header graph of PROJECT as a cons tree.
The top level is a list of entries. Entries have the form (FILE .
INCLUDEES), where each of INCLUDEES is itself an entry, or the keyword
:CIRCLE for a circular inclusion.

In each entry, FILE is either a string (for a file) or a keyword (for
a standard include)."
  (assure include-tree
    (let* ((genome (genome project))
           (included-headers (included-headers genome)))
      ;; Populate the symbol table, recording header dependencies as a
      ;; side effect.
      (symbol-table project)
      (labels ((rec (path seen)
                 (if (find path seen :test #'equal)
                     (list :circle path)
                     (let ((seen (cons path seen)))
                       (cons path
                             (mapcar (op (rec _ seen))
                                     (href included-headers path)))))))
        (mapcar (op (rec _ nil))
                (remove-if #'header-file?
                           (mapcar #'car
                                   (evolve-files project))))))))

(defgeneric file-include-tree (project file)
  (:documentation "Return the tree of includes rooted at FILE in PROJECT.")
  (:method ((project t) (file software))
    (file-include-tree project
                       (pathname-relativize (project-dir project)
                                            (original-path file))))
  (:method ((project t) (file file-ast))
    (file-include-tree project
                       (namestring (full-pathname file))))
  (:method ((project t) (file string))
    (aget file
          (project-include-tree project)
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
  (with-attr-table header
    (symbol-table (first (children header)))))

(defmethod lookup ((obj c/cpp-root) (key string))
  ;; Enables the use of the `@' macro directly against projects.
  (lookup (project-directory obj) key))

(defmethod symbol-table :around ((ast c/cpp-translation-unit) &optional in)
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

(defvar *system-header-cache* (dict)
  "Store system headers that have already been parsed.")

(defvar *system-header-symbol-table-cache* (dict)
  "Cache system header symbol tables.")

(define-node-class c/cpp-system-header (synthetic-header)
  ((header-name :initarg :header-name
                :accessor header-name))
  (:documentation "Node for representing synthetic system headers."))

(defmethod original-path ((self c/cpp-system-header))
  (make-keyword (header-name self)))

(defmethod print-object ((self c/cpp-system-header) stream)
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
    (clrhash *system-header-cache*))
  (synchronized ('*system-header-symbol-table-cache*)
    (clrhash *system-header-symbol-table-cache*)))

(defmethod get-standard-path-header ((project c/cpp-project) (path-string string)
                                     &key header-dirs
                                     &aux (genome (genome project)))
  (when (or (no header-dirs)
            (member :stdinc header-dirs))
    (synchronized ('*system-header-cache*)
      (symbol-macrolet ((header-hash (gethash
                                      path-string
                                      (system-headers/string->ast genome)))
                        (genome (genome project)))
        (labels ((populate-header-entry (project path-string)
                   (ensure2 (cache-lookup *system-header-cache* project path-string)
                     (make-instance
                      'c/cpp-system-header
                      :header-name path-string
                      :children
                      (nest (ensure-list)
                            (parse-header-synopsis
                             path-string
                             :namespace
                             (when (eql (component-class project) 'cpp)
                               "std")
                             :class-ast)
                            (format-symbol :sel/sw/ts "~a-AST")
                            (component-class project))))))
          (lret ((header
                  (ensure2 header-hash
                    (populate-header-entry project path-string))))
            (setf genome
                  (copy genome
                        :system-headers (adjoin header
                                                (system-headers genome))))))))))

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

(defparameter *include-file-stack* nil
  "Stack of include file names currently being processed during type
inference.  Used to prevent circular attr propagation.")

(define-condition circular-inclusion (error)
  ((header :initarg :header :reader header)
   (stack :initarg :stack :reader include-file-stack))
  (:default-initargs
   :stack *include-file-stack*)
  (:report (lambda (c s)
             (with-slots (header) c
               (format s "Circular inclusion of ~a" header)))))

#+debug-fstfi2
(defparameter *include-files-to-note* nil
  "A list of strings that, if present in the namestring of
an include file, cause the symbol table of that
file to be printed for debugging purposes.")

(defun find-symbol-table-from-include (project include-ast
                                       &key (in (empty-map))
                                         (global *global-search-for-include-files*)
                                       &aux #+debug-fstfi2
                                            (iftn *include-files-to-note*))
  "Find the symbol table in PROJECT for the include file
included by INCLUDE-AST.  IN is the symbol table before entry
to the include-ast.  If GLOBAL is true, search for non-system
include files in all directories of the project."
  (declare (fset:map in))
  #+debug-fstfi
  (format t "Enter find-symbol-table-from-include on ~a~%"
          (source-text include-ast))
  (labels ((process-std-header (project path-ast)
             "Retrieve a standard header from the cache."
             (let ((path-string (trim-path-string path-ast)))
               (if-let ((system-header
                         (get-standard-path-header
                          project path-string
                          :header-dirs *header-dirs*)))
                 (progn
                   (update-header-graph system-header)
                   (let ((*include-file-stack*
                          (cons system-header *include-file-stack*)))
                     (symbol-table system-header in)))
                 nil)))
           (relativize (path)
             (if (keywordp path) path
                 (pathname-relativize (project-dir project)
                                      path)))
           (update-header-graph (includee)
             (let ((includee-path (relativize (original-path includee)))
                   (includer (car *include-file-stack*)))
               (when includer
                 (let ((includer-path (relativize (original-path includer))))
                   (pushnew includee-path
                            (href (included-headers (genome project))
                                  includer-path)
                            :test #'equal)
                   (pushnew includer-path
                            (href (including-files (genome project))
                                  includee-path)
                            :test #'equal)))))
           (safe-symbol-table (software)
             "Extract a symbol table from SOFTWARE, guarding for circularity."
             (cond
               ((null software) nil)
               ((member software *include-file-stack*)
                ;; Just returning nil might still result in a global
                ;; search. We found the header, we just can't use it.
                (update-header-graph software)
                (error 'circular-inclusion :header software))
               (t
                (update-header-graph software)
                (let* ((*include-file-stack* (cons software *include-file-stack*))
                       (st (symbol-table software in)))
                  #+debug-fstfi2
                  (progn
                    (format t "~3@{~a~:*~}~a~a~%" #\Space
                            software)
                    (let ((ns (name-string (original-path software))))
                      (when (some (op (search _ ns)) iftn)
                        (format t "~a~%" st))))
                  st))))
           (process-header (path-ast &key base file (global global))
             "Get the corresponding symbol table for the relative path
              represented by PATH-AST."
             #+debug-fstfi (format t "Enter process-program-header on ~a~%" path-ast)
             (when-let* ((file (or file (find-enclosing 'file-ast project include-ast))))
               (safe-symbol-table
                (get-program-header project file include-ast path-ast
                                    :base base
                                    :global global))))
           (header-symbol-table (file header-dirs path-ast)
             (some (lambda (dir)
                     (econd ((eql dir :current)
                             (process-header path-ast :file file :global nil))
                            ((member dir '(:always :system))
                             nil)
                            ((eql dir :stdinc)
                             (process-std-header project path-ast))
                            ((stringp dir)
                             (process-header path-ast :base dir :global nil))))
                   header-dirs)))
    (let* ((file (find-enclosing 'file-ast project include-ast))
           (*include-file-stack*
            ;; Initialize the stack with a top-level file, if needed.
            (or *include-file-stack*
                (and-let* ((file)
                           (sw (aget (namestring (full-pathname file))
                                     (evolve-files project)
                                     :test #'equal)))
                  (list sw))))
           (*header-dirs*
            (or (file-header-dirs project include-ast :file file)
                *header-dirs*
                *default-header-dirs*)))
      (handler-case
          (ematch include-ast
            ((c/cpp-preproc-include
              (c/cpp-path (and path-ast (c/cpp-string-literal))))
             (or (header-symbol-table file
                                      *header-dirs*
                                      path-ast)
                 ;; Fall back to global search.
                 (and global
                      (process-header path-ast :global t))))
            ((c/cpp-preproc-include
              (c/cpp-path (and path-ast (c/cpp-system-lib-string))))
             (header-symbol-table file
                                  (member :always *header-dirs*)
                                  path-ast)))
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
) ; #+(or :TREE-SITTER-C :TREE-SITTER-CPP)
