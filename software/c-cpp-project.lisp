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
           :c/cpp-root
           :*global-search-for-include-files*
           :include-not-found-warning
           :include-conflict-error
           :include-conflict-error.ast
           :include-conflict-error.candidates
           :file-preproc-defs
           :get-implicit-header
           :command-implicit-header))

(in-package :software-evolution-library/software/c-cpp-project)
(in-readtable :curry-compose-reader-macros)

(defclass c/cpp-project
    (directory-project compilation-database-project parseable-project compilable
     include-paths-mixin normal-scope)
  ()
  (:documentation "Mixin for common project functionality between C and C++."))

(define-node-class c/cpp-root (functional-tree-ast normal-scope-ast)
  ((system-headers :reader system-headers
                   :initarg :system-headers
                   :initform nil)
   (system-headers/string->ast :accessor system-headers/string->ast
                               :initform (dict))
   (implicit-headers
    :accessor implicit-headers
    :initarg :implicit-headers)
   (implicit-headers-table
    :reader implicit-headers-table
    :initform (dict))
   (project-directory :accessor project-directory
                      :initarg :project-directory
                      :initform nil)
   (child-slots :initform '((project-directory . 1) (system-headers . 0))
                :allocation :class))
  (:default-initargs
   :system-headers nil
   :implicit-headers nil)
  (:documentation "Node for c/cpp-project objects that allows for storing the
system-headers directly in the tree. Note that system headers are lazily added
by the symbol-table attribute."))

(define-node-class synthetic-header (functional-tree-ast)
  ((children :initarg :children
             :accessor children
             :initform nil)
   (child-slots :initform '((children . 0))
                :allocation :class))
  (:documentation "Superclass of synthetic headers."))

(define-node-class c/cpp-system-header (synthetic-header)
  ((header-name :initarg :header-name
                :accessor header-name))
  (:documentation "Node for representing synthetic system headers."))

(defmethod print-object ((self c/cpp-system-header) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (header-name self)))
  self)

(defgeneric get-standard-path-header (project system-header-string
                                      &key header-dirs)
  (:method (project system-header-string &key header-dirs)
    (declare (ignore header-dirs))
    nil)
  (:method (project (system-header-ast c/cpp-system-lib-string) &key header-dirs)
    (get-standard-path-header project (trim-path-string system-header-ast)
                              :header-dirs header-dirs))
  (:documentation "Get the system header indicated by SYSTEM-HEADER-STRING from
the standard path and add it to PROJECT."))

(define-node-class implicit-header (synthetic-header)
  ((source-file
    :initarg :source-file
    :reader source-file)))

(defgeneric command-implicit-header (command lang)
  (:documentation "Synthesize an implicit header for COMMAND.")
  (:method ((co command-object) lang)
    (when-let (defs (command-preproc-defs co))
      (let ((source
             (mapconcat (lambda (cons)
                          (preproc-macro-source (car cons) (cdr cons)))
                        defs
                        "")))
        (make 'implicit-header
              :source-file (command-file co)
              :children
              (collect-if
               (of-type '(or c/cpp-preproc-def c/cpp-preproc-function-def))
               (from-string lang source)))))))

(defun clear-implicit-headers (genome)
  "Clear cached implicit headers in GENOME.
For development."
  (setf (implicit-headers genome) nil)
  (clrhash (implicit-headers-table genome))
  (values))

(defgeneric get-implicit-header (project file)
  (:method ((project c/cpp-project) (file file-ast))
    (get-implicit-header project (full-pathname file)))
  (:method ((project c/cpp-project) (file software))
    (get-implicit-header project (original-path file)))
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
              (push header (implicit-headers genome)))))))))

(defmethod lookup ((obj c/cpp-root) (key string))
  ;; Enables the use of the `@' macro directly against projects.
  (lookup (project-directory obj) key))

#+(or :TREE-SITTER-C :TREE-SITTER-CPP)
(progn


;;; System Headers

(defvar *system-header-cache* (dict)
  "Store system headers that have already been parsed.")

(defvar *system-header-symbol-table-cache* (dict)
  "Cache system header symbol tables.")

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

(defun file-header-dirs (project ast &key (file (find-enclosing 'file-ast project ast)))
  (when (and project file)
    (when-let ((co (command-object project file)))
      (command-header-dirs (only-elt co)))))

(defun file-preproc-defs (project ast &key (file (find-enclosing 'file-ast project ast)))
  (when (and project file)
    (when-let ((co (command-object project file)))
      (command-preproc-defs co))))

(defmethod get-standard-path-header ((project c/cpp-project) (path-string string)
                           &key header-dirs
                           &aux (genome (genome project)))
  (when (or (no header-dirs)
            (member :stdinc header-dirs))
    (synchronized ('*system-header-cache*)
      (symbol-macrolet ((header-hash (gethash
                                      path-string
                                      (system-headers/string->ast genome))))
        (labels ((populate-header-entry (project path-string)
                   (lret ((system-header
                           (ensure2 (cache-lookup *system-header-cache* project path-string)
                             (make-instance
                              'c/cpp-system-header
                              :header-name path-string
                              :children
                              (nest (ensure-list)
                                    (parse-header-synopsis path-string :class-ast)
                                    (format-symbol :sel/sw/ts "~a-AST")
                                    (component-class project))))))
                     (setf header-hash system-header)
                     (assert (slot-exists-p (genome project) 'system-headers))
                     (setf (genome project)
                           (copy (genome project)
                                 :system-headers (cons system-header
                                                       (system-headers genome)))))))
          (or header-hash
              (populate-header-entry project path-string)))))))

(defun trim-path-string (path-ast &aux (text (text path-ast)))
  "Return the text of PATH-AST with the quotes around it removed."
  (subseq text 1 (1- (length text))))

(defmethod from-file :around ((project c/cpp-project) dir)
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


;;; Program headers.

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
signals a restartable error if their are collisions."
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
         (tweaked-include-path-dir (substitute :back :up (pathname-directory include-path)))
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


;;; Symbol Table

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

(defparameter *global-search-for-include-files* nil
  "When true, search for include files in the entire directory tree
of a project.  Used to set default value of the :GLOBAL keyword
arg of FIND-SYMBOL-TABLE-FROM-INCLUDE.  If there are duplicates
but the file is available locally, prioritize the local file.")

#+debug-fstfi2
(defparameter *include-files-to-note* nil
  "A list of strings that, if present in the namestring of
an include file, cause the symbol table of that
file to be printed for debugging purposes.")

(defun find-symbol-table-from-include (project include-ast
                                       &key (in (empty-map))
                                         (global *global-search-for-include-files*)
                                         header-dirs
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
  (labels ((merge-cached-symbol-table (header)
             "Merge the cached symbol table for HEADER into ours."
             (let ((cached-table
                    (synchronized ('*system-header-symbol-table-cache*)
                      (ensure2 (cache-lookup *system-header-symbol-table-cache*
                                             project header)
                        (with-attr-table
                            ;; Shallow-copy the root so we get a
                            ;; separate table.
                            (copy (attrs-root *attrs*))
                          (symbol-table header (empty-map))
                          (attrs-table *attrs*)))))
                   (target-table (attrs-table *attrs*)))
               (do-hash-table (node alist cached-table)
                 (setf (gethash node target-table)
                       (if-let (old (gethash node target-table))
                         ;; Preserve any previously computed attributes.
                         (append alist old)
                         alist)))))
           (process-std-header (project path-ast)
             "Retrieve a standard header from the cache."
             (if-let ((system-header
                       (get-standard-path-header
                        project (trim-path-string path-ast)
                        :header-dirs header-dirs)))
               (progn
                 (merge-cached-symbol-table system-header)
                 (symbol-table system-header in))
               nil))
           (safe-symbol-table (software)
             "Extract a symbol table from SOFTWARE, guarding for circularity."
             (cond
               ((or (null software)
                    (member software *include-file-stack* :test #'equal))
                (when software
                  (format t "Note: skipping nested include of ~a~%" software))
                nil)
               (t
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
           (header-dirs? (file-header-dirs project include-ast :file file))
           (header-dirs (or header-dirs? *default-header-dirs*)))
      (ematch include-ast
        ((c/cpp-preproc-include
          (c/cpp-path (and path-ast (c/cpp-string-literal))))
         (or (header-symbol-table file
                                  header-dirs
                                  path-ast)
             ;; Fall back to global search.
             (and global
                  (process-header path-ast :global t))))
        ((c/cpp-preproc-include
          (c/cpp-path (and path-ast (c/cpp-system-lib-string))))
         (header-symbol-table file
                              (member :always header-dirs)
                              path-ast))))))

(defmethod symbol-table ((node c/cpp-preproc-include) &optional (in (empty-map)))
  (let ((root (attrs-root *attrs*)))
    (if (typep root 'directory-project)
        (symbol-table-union root
                            in
                            (find-symbol-table-from-include root node :in in))
        (call-next-method))))

(defmethod symbol-table ((node c/cpp-system-header) &optional in)
  (if-let ((root-ast (car (children node))))
    (symbol-table root-ast in)
    (empty-map)))

) ; #+(or :TREE-SITTER-C :TREE-SITTER-CPP)
