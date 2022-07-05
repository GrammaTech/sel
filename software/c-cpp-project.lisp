(defpackage :software-evolution-library/software/c-cpp-project
  (:nicknames :sel/software/c-cpp-project :sel/sw/c-cpp-project)
  (:use :gt/full
        :functional-trees/attrs
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c-cpp
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/parseable
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/software/compilable
        :software-evolution-library/software/directory
        :software-evolution-library/components/file)
  (:export :c/cpp-project
           :get-system-header
           :header-name
           :system-headers
           :c/cpp-root
           :*global-search-for-include-files*
           :include-not-found-warning
           :include-conflict-error
           :include-conflict-error.ast
           :include-conflict-error.candidates))

(in-package :software-evolution-library/software/c-cpp-project)
(in-readtable :curry-compose-reader-macros)

(defclass c/cpp-project
    (directory-project parseable-project compilable include-paths-mixin normal-scope)
  ()
  (:documentation "Mixin for common project functionality between C and C++."))

(define-node-class c/cpp-root (functional-tree-ast normal-scope-ast)
  ((system-headers :accessor system-headers
                   :initarg :system-headers
                   :initform nil)
   (system-headers/string->ast :accessor system-headers/string->ast
                               :initform (make-hash-table :test #'equal))
   (project-directory :accessor project-directory
                      :initarg :project-directory
                      :initform nil)
   (child-slots :initform '((project-directory . 1) (system-headers . 0))
                :allocation :class))
  (:documentation "Node for c/cpp-project objects that allows for storing the
system-headers directly in the tree. Note that system headers are lazily added
by the symbol-table attribute."))

(define-node-class c/cpp-system-header (functional-tree-ast)
  ((header-name :initarg :header-name
                :accessor header-name)
   (children :initarg :children
             :accessor children
             :initform nil)
   (child-slots :initform '((children . 0))
                :allocation :class))
  (:documentation "Node for representing system headers."))

(defmethod print-object ((self c/cpp-system-header) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (header-name self)))
  self)

(defgeneric get-system-header (project system-header-string)
  (:method (project system-header-string) nil)
  (:method (project (system-header-ast c/cpp-system-lib-string))
    (get-system-header project (trim-path-string system-header-ast)))
  (:documentation "Get the system header indicated by SYSTEM-HEADER-STRING
and add it to PROJECT."))

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

(defmethod get-system-header ((project c/cpp-project) (path-string string)
                              &aux (genome (genome project)))
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
                   (push system-header (system-headers genome)))))
        (or header-hash
            (populate-header-entry project path-string))))))

(defun trim-path-string (path-ast &aux (text (text path-ast)))
  "Return the text of PATH-AST with the quotes around it removed."
  (subseq text 1 (1- (length text))))

(defmethod from-file :around ((project c/cpp-project) file)
  (labels ((maybe-populate-header (ast)
             (match ast
               ((c/cpp-preproc-include
                 (c/cpp-path (and path (c/cpp-system-lib-string))))
                (get-system-header project (trim-path-string path))))))
    (let ((result (call-next-method)))
      (setf #1=(genome result) (make-instance 'c/cpp-root
                                              :project-directory #1#))
      (mapc #'maybe-populate-header project)
      result)))



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

(defun find-symbol-table-from-include (project include-ast
                                       &key (in (empty-map))
                                         (global *global-search-for-include-files*))
  "Find the symbol table in PROJECT for the include file
included by INCLUDE-AST.  IN is the symbol table before entry
to the include-ast.  If GLOBAL is true, search for non-system
include files in all directories of the project."
  (declare (fset:map in))
  (labels ((merge-cached-symbol-table (header)
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
                 (if-let (old (gethash node target-table))
                   ;; Preserve any previously computed attributes.
                   (append old alist)
                   alist))))
           (process-system-header (project path-ast)
             (if-let ((system-header
                       (get-system-header
                        project (trim-path-string path-ast))))
               (progn
                 (merge-cached-symbol-table system-header)
                 (symbol-table system-header in))
               nil))
           (process-relative-header (path-ast)
             "Get the corresponding symbol table for the relative path
              represented by PATH-AST."
             #+debug-fstfi (format t "Enter process-relative-header on ~a~%" path-ast)
             (if-let* ((file (find-enclosing 'file-ast project include-ast)))
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
                      (include-path-dir (substitute :back :up (pathname-directory include-path)))
                      (absolute-include-path (path-join absolute-file-path
                                                        (make-pathname :directory include-path-dir
                                                                       :defaults include-path)))
                      (include-path-string
                        (namestring (canonical-pathname absolute-include-path))))
                 #+debug-fstfi
                 (macrolet ((%d (v)
                              `(format t "~a = ~s~%" ',v ,v)))
                   (%d project-dir) (%d file-path) (%d absolute-file-path)
                   (%d include-path) (%d include-path-dir)
                   (%d absolute-include-path) (%d include-path-string))
                 (unless (equal (pathname-directory include-path) include-path-dir)
                   (warn "~A in ~A may be interpreted incorrecly in the presence of symlinks"
                         (remove #\Newline (source-text include-ast))
                         (namestring file-path)))
                 (let ((software
                         (if global
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
                                                (cdr match))))))
                             ;; Search only relative to the location of the file
                             ;; containing the include-ast.  TODO: add a set
                             ;; of paths to search in. TODO: Allow searching
                             ;; up through grandparent directories (as some
                             ;; compilers do).
                             (aget include-path-string
                                   (evolve-files project)
                                          :test #'equal))))
                   (if (or (null software)
                           (member software *include-file-stack*))
                       nil
                       (let ((*include-file-stack* (cons software *include-file-stack*)))
                         (symbol-table software in)))))
               nil)))
    (ematch include-ast
      ((c/cpp-preproc-include
        (c/cpp-path (and path (c/cpp-string-literal))))
       (or (process-relative-header path)
           ;; If a quoted header cannot be found, we should fall back
           ;; to looking in system headers.
           (process-system-header project path)
           (progn
             (warn 'include-not-found-warning :include-ast include-ast)
             (empty-map))))
      ((c/cpp-preproc-include
        (c/cpp-path (and path (c/cpp-system-lib-string))))
       (or (process-system-header project path)
           (progn
             (warn 'include-not-found-warning :include-ast include-ast)
             (empty-map)))))))

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
