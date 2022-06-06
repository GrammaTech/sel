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
        :software-evolution-library/software/directory)
  (:export :c/cpp-project
           :get-system-header
           :header-name
           :system-headers
           :c/cpp-root))

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
  (clrhash *system-header-cache*)
  (clrhash *system-header-symbol-table-cache*))

(defmethod get-system-header ((project c/cpp-project) (path-string string)
                              &aux (genome (genome project)))
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
          (populate-header-entry project path-string)))))

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

(defmethod symbol-table-union ((root c/cpp-project) table-1 table-2 &key)
  (multi-map-symbol-table-union
   table-1 table-2
   :allow-multiple (multi-declaration-keys root)))

(defun find-symbol-table-from-include (project include-ast
                                       &optional (in (empty-map)))
  (declare (fset:map in))
  (labels ((merge-cached-symbol-table (header)
             (let ((cached-table
                    (ensure2 (cache-lookup *system-header-symbol-table-cache*
                                           project header)
                      (with-attr-table
                          ;; Shallow-copy the root so we get a
                          ;; separate table.
                          (copy (attrs-root *attrs*))
                        (symbol-table header (empty-map))
                        (attrs-table *attrs*))))
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
               (empty-map)))
           (process-relative-header (path-ast)
             "Get the corresponding symbol table for the relative path
              represented by PATH-AST."
             (if-let ((software
                       (aget (trim-path-string path-ast)
                             (evolve-files project)
                             :test #'equal)))
               (symbol-table software in)
               (empty-map))))
    (ematch include-ast
      ((c/cpp-preproc-include
        (c/cpp-path (and path (c/cpp-string-literal))))
       (process-relative-header path))
      ((c/cpp-preproc-include
        (c/cpp-path (and path (c/cpp-system-lib-string))))
       (process-system-header project path)))))

(defmethod symbol-table ((node c/cpp-preproc-include) &optional (in (empty-map)))
  (let ((root (attrs-root *attrs*)))
    (if (typep root 'directory-project)
        (symbol-table-union root
                            in
                            (find-symbol-table-from-include root node in))
        (call-next-method))))

(defmethod symbol-table ((node c/cpp-system-header) &optional in)
  (if-let ((root-ast (car (children node))))
    (symbol-table root-ast in)
    (empty-map)))

) ; #+(or :TREE-SITTER-C :TREE-SITTER-CPP)
