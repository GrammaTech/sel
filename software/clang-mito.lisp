(in-package :software-evolution)

(define-software clang-mito (software)
  ((headers :initarg :headers
            :accessor headers
            :initform (make-hash-table :test 'equal)
            :copier copy-hash-table)
   (macros  :initarg :macros
            :accessor macros
            :initform (make-hash-table :test 'equal)
            :copier copy-hash-table)
   (types   :initarg :types
            :accessor types
            :initform (make-hash-table :test 'equal)
            :copier copy-hash-table)
   (sorted-types
            :initarg :sorted-types
            :accessor sorted-types
            :initform nil
            :copier copy-seq)
   (globals :initarg :globals
            :accessor globals
            :initform (make-hash-table :test 'equal)
            :copier copy-hash-table)))

(defmethod union-mito ((this clang-mito) (that clang-mito))
  (merge-hash-tables (headers this) (headers that))
  (merge-hash-tables (macros this)  (macros that))
  (loop for ty in (sorted-types that)
     do (when (not (gethash (car ty) (types this)))
          (setf (sorted-types this)
                (cons ty (sorted-types this)))))
  (merge-hash-tables (types this)   (types that))
  (merge-hash-tables (globals this) (globals that)))

(defmethod genome-string ((clang-mito clang-mito) &optional stream)
  (format stream "~a"
    (apply #'concatenate 'string
      (concatenate 'list
        (loop for key being the hash-keys of (headers clang-mito)
           collecting (format nil "#include <~a>~%" key))
        (loop for key being the hash-keys of (macros clang-mito)
           using (hash-value value)
           collecting (format nil "#define ~a~%" value))
        (loop for ty in (reverse (sorted-types clang-mito))
           collecting (format nil "~a~%" (cdr ty)))
        (loop for key being the hash-keys of (globals clang-mito)
           using (hash-value value)
           collecting (format nil "~a~%" value))))))

;; If a macro with the given name already exists, do nothing; otheriwse,
;; destructively update the genome to add the macro.
(defmethod add-macro ((clang-mito clang-mito) name body)
  (setf (gethash name (macros clang-mito))
        (gethash name (macros clang-mito) body)))

;; Add an #include directive for a given header file.
(defmethod add-include ((clang-mito clang-mito) header)
  (setf (gethash header (headers clang-mito)) t))

;; If the named function is defined in a certain header file(s),
;; destructively update the genome with those header names.
(defmethod add-includes-for-function ((clang-mito clang-mito) name)
  (loop for header in (resolve-function-headers name)
     do (add-include clang-mito header))
  clang-mito)

;; Add a type and its dependencies, transitively.
(defmethod add-type ((clang-mito clang-mito) type-id)
  (let ((type (gethash type-id *type-database*)))
    (when type
      (let ((hash (aget :HASH type))
            (decl (aget :DECL type))
            (reqs (aget :REQS type))
            (header (aget :INCLUDE type)))
        (if header
            (setf (gethash header (headers clang-mito)) t)
            (add-type-rec clang-mito hash decl reqs))))))

(defmethod add-type-rec ((clang-mito clang-mito) type-id decl reqs)
  (when (not (gethash type-id (types clang-mito)))
    (setf (gethash type-id (types clang-mito)) "// pending")
    (loop for req in reqs do (add-type clang-mito req))
    (setf (sorted-types clang-mito)
          (cons (cons type-id decl)
                (sorted-types clang-mito)))
    (setf (gethash type-id (types clang-mito)) decl)))
