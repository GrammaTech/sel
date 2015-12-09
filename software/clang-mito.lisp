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
   (globals :initarg :globals
            :accessor globals
            :initform (make-hash-table :test 'equal)
            :copier copy-hash-table)))

(defmethod union-mito ((this clang-mito) (that clang-mito))
  (merge-hash-tables (headers this) (headers that))
  (merge-hash-tables (macros this)  (macros that))
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
        (loop for key being the hash-keys of (types clang-mito)
           using (hash-value value)
           collecting (format nil "~a~%" value))
        (loop for key being the hash-keys of (globals clang-mito)
           using (hash-value value)
           collecting (format nil "~a~%" value))))))

;; If a macro with the given name already exists, do nothing; otheriwse,
;; destructively update the genome to add the macro.
(defmethod add-macro ((clang-mito clang-mito) name body)
  (setf (gethash name (macros clang-mito))
        (gethash name (macros clang-mito) body)))

;; If the named function is defined in a certain header file(s),
;; destructively update the genome with those header names.
(defmethod add-includes-for-function ((clang-mito clang-mito) name)
  (loop for header in (resolve-function-headers name)
     do (setf (gethash header (headers clang-mito)) t))
  clang-mito)
