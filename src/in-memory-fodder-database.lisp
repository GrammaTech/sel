;;; Base class for all fodder database implementations
;;; with data stored entirely in a LISP representation
(in-package :se)

(defclass in-memory-database (fodder-database)
  ;; The current implementation of the database
  ;; has redundant data, trading space for query time.
  ;; It is assumed that all in-memeory databases will be fairly
  ;; small; otherwise, Mongo or Pliny should be utilized.
  ((ast-database-ht
    :initarg :ast-database-ht
    :accessor ast-database-ht
    :initform (make-hash-table :test 'equal)
    :documentation
    "The database of source code snippets, grouped by AST class name.")
   (ast-database-list
    :initarg :ast-database-list
    :accessor ast-database-list
    :initform nil
    :documentation "The database of source code snippets as a raw list.")
   (ast-database-full-stmt-list
    :initarg :ast-database-full-stmt-list
    :accessor ast-database-full-stmt-list
    :initform nil
    :documentation
    "The database of source code snippets which are full statements.")
   (type-database-ht
    :initarg :type-database-ht
    :accessor type-database-ht
    :initform (make-hash-table :test 'equal)
    :documentation
    "An auxillary database of type snippets, grouped by hash-code")))

(defmethod size ((db in-memory-database))
  (length (ast-database-list db)))

(defmethod database-emptyp ((db in-memory-database))
  (zerop (size db)))

(defmethod find-snippets ((db in-memory-database)
                          &key ast-class full-stmt decls limit)
  (let ((snippets (->> (cond (ast-class
                              (gethash ast-class (ast-database-ht db)))
                             (full-stmt
                              (ast-database-full-stmt-list db))
                             (t (ast-database-list db)))
                       (remove-if
                        (cond
                          ((eql decls :only)
                           (complement {aget :is-decl}))
                          (decls #'null)
                          (t {aget :is-decl}))))))
    (if (and limit (< limit (length snippets)))
        (mapcar {aref (coerce snippets 'vector)}
                (random-sample-without-replacement (length snippets) limit))
        snippets)))

(defmethod find-type ((db in-memory-database) hash)
  (list (gethash hash (type-database-ht db))))
