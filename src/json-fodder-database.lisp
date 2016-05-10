;;; Concrete implementation of the database interface
;;; for an external JSON fodder database.
(in-package :software-evolution)

(defclass json-database (fodder-database)
  ;; The current implementation of the database
  ;; has redundant data, trading space for query time.
  ;; It is assumed that all JSON databases will be fairly small;
  ;; otherwise, Mongo should be utilized.

  ;; TODO: We could potentially support all query types in a smaller
  ;; memory footprint by stable sorting the ast-database-list first by
  ;; full-stmt and then by AST class.  We could keep an index where the full
  ;; statements end.  Additionally, we could keep indexes where each AST
  ;; class begins and ends.
  ((json-stream
    :initarg :json-stream :accessor json-stream
    :initform (error "JSON-STREAM field is required for DATABASE.")
    :documentation "Stream of incoming JSON.")
   (ast-database-ht
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
    "An auxillary database of type snippets, grouped by hash-code")
   (size :reader size :type integer)))

(defmethod print-object ((db json-database) stream)
  (print-unreadable-object (db stream :type t)
    (when (subtypep (type-of (json-stream db)) 'file-stream)
      (format stream "~a:" (pathname (json-stream db))))
    (prin1 (length (ast-database-list db)) stream)))

(defmethod initialize-instance :after ((db json-database) &key)
  ;; Initialize (load) a new json database.
  (dolist (snippet (shuffle (load-json-with-caching db)))
    (let ((ast-class (aget :ast-class snippet)))
      (if ast-class
          ;; This entry describes a code snippet
          (progn
            (setf (ast-database-list db)
                  (cons snippet (ast-database-list db)))
            (setf (ast-database-full-stmt-list db)
                  (if (aget :full-stmt snippet)
                      (cons snippet (ast-database-full-stmt-list db))
                      (ast-database-full-stmt-list db)))
            (let ((cur (gethash ast-class (ast-database-ht db))))
              (setf (gethash ast-class (ast-database-ht db))
                    (cons snippet cur))))
          ;; This entry describes a type, perhaps
          (let ((type-id (aget :hash snippet)))
            (when type-id
              (setf (gethash type-id (type-database-ht db)) snippet))))))
  (setf (slot-value db 'size) (length (ast-database-list db))))

(defmethod load-json-with-caching ((db json-database))
  (let ((json:*identifier-name-to-key* 'se-json-identifier-name-to-key))
    (if (subtypep (type-of (json-stream db)) 'file-stream)
        (let* ((json-db-path (pathname (json-stream db)))
               (json-stored-db-path (make-pathname
                                     :directory (pathname-directory json-db-path)
                                     :name (pathname-name json-db-path)
                                     :type "dbcache")))
          (if (and (probe-file json-stored-db-path)
                   (> (file-write-date json-stored-db-path)
                      (file-write-date json-db-path)))
              ;; Cache exists and is newer than the original
              ;; JSON database; use the cache.
              (cl-store:restore json-stored-db-path)
              ;; Cache does not yet exist or has been invalidated;
              ;; load from JSON and write back to the cache.
              (cl-store:store (json:decode-json-from-source (json-stream db))
                              json-stored-db-path)))
        (json:decode-json-from-source (json-stream db)))))

(defmethod find-snippets ((db json-database) &key classes full-stmt limit)
  (let ((snippets (cond (classes
                         (mappend
                          (lambda (class)
                            (gethash class (ast-database-ht db)))
                          classes))
                        (full-stmt
                         (ast-database-full-stmt-list db))
                        (t (ast-database-list db)))))
    (if (and limit (< limit (length snippets)))
        (let ((start (random (- (length snippets) limit))))
          (subseq snippets start (+ start limit)))
        snippets)))

(defmethod find-types ((db json-database) &key hash)
  (if hash
      (list (gethash hash (type-database-ht db)))
      (loop for k being the hash-keys of (type-database-ht db)
         using (hash-value v)
         collecting v)))

(defun se-json-identifier-name-to-key (json-identifier)
  (make-keyword (string-upcase (regex-replace-all "--" json-identifier "-"))))
