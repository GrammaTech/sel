;;; Concrete implementation of the database interface
;;; for an external JSON fodder database parsed and stored
;;; entirely within the current LISP image
(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)

(defclass json-database (in-memory-database)
  ((json-stream
    :initarg :json-stream :accessor json-stream
    :initform (error "JSON-STREAM field is required for DATABASE.")
    :documentation "Stream of incoming JSON.")))

(defmethod print-object ((db json-database) stream)
  (print-unreadable-object (db stream :type t)
    (when (subtypep (type-of (json-stream db)) 'file-stream)
      (format stream "~a:" (pathname (json-stream db))))
    (prin1 (length (ast-database-list db)) stream)))

(defmethod initialize-instance :after ((db json-database) &key)
  ;; Initialize (load) a new json database.
  (dolist (snippet (load-json-with-caching db))
    (cond ((and (assoc :hash snippet)
                (assoc :reqs snippet)
                (assoc :type snippet))
           ;; Types
           (setf (gethash (aget :hash snippet)
                          (type-database-ht db))
                 snippet))
          ((and (assoc :hash snippet)
                (assoc :name snippet)
                (assoc :body snippet))
           ;; Macros
           (setf (gethash (aget :hash snippet)
                          (macro-database-ht db))
                 snippet))
          (t ;; ASTs
             (when-let ((ast-class (aget :ast-class snippet)))
               (setf (ast-database-list db)
                     (cons snippet (ast-database-list db)))
               (setf (ast-database-full-stmt-list db)
                     (if (aget :full-stmt snippet)
                         (cons snippet (ast-database-full-stmt-list db))
                         (ast-database-full-stmt-list db)))
               (setf (gethash ast-class (ast-database-ht db))
                     (cons snippet
                           (gethash ast-class (ast-database-ht db)))))))))

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

(defun se-json-identifier-name-to-key (json-identifier)
  (make-keyword (string-upcase (regex-replace-all "--" json-identifier "-"))))
