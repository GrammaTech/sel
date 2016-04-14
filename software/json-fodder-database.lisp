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

  (;; Stream of incoming JSON.
   (json-stream :initarg :json-stream
                :accessor json-stream
                :initform (error "JSON-STREAM field is required for DATABASE."))
   ;; The database of source code snippets, grouped by AST class name.
   (ast-database-ht :initarg :ast-database-ht
                    :accessor ast-database-ht
                    :initform (make-hash-table :test 'equal))
   ;; The database of source code snippets as a raw list.
   (ast-database-list :initarg :ast-database-list
                      :accessor ast-database-list
                      :initform nil)
   ;; The database of source code snippets which are full statements.
   (ast-database-full-stmt-list :initarg :ast-database-full-stmt-list
                                :accessor ast-database-full-stmt-list
                                :initform nil)
   ;; An auxillary database of type snippets, grouped by hash-code
   (type-database-ht :initarg :type-database-ht
                     :accessor type-database-ht
                     :initform (make-hash-table :test 'equal))))

(defmethod print-object ((db json-database) stream)
  (print-unreadable-object (db stream :type t)
    (when (subtypep (type-of (json-stream db)) 'file-stream)
      (format stream "~a:" (pathname (json-stream db))))
    (prin1 (length (ast-database-list db)) stream)))

(defmethod initialize-instance :after ((db json-database) &key)
  ;; Initialize a new json database.
  ;; Load the snippet database.
  (dolist (snippet (shuffle (load-json-with-caching db)))
    (let ((ast-class (aget :ast--class snippet)))
      (if ast-class
          ;; This entry describes a code snippet
          (progn
            (setf (ast-database-list db)
                  (cons snippet (ast-database-list db)))
            (setf (ast-database-full-stmt-list db)
                  (if (aget :full--stmt snippet)
                      (cons snippet (ast-database-full-stmt-list db))
                      (ast-database-full-stmt-list db)))
            (let ((cur (gethash ast-class (ast-database-ht db))))
              (setf (gethash ast-class (ast-database-ht db))
                    (cons snippet cur))))
          ;; This entry describes a type, perhaps
          (let ((type-id (aget :hash snippet)))
            (when type-id
              (setf (gethash type-id (type-database-ht db)) snippet)))))))

(defmethod load-json-with-caching ((db json-database))
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
      (json:decode-json-from-source (json-stream db))))

(defmethod find-snippets ((db json-database)
                          &key classes full-stmt (n most-positive-fixnum))
  (let ((snippets (cond ((and classes (listp classes))
                         (mappend
                          (lambda (class)
                            (gethash class (ast-database-ht db)))
                          classes))
                        ((and classes (stringp classes))
                         (gethash classes (ast-database-ht db)))
                        (full-stmt
                         (ast-database-full-stmt-list db))
                        (t (ast-database-list db)))))
    (if (<= (length snippets) n)
        snippets
        (let ((start (random (- (length snippets) n))))
          (subseq snippets start (+ start n))))))

(defmethod find-types ((db json-database) &key hash)
  (if hash
      (list (gethash hash (type-database-ht db)))
      (loop for k being the hash-keys of (type-database-ht db)
         using (hash-value v)
         collecting v)))

(defmethod byte-sorted-snippets ((db json-database)
                                  target-bytes
                                  n-elems-to-return
                                  &key (class nil)
                                       (k-elems-to-consider most-positive-fixnum)
                                       (filter #'identity)
                                       (sort-predicate #'<)
                                       (similarity-fn #'diff-scalar))
  (let ((fodder (if class (find-snippets db :classes class)
                          (find-snippets db :full-stmt t))))
    (if (< k-elems-to-consider (length fodder))
      (let ((start (random (- (length fodder) k-elems-to-consider))))
        (json-sorted-snippets-nonmemoized
          (subseq fodder start (+ start k-elems-to-consider))
          target-bytes
          #'byte-sorted-snippets-common
          n-elems-to-return
          :filter filter
          :sort-predicate sort-predicate
          :similarity-fn similarity-fn))
      (json-sorted-snippets-memoized
        fodder
        target-bytes
        #'byte-sorted-snippets-common
        n-elems-to-return
        :filter filter
        :sort-predicate sort-predicate
        :similarity-fn similarity-fn))))

(defmethod disasm-sorted-snippets ((db json-database)
                                   target-disasm
                                   n-elems-to-return
                                   &key (class nil)
                                        (k-elems-to-consider most-positive-fixnum)
                                        (filter #'identity)
                                        (sort-predicate #'<)
                                        (similarity-fn #'diff-scalar))
  (let ((fodder (if class (find-snippets db :classes class)
                          (find-snippets db :full-stmt t))))
    (if (< k-elems-to-consider (length fodder))
      (let ((start (random (- (length fodder) k-elems-to-consider))))
        (json-sorted-snippets-nonmemoized
          (subseq fodder start (+ start k-elems-to-consider))
          target-disasm
          #'disasm-sorted-snippets-common
          n-elems-to-return
          :filter filter
          :sort-predicate sort-predicate
          :similarity-fn similarity-fn))
      (json-sorted-snippets-memoized
        fodder
        target-disasm
        #'disasm-sorted-snippets-common
        n-elems-to-return
        :filter filter
        :sort-predicate sort-predicate
        :similarity-fn similarity-fn))))

(defun-memoized json-sorted-snippets-memoized (fodder
                                               target
                                               sort-fn
                                               n-elems-to-return
                                               &key
                                               (filter #'identity)
                                               (sort-predicate #'<)
                                               (similarity-fn #'diff-scalar))
  (funcall sort-fn fodder target n-elems-to-return
                   :filter filter
                   :sort-predicate sort-predicate
                   :similarity-fn similarity-fn))

(defun json-sorted-snippets-nonmemoized (fodder
                                         target
                                         sort-fn
                                         n-elems-to-return
                                         &key
                                         (filter #'identity)
                                         (sort-predicate #'<)
                                         (similarity-fn #'diff-scalar))
  (funcall sort-fn fodder target n-elems-to-return
                   :filter filter
                   :sort-predicate sort-predicate
                   :similarity-fn similarity-fn))
