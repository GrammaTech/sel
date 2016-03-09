;;; Concrete implementation of the database interface
;;; for an external Mongo fodder database.

(in-package :software-evolution)

;;; Each Mongo database instance needs to store the name of the
;;; database we are connecting to, the database host, and port.
(defclass mongo-database (fodder-database)
  ((db :initarg :db :accessor db :initform nil)
   (host :initarg :host :accessor host :initform *mongo-default-host*)
   (port :initarg :port :accessor port :initform *mongo-default-port*)))

(defmethod open-database ((mongo-database mongo-database) file)
  (with-open-file (json-stream file)
    (let ((mongo-config (json:decode-json-from-source json-stream)))
      (open-database-from-json mongo-database mongo-config))))

(defmethod open-database-from-json ((mongo-database mongo-database)
                                    mongo-config)
  ;; Clobber the existing database.
  (setf (db mongo-database) nil)
  (setf (host mongo-database) *mongo-default-host*)
  (setf (port mongo-database) *mongo-default-port*)

  ;; Populate parameters.
  (when (aget :mongo-db mongo-config)
        (setf (db mongo-database) (aget :mongo-db mongo-config)))
  (when (aget :host mongo-config)
        (setf (host mongo-database) (aget :host mongo-config)))
  (when (aget :port mongo-config)
        (setf (port mongo-database) (aget :port mongo-config)))

  mongo-database)

(defmethod find-snippets ((mongo-database mongo-database)
                          &key classes
                               full-stmt
                               (n most-positive-fixnum))
  (let* ((kv (cond ((and classes (listp classes)) ($in "ast_class" classes))
                   ((and classes (stringp classes)) (kv "ast_class" classes))
                   (full-stmt (kv "full_stmt" t))
                   (t :all))))
    (find-snippets-kv mongo-database kv :n n)))

(defmethod find-snippets-kv ((mongo-database mongo-database) kv
                             &key (n most-positive-fixnum))
  "Find snippets in the Mongo database matching the predicate KV.
:N <N> - Limit to N randomly drawn snippets"
  (with-mongo-connection (:db (db mongo-database)
                          :host (host mongo-database)
                          :port (port mongo-database))
    (let ((count (get-element "n" (caadr (db.count "asts" kv)))))
      (when count
        (do* ((result (db.find "asts" kv
                               :limit (min count n)
                               :skip (if (<= count n)
                                         0 (random (- count n))))
                      (db.next "asts" cursor))
              (cursor (nth 5 (first result))
                      (nth 5 (first result)))
              (documents (second result)
                         (append documents (second result))))
             ((or (zerop cursor)
                  (>= (length documents)
                      (min count n)))
                (subseq (mongo-documents-to-cljson documents) 0 (min count n)))
             ())))))

(defmethod find-types ((mongo-database mongo-database) &key hash)
  "Find types in the type database (optionally) matching the keyword
paremeter HASH"
  (find-types-kv mongo-database (if hash (kv "hash" hash) :all)))

(defmethod find-types-kv ((mongo-database mongo-database) kv)
  "Find the types in the type database matching the predicate KV."
  (with-mongo-connection (:db (db mongo-database)
                          :host (host mongo-database)
                          :port (port mongo-database))
    (do* ((result (db.find "types" kv)
                  (db.next "types" cursor))
          (cursor (nth 5 (first result))
                  (nth 5 (first result)))
          (documents (second result)
                     (append documents (second result))))
         ((zerop cursor) (mongo-documents-to-cljson documents))
         ())))

(defun mongo-documents-to-cljson (documents)
  "Convert a list of Mongo documents into a list of the form
(((:key1 . value1) (:key2 . value2) ...)) ((:key1 . value1) (:key2 . value2)))
formatted for interoperability with cl-json representations."
  (mapcar
    (lambda (document)
      (loop for k being the hash-keys of (cl-mongo::elements document)
        using (hash-value v)
        collecting (cons (make-keyword (regex-replace-all "_"
                                                          (string-upcase k)
                                                          "--"))
                         v)))
    documents))
