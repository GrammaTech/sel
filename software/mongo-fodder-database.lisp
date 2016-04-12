;;; Concrete implementation of the database interface
;;; for an external Mongo fodder database.

(in-package :software-evolution)

;;; Each Mongo database instance needs to store the name of the
;;; database we are connecting to, the database host, and port.
(defclass mongo-database (fodder-database)
  ((db :initarg :db :accessor db
       :initform *mongo-default-db* :type simple-string)
   (host :initarg :host :accessor host
         :initform *mongo-default-host* :type simple-string)
   (port :initarg :port :accessor port
         :initform *mongo-default-port* :type integer)))

(defvar fodder-collection "asts"
  "Name of the default collection holding fodder ASTs.")

(defmethod print-object ((obj mongo-database) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a@~a:~d" (db obj) (host obj) (port obj))))

(defmethod find-snippets ((obj mongo-database) &key classes full-stmt limit)
  (let ((kv (cond (classes ($in "ast_class" classes))
                  (full-stmt (kv "full_stmt" t))
                  (t :all))))
    (with-mongo-connection (:db (db obj) :host (host obj) :port (port obj))
      ;; TODO: Sort out this `kv-with-random' stuff.
      (labels ((kv-with-random (kv-rand kv-pred)
                 (if (equal kv-pred :all)
                     kv-rand
                     (kv kv-rand kv-pred))))
        (let* ((rnd (random 1.0))
               (snippets-above (find-snippets-kv-exe-query
                                obj
                                (kv-with-random ($>= "random" rnd) kv)
                                :limit limit :field "random" :asc t))
               (snippets-below
                (when (< (length snippets-above) limit)
                  (find-snippets-kv-exe-query
                   obj
                   (kv-with-random ($< "random" rnd) kv)
                   :limit (when limit (- limit (length snippets-above)))
                   :field "random"
                   :asc nil))))
          (append snippets-below snippets-above))))))

(defgeneric find-snippets-kv-exe-query (mongo-database kv &key)
  (:documentation "Return snippets from MONGO-DATABASE matching KV.
LIMIT -- optionally limits the number of documents returned
FIELD -- optionally selects the field on which to sort results
  ASC -- sets the sort to ascending or descending"))

(defmethod find-snippets-kv-exe-query ((obj mongo-database) kv
                                       &key limit field (asc t))
  (with-mongo-connection (:db (db obj) :host (host obj) :port (port obj))
    (do* ((result (db.sort fodder-collection kv
                           :limit (or limit 0)
                           :field field
                           :asc asc)
                  (db.next fodder-collection cursor))
          (cursor (cursor result)
                  (cursor result))
          (documents (documents result)
                     (append documents (documents result))))
         ((or (zerop cursor) (and limit (>= (length documents) limit)))
          (mapcar #'document-cljson
                  (if limit (take limit documents) documents))))))

(defmethod find-types ((mongo-database mongo-database) &key hash)
  (find-types-kv mongo-database (if hash (kv "hash" hash) :all)))

(defmethod find-types-kv ((obj mongo-database) kv)
  (with-mongo-connection (:db (db obj) :host (host obj) :port (port obj))
    (do* ((result (db.find "types" kv)
                  (db.next "types" cursor))
          (cursor (cursor results)
                  (cursor results))
          (documents (documents result)
                     (append documents (documents result))))
         ((zerop cursor) (mapcar #'document-cljson documents)))))


;;; Utility functions
(defun document-alist (document)
  (hash-table-alist (cl-mongo::elements document)))

(defun cljson-key (key)
  (make-keyword (regex-replace-all "_" (string-upcase key) "--")))

(defun document-cljson (document)
  (mapcar (lambda-bind ((key . value)) (cons (cljson-key key) value))
          (document-alist document)))
