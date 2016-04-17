;;; Concrete implementation of the database interface for an external
;;; Mongo fodder database with a mongo-middle-man caching server.

(in-package :software-evolution)

(defvar cache-collection "asts-cache"
  "Name of the default collection holding caches for `fodder-collection'.")

(defvar *processing-seconds* 30
  "Default number of seconds to allow middle man processing before continuing.")

(defclass mongo-middle-database (mongo-database)
  ((cache-collection :initarg :cache-collection :accessor cache-collection
                     :type simple-string :initform cache-collection)
   (middle-host :initarg :middle-host :accessor middle-host
                :type simple-string :initform "127.0.0.1")
   (middle-port :initarg :middle-port :accessor middle-port
                :type integer :initform 27018))
  (:documentation
   "Mongo fodder database with a MONGO-MIDDLE-MAN caching server."))

(defmethod print-object ((obj mongo-middle-database) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a:~d-->~a@~a:~d"
            (middle-host obj) (middle-port obj)
            (db obj) (host obj) (port obj))))

(defmethod weighted-pick ((obj fodder-database) predicate weight
                          &key target key limit classes filter
                            (limit-considered infinity))
  (declare (ignorable key classes filter limit-considered))
  (mongo-docs-for-ids obj
    (random-elt-with-decay
     (sorted-snippet-ids obj :target target :limit limit)
     weight)))

(defmethod mongo-docs-for-ids ((obj mongo-middle-database) ids)
  "Return the doc for the given ID."
  (with-mongo-connection (:db (db obj) :host (host obj) :port (port obj))
    (mapcar #'document-cljson
            (remove-if-not {typep _ 'document}
                           (mapcar (lambda (id)
                                     (first (documents
                                             (db.find "asts" (kv "_id" id)))))
                                   ids)))))

(defmethod sorted-snippets ((obj mongo-middle-database) predicate
                            &key target key limit classes filter
                              (limit-considered infinity))
  (declare (ignorable predicate key classes filter limit-considered))
  (unless target (error "Mongo Middle Database requires a TARGET."))
  (mongo-docs-for-ids obj (sorted-snippet-ids obj :target target :limit limit)))

(defmethod submit ((obj mongo-middle-database) target)
  ;; Submit TARGET to the middle man server, return seconds since
  ;; processing began.
  (destructuring-bind (hash . seconds)
      (with-client-socket (sock stream (middle-host obj) (middle-port obj))
        (format stream "~S~%" target)
        (force-output stream)
        (read stream))
    (values seconds hash)))

(defmethod sorted-snippet-ids ((obj mongo-middle-database) &key target limit
                               &aux tag)
  (unless target (error "Mongo Middle Database requires a TARGET."))
  (multiple-value-bind (time this-tag) (submit obj target)
    (setf tag this-tag)
    (when (> (- *processing-seconds* time) 0)
      (sleep (- *processing-seconds* time))))

  (with-mongo-connection (:db (db obj) :host (host obj) :port (port obj))
    (do* ((result (db.sort (cache-collection obj) (kv "tag" tag) :field "res")
                  (db.next (cache-collection obj) cursor))
          (cursor (cursor result)
                  (cursor result))
          (documents (mapcar {get-element "id"} (documents result))
                     (append documents
                             (mapcar {get-element "id"} (documents result)))))
         ((or (zerop cursor) (and limit (>= (length documents) limit)))
          (if limit (take limit documents) documents)))))
