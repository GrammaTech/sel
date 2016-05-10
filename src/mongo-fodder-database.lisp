;;; Concrete implementation of the database interface
;;; for an external Mongo fodder database.

(in-package :software-evolution)

(defvar fodder-collection "asts"
  "Name of the default collection holding fodder ASTs.")

;;; Each Mongo database instance needs to store the name of the
;;; database we are connecting to, the database host, and port.
(defclass mongo-database (fodder-database)
  ((source-collection :initarg :source-collection :accessor source-collection
                      :type simple-string :initform fodder-collection)
   (db :initarg :db :accessor db
       :initform *mongo-default-db* :type simple-string)
   (host :initarg :host :accessor host
         :initform *mongo-default-host* :type simple-string)
   (port :initarg :port :accessor port
         :initform *mongo-default-port* :type integer)
   (size :reader size :type integer)))

(defmethod initialize-instance :after ((db mongo-database) &key)
  (update-size db))

(defmethod update-size ((obj mongo-database))
  (with-mongo-connection (:db (db obj) :host (host obj) :port (port obj))
    (setf (slot-value obj 'size)
          (get-element "n" (first (second (db.count "asts" :all)))))))

(defmethod print-object ((obj mongo-database) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a@~a:~d" (db obj) (host obj) (port obj))))

(defmethod find-snippets ((obj mongo-database)
                          &key full-stmt classes limit
                          &aux (pred (if (random-bool) #'< #'>=))
                               (rnd (random 1.0)))
  (flet ((add-random ()
           ;; Only use random draws if a limit is specified
           (when limit
             (cond ((equal pred #'<=) ($<= "random" rnd))
                   ((equal pred #'<)  ($<  "random" rnd))
                   ((equal pred #'>=) ($>= "random" rnd))
                   ((equal pred #'>)  ($>  "random" rnd))
                   (t nil))))
         (add-random-compliment ()
           ;; Only use random draws if a limit is specified
           (when limit
             (cond ((equal pred #'<=) ($> "random" rnd))
                   ((equal pred #'<)  ($>= "random" rnd))
                   ((equal pred #'>=) ($<  "random" rnd))
                   ((equal pred #'>)  ($<= "random" rnd))
                   (t nil))))
         (add-classes (kv)
           (if kv
               (if classes
                   (kv ($in "ast_class" classes) kv)
                   kv)
               (when classes
                 ($in "ast_class" classes))))
         (add-full-stmt (kv)
           (if kv
               (if full-stmt
                   (kv (kv "full_stmt" t) kv)
                   kv)
               (when full-stmt
                 (kv "full_stmt" t))))
         (find-snippets-kv (kv limit)
           (with-mongo-connection (:db (db obj)
                                   :host (host obj)
                                   :port (port obj))
             (do* ((result
                    (db.sort (source-collection obj) kv :limit (or limit 0)
                                                        :field "random")
                    (db.next (source-collection obj) cursor))
                   (cursor (cursor result)
                           (cursor result))
                   (documents (documents result)
                              (append documents (documents result))))
                  ((or (zerop cursor)
                       (and limit (>= (length documents) limit)))
                   (mapcar #'document-cljson documents))))))
    (let* ((snippets (find-snippets-kv
                       (or (add-full-stmt (add-classes (add-random))) :all)
                       limit)))
      (if (and limit (< (length snippets) limit))
          (take (or limit infinity)
                (append snippets
                  (find-snippets-kv
                    (or (add-full-stmt (add-classes (add-random-compliment)))
                        :all)
                    (- limit (length snippets)))))
          (take (or limit infinity) snippets)))))

(defmethod find-types ((mongo-database mongo-database) &key hash)
  (find-types-kv mongo-database (if hash (kv "hash" hash) :all)))

(defmethod find-types-kv ((obj mongo-database) kv)
  (with-mongo-connection (:db (db obj) :host (host obj) :port (port obj))
    (do* ((result (db.find "types" kv)
                  (db.next "types" cursor))
          (cursor (cursor result)
                  (cursor result))
          (documents (documents result)
                     (append documents (documents result))))
         ((zerop cursor) (mapcar #'document-cljson documents)))))


;;; Utility functions
(defun document-alist (document)
  (hash-table-alist (cl-mongo::elements document)))

(defun cljson-key (key)
  (make-keyword (regex-replace-all "_" (string-upcase key) "-")))

(defun document-cljson (document)
  (mapcar (lambda-bind ((key . value)) (cons (cljson-key key) value))
          (document-alist document)))
