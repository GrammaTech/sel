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
                          &key full-stmt classes limit expanded-window)
  (flet ((window ()
           ;; Only use a window if limit is less than half the size of
           ;; OBJ.
           (when (< limit (/ (size obj) 2))
             ;; When limited, take a random subset of
             ;; the corpus by requesting a window of
             ;; size equal to the fraction of LIMIT in
             ;; database SIZE times 2.
             (let ((frac (or expanded-window
                             (* (/ limit (size obj)) 2)))
                   (window-base (random 1.0)))
               (kv ($>= "random" window-base)
                   ($<= "random" (+ window-base frac))))))
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
         #-(or )
         (debug (kv)
           (format t "~a(~d)-~a-~a->~a~%" limit (size obj) classes full-stmt kv)
           kv))
    (with-mongo-connection (:db (db obj) :host (host obj) :port (port obj))
      (let ((documents
             (do* ((result
                    (db.find (source-collection obj)
                             (or (add-full-stmt (add-classes (window))) :all)
                             :limit (or limit 0))
                    (db.next (source-collection obj) cursor))
                   (cursor (cursor result)
                           (cursor result))
                   (documents (documents result)
                              (append documents (documents result))))
                  ((or (zerop cursor) (and limit (>= (length documents) limit)))
                   (mapcar #'document-cljson documents)))))
        (if (< (length documents) limit)
            ;; The difficulty here is how to narrow the range of the
            ;; database randomly returned to something reasonable, when
            ;; we don't know what fraction of the randomly returned
            ;; documents will be acceptable given our other filters
            ;; (namely CLASSES and FULL-STMT).  To heuristically do a
            ;; reasonable job of this most of the time we first multiple
            ;; the expected fraction of the database needed by 2 above
            ;; in the calculation of FRAC used to calculate the WINDOW.
            ;;
            ;; Additionally, below, if we have undershot the first time,
            ;; then we use the optional EXPANDED-WINDOW keyword argument
            ;; to pull a larger (doubled) random fraction of the
            ;; database.  This should satisfy limit in a small number of
            ;; total queries.
            (append documents
                    ;; To ensure we don't recurse infinitely when no
                    ;; documents satisfy the query, abort when our
                    ;; window is as large as the database.
                    ;;
                    ;; When LIMIT *is* less than the number of
                    ;; documents in the database, we know that in this
                    ;; call we did *not* have a random window because
                    ;; limit was greater than half the size of the
                    ;; database.
                    (when (< (* 2 limit) (size obj))
                      (find-snippets obj
                        :full-stmt full-stmt :classes classes
                        :limit (- limit (length documents))
                        :expanded-window (* 2 limit))))
            (take limit documents))))))

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
  (make-keyword (regex-replace-all "_" (string-upcase key) "--")))

(defun document-cljson (document)
  (mapcar (lambda-bind ((key . value)) (cons (cljson-key key) value))
          (document-alist document)))
