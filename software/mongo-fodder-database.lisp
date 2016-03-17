;;; Concrete implementation of the database interface
;;; for an external Mongo fodder database.

(in-package :software-evolution)

;;; Each Mongo database instance needs to store the name of the
;;; database we are connecting to, the database host, and port.
(defclass mongo-database (fodder-database)
  ((db :initarg :db :accessor db :type simple-string)
   (host :initarg :host :accessor host :type simple-string)
   (port :initarg :port :accessor port :type integer)))

(defmethod print-object ((obj mongo-database) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a@~a:~d" (db obj) (host obj) (port obj))))

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
  (find-types-kv mongo-database (if hash (kv "hash" hash) :all)))

(defmethod find-types-kv ((mongo-database mongo-database) kv)
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

(defmethod byte-sorted-snippets ((mongo-database mongo-database)
                                 target-bytes n-elems-to-return
                                 &key (class nil)
                                      (k-elems-to-consider
                                       most-positive-fixnum))
  (sorted-snippets-common
    mongo-database
    target-bytes
    #'byte-sorted-snippets-common
    n-elems-to-return
    :class class
    :k-elems-to-consider k-elems-to-consider))

(defmethod disasm-sorted-snippets ((mongo-database mongo-database)
                                   target-disasm n-elems-to-return
                                   &key (class nil)
                                        (k-elems-to-consider
                                         most-positive-fixnum))
  (sorted-snippets-common
    mongo-database
    target-disasm
    #'disasm-sorted-snippets-common
    n-elems-to-return
    :class class
    :k-elems-to-consider k-elems-to-consider))

(defmethod sorted-snippets-common ((mongo-database mongo-database)
                                   target sort-fn n-elems-to-return
                                   &key (class nil)
                                        (k-elems-to-consider
                                         most-positive-fixnum))
  (with-mongo-connection (:db (db mongo-database)
                          :host (host mongo-database)
                          :port (port mongo-database))
    ;; Find the number of elements in the database matching the predicate.
    (let ((count (get-element "n" (caadr (db.count "asts"
                                                   (if class
                                                       (kv "ast_class" class)
                                                       (kv "full_stmt" t)))))))
      (if (< k-elems-to-consider count)
          ;; Sample k-elems-to-consider from the full set of elements
          ;; matching the predicate.
          (mongo-sorted-snippets-unmemoized mongo-database
                                            target
                                            sort-fn
                                            n-elems-to-return
                                            :class class
                                            :k-elems-to-consider
                                             k-elems-to-consider)
          ;; Sample all elements matching the predicate in a memoized
          ;; sort.
          (mongo-sorted-snippets-memoized mongo-database
                                          target
                                          sort-fn
                                          n-elems-to-return
                                          :class class)))))

(defun-memoized mongo-sorted-snippets-memoized (mongo-database target sort-fn
                                                n-elems-to-ret
                                                &key (class nil))
  (with-mongo-connection (:db (db mongo-database)
                          :host (host mongo-database)
                          :port (port mongo-database))
    ;; Iteratively pull chunks from the database.  According to Mongo
    ;; documentation, each chunk should be no greater than 16 MB in size.
    ;; After pulling the chunk, sort by similarity, keeping no more than
    ;; N-ELEMS-TO-RETURN.  This minimizes the number of results in memory at
    ;; any given time.
    (do* ((result (db.find "asts" (if class (kv "ast_class" class)
                                            (kv "full_stmt" t))
                                  :limit 0)
                  (db.next "asts" cursor))
          (cursor (nth 5 (first result))
                  (nth 5 (first result)))
          (sorted-snippets
                  (funcall sort-fn (mongo-documents-to-cljson
                                     (second result))
                                   target
                                   n-elems-to-ret)
                  (funcall sort-fn (append sorted-snippets
                                           (mongo-documents-to-cljson
                                             (second result)))
                                   target
                                   n-elems-to-ret)))
         ((zerop cursor) sorted-snippets)
         ())))

(defun mongo-sorted-snippets-unmemoized (mongo-database target sort-fn
                                         n-elems-to-ret
                                         &key (class nil)
                                              (k-elems-to-consider
                                               most-positive-fixnum))
  (funcall sort-fn
           (if class (find-snippets-kv mongo-database (kv "ast_class" class)
                                       :n k-elems-to-consider)
                     (find-snippets-kv mongo-database (kv "full" t)
                                       :n k-elems-to-consider))
           target
           n-elems-to-ret))

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
