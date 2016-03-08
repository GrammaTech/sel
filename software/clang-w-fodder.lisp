;;; clang-w-fodder.lisp --- clang software with a source fodder database

;;; clang software representation with a Mongo database containing
;;; AST entries as fodder for the evolution process.

;;; Each AST entry in the Mongo database contains source text and,
;;; where applicable, the corresponding binary bytes.
(in-package :software-evolution)

(defvar *mongo-host* *mongo-default-host*
  "Host system for Mongo database")
(defvar *mongo-port* *mongo-default-port*
  "Port to connection to Mongo database")
(defvar *mongo-db-name* nil
  "Mongo database name")

(defvar *fodder-selection-bias* 0.5
  "The probability that a clang-w-fodder mutation will use the code database.")

(define-software clang-w-fodder (clang) ())

(defmethod from-file :before ((obj clang-w-fodder) path)
  (assert (not (null *mongo-db-name*))))

(defun clang-w-fodder-setup-db (db &key (host *mongo-default-host*)
                                        (port *mongo-port*))
  (setf *mongo-db-name* db)
  (setf *mongo-host* host)
  (setf *mongo-port* port))

(defgeneric pick-snippet (clang-w-fodder &key full class pt)
  (:documentation "Return a snippet from the fodder database.

With keyword :FULL t, select a full statement element.

With keyword argument :CLASS, select an element of the specified class.

With keyword argument :PT select an element similar to that at :PT in
CLANG-W-FODDER in a method-dependent fashion."))
(defmethod pick-snippet ((clang-w-fodder clang-w-fodder) &key full class pt)
  (let* ((kv (cond (class (kv "ast_class" class))
                   ((or full (and pt (full-stmt-p clang-w-fodder pt)))
                             (kv "full_stmt" t))
                   (t :all)))
         (snippet (first (find-snippets kv :n 1))))
    (if snippet
        (error (make-condition 'mutate
                               :text (format nil "No valid snippet found")))
        (progn
          (populate-type-db-from-snippet snippet)
          snippet))))

(defun find-snippets (kv &key (n most-positive-fixnum))
  "Find snippets in the Mongo database matching the predicate KV.
:N <N> - Limit to N randomly drawn snippets"
  (let ((count (get-element "n" (caadr
                                  (with-mongo-connection (:db *mongo-db-name*
                                                          :host *mongo-host*
                                                          :port *mongo-port*)
                                    (db.count "asts" kv))))))
    (when count
      (with-mongo-connection (:db *mongo-db-name*
                              :host *mongo-host*
                              :port *mongo-port*)
        (mongo-result-to-cljson (db.find "asts" kv
                                         :limit (if (<= count n) count n)
                                         :skip (if (<= count n)
                                                   0 (random (- count n)))))))))

(defun mongo-result-to-cljson (result)
  "Convert a Mongo result into a list of the form
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
    (second result)))

(defun populate-type-db-from-snippet (snippet)
  "Populate the *type-database* hash table with type information
required for snippet insertion."
  (loop for type-hash in (aget :types snippet)
        do (populate-type-db-from-type-hash type-hash))
  *type-database*)

(defun-memoized populate-type-db-from-type-hash (hash)
  "Populate the *type-database* hash table with type information
for the type in the Mongo database with the given hash"
  (let ((type (mongo-result-to-cljson (db.find "types" (kv "hash" hash)))))
    (when type
      (setf (gethash (aget :hash type) *type-database*) type))))

(defmethod mutate ((clang-w-fodder clang-w-fodder))
  (unless (> (size clang-w-fodder) 0)
    (error (make-condition 'mutate :text "No valid IDS" :obj clang-w-fodder)))
  (unless *mongo-db-name*
    (error (make-condition 'mutate
             :text "No valid Mongo database for fodder"
             :obj clang-w-fodder)))

  (if (random-bool :bias (- 1 *fodder-selection-bias*))
      ;; Perform a standard clang mutation
      (call-next-method)
      ;; Perform a mutation using fodder
      (let* ((bad   (pick-bad  clang-w-fodder))
             (bad-stmt  (if (full-stmt-p clang-w-fodder bad) bad
                            (enclosing-full-stmt clang-w-fodder bad)))
             (mutation (random-elt '(:replace-fodder-same :replace-fodder-full
                                     :insert-fodder  :insert-fodder-full)))
             (value (update-mito-from-snippet clang-w-fodder
                      (ecase mutation
                        (:replace-fodder-same
                         (pick-snippet clang-w-fodder
                                       :pt bad
                                       :class
                                       (get-ast-class clang-w-fodder bad)))
                        ((:replace-fodder-full :insert-fodder-full)
                         (pick-snippet clang-w-fodder :pt bad :full t))
                        (:insert-fodder
                         (pick-snippet clang-w-fodder)))))
             (stmt (ecase mutation
                     ((:replace-fodder-same :insert-fodder)
                      bad)
                     ((:replace-fodder-full :insert-fodder-full)
                      bad-stmt)))
             (op (case mutation
                   ((:replace-fodder-same :replace-fodder-full)
                    (list :replace
                          (cons :stmt1 stmt)
                          (cons :value1 value)))
                   ((:insert-fodder :insert-fodder-full)
                    (list :insert
                          (cons :stmt1 stmt)
                          (cons :value1 value))))))

        (apply-mutation clang-w-fodder op)
        (values clang-w-fodder (cons mutation (cdr op))))))
