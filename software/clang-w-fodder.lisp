;;; clang-w-fodder.lisp --- clang software with a source fodder database

;;; clang software representation with a JSON 'database' containing
;;; AST entries as fodder for the evolution process.

;;; Each AST entry in the JSON 'database' contains source text and,
;;; where applicable, the corresponding binary bytes.
(in-package :software-evolution)

(defvar *json-database* (make-hash-table :test 'equal)
  "A database of source code snippets, grouped by AST class name.")

(defvar *json-database-bins* nil
  "The inverse cumulative distribution function for the AST class name of
a uniformly selected element of the JSON database.")

(defvar *json-database-full-stmt-bins* nil
  "The inverse cumulative distribution function for the AST class name of
a uniformly selected element of the JSON database.")

(defvar *json-database-binary-fodder* nil
  "A database of source code snippets with binary fodder.")

(defvar *fodder-selection-bias* 0.5
  "The probability that a clang-w-fodder mutation will use the code database.")

(defun populate-database-bins ()
  ;; All database bins
  (setf *json-database-bins*
        (compute-icdf-with-filter (constantly t)))

  ;; "Full statement" database bins
  (setf *json-database-full-stmt-bins*
        (compute-icdf-with-filter
            #'(lambda (k v)
                (declare (ignorable k))
                (remove-if-not {aget :full--stmt} v))))

  (assert (not (null *json-database-full-stmt-bins*))
          (*json-database-full-stmt-bins*)
          "Full stmt bins should not be null"))

(defun compute-icdf-with-filter (filter &aux bins)
  (let ((total 0)
        (totalp 0))

    (maphash (lambda (k v)
               (when (funcall filter k v)
                 (setf total (+ total (length v)))))
             *json-database*)

    (maphash (lambda (k v)
               (when (funcall filter k v)
                 (setq totalp (+ totalp (/ (length v) total)))
                 (setq bins
                       (cons (cons totalp k) bins))))
             *json-database*)

    (reverse bins)))

(define-software clang-w-fodder (clang) ())

(defmethod from-file :before ((obj clang-w-fodder) path)
  (assert (not (null *json-database-bins*))))

(defun load-json-with-caching (json-db-path)
  (let ((json-stored-db-path (make-pathname
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
        (with-open-file (json-stream json-db-path)
          (cl-store:store (json:decode-json-from-source json-stream)
                          json-stored-db-path)))))

(defun clang-w-fodder-setup-db (json-db-path)
  ;; Clobber the existing database
  (setq *json-database* (make-hash-table :test 'equal))
  (setq *json-database-bins* '())
  (setq *json-database-binary-fodder* '())

  ;; Load the snippet database and classify by AST class.
  (dolist (snippet (load-json-with-caching json-db-path))
    (let ((ast-class (aget :AST--CLASS snippet)))
      (if ast-class
          ;; This entry describes a code snippet
          (progn
            (when (aget :BINARY--contents snippet)
              (setf *json-database-binary-fodder*
                    (append *json-database-binary-fodder* (list snippet))))
            (let ((cur (gethash ast-class *json-database*)))
              (setf (gethash ast-class *json-database*) (cons snippet cur))))

          ;; This entry describes a type, perhaps
          (let ((type-id (aget :HASH snippet)))
            (when type-id
              (setf (gethash type-id *type-database*) snippet))))))

  ;; Compute the bin sizes so that (random-snippet) becomes useful.
  (populate-database-bins))

(defgeneric pick-json (clang-w-fodder &key full class pt)
  (:documentation "Return a JSON element from the fodder database.

With keyword :FULL t, select a full statement element.

With keyword argument :CLASS, select an element of the specified class.

With keyword argument :PT select an element similar to that at :PT in
CLANG-W-FODDER in a method-dependent fashion."))

(defmethod pick-json ((clang-w-fodder clang-w-fodder) &key full class pt)
  (let* ((key
          (or class ; Specific class, or just a full class, or any class.
              (cdr (find-if [{< (random 1.0)} #'car] ; Pick by bin size.
                            (if (or full (and pt (full-stmt-p clang-w-fodder pt)))
                                *json-database-full-stmt-bins*
                                *json-database-bins*)))))
         (bin (multiple-value-bind (result exists) (gethash key *json-database*)
                (unless exists
                  (error (make-condition 'mutate
                                         :text (format nil "No valid snippet for ~S" key)
                                         :obj clang-w-fodder)))
                result)))
    (random-elt bin)))

(defmethod mutate ((clang-w-fodder clang-w-fodder))
  (unless (> (size clang-w-fodder) 0)
    (error (make-condition 'mutate :text "No valid IDS" :obj clang-w-fodder)))
  (unless (> (hash-table-size *json-database*) 0)
    (error (make-condition 'mutate
             :text "No valid JSON 'database' for fodder"
             :obj clang-w-fodder)))

  (if (random-bool :bias (- 1 *fodder-selection-bias*))
      ;; Perform a standard clang mutation
      (call-next-method)
      ;; Perform a mutation using fodder
      (let* ((bad   (pick-bad  clang-w-fodder))
             (bad-stmt  (enclosing-full-stmt clang-w-fodder bad))
             (mutation (random-elt '(:replace-fodder-same :replace-fodder-full
                                     :insert-fodder  :insert-fodder-full)))
             (value (update-mito-from-snippet clang-w-fodder
                      (ecase mutation
                        (:replace-fodder-same
                         (pick-json clang-w-fodder
                                    :pt bad
                                    :class (get-ast-class clang-w-fodder bad)))
                        ((:replace-fodder-full :insert-fodder-full)
                         (pick-json clang-w-fodder :pt bad :full t))
                        (:insert-fodder
                         (pick-json clang-w-fodder)))))
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
