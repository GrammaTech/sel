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

(defvar *type-database* nil
  "A database of user-defined types.")

(defvar *fodder-selection-bias* 0.5
  "The probability that a clang-w-fodder mutation will use the code database.")

(defun select-random-bin ()
  (let ((binprob (random 1.0)))
    (cdr (find-if (lambda (datum)
                    (<= binprob (car datum))) *json-database-bins*))))

(defun select-random-full-stmt-bin ()
  (let ((binprob (random 1.0)))
    (cdr (find-if (lambda (datum)
                    (<= binprob (car datum)))
                  *json-database-full-stmt-bins*))))

(defun random-full-stmt-snippet ()
  (random-elt (gethash (select-random-full-stmt-bin)
                       *json-database*
                       '("/* bad snippet */"))))

(defun random-snippet ()
  (assert (not (null *json-database-bins*)) (*json-database-bins*)
          "*json-database-bins* must be precomputed.")
  (random-elt (gethash (select-random-bin)
                       *json-database*
                       '("/* bad snippet */"))))

(defun populate-database-bins ()
  ;; All database bins
  (setf *json-database-bins*
        (compute-icdf-with-filter (constantly t)))

  ;; "Full statement" database bins
  (setf *json-database-full-stmt-bins*
        (compute-icdf-with-filter 
            #'(lambda (k v) (remove-if-not {aget :full--stmt} v)))))

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

(defun clang-w-fodder-setup-db (json-db-path)
  ;; Clobber the existing database
  (setq *json-database* (make-hash-table :test 'equal))
  (setq *type-database* (make-hash-table :test 'equal))
  (setq *json-database-bins* '())
  (setq *json-database-binary-fodder* '())

  ;; Load the snippet database and classify by AST class.
  (dolist (snippet (with-open-file (json-stream json-db-path)
                     (json:decode-json-from-source json-stream)))
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

(defgeneric pick-any-json (clang-w-fodder pt &key)
  (:documentation "Pick any JSON element from the fodder database"))
(defgeneric pick-full-stmt-json (clang-w-fodder pt &key)
  (:documentation "Pick any full-stmt JSON element from the fodder database"))
(defgeneric pick-json-by-class (clang-w-fodder class &key)
  (:documentation "Pick any JSON element of the same class from the fodder database"))

(defmethod pick-any-json ((clang-w-fodder clang-w-fodder) pt &key)
  (prepare-code-snippet clang-w-fodder
                        pt
                        (if (is-full-stmt clang-w-fodder pt)
                            (random-full-stmt-snippet)
                            (random-snippet))))

(defmethod pick-full-stmt-json ((clang-w-fodder clang-w-fodder) pt &key)
  (prepare-code-snippet clang-w-fodder
                        pt
                        (random-full-stmt-snippet)))

(defmethod pick-json-by-class ((clang-w-fodder clang-w-fodder) class &key)
  (prepare-code-snippet clang-w-fodder
                        pt
                        (random-snippet-by-class class)))

(defun random-snippet-by-class (class)
  (let ((asts (gethash class *json-database*)))
    (if asts (random-elt asts) (random-snippet))))

(defmethod prepare-code-snippet ((clang-w-fodder clang-w-fodder)
                                 pt
                                 snippet)
  (update-mito-from-snippet clang-w-fodder snippet)
  (recontextualize clang-w-fodder snippet pt))

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
             (op (case mutation
                   (:replace-fodder-same
                    `(:replace
                      (:stmt1  . ,bad)
                      (:value1 . ,(pick-json-by-class
                                   clang-w-fodder
                                   (get-ast-class clang-w-fodder bad)))))
                   (:replace-fodder-full
                    `(:replace
                      (:stmt1  . ,bad-stmt)
                      (:value1 . ,(pick-full-stmt-json clang-w-fodder bad))))
                   (:insert-fodder
                    `(:insert
                      (:stmt1  . ,bad)
                      (:value1 . ,(random-snippet))))
                   (:insert-fodder-full
                    `(:insert
                      (:stmt1  . ,bad-stmt)
                      (:value1 . ,(pick-full-stmt-json clang-w-fodder bad)))))))

        (apply-mutation clang-w-fodder op)
        (values clang-w-fodder (cons mutation (cdr op))))))
