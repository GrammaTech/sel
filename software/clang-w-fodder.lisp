;;; clang-w-fodder.lisp --- clang software with a source fodder database

;;; clang software representation with a Mongo database containing
;;; AST entries as fodder for the evolution process.

;;; Each AST entry in the Mongo database contains source text and,
;;; where applicable, the corresponding binary bytes.
(in-package :software-evolution)

(defvar *database* "Database utilized for fodder selection")

(defvar *fodder-selection-bias* 0.5
  "The probability that a clang-w-fodder mutation will use the code database.")

(define-software clang-w-fodder (clang) ())

(defmethod from-file :before ((obj clang-w-fodder) path)
  (assert (not (null *database*))))

(defun clang-w-fodder-setup-db (file)
  ;; Clobbber the existing database.
  (setf *database* nil)

  (multiple-value-bind (stdout stderr exit)
    (shell (format nil "grep -i mongo-db ~a" file))
    (declare (ignorable stdout stderr))

    ;; Test if we can find mongo-db in the JSON file. If so, this is JSON
    ;; file with the Mongo configuration.  Otherwise, its a flat file
    ;; of JSON-formatted ASTs.
    (if (zerop exit)
        (setf *database* (open-database (make-instance 'mongo-database)
                                        file))
        (setf *database* (open-database (make-instance 'json-database)
                                        file)))))

(defgeneric pick-snippet (clang-w-fodder &key full class pt)
  (:documentation "Return a snippet from the fodder database.

With keyword :FULL t, select a full statement element.

With keyword argument :CLASS, select an element of the specified class.

With keyword argument :PT select an element similar to that at :PT in
CLANG-W-FODDER in a method-dependent fashion."))
(defmethod pick-snippet ((clang-w-fodder clang-w-fodder) &key full class pt)
  (let* ((snippet (first (find-snippets *database*
                                        :full-stmt 
                                          (or full
                                              (and pt
                                                   (full-stmt-p
                                                      clang-w-fodder
                                                      pt)))
                                        :classes class 
                                        :n 1))))
    (if (not snippet)
        (error (make-condition 'mutate
                               :text (format nil "No valid snippet found")))
        (progn
          (add-types-for-snippet clang-w-fodder snippet)
          snippet))))

(defmethod add-types-for-snippet ((clang-w-fodder clang-w-fodder) snippet)
  "Populate the clang-w-fodder software object's mitochondria with the
types required for snippet insertion."
  (loop for type-id in (aget :types snippet)
        do (add-type (mitochondria clang-w-fodder) *database* type-id)))

(defmethod mutate ((clang-w-fodder clang-w-fodder))
  (unless (> (size clang-w-fodder) 0)
    (error (make-condition 'mutate :text "No valid IDS" :obj clang-w-fodder)))
  (unless *database*
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
