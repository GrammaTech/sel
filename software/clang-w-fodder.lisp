;;; clang-w-fodder.lisp --- clang software with a source fodder database

;;; clang software representation with a Mongo database containing
;;; AST entries as fodder for the evolution process.

;;; Each AST entry in the Mongo database contains source text and,
;;; where applicable, the corresponding binary bytes.
(in-package :software-evolution)

(defvar *database* "Database utilized for fodder selection")

(defvar *fodder-selection-bias* 0.5
  "The probability that a clang-w-fodder mutation will use the code database.")

(defvar *fodder-mutation-types*
  '(:replace-fodder-same :replace-fodder-full
    :insert-fodder       :insert-fodder-full)
  "Types of valid fodder mutations")

(define-software clang-w-fodder (clang) ())

(defmethod from-file :before ((obj clang-w-fodder) path)
  (assert (not (null *database*))))

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
        snippet)))

(defmethod mutate ((clang-w-fodder clang-w-fodder))
  (unless *database*
    (error (make-condition 'mutate
             :text "No valid Mongo database for fodder"
             :obj clang-w-fodder)))
  (call-next-method))

(defmethod mutation-types-clang ((clang-w-fodder clang-w-fodder))
  (let ((existing-mutation-types (call-next-method)))
    (append (loop for mutation-type in *fodder-mutation-types*
              collecting (cons mutation-type
                               (/ *fodder-selection-bias*
                                  (length *fodder-mutation-types*))))
            (loop for mutation-type in existing-mutation-types
              collecting (cons (car mutation-type)
                               (* (- 1 *fodder-selection-bias*)
                                  (cdr mutation-type)))))))

(defmethod mutate-clang ((clang-w-fodder clang-w-fodder) mutation-type)
  (if (not (member mutation-type *fodder-mutation-types*))
      ;; This isn't one of our mutation types, dispatch to the
      ;; next software object method.
      (call-next-method)
      ;; Perform a mutation using fodder
      (let* ((bad   (pick-bad  clang-w-fodder))
             (bad-stmt  (if (full-stmt-p clang-w-fodder bad) bad
                            (enclosing-full-stmt clang-w-fodder bad)))
             (value (update-mito-from-snippet clang-w-fodder
                      (ecase mutation-type
                        (:replace-fodder-same
                         (pick-snippet clang-w-fodder
                                       :pt bad
                                       :class
                                       (get-ast-class clang-w-fodder bad)))
                        ((:replace-fodder-full :insert-fodder-full)
                         (pick-snippet clang-w-fodder :pt bad :full t))
                        (:insert-fodder
                         (pick-snippet clang-w-fodder)))
                      *database*))
             (stmt (ecase mutation-type
                     ((:replace-fodder-same :insert-fodder)
                      bad)
                     ((:replace-fodder-full :insert-fodder-full)
                      bad-stmt)))
             (op (case mutation-type
                   ((:replace-fodder-same :replace-fodder-full)
                    (list :replace
                          (cons :stmt1 stmt)
                          (cons :value1 value)))
                   ((:insert-fodder :insert-fodder-full)
                    (list :insert
                          (cons :stmt1 stmt)
                          (cons :value1 value))))))

        (apply-mutation clang-w-fodder op)
        (values clang-w-fodder (cons mutation-type (cdr op))))))
