;;; clang-w-fodder.lisp --- clang software with a source fodder database

;;; clang software representation with a Mongo database containing
;;; AST entries as fodder for the evolution process.

;;; Each AST entry in the Mongo database contains source text and,
;;; where applicable, the corresponding binary bytes.
(in-package :software-evolution)

(defvar *database* nil
  "Database utilized for fodder selection")

(defvar *fodder-selection-bias* 0.5
  "The probability that a clang-w-fodder mutation will use the code database.")

(defvar *fodder-mutation-types*
  '(replace-fodder-same replace-fodder-full
    insert-fodder       insert-fodder-full)
  "Types of valid fodder mutations")

(define-software clang-w-fodder (clang) ())

(defmethod from-string :before ((obj clang-w-fodder) string)
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
                                        :classes (if class (list class) nil)
                                        :limit 1))))
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
    (append (loop :for mutation-type :in *fodder-mutation-types*
               :collecting (cons mutation-type
                                 (/ *fodder-selection-bias*
                                    (length *fodder-mutation-types*))))
            (loop :for mutation-type :in existing-mutation-types
               :collecting (cons (car mutation-type)
                                 (* (- 1 *fodder-selection-bias*)
                                    (cdr mutation-type)))))))

(defun pick-bad-fodder (software &optional full-stmt-p same-class)
  "Choose a bad AST and a fodder snippet"
  (let* ((bad   (pick-bad  software))
         (bad-stmt  (if (full-stmt-p software bad) bad
                        (enclosing-full-stmt software bad)))
         (value (update-mito-from-snippet software
                 (cond
                   (same-class
                    (pick-snippet software
                                  :pt bad
                                  :class
                                  (get-ast-class software bad)))
                   (full-stmt-p
                    (pick-snippet software :pt bad :full t))
                   (t
                    (pick-snippet software)))
                 *database*))
         (stmt (if full-stmt-p bad-stmt bad)))
    (list (cons :stmt1 stmt) (cons :value1 value))))

;; Fodder mutation classes
(define-mutation insert-fodder (clang-insert)
  ((targeter :initform #'pick-bad-fodder)))

(define-mutation insert-fodder-full (clang-insert)
  ((targeter :initform {pick-bad-fodder _ t nil})))

(define-mutation replace-fodder-same (clang-replace)
  ((targeter :initform {pick-bad-fodder _ nil t})))

(define-mutation replace-fodder-full (clang-replace)
  ((targeter :initform {pick-bad-fodder _ t nil})))
