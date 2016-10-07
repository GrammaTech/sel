;;; clang-w-fodder.lisp --- clang software with a source fodder database

;;; clang software representation with a Mongo database containing
;;; AST entries as fodder for the evolution process.

;;; Each AST entry in the Mongo database contains source text and,
;;; where applicable, the corresponding binary bytes.
(in-package :software-evolution)

(defvar *database* nil
  "Database utilized for fodder selection")

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
                                        :ast-class (if class class nil)
                                        :limit 1))))
    (if (not snippet)
        (error (make-condition 'mutate
                 :text (format nil "No valid snippet found")))
        snippet)))

(defvar *clang-w-fodder-new-mutation-types*
  '(replace-fodder-same replace-fodder-full insert-fodder insert-fodder-full))

(defvar *clang-w-fodder-mutation-types*
  (let ((orig-types (un-cumulative-distribution *clang-mutation-types*)))
    (cumulative-distribution
     (normalize-probabilities
      (append orig-types
              (mapcar {cons _ (/ (reduce #'+ (mapcar #'cdr orig-types))
                                 (length *clang-w-fodder-new-mutation-types*))}
                      *clang-w-fodder-new-mutation-types*)))))
  "Cumulative distribution of normalized probabilities of weighted mutations.
By default the weights are assigned so that half of all mutations will
be fodder mutations.")

(defmethod pick-mutation-type ((obj clang-w-fodder))
  (random-pick *clang-w-fodder-mutation-types*))

(defun pick-bad-fodder (software &optional full-stmt-p same-class)
  "Choose a bad AST and a fodder snippet"
  (let* ((bad (aget :counter (pick-bad software)))
         (bad-stmt  (if (full-stmt-p software bad) bad
                        (enclosing-full-stmt software bad)))
         (value (cond (same-class
                        (pick-snippet software
                                      :pt bad
                                      :class (get-ast-class software bad)))
                      (full-stmt-p
                        (pick-snippet software :pt bad :full t))
                      (t
                        (pick-snippet software))))
         (stmt (if full-stmt-p bad-stmt bad)))
    (list (cons :stmt1 stmt) (cons :value1 value))))

(defmethod recontextualize-mutation :around ((obj clang-w-fodder) mutation)
  (if (member (type-of mutation) *clang-w-fodder-new-mutation-types*)
      (destructuring-bind (op . properties)
          (first (build-op mutation obj))
        (declare (ignorable op))
        (let ((stmt1 (aget :stmt1 properties))
              (snippet (aget :value1 properties)))
          ;; Add includes/types/macros for the snippets to be inserted.
          ;; NOTE: If we insert a macro or type which causes
          ;; compilation errors, the number of ASTs in the software
          ;; object may change (decrease).
          (update-headers-from-snippet obj
                                       snippet
                                       *database*)
          ;; Ensure stmt1 is still in range if compilation errors are
          ;; introduced.
          (call-next-method obj
                            (at-targets mutation
                                        (list (cons :stmt1 (min stmt1
                                                                (size obj)))
                                              (cons :value1 snippet))))))
      (call-next-method)))

;; Fodder mutation classes
(define-mutation insert-fodder (clang-insert)
  ((targeter :initform #'pick-bad-fodder)))

(define-mutation insert-fodder-full (clang-insert)
  ((targeter :initform {pick-bad-fodder _ t nil})))

(define-mutation replace-fodder-same (clang-replace)
  ((targeter :initform {pick-bad-fodder _ nil t})))

(define-mutation replace-fodder-full (clang-replace)
  ((targeter :initform {pick-bad-fodder _ t nil})))
