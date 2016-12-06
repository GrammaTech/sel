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

(defgeneric pick-snippet (clang-w-fodder &key full class pt decl)
  (:documentation "Return a snippet from the fodder database.

With keyword :FULL t, select a full statement element.

With keyword argument :CLASS, select an element of the specified class.

With keyword argument :PT select an element similar to that at :PT in
CLANG-W-FODDER in a method-dependent fashion.

With keyword argument :decl select a declaration."))

(defmethod pick-snippet ((obj clang-w-fodder) &key full class pt decl)
  (let* ((snippet (first (find-snippets *database*
                                        :full-stmt
                                        (or full (and pt (full-stmt-p obj pt)))
                                        :ast-class (if class class nil)
                                        :decls (if decl :only nil)
                                        :limit 1))))
    (if (not snippet)
        (error (make-condition 'mutate
                 :text (format nil "No valid snippet found")))
        snippet)))

(defvar *clang-w-fodder-new-mutation-types*
  '(replace-fodder-same
    replace-fodder-full
    insert-fodder
    insert-fodder-full)
  "Fodder mutation types.")

(defvar *clang-w-fodder-mutation-types*
  (cumulative-distribution
   (normalize-probabilities
    '((cut-decl                . 10)    ; All values are /200 total.
      (swap-decls              . 10)
      (insert-fodder-decl      . 10)
      (rename-variable         . 10)
      (clang-promote-guarded   . 10)
      (clang-cut               . 10)
      (clang-cut-full          . 25)
      (clang-insert            .  1)
      (clang-insert-same       .  2)
      (clang-insert-full       .  2)
      (clang-insert-full-same  . 10)
      (clang-swap              .  1)
      (clang-swap-same         .  2)
      (clang-swap-full         .  2)
      (clang-swap-full-same    . 10)
      (clang-replace           .  1)
      (clang-replace-same      .  2)
      (clang-replace-full      .  2)
      (clang-replace-full-same . 10)
      (replace-fodder-same     . 25)
      (replace-fodder-full     . 15)
      (insert-fodder           . 15)
      (insert-fodder-full      . 15))))
  "Cumulative distribution of normalized probabilities of weighted mutations.
Currently the weights are assigned so that we roughly preserve the
total fraction of decl, swap, and cut mutations from
`*clang-mutation-types*'.  By contrast the fraction of insert, swap,
and replace mutations are decreased precipitously to make room for the
new fodder mutations.  Fodder mutations make up 2/5 of all
mutations.")

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

(defun pick-decl-fodder (software)
  (unless (functions software)
    (error (make-condition 'no-mutation-targets
             :obj software
             :text "Could not find any functions.")))
  (list (cons :stmt1 (->> (random-elt (functions software))
                          (aget :children)
                          (first)
                          (get-ast software)
                          (aget :children)
                          (first)))
        ;; Only return variable declarations from `pick-snippet'.
        (cons :value1 (pick-snippet software :class "Var" :decl :only))))

(defmethod recontextualize-mutation :around ((obj clang-w-fodder) mutation)
  (when (member (type-of mutation) *clang-w-fodder-new-mutation-types*)
    (destructuring-bind (op . properties)
        (first (build-op mutation obj))
      (declare (ignorable op))
      (let ((snippet (aget :value1 properties)))
        ;; Add includes/types/macros for the snippets to be inserted.
        (update-headers-from-snippet obj
                                     snippet
                                     *database*))))
  (call-next-method))

;; Fodder mutation classes
(define-mutation insert-fodder-decl (clang-insert)
  ((targeter :initform #'pick-decl-fodder)))

(define-mutation insert-fodder (clang-insert)
  ((targeter :initform #'pick-bad-fodder)))

(define-mutation insert-fodder-full (clang-insert)
  ((targeter :initform {pick-bad-fodder _ t nil})))

(define-mutation replace-fodder-same (clang-replace)
  ((targeter :initform {pick-bad-fodder _ nil t})))

(define-mutation replace-fodder-full (clang-replace)
  ((targeter :initform {pick-bad-fodder _ t nil})))
