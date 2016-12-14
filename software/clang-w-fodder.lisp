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
    insert-fodder-full
    insert-fodder-decl)
  "Fodder mutation types.")

(defvar *clang-w-fodder-mutation-types*
  (cumulative-distribution
   (normalize-probabilities
    '((cut-decl                . 10)    ; All values are /200 total.
      (swap-decls              . 10)
      (insert-fodder-decl      . 4)
      (insert-fodder-decl-rep  . 6)
      (rename-variable         . 10)
      (clang-promote-guarded   . 4)
      (explode-for-loop        . 4)
      (coalesce-while-loop     . 2)
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
                          (mapcar {get-ast software})
                          (remove-if [{string= "ParmVar"} {aget :ast-class}])
                          (first)
                          (aget :children)
                          (first)))
        ;; Only return variable declarations from `pick-snippet'.
        (cons :value1 (pick-snippet software :class "Var" :decl :only))))

(defun pick-decl-fodder-and-rename (software)
  "Combination of `pick-decl-fodder' and `pick-rename-variable'.
When the decl picked for insertion provides a new name, update the
rename target to use this new name."
  (let ((decl-target (pick-decl-fodder software))
        (rename-target (pick-rename-variable software)))
    (when-let ((new-name (car (aget :declares (aget :value1 decl-target)))))
      (setf (aget :new-var rename-target) new-name))
    (list (cons :decl-fodder decl-target)
          (cons :rename-variable rename-target))))

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
(define-mutation insert-fodder-decl-rep (clang-insert)
  ((targeter :initform #'pick-decl-fodder-and-rename))
  (:documentation
   "First apply `insert-fodder-decl' then `rename-variable' mutations.
Ensure that the name of the inserted decl is used by
`rename-variable'."))

(defmethod build-op ((mut insert-fodder-decl-rep) (obj clang-w-fodder))
  ;; Apply the var op first as it has the higher value of STMT1.
  (list (cons :rename-variable
              (build-op (make-instance 'rename-variable
                          :object obj
                          :targets (aget :rename-variable (targets mut)))
                        obj))
        (cons :decl-fodder
              (build-op (make-instance 'insert-fodder-decl
                          :object obj
                          :targets (aget :decl-fodder (targets mut)))
                        obj))))

(defmethod apply-mutation ((obj clang) (mut insert-fodder-decl-rep))
  ;; For `insert-fodder-decl-rep' we recontextualize and apply in two
  ;; interleaved steps to ensure that the changes from the first
  ;; mutation are in place before the second mutation is
  ;; recontextualized.
  (restart-case
      (let ((ops (build-op mut obj)))
        (apply-mutation-ops obj (recontextualize-mutation
                                 obj (aget :decl-fodder ops)))
        (apply-mutation-ops obj (recontextualize-mutation
                                 obj (aget :rename-variable ops))))
    (skip-mutation ()
      :report "Skip mutation and return nil"
      (values nil 1))
    (tidy ()
      :report "Call clang-tidy before re-attempting mutation"
      (clang-tidy obj)
      (apply-mutation obj mut))
    (mutate ()
      :report "Apply another mutation before re-attempting mutations"
      (mutate obj)
      (apply-mutation obj mut))))

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
