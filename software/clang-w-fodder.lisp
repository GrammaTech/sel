;;; clang-w-fodder.lisp --- clang software with a source fodder database

;;; clang software representation with a database containing AST
;;; entries as fodder for the evolution process.

;;; Each AST entry in the database contains source text and, where
;;; applicable, the corresponding binary bytes.
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

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
    insert-fodder-decl
    insert-fodder-decl-rep)
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
      (explode-for-loop        . 2)
      (coalesce-while-loop     . 2)
      (expand-arithmatic-op    . 2)
      (clang-cut               . 10)
      (clang-cut-full          . 25)
      (clang-insert            .  1)
      (clang-insert-same       .  2)
      (clang-insert-full       .  2)
      (clang-insert-full-same  . 10)
      (clang-swap              .  1)
      (clang-swap-same         .  2)
      (clang-swap-full         .  2)
      (clang-swap-full-same    .  5)
      (clang-move              .  5)
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
  (let* ((bad (pick-bad software))
         (bad-stmt  (if (ast-full-stmt bad) bad
                        (enclosing-full-stmt software bad)))
         (value (cond (same-class
                        (pick-snippet software
                                      :pt bad
                                      :class (ast-class bad)))
                      (full-stmt-p
                        (pick-snippet software :pt bad :full t))
                      (t
                        (pick-snippet software))))
         (stmt (if full-stmt-p bad-stmt bad)))
    (list (cons :stmt1 stmt)
          (cons :value1 value))))

(defun pick-decl-fodder (software)
  (let ((function-entry-stmts (->> (functions software)
                                   (mapcar {function-body software})
                                   (mapcar {get-immediate-children software})
                                   (mapcar {first})
                                   (remove-if #'null))))
    (if (null function-entry-stmts)
        (error (make-condition 'no-mutation-targets
                 :obj software
                 :text "Could not find any functions with non-empty bodies.")))
    (list (cons :stmt1 (random-elt function-entry-stmts))
          ;; Only return variable declarations from `pick-snippet'.
          (cons :value1 (pick-snippet software :class "DeclStmt")))))

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

(defun bind-vars-in-snippet (obj snippet pt)
  "Bind free variables in SNIPPET to in-scope vars at PT.

Returns modified text, and names of bound variables.
"
  (let* ((in-scope (mapcar [#'unpeel-bananas {aget :name}]
                           (get-vars-in-scope obj pt)))
         (var-replacements
          (mapcar
           (lambda-bind ((var index))
              (declare (ignorable index))
              (cons var (binding-for-var obj in-scope var)))
           (aget :unbound-vals snippet)))
         (replacements
          (append var-replacements
                  (mapcar
                   (lambda-bind ((fun . fun-info))
                     (cons fun
                           (car (binding-for-function obj
                                                      (prototypes obj)
                                                      (peel-bananas fun)
                                                      (third fun-info)))))
                   (aget :unbound-funs snippet)))))
    (values (peel-bananas
             (apply-replacements replacements (aget :src-text snippet)))
            (mapcar [#'peel-bananas #'cdr] var-replacements))))

(defun prepare-fodder (obj snippet pt)
  (flet
      ((var-type (in-scope var-name)
         (->> (find-if [{string= var-name} {aget :name}] in-scope)
              (find-var-type obj))))
    (multiple-value-bind (text vars)
        (bind-vars-in-snippet obj snippet pt)
      (let ((in-scope (get-vars-in-scope obj pt)))
        (if-let ((asts
                  (parse-source-snippet
                   text
                   ;; Variable and type names
                   (mapcar #'list
                           vars
                           (mapcar {var-type in-scope} vars))
                   (aget :includes snippet))))
          (car asts)
          (error (make-condition 'mutate
                                 :text "Failed to parse fodder"
                                 :obj obj)))))))

(defun prepare-fodder-op (obj op)
  (destructuring-bind (kind . properties) op
    (if-let ((snippet (aget :value1 properties))
             (pt (aget :stmt1 properties)))
      ;; When :value1 is present, rebind variables and parse the
      ;; resulting code to generate an AST.
      (cons kind (acons :value1 (prepare-fodder obj snippet pt)
                        properties))
      ;; Otherwise return the original OP
      op)))

(defmethod recontextualize-mutation :around ((obj clang-w-fodder) mutation)
  (if (member (type-of mutation) *clang-w-fodder-new-mutation-types*)
      (recontextualize-mutation obj (mapcar {prepare-fodder-op obj}
                                            (build-op mutation obj)))
      (call-next-method)))

;; Fodder mutation classes
(define-mutation insert-fodder-decl-rep (clang-insert)
  ((targeter :initform #'pick-decl-fodder-and-rename))
  (:documentation
   "First apply `insert-fodder-decl' then `rename-variable' mutations.
Ensure that the name of the inserted decl is used by
`rename-variable'."))

(defmethod build-op ((mut insert-fodder-decl-rep) (obj clang-w-fodder))
  ;; Apply the var op first as it has the higher value of STMT1.

  (sort
   (append (build-op (make-instance 'rename-variable
                                    :object obj
                                    :targets (aget :rename-variable (targets mut)))
                     obj)
           (build-op (make-instance 'insert-fodder-decl
                                    :object obj
                                    :targets (aget :decl-fodder (targets mut)))
                     obj))
   #'ast-later-p :key [{aget :stmt1} #'cdr]))

(defmethod apply-mutation :after ((obj clang-w-fodder) mutation)
  (when (member (type-of mutation) *clang-w-fodder-new-mutation-types*)
    (when-let ((snippet (aget :value1 (cdr (targets mutation)))))
      ;; Add includes/types/macros for the snippets inserted.
      (update-headers-from-snippet obj snippet *database*))))

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

(defun parse-source-snippet (snippet unbound-vals includes &key top-level)
  "Build ASTs for SNIPPET, returning a list of root ast-refs.

UNBOUND-VALS should have the form ((name clang-type) ... )

INCLUDES is a list of files to include.

SNIPPET may include one or more full statements. It should compile in
a context where all UNBOUND-VALS are defined and all INCLUDES are
included.

TOP-LEVEL indicates that the snippet is a construct which can exist
outside a function body, such as a type or function declaration.
"
  (let* ((preamble  (format nil "
/* generated includes */
~{#include ~a~&~}
/* generated declarations */
~:{~a ~a;~%~}~%
"
                            includes (mapcar «list [#'type-decl-string #'second]
                                                   #'first»
                                             unbound-vals)))
         (wrapped (format nil
                          (if top-level
                              "int __snippet_marker;~%~a~%"
                              "void main() {int __snippet_marker; ~a;}")
                          snippet))
         (obj (make-instance 'clang :genome (concatenate 'string
                                                         preamble wrapped)))
         (block-children (if top-level
                             (->> (make-ast-ref :ast (ast-root obj))
                                  (get-immediate-children obj))
                             (->> (functions obj)
                              (first)
                              (function-body obj)
                              (get-immediate-children obj)))))
    (mapc (lambda (ast)
            ;; These ASTs are not part of any genome so clear their paths
            (setf (ast-ref-path ast) nil))
          (subseq block-children
                  (1+ (position-if [{string= "__snippet_marker"} #'car
                                    #'ast-declares]
                                   block-children))))))
