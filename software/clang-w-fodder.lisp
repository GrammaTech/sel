;;; clang-w-fodder.lisp --- clang software with a source fodder database
;;;
;;; clang software representation with a database containing AST
;;; entries as fodder for the evolution process.
;;;
;;; Each AST entry in the database contains source text and, where
;;; applicable, the corresponding binary bytes.
(defpackage :software-evolution-library/software/clang-w-fodder
  (:nicknames :sel/software/clang-w-fodder :sel/sw/clang-w-fodder)
  (:use :gt/full
        :metabang-bind
        :software-evolution-library
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang
        :software-evolution-library/components/searchable
        :software-evolution-library/components/fodder-database)
  (:import-from :functional-trees :path-later-p)
  (:export :clang-w-fodder
           :pick-snippet
           :*clang-w-fodder-mutation-types*
           :*clang-w-fodder-new-mutation-types*
           :*database*
           :replace-fodder-same
           :replace-fodder-full
           :insert-fodder-decl
           :insert-fodder-decl-rep
           :insert-fodder
           :insert-fodder-full
           :prepare-fodder
           :bind-vars-in-snippet))
(in-package :software-evolution-library/software/clang-w-fodder)
(in-readtable :curry-compose-reader-macros)

(defvar *database* nil
  "Database utilized for fodder selection")

(define-software clang-w-fodder (clang)
  ()
  (:documentation "clang software resentation with a database
containing AST entries as fodder for the evolution process."))

(defmethod from-string :before ((obj clang-w-fodder) string)
  "Ensure the fodder database has been initialized prior to creating
a `clang-w-fodder' software OBJ from STRING.
* OBJ object to be populated from source in STRING
* STRING source code to populate OBJ with
"
  (declare (ignorable string))
  (assert (not (null *database*))))

(defgeneric pick-snippet (clang-w-fodder &key full class pt decl)
  (:documentation "Return a snippet from the fodder database.

With keyword :FULL t, select a full statement element.

With keyword argument :CLASS, select an element of the specified class.

With keyword argument :PT select an element similar to that at :PT in
CLANG-W-FODDER in a method-dependent fashion.

With keyword argument :decl select a declaration."))

(defmethod pick-snippet ((obj clang-w-fodder) &key full class pt decl)
  "Return a snippet from the fodder database.
* OBJ software object to pick a fodder snippet for
* FULL select a full statement element if true
* CLASS select an element of the specified class
* PT select an element similar to PT in OBJ in a method-dependent fashion
* DECL select a decl AST if true
"
  (let* ((snippet (first (find-snippets *database*
                                        :full-stmt
                                        (or full (and pt (ast-full-stmt pt)))
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
  "Select type of mutation to apply to OBJ."
  (random-pick *clang-w-fodder-mutation-types*))

(defun pick-bad-fodder (software &optional full-stmt-p same-class)
  "Choose a bad AST and a fodder snippet.

* SOFTWARE clang object to pick a bad AST from
* FULL-STMT-P only pick full statements if true
* SAME-CLASS pick a fodder snipper with the same class as the bad AST if true
"
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
  "Pick a DeclStmt AST from the fodder database to insert into a function
body entry of SOFTWARE.
* SOFTWARE clang object to pick a DeclStmt AST for
"
  (let ((function-entry-stmts (nest (remove-if #'null)
                                    (mapcar #'first)
                                    (mapcar #'child-asts)
                                    (mapcar #'function-body)
                                    (functions software))))
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
      (setf (aget :new-var rename-target)
            (convert 'clang-ast
                     `(:var ,new-name
                            :annotations ((:name . ,new-name))
                            :syn-ctx :generic))))
    (list (cons :decl-fodder decl-target)
          (cons :rename-variable rename-target))))

(defun bind-vars-in-snippet (obj snippet pt)
  "Bind free variables in SNIPPET to in-scope vars at PT.

Returns modified text, and names of bound variables.
"
  (labels ((unpeel-bananas (text)
             (concatenate 'string "(|" text "|)"))
           (peel-bananas (text)
             (apply-replacements '(("(|" . "") ("|)" . "")) text)))
    (let* ((in-scope (mapcar [#'unpeel-bananas #'ast-name {aget :name}]
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
                             (bind (((new-fun _1 _2 _3)
                                     (binding-for-function obj
                                                           (prototypes obj)
                                                           (peel-bananas fun)
                                                           (third fun-info))))
                                   (declare (ignorable _1 _2 _3))
                                   (unpeel-bananas (ast-name new-fun)))))
                     (aget :unbound-funs snippet)))))
      (values (peel-bananas
               (apply-replacements replacements (aget :src-text snippet)))
              (mapcar [#'peel-bananas #'cdr] var-replacements)))))

(defun prepare-fodder (obj database snippet pt)
  "Prepare SNIPPET for insertion into OBJ at PT.
* OBJ clang software object to be injected with SNIPPET
* DATABASE fodder database containing snippet
* SNIPPET fodder snippet to inject
* PT point where fodder snippet is to be injected
"
  (flet
      ((var-type (in-scope var-name)
         (nest (find-var-type obj)
               (find-if [{name= var-name} {aget :name}] in-scope))))
    (multiple-value-bind (text vars)
        (bind-vars-in-snippet obj snippet pt)
      (let ((in-scope (get-vars-in-scope obj pt)))
        (if-let ((asts
                  (convert
                   'clang-ast
                   text
                   ;; Variable and type names
                   :unbound-vals (mapcar #'list
                                         vars
                                         (mapcar {var-type in-scope} vars))
                   :includes (aget :includes snippet)
                   :macros (nest (remove-if #'null)
                                 (mapcar {find-macro database}
                                         (aget :macros snippet))))))
          (car asts)
          (error (make-condition 'mutate
                                 :text "Failed to parse fodder"
                                 :obj obj)))))))

(defun prepare-fodder-op (obj op)
  "Prepare list of fodder mutation operations to be applied to OBJ.
* OBJ clang software object to be modified by OPS
* OP list of fodder operations to recontextualize and perform
"
  (destructuring-bind (kind . properties) op
    (if-let ((snippet (aget :value1 properties))
             (pt (aget :stmt1 properties)))
      ;; When :value1 is present, rebind variables and parse the
      ;; resulting code to generate an AST.
      (cons kind (acons :value1 (prepare-fodder obj *database* snippet pt)
                        properties))
      ;; Otherwise return the original OP
      op)))

(defmethod recontextualize-mutation :around ((obj clang-w-fodder) mutation)
  "Wrapper around `recontextualize-mutation' to allow for parsing and rebinding
of variables and functions in fodder snippets prior to fodder mutations.
* OBJ clang software object to be modified
* MUTATION operations to be performed
"
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
  "Return an association list with the operations to apply a
`insert-fodder-decl-rep' MUT to OBJ.
* MUT defines the targets of the insertion operation
* OBJ object to be modified by the mutation
"
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
   {path-later-p obj}
   :key [{aget :stmt1} #'cdr]))

(defmethod apply-mutation :after ((obj clang-w-fodder) mutation)
  "Inject fodder dependencies such as types, macros, and headers after a fodder
MUTATION has been applied to OBJ.
* OBJ software object modified by MUTATION
* MUTATION operations applied to OBJ
"
  (when (member (type-of mutation) *clang-w-fodder-new-mutation-types*)
    (when-let ((snippet (aget :value1 (cdr (targets mutation)))))
      ;; Add includes/types/macros for the snippets inserted.
      (update-headers-from-snippet obj snippet *database*))))

(define-mutation insert-fodder-decl (clang-insert)
  ((targeter :initform #'pick-decl-fodder))
  (:documentation "Insert a Decl fodder AST into a clang software object."))

(define-mutation insert-fodder (clang-insert)
  ((targeter :initform #'pick-bad-fodder))
  (:documentation "Insert a fodder AST into a clang software object."))

(define-mutation insert-fodder-full (clang-insert)
  ((targeter :initform {pick-bad-fodder _ t nil}))
  (:documentation "Insert a full statement fodder AST into a clang software
object"))

(define-mutation replace-fodder-same (clang-replace)
  ((targeter :initform {pick-bad-fodder _ nil t}))
  (:documentation "Replace an AST in a clang software object with a fodder
AST of the same class."))

(define-mutation replace-fodder-full (clang-replace)
  ((targeter :initform {pick-bad-fodder _ t nil}))
  (:documentation "Replace an AST in a clang software object with a full
statement fodder AST."))
