;;; super-mutant-clang.lisp --- Super mutant specialization for Clang software.
(defpackage :software-evolution-library/software/super-mutant-clang
  (:nicknames :sel/software/super-mutant-clang :sel/sw/super-mutant-clang)
  (:use :gt/full
        :metabang-bind
        :software-evolution-library
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang
        :software-evolution-library/software/super-mutant))
(in-package :software-evolution-library/software/super-mutant-clang)
(in-readtable :curry-compose-reader-macros)

(defmethod create-super-soft ((base clang) mutants)
  (labels
      ((ensure-functions-compatible (f1 f2 mutant)
         (unless (ast-equal-p f1 f2)
           (unless (name= (ast-name f1) (ast-name f2))
             (error (make-condition 'mutate
                                    :text "Mismatched function names"
                                    :obj mutant)))
           (unless (eq (ast-void-ret f1) (ast-void-ret f2))
             (error (make-condition 'mutate
                                    :text "Mismatched return types"
                                    :obj mutant)))
           (unless (and (eq (ast-varargs f1) (ast-varargs f2))
                        (ast-args-equal (ast-args f1) (ast-args f2)))
             (error (make-condition 'mutate
                                    :text "Mismatched function arguments"
                                    :obj mutant)))))
       (process-ast (asts)
         "Process variants of ASTs to determine the edits necessary.

Returns a pair of (base-ast . variants).

base-ast indicates the AST in the base mutant to replace. If it's nil,
then an insert is required.

There are several cases here:
* Global decl, present in base mutant
  No edits needed, return (base-ast . nil)
* Global decl, missing in base mutant
  Return (nil . variant-ast)
* Function, identical in all mutants where it appears.
  Return (base-ast . variant-ast). Will insert if base-ast is nil.
* Function, differs across mutants
  Return (base-ast . variants-hashtable). Will generate a super-function to
  replace base-ast.
"
         (let* ((non-null-asts (remove nil asts))
                (head (car non-null-asts)))
           (if (not (function-decl-p head))
               ;; Non-function decls
               (progn
                 ;; Top-level decls must be identical across variants.
                 (mapc (lambda (ast)
                         (unless (ast-equal-p head ast)
                           (error (make-condition 'mutate
                                                  :text
                                                  "Mismatched global decls"))))
                       non-null-asts)
                 (if (car asts)
                     ;; No change
                     (list (car asts))
                     ;; Insert decls
                     (cons nil (car non-null-asts))))
               ;; Functions
               (if (every {ast-equal-p head} non-null-asts)
                   ;; All identical (but may be missing in some mutants)
                   (cons (car asts) (car non-null-asts))
                   ;; Function bodies may differ as long as name and arguments
                   ;; are the same. Collect all unique variants, along with the
                   ;; mutants they came from.
                   (let ((variants (make-hash-table :test #'equalp)))
                     (mapc (lambda-bind ((i ast) mutant)
                             (when ast
                               (ensure-functions-compatible head ast mutant)
                               (let ((body (function-body mutant ast)))
                                 (unless body
                                   (error
                                    (make-condition 'mutate
                                                    :text
                                                    (format nil
                                                            "Missing body for ~a"
                                                            (ast-name ast))
                                                    :obj mutant)))
                                 (if-let ((value (gethash ast variants)))
                                   (pushnew i (third value))
                                   (setf (gethash ast variants)
                                         (list ast body (list i)))))))
                           (indexed asts)
                           mutants)
                     (cons (when (car asts) (function-body base head))
                           (hash-table-values variants)))))))
       (make-super-body (variants &key make-decl)
         "Create body for a super-mutant function.

The body is a switch statement with cases for each variant. If MAKE-DECL is
true, create a complete function decl which contains the body."
         (let* ((getenv (make-call-expr "getenv"
                                        (list (make-literal "SUPER_MUTANT"))
                                        :generic))
                (body
                 (nest (make-block)
                       (list)
                       (make-switch-stmt
                        (make-call-expr "atoi" (list getenv)
                                        :generic))
                       (mapcar (lambda-bind ((decl body indices))
                                 (declare (ignorable decl))
                                 ;; Add default case to make the
                                 ;; compiler happy.
                                 (list (if (ast-equal-p decl (caar variants))
                                           (cons t indices)
                                           indices)
                                       (list body (make-break-stmt))))
                               variants))))
           (if make-decl
               (let ((decl (caar variants)))
                 (replace-nth-child decl
                                    (position-if «and [{subtypep _ 'ast}
                                                       #'type-of]
                                                      [{eq :CompoundStmt}
                                                       #'ast-class]»
                                                 (ast-children decl))
                                    body))
               body)))
       (create-mutation-ops (variants-list)
         (let ((inserts nil))
           (iter (for (orig . variants) in variants-list)
                 (cond
                   (orig
                    ;; We've reached an AST in the base genome. If we're waiting
                    ;; to insert any ASTs, handle them now.
                    (when inserts
                      (appending
                       (mapcar (lambda (ast)
                                 `(:insert
                                   (:stmt1 . ,orig)
                                   ;; For function with variants, insert the
                                   ;; merged version. Otherwise just insert the
                                   ;; ast.
                                   (:value1 .
                                    ,(if (consp ast)
                                         (make-super-body ast :make-decl t)
                                         ast))))
                               (reverse inserts)))
                      (setf inserts nil))
                    ;; Function with multiple variants. Replace the original
                    ;; function body with the merged version.
                    (when (consp variants)
                      (collecting
                       `(:set (:stmt1 . ,orig)
                              (:value1 . ,(make-super-body variants))))))
                   ;; New AST not present in base genome. Save these
                   ;; and insert them before the next AST in the base
                   ;; genome.
                   (variants
                    (push variants inserts))))))
       (sort-mutation-ops (ops)
         (sort ops #'ast-later-p :key [{aget :stmt1} #'cdr]))
       (add-stdlib-include (obj)
         ;; Needed for getenv()
         (add-include obj "<stdlib.h>")))
    (nest (add-stdlib-include)
          (apply-mutation-ops (copy base))
          (sort-mutation-ops)
          ;; Create mutations to insert super-functions into genome of
          ;; first variant
          (create-mutation-ops)
          (remove nil)
          (mapcar #'process-ast)
          (collate-ast-variants)
          ;; Collect top-level ASTs and their declared identifiers
          (mapcar [{mapcar (lambda (ast)
                             (cons (mapcar #'ast-name (ast-declares ast)) ast))}
                   #'roots]
                  mutants))))
