;;; clang-deduplicate.lisp --- deduplication of clang ASTs

(in-package :software-evolution)

(define-software clang-deduplicate (clang) ())

;;; AST De-duplication
;;; To save memory, keep one canonical copy of each AST form. After
;;; reading ASTs from clang-mutate, replace them with their canonical
;;; versions (or add them to the list if they are unique).
;;;
;;; ASTs are stored in a weak hash table, so they can be freed when
;;; they are no longer used by the population.

(defvar *ast-table*
  (make-hash-table :test #'equalp #+sbcl :weakness #+sbcl :value)
  "Set of all unique ASTs.")

(defvar *ast-lock*
  (bordeaux-threads:make-lock "ast-table")
  "Lock for `*ast-table*'.")

(defun deduplicate-asts (asts ast-table)
  "Replace each AST with the equivalent from AST-TABLE.
Update AST-TABLE with any new ASTs."
  (mapcar (lambda (ast)
            (or (gethash ast ast-table)
                (setf (gethash ast ast-table) ast)))
          asts))

(defmethod clang-mutate :around ((obj clang-deduplicate) op &key script)
  "Deduplicate ASTs after reading them."
  (declare (ignorable script))
  (let ((result (call-next-method)))
    (if (eq (car op) :sexp)
        (with-lock-held (*ast-lock*)
          (deduplicate-asts result *ast-table*))
        result)))
