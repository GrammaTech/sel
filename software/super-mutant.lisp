;;; super-mutant.lisp --- evaluate multiple mutants in a single
;;; phenome
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(define-software super-mutant (software)
  ((mutants :accessor mutants :initarg :mutants :initform nil
            :type list
            :documentation "Mutants to combine.")
   (super-soft :accessor super-soft :initform nil
               :type (or null software)
               :documentation "Software object containing combined genome.")
   (phenome-results :accessor phenome-results :initform nil
                    :type list
                    :documentation "Cached results from PHENOME method.")))

(defmethod phenome ((obj super-mutant) &key (bin (temp-file-name)))
  (phenome (super-soft obj) :bin bin))

(defmethod phenome-wrapper ((obj super-mutant) wrapper which-mutant &key bin)
  "Create a script which wraps phenome of OBJ to run the desired mutant.
* OBJ the super-mutant
* WRAPPER the path for the wrapper script
* WHICH-MUTANT the index of the mutant to enable
* BIN the path for the super-mutant phenome

First generates a phenome unless it has already been cached. Then
writes out a wrapper script which sets the SUPER_MUTANT environment
variable to WHICH_MUTANT before running the phenome.

Returns the results of the PHENOME call, except that WRAPPER replaces
the first return value.
"
  (destructuring-bind (result-bin . rest)
      (or (phenome-results obj)
          (setf (phenome-results obj)
                (multiple-value-list (phenome obj :bin bin))))
    ;; Create a wrapper script to ensure the correct mutant
    ;; is evaluated
    (cons (when result-bin
            (string-to-file (format nil "#!/bin/sh~%SUPER_MUTANT=~d ~a"
                                    which-mutant
                                    result-bin)
                            wrapper)
            (shell "chmod +x ~s" wrapper)
            wrapper)
          rest)))

(defmethod evaluate ((test function) (super super-mutant)
                     &rest extra-keys &key &allow-other-keys)
  (declare (ignorable extra-keys))
  (with-temp-file (super-bin)
    (flet
        ((make-phenome-proxy (obj which-mutant)
           ;; Make a copy of OBJ and dynamically add an eql-specialized
           ;; version of PHENOME which implements super-mutant behavior.
           (flet ((proxy-phenome (obj &key (bin (temp-file-name)))
                    (declare (ignorable obj))
                    (values-list (phenome-wrapper super bin which-mutant
                                                  :bin super-bin))))

             (let* ((proxy (copy obj))
                    (method
                     (make-instance 'standard-method
                                    :name 'phenome
                                    :function #'proxy-phenome
                                    :lambda-list '(obj &key bin)
                                    :specializers
                                    (list (intern-eql-specializer proxy)))))
               (add-method #'phenome method)
               (values proxy method)))))

      (mapc (lambda-bind ((i obj))
              ;; Create a proxy to override phenome behavior, then
              ;; evaluate that in the normal way.
              (multiple-value-bind (proxy method)
                  (make-phenome-proxy obj i)
                (unwind-protect
                     (multiple-value-bind (fit extra)
                         (evaluate test proxy)
                       (setf (fitness obj) fit)
                       (setf (fitness-extra-data obj) extra))
                  ;; Clean up the temporary proxy method
                  (remove-method #'phenome method))))
            (indexed (mutants super))))))

(defmethod genome ((obj super-mutant))
  (genome (super-soft obj)))

(defmethod super-soft :before ((obj super-mutant))
  (with-slots (super-soft) obj
    (unless super-soft
      (setf super-soft (create-super-soft (car (mutants obj))
                                          (mutants obj))))))

(defun collate-ast-variants (ast-roots)
  "Collect and align variants of top-level ASTs.

* AST-ROOTS a (key . ast) pair for each top-level AST across all
  mutants. Grouped into sublists per mutant.

Returns a list of variants for each key. Keys are merged
across mutants, while preserving the original order as much as
possible. The position of each variant in the variants list matches
the position of the corresponding mutant in AST-ROOTS. In the event of
inserted or deleted ASTs, the variant list will contain nils.

We assume that the order of common keys is consistent across all
variants. If this is not true, the result may have duplicate keys.
"
  (iter (for roots in ast-roots)
        (for i upfrom 0)
        ;; Dummy value avoids special cases due to empty result list
        (with result = (list 'dummy))
        (iter (for (key . variant) in roots)
              (with previous = result)
              (with current = (cdr result))

              ;; Search for a matching key in the results
              (if-let ((matching (find key (tails current)
                                       :key #'caar
                                       :test #'equal)))
                ;; If found, advance to that key and add the new variant there
                (progn
                  ;; add a nil in each skipped key
                  (iter (for c on current)
                        (while (not (eq c matching)))
                        (push nil (cdr (car c))))
                  (push variant (cdr (car matching)))
                  (setf previous matching
                        current (cdr matching)))
                ;; Otherwise, insert new entry at current position,
                ;; with nils for the previous mutants
                (progn
                  (setf (cdr previous) (cons (list* key variant
                                                    (make-list i))
                                             current)
                        previous (cdr previous))))

              ;; Add nil to remaining keys
              (finally (iter (for c on current)
                             (push nil (cdr (car c))))))
        (finally
         ;; Strip dummy value and keys, and put variants in correct order.
         (return (mapcar [#'reverse #'cdr] (cdr result))))))

(defmethod create-super-soft ((base clang) mutants)
  (labels
      ((ensure-functions-compatible (f1 f2 mutant)
         (unless (eq f1 f2)
           (unless (string= (ast-name f1) (ast-name f2))
             (error (make-condition 'mutate
                                    :text "Mismatched function names"
                                    :obj mutant)))
           (unless (eq (ast-void-ret f1) (ast-void-ret f2))
             (error (make-condition 'mutate
                                    :text "Mismatched return types"
                                    :obj mutant)))
           (unless (and (eq (ast-varargs f1) (ast-varargs f2))
                        (equal (ast-args f1) (ast-args f2)))
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
                 ;; Don't collect anything.
                 (mapc (lambda (ast)
                         (unless (eq (ast-ref-ast head) (ast-ref-ast ast))
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
               (if (every [{eq (ast-ref-ast head)} #'ast-ref-ast]
                          non-null-asts)
                   ;; All identical (but may be missing in some mutants)
                   (cons (car asts) (car non-null-asts))
                   ;; Function bodies may differ as long as name and arguments
                   ;; are the same. Collect all unique variants, along with the
                   ;; mutants they came from.
                   (let ((variants (make-hash-table)))
                     (mapc (lambda-bind ((i ref) mutant)
                             (when ref
                               (ensure-functions-compatible head ref mutant)
                               (let ((ast (ast-ref-ast ref))
                                     (body (function-body mutant ref)))
                                 (unless body
                                   (error
                                    (make-condition 'mutate
                                                    :text
                                                    (format nil
                                                            "Missing body for ~a"
                                                            (ast-name ref))
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
                 (->> (mapcar (lambda-bind ((decl body indices))
                                (declare (ignorable decl))
                                ;; Add default case to make the
                                ;; compiler happy.
                                (list (if (eq decl (caar variants))
                                          (cons t indices)
                                          indices)
                                      (list body (make-break-stmt))))
                              variants)
                      (make-switch-stmt
                       (make-call-expr "atoi" (list getenv)
                                       :generic))
                      (list)
                      (make-block))))
           (if make-decl
               (let ((decl (caar variants)))
                 (->> (replace-nth-child decl
                                         (position-if «and #'listp
                                                           [{eq :CompoundStmt}
                                                            #'ast-class
                                                            #'car]»
                                                      (cdr decl))
                                         (ast-ref-ast body))
                      (make-ast-ref :ast)))
               body)))
       (create-mutation-ops (variants-list)
         (iter (for (orig . variants) in variants-list)
               (with inserts = nil)
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
                                 (:value1 . ,(if (consp ast)
                                                 (make-super-body ast
                                                                  :make-decl t)
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
    (-<>>
     ;; Collect top-level ASTs and their declared identifiers
     (mapcar [{mapcar (lambda (ast) (cons (ast-declares ast) ast))} #'roots]
             mutants)
     (collate-ast-variants)
     (mapcar #'process-ast)
     (remove nil)
     (create-mutation-ops)
     (sort <>
           #'ast-later-p :key [{aget :stmt1} #'cdr])
     ;; Substitute super-functions into genome of first variant
     (apply-mutation-ops (copy base))
     (add-include <> "<stdlib.h>"))))

(defmethod create-super-soft ((base project) mutants)
  (assert (every (lambda (mutant)
                   (and (eq (length (evolve-files base))
                            (length (evolve-files mutant)))
                        (every #'string=
                               (mapcar #'car (evolve-files base))
                               (mapcar #'car (evolve-files mutant)))))
                 mutants)
          nil
          "All project mutants must have the same file names.")

  (let ((super (copy base)))
    (->> (apply #'mapcar                ; create super-soft for each file
                (lambda (&rest mutants)
                  (create-super-soft (car mutants) mutants))
                (mapcar [{mapcar #'cdr} #'evolve-files] mutants))
         (mapcar (lambda (base-file super-file) ; combine with filenames
                   (cons (car base-file) super-file))
                 (evolve-files base))
         (setf (evolve-files super)))
    super))


(defvar *mutants-at-once* 4
  "Number of variants to combine into a single super-mutant.")

;;; Evolution
(defun super-evolve (test &key
                    max-evals max-time period period-fn
                    every-pre-fn every-post-fn filter analyze-mutation-fn)

  (unless *start-time* (setq *start-time* (get-internal-real-time)))
  (setq *running* t)
  (flet
      ((fitness-check (variant)
         (assert (fitness variant) (variant) "Variant with no fitness"))
       (new-individuals ()
         (iter (with count = 0)
               (restart-case
                   (multiple-value-bind (variant mutation-info)
                       (new-individual)
                     (collect variant into variants)
                     (collect mutation-info into infos)
                     (incf count))

                 (ignore-failed-mutation ()
                   :report
                   "Ignore failed mutation and continue evolution"))
               (while (< count *mutants-at-once*))
               (finally (return (values variants infos))))))

    (block search-target-reached
      (iter (until (or (not *running*)
                       (and max-evals
                            (> *fitness-evals* max-evals))
                       (and max-time
                            (> (/ (- (get-internal-real-time) *start-time*)
                                  internal-time-units-per-second)
                               max-time))))
            (multiple-value-bind (variants mutation-infos)
                (new-individuals)
              (when every-pre-fn
                (mapcar every-pre-fn variants))
              (let ((super (make-instance 'super-mutant :mutants variants)))
                ;; Building super-mutant genome may fail if the
                ;; variants are incompatible.
                (restart-case
                    (genome super)
                  (ignore-failed-mutation ()
                    :report
                    "Ignore failed mutation and continue evolution"
                    (next-iteration)))
                (evaluate test super))
              (when analyze-mutation-fn
                (mapcar (lambda (variant info)
                          (funcall analyze-mutation-fn variant info test))
                        variants
                        mutation-infos))
              (when every-post-fn
                (mapcar every-post-fn variants))
              (incf *fitness-evals* (length variants))
              (when (and period period-fn
                         (zerop (mod *fitness-evals* period)))
                (funcall period-fn))
              (mapcar #'fitness-check variants)
              (mapcar #'incorporate
                      (if filter
                          (remove-if-not filter variants)
                          variants))
              (when *target-fitness-p*
                (iter (for v in variants)
                      (when (funcall *target-fitness-p* v)
                        (return-from search-target-reached v))))))))
  (setq *running* nil))
