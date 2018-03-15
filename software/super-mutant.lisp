;;;; super-mutant.lisp --- evaluate multiple mutants in a single
;;;
;;; For many software objects, a large portion of the search time will
;;; be spent on compiling mutants in order to test their fitness. This
;;; overhead can be reduced significantly through the use of
;;; super-mutants, which bundle multiple changes into a single genome,
;;; guarded by runtime flags. This allows several mutants to be
;;; evaluated with a single compilation.
;;;
;;; Super-mutants can be enabled with the :SUPER-MUTANT-COUNT argument
;;; to EVOLVE. When this is greater than one, each iteration of the
;;; search loop will generate the indicated number of mutants. The
;;; mutants will then be combined into a super-mutant, producing a
;;; single phenome which is used to evaluate all variants.
;;;
;;; This process is almost entirely transparent to the client
;;; code. The callback functions EVERY-PRE-FN, EVERY-POST-FN and
;;; FILTER will be called individually on each mutant. The TEST
;;; function will also be called on each variant, but calls to PHENOME
;;; will return a wrapper which invokes the super-mutant with the
;;; desired mutations enabled.
;;;
;;; @subsection Limitations
;;;
;;; * Super-mutants are currently only implemented for CLANG and
;;;   CLANG-PROJECT software objects.
;;;
;;; * Global variable declarations must match across all
;;;   mutants. Declarations may be added or deleted, however.
;;;
;;; * Functions must have identical arguments and return types across
;;;   all mutants. Functions may be added or deleted, but all existing
;;;   copies must match.
;;;
;;;
;;; @subsection Interactions Between Mutants
;;;
;;; When combining multiple genomes, there is some risk of
;;; interactions between the individual mutants. These primarily
;;; relate to compilation errors. For example, if a mutation deletes a
;;; global variable which is referenced in the program, that would
;;; normally lead to an error. But if that mutant is combined with
;;; another which still has the variable, the combined mutant will
;;; compile and both will run as if the variable was present.
;;;
;;; Conversely, a compilation error in any mutant will cause
;;; compilation to fail for the entire super-mutant.
;;;
;;; If all mutants are free from compilation errors, their combination
;;; should also compile, and its runtime behavior should be
;;; functionally indistinguishable from that of the individual
;;; mutants.
;;;
;;; @subsection Performance Considerations
;;;
;;; The super-mutant genome will introduce a GETENV call and a SWITCH
;;; statement. If these are placed in a performance-critical function,
;;; execution time might increase significantly. If execution time is
;;; part of the fitness evaluation, use super-mutants with caution.
;;;
;;; A compilation failure in a single mutant will cause PHENOME to
;;; fail for the entire super-mutant. If mutations frequently
;;; introduce compilation failures, this can lead to a lot of wasted
;;; compilation. This problem will get worse as :SUPER-MUTANT-COUNT
;;; increases.
;;;
;;; Similarly, if the individual mutants are not compatible (due to
;;; differences in global variables or function signatures), a MUTATE
;;; error will be raised and the entire super-mutant discarded. This
;;; can lead to wasted mutations.
;;;
;;; @texi{super-mutants}

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
  "Phenotype of the software.
This method will link, compile or serialize the software object as
necessary returning an executable version of the software suitable for
testing and evaluation.  Returns multiple values holding in order; (1)
the binary path to which the executable was compiled, (2) the errno,
or a numeric indication of success, of the compilation process, (3)
STDERR of the compilation process, or a string holding error output
relevant to phenome generation, (4) STDOUT of the compilation process,
or a string holding non-error output relevant to phenome
generation, (5) the source file name used during compilation. "
  (phenome (super-soft obj) :bin bin))

(defgeneric phenome-wrapper (obj wrapper which-mutant &key bin))
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
  "Evaluate SUPER-MUTANT, setting fitness for all variants."
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
                                    :function
                                    #+ccl #'proxy-phenome
                                    #+sbcl (lambda (args other)
                                             (declare (ignorable other))
                                             (apply #'proxy-phenome args))
                                    #-(or sbcl ccl)
                                    (error "make-phenome-proxy not implemented")
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
  "Return a genome which combines all variants in SUPER-MUTANT."
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

(defgeneric create-super-soft (base mutants)
  (:documentation "Create a software object which combines multiple variants.
* BASE object to use as a starting point
* MUTANTS objects to combine

BASE is typically a member of MUTANTS."))

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
     ;; Create mutations to insert super-functions into genome of
     ;; first variant
     (create-mutation-ops)
     (sort <>
           #'ast-later-p :key [{aget :stmt1} #'cdr])
     (apply-mutation-ops (copy base))
     ;; Needed for getenv()
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
