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
;;; Super-mutants are currently only implemented for the specific
;;; software objects mentioned in the following subsections of this
;;; section.
;;;
;;; @menu
;;; * Clang Super-Mutants:: Clang software object super-mutants
;;; * ASM Super-Mutants:: Assembly software object super-mutants
;;; @end menu
;;;
;;; @node Clang Super-Mutants, ASM Super-Mutants, Super-Mutants, Super-Mutants
;;; @subsection Clang Super-Mutants
;;; @cindex clang-super-mutants
;;;
;;; Super mutants composed of CLANG and CLANG-PROJECT software objects
;;; work by combining the genomes of multiple objects into a single
;;; software object, identifying functions with differences between
;;; objects, and wrapping the bodies of those functions in SWITCH
;;; statements switching on the value of an environment variable which
;;; controls which mutant is exercised in any given execution.
;;;
;;; @subsubsection Interactions Between Mutants
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
;;; @subsubsection Performance Considerations
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
;;; @node ASM Super-Mutants, Utilities, Clang Super-Mutants, Super-Mutants
;;; @subsection ASM Super Mutants
;;; @cindex asm-super-mutants
;;;
;;; @include include/asm-super-mutant.texi
;;;
;;; @texi{super-mutant}
(defpackage :software-evolution-library/software/super-mutant
  (:nicknames :sel/software/super-mutant :sel/sw/super-mutant)
  (:use :gt/full
        :metabang-bind
        :software-evolution-library)
  (:export :super-mutant
           :mutants
           :super-soft
           :collate-ast-variants
           :phenome-results
           :create-super-soft))
(in-package :software-evolution-library/software/super-mutant)
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

(defmethod create-super ((variant software) &optional rest-variants)
  "Creates a SUPER-MUTANT and populates variants. Returns the super-mutant."
  (make-instance 'super-mutant :mutants (cons variant rest-variants)))

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
  (with-temporary-file (:pathname super-bin)
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
                     (make-instance
                         'standard-method
                       :function
                       #+ccl
                       #'proxy-phenome
                       #+sbcl
                       (lambda (args other)
                         (declare (ignorable other))
                         (apply #'proxy-phenome args))
                       #-(or sbcl ccl)
                       (error "make-phenome-proxy not implemented")
                       :lambda-list '(obj &key bin)
                       #+(or sbcl ccl)
                       :specializers
                       #+ccl
                       (list (ccl::intern-eql-specializer proxy))
                       #+sbcl
                       (list (sb-pcl::intern-eql-specializer proxy)))))
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
  (let ((result (list 'dummy))) ;; dummy value avoids special cases
    (iter (for roots in ast-roots)
          (for i upfrom 0)
          (let ((previous result)
                (current (cdr result)))
            (iter (for (key . variant) in roots)
                  ;; Search for a matching key in the results
                  (if-let ((matching (find key (tails current)
                                           :key #'caar
                                           :test #'equal)))
                    ;; If found, advance to that key and
                    ;; add the new variant there
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
                                 (push nil (cdr (car c)))))))
          (finally
           ;; Strip dummy value and keys, and put variants in correct order.
           (return (mapcar [#'reverse #'cdr] (cdr result)))))))

(defgeneric create-super-soft (base mutants)
  (:documentation "Create a software object which combines multiple variants.
* BASE object to use as a starting point
* MUTANTS objects to combine

BASE is typically a member of MUTANTS."))
