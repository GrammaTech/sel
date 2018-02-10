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
               ;; TODO: how much memory is used up by these temporary
               ;; functions?  Will they be freed along with the
               ;; associated object or do we need to use
               ;; `remove-method' on them?
               (add-method #'phenome method)
               (values proxy method)))))

      (mapc (lambda-bind ((i obj))
              ;; Create a proxy to override phenome behavior, then
              ;; evaluate that in the normal way.
              (multiple-value-bind (fit extra)
                  (evaluate test
                            (make-phenome-proxy obj i))

                (setf (fitness obj) fit)
                (setf (fitness-extra-data obj) extra)))
            (indexed (mutants super))))))

(defmethod genome ((obj super-mutant))
  (genome (super-soft obj)))

(defmethod super-soft :before ((obj super-mutant))
  (with-slots (super-soft) obj
    (unless super-soft
      (setf super-soft (create-super-soft (car (mutants obj))
                                          (mutants obj))))))

(defmethod create-super-soft ((base clang) mutants)
  (labels
      ((functions-compatible-p (f1 f2)
         (or (eq f1 f2)
             (and (string= (ast-name f1) (ast-name f2))
                  (eq (ast-void-ret f1) (ast-void-ret f2))
                  (eq (ast-varargs f1) (ast-varargs f2))
                  (equal (ast-args f1) (ast-args f2)))))
       (process-ast (&rest asts)
         (destructuring-bind (head . rest) asts
           (if (function-decl-p head)
               ;; Function bodies may differ as long as name and
               ;; arguments are the same. Collect all unique variants,
               ;; along with the mutants they came from.
               (let ((variants (make-hash-table)))
                 (mapc (lambda-bind ((i ref) mutant)
                         (assert (functions-compatible-p head ref))
                         (let ((ast (ast-ref-ast ref))
                               (body (function-body mutant ref)))
                           (unless body
                             (error
                              (make-condition 'mutate
                                              :text
                                              (format nil "Missing body for ~a"
                                                      (ast-name ref))
                                              :obj mutant)))
                           (if-let ((value (gethash ast variants)))
                             (pushnew i (second value))
                             (setf (gethash ast variants)
                                   (list body (list i))))))
                       (indexed asts)
                       mutants)
                 (cons (function-body base head)
                       variants))
               ;; Top-level decls must be identical across
               ;; variants. Don't collect anything.
               (progn
                 (assert (every [{eq (ast-ref-ast head)} #'ast-ref-ast] rest))
                 nil))))
       (make-super-function (variants-ht)
         (->> (mapcar (lambda-bind ((body indices))
                        (list (if (member 0 indices)
                                  ;; Add default case to make compiler happy
                                  (cons t indices)
                                  indices)
                              (list body (make-break-stmt))))
                      (hash-table-values variants-ht))
              (make-switch-stmt
               (make-call-expr
                "atoi"
                (list (make-call-expr "getenv"
                                      (list (make-literal "SUPER_MUTANT"))
                                      :generic))
                :generic)))))
    (-<>>
     ;; Collect all variants of each function.
     (apply #'mapcar
            #'process-ast
            (mapcar #'roots mutants))
     (remove nil)
     ;; Create mutation ops to replace original functions with
     ;; super-functions
     (mapcar (lambda-bind ((orig . variants))
               `(:set (:stmt1 . ,orig)
                      (:value1 . ,(make-super-function variants)))))
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
         (iter (for i below *mutants-at-once*)
               (restart-case
                   (multiple-value-bind (variant mutation-info)
                       (new-individual)
                     (collect variant into variants)
                     (collect mutation-info into infos))
                 (ignore-failed-mutation ()
                   :report
                   "Ignore failed mutation and continue evolution"))
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
                ;; Building super-mutant genome may fail if e.g. one
                ;; of the variants has deleted a function body.
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
