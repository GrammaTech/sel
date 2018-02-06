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
               :documentation "Software object containing combined genome.")))

(defmethod phenome ((obj super-mutant) &key (bin (temp-file-name)))
  (phenome (super-soft obj) :bin bin))

(defmethod evaluate ((test function) (super super-mutant)
                     &rest extra-keys &key &allow-other-keys)
  (declare (ignorable extra-keys))
  (flet
      ((make-phenome-proxy (phenome-results obj)
         (let ((proxy (copy obj)))
           ;; TODO: how much memory is used up by these temporary functions?
           ;; Will they be freed along with the associated object or do we
           ;; need to use `remove-method' or `fmakunbound' on them?
           (eval `(defmethod phenome ((proxy (eql ,proxy)) &key bin)
                    (values-list
                     (cons (if bin
                               (progn (shell "cp -a ~a ~a"
                                             ,(car phenome-results) bin)
                                      bin)
                               ,(car phenome-results))
                           ',(cdr phenome-results)))))
           proxy)))

    (with-temp-file (bin)
     ;; Compile the super-mutant
     (destructuring-bind (bin . rest)
         (multiple-value-list (phenome super :bin bin))
       (mapc (lambda-bind ((i obj))
               ;; Create a wrapper script to ensure the correct mutant
               ;; is evaluated
               (with-temp-file-of (wrapper)
                 (format nil "#!/bin/sh~%SUPER_MUTANT=~d ~a" i bin)
                 (shell "chmod +x ~s" wrapper)

                 ;; Perform normal evaluation on the proxy, which will
                 ;; return the wrapper script as its phenome
                 (multiple-value-bind (fit extra)
                     (evaluate test
                               (make-phenome-proxy (if bin
                                                       (cons wrapper rest)
                                                       (cons nil rest))
                                                   obj))

                   (setf (fitness obj) fit)
                   (setf (fitness-extra-data obj) extra))))
             (indexed (mutants super)))))))

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
                         (let ((ast (ast-ref-ast ref)))
                           (if-let ((value (gethash ast variants)))
                             (pushnew i (second value))
                             (setf (gethash ast variants)
                                   (list (function-body mutant ref)
                                         (list i))))))
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
                        (list indices (list body (make-break-stmt))))
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
     (apply-mutation-ops (copy base)))))


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
