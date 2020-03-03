;;; searchable.lisp --- Functions to recall "similar" snippets/fodder
;;;
;;; Interface for all external assets searchable for snippets
;;; similar to a given target.
;;;
(defpackage :software-evolution-library/components/searchable
  (:nicknames :sel/components/searchable :sel/cp/searchable)
  (:use :gt/full
        :diff
        :software-evolution-library)
  (:export :searchable
           :find-snippets
           :weighted-pick
           :similar-snippets
           :diff-scalar))
(in-package :software-evolution-library/components/searchable)
(in-readtable :curry-compose-reader-macros)

(defclass searchable ()
  ()
  (:documentation "DOCFIXME"))

(defgeneric find-snippets (database &key ast-class full-stmt decls limit)
  (:documentation "Find snippets in the fodder database DATABASE.

* AST-CLASS   AST class all snippets should match
* FULL-STMT   Limit results to full statements if non-nil.
* DECLS       Include decls in result if non-nil.
              Limit results to decls if the keyword :ONLY.
* LIMIT       Limit to N randomly drawn snippets"))

(defgeneric weighted-pick
    (searchable target weight
     &key predicate metric key limit ast-class filter limit-considered)
  (:documentation
   "Perform a random pick weighted by weight from `similar-snippets'.
All other arguments are passed through to sorted snippets."))

(defmethod weighted-pick ((obj searchable) target weight
                          &key key limit ast-class
                               (predicate #'<)
                               (metric #'diff-scalar)
                               (filter #'null)
                               (limit-considered infinity))
  "DOCFIXME"
  (random-elt-with-decay
    (similar-snippets obj target
                      :predicate predicate :metric metric
                      :key key :limit limit :ast-class ast-class
                      :filter filter :limit-considered limit-considered)
    weight))

(defgeneric similar-snippets
    (searchable target
     &key predicate metric key limit ast-class limit-considered filter)
  (:documentation
   "Return snippets from SEARCHABLE similar to TARGET
* SEARCHABLE DOCFIXME
* TARGET DOCFIXME
* :PREDICATE predicate for similarity metric
* :METRIC a function to generate a similarity metric
* :KEY a function called on each snippet before metric
* :LIMIT only return the MANY most similar snippets
* :AST-CLASS only consider snippets matching this AST class
* :LIMIT-CONSIDERED  limit search to MANY-CONSIDERED random snippets
* :FILTER limit search to snippets for which FILTER returns false"))

(defmethod similar-snippets ((db searchable) target
                            &key key ast-class limit
                                 (predicate #'<)
                                 (metric #'diff-scalar)
                                 (filter #'null)
                                 (limit-considered infinity))
  "DOCFIXME"
  (declare (ignorable target))
  (let ((base (sort (remove-if filter
                               (find-snippets db
                                 :ast-class ast-class :full-stmt (not ast-class)
                                 :limit limit-considered))
                    predicate :key [{funcall metric target}
                                    {funcall key}])))
    (if limit (take limit base) base)))

(defun diff-scalar (original-seq modified-seq)
  "Return an integer representing the diff size of two sequences
Sum O + |O - M| over each diff region.  O is the length of the
original diff region and M is the length of the modified diff
region."
  (reduce (lambda (acc region)
            (+ acc
               (ecase (type-of region)
                 (common-diff-region 0)
                 (modified-diff-region
                   (+ (original-length region)
                      (abs (- (original-length region)
                              (modified-length region))))))))
          (diff:compute-raw-seq-diff original-seq modified-seq)
          :initial-value 0))
