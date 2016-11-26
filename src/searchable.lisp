;;; Interface for all external assets searchable for snippets
;;; similar to a given target.

(in-package :software-evolution)

(defclass searchable () ())

(defgeneric weighted-pick
    (searchable target weight
     &key predicate metric key limit ast-class filter limit-considered)
  (:documentation
   "Perform a random pick weighted by weight from `sorted-snippets'.
All other arguments are passed through to sorted snippets."))

(defmethod weighted-pick ((obj searchable) target weight
                          &key key limit ast-class
                               (predicate #'<)
                               (metric #'diff-scalar)
                               (filter #'null)
                               (limit-considered infinity))
  (random-elt-with-decay
    (sorted-snippets obj target
                     :predicate predicate :metric metric
                     :key key :limit limit :ast-class ast-class
                     :filter filter :limit-considered limit-considered)
    weight))

(defgeneric sorted-snippets
    (searchable target
     &key predicate metric key limit ast-class limit-considered filter)
  (:documentation
   "Return snippets from SEARCHABLE similar to TARGET

:PREDICATE -------- predicate for similarity metric
:METRIC ----------- a function to generate a similarity metric
:KEY -------------- a function called on each snippet before metric
:LIMIT ------------ only return the MANY most similar snippets
:AST-CLASS--------- only consider snippets matching this AST class
:LIMIT-CONSIDERED - limit search to MANY-CONSIDERED random snippets
:FILTER ----------- limit search to snippets for which FILTER returns false"))

(defmethod sorted-snippets ((db searchable) target
                            &key key ast-class limit
                                 (predicate #'<)
                                 (metric #'diff-scalar)
                                 (filter #'null)
                                 (limit-considered infinity))
  (declare (ignorable target))
  (let ((base (sort (remove-if filter
                               (find-snippets db
                                 :ast-class ast-class :full-stmt (not ast-class)
                                 :limit limit-considered))
                    predicate :key [{funcall metric target}
                                    {funcall key}])))
    (if limit (take limit base) base)))
