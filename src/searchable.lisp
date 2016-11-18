;;; Interface for all external assets searchable for snippets
;;; similar to a given target.

(in-package :software-evolution)

(defclass searchable () ())

(defgeneric weighted-pick
    (searchable predicate weight
     &key target key limit ast-class filter limit-considered)
  (:documentation
   "Perform a random pick weighted by weight from `sorted-snippets'.
All other arguments are passed through to sorted snippets."))

(defmethod weighted-pick ((obj searchable) predicate weight
                          &key target key limit ast-class
                               (filter #'null)
                               (limit-considered infinity))
  (random-elt-with-decay
    (sorted-snippets obj predicate
                     :target target :key key :limit limit :ast-class ast-class
                     :filter filter :limit-considered limit-considered)
    weight))

(defgeneric sorted-snippets
    (searchable predicate
     &key target key limit ast-class limit-considered filter)
  (:documentation
   "Return snippets from SEARCHABLE sorted by PREDICATE.

:TARGET ----------- specify the TARGET for a comparison based predicate
                    (this may be used to identify cached comparison results)
:KEY -------------- a function called on each snippet before predicate
:LIMIT ------------ only return the MANY most similar snippets
:AST-CLASS--------- only consider snippets matching this AST class
:LIMIT-CONSIDERED - limit search to MANY-CONSIDERED random snippets
:FILTER ----------- limit search to snippets for which FILTER returns false"))

(defmethod sorted-snippets ((db searchable) predicate
                            &key target key ast-class limit
                                 (filter #'null)
                                 (limit-considered infinity))
  (declare (ignorable target))
  (let ((base (sort (remove-if filter
                               (find-snippets db
                                 :ast-class ast-class :full-stmt (not ast-class)
                                 :limit limit-considered))
                    predicate :key key)))
    (if limit (take limit base) base)))
