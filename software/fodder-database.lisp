;;;  Interface for all fodder database implementations.

(in-package :software-evolution)

(defclass fodder-database () ())

(defgeneric find-snippets (database &key classes full-stmt limit)
  (:documentation "Find snippets in the fodder database DATABASE.

:CLASSES --- AST class(es) all snippets should match
:FULL-STMT - Limit results to full statements if non-nil.
:LIMIT ----- Limit to N randomly drawn snippets"))

(defgeneric find-types (type-database &key hash)
  (:documentation "Find the types in the type database (optionally)
matching the keyword parameter HASH"))

(defgeneric sorted-snippets
    (database predicate
     &key key limit class limit-considered filter)
  (:documentation
   "Return snippets from DATABASE sorted by PREDICATE.

:KEY -------------- a function called on each snippet before predicate
:LIMIT ------------ only return the MANY most similar snippets
:CLASSES ---------- only consider snippets matching these AST classes
:LIMIT-CONSIDERED - limit search to MANY-CONSIDERED random snippets
:FILTER ----------- limit search to snippets for which FILTER returns false"))

(defmethod sorted-snippets ((db fodder-database) predicate
                          &key key classes limit filter
                            (limit-considered infinity))
  (let ((fodder (find-snippets db :classes classes :full-stmt (not classes))))
    (if (< limit-considered (length fodder))
        (let ((start (random (- (length fodder) limit-considered))))
          (sorted-snippets-unmemoized
           (subseq fodder start (+ start limit-considered))
           predicate :limit limit :key key))
        (sorted-snippets-memoized fodder predicate :limit limit :key key))))

(defun-memoized sorted-snippets-memoized (fodder predicate &key limit key)
  (let ((base (apply #'sort (remove-if filter fodder) predicate
                     (if key (list :key key) '()))))
    (if limit (take limit base) base)))

(defun sorted-snippets-unmemoized (fodder predicate &key limit key)
  (let ((base (apply #'sort (remove-if filter fodder) predicate
                     (if key (list :key key) '()))))
    (if limit (take limit base) base)))
