;;; Multi-objective selection based on Pareto dominance.

;; Fitness values should be a list of numeric or vector (lexicase)
;; scores, where each element represents a single objective. All
;; fitness values in the same population must be the same length and
;; have their scores in the same order.

(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)

(defvar *pareto-comparison-set-size* nil
  "Size of comparison set for Pareto tournaments.")

(defun multi-objective-scalar (fitness)
  (iter (for f in fitness)
        (summing (cond ((numberp f) f)
                       ((or (listp fitness) (vectorp fitness))
                        (/ (reduce #'+ f) (length f)))
                       (:otherwise
                        (error "Can't convert fitness component ~a to a scalar"
                               f))))))

(defun dominates-all (predicates comparison-set candidate)
  "Does CANDIDATE dominate all of COMPARISON-SET?

PREDICATES is a list of functions, of equal length to the fitness
values, which are used to compare each element of the fitness values.
"
  (every (lambda (compare)
           (assert (eq (length (fitness candidate))
                       (length (fitness compare))))
           (assert (eq (length (fitness candidate))
                       (length predicates)))

           (and (some #'funcall predicates (fitness candidate)
                      (fitness compare))
                (notany #'funcall predicates (fitness compare)
                        (fitness candidate))))
         comparison-set))

(defun pareto-selector (candidates
                        &key
                          (predicate *fitness-predicate*)
                          (comparison-set
                           (iter (for i below *pareto-comparison-set-size*)
                                 (collect (random-elt *population*)))))
  "Return members of CANDIDATES which dominate a random COMPARISON-SET.

Set `*tournament-selector*' to `pareto-selector' to use Pareto selection in
tournaments."
  (bind (;; Some trickiness to respect *fitness-predicate* and handle eviction.
         ;; Give dominators a good score according to the global
         ;; *fitness-predicate*. But choose "winners" according to our local
         ;; predicate. This allows us to choose non-dominating individuals when
         ;; the predicate is complemented for eviction.
         ((dominator-score non-dominator-score)
          (sort (list 0 1) *fitness-predicate*))
         (winner-score (extremum '(0 1) predicate))
         ;; Comparison predicates for each fitness component
         (component-predicates
          (iter (for f in (fitness (car candidates)))
                (collecting
                 ;; For each lexicase objective, generate a test order
                 ;; and use that across the entire tournament.
                 (if (vectorp f)
                     {lexicase-better-p (shuffle (iota (length f)))}
                     *fitness-predicate*)))))

    ;; Score all candidates based on dominance of the comparison set and pick
    ;; winners.
    (or (iter (for c in candidates)
              (when (eq winner-score
                        (if (dominates-all component-predicates
                                           comparison-set c)
                            dominator-score
                            non-dominator-score))
                (collecting c)))
        ;; If there are no winners, choose all candidates.
        candidates)))

(defun crowding-distance (software)
  "Compute crowding distance for SOFTWARE by comparing with whole population."
  (labels
      ((summed-distance (key group)
         (iter (for f in (coerce (funcall key software) 'list))
               (for i upfrom 0)

               (cond
                 ;; Numeric objectives
                 ((numberp f)
                  (setf group (sort group #'< :key [{elt _ i} key]))
                  (let ((index (position software group)))
                    (sum
                     (if (or (zerop index) (eq index (1- (length group))))
                         ;; Boundary solutions have infinite distance
                         infinity
                         ;; Otherwise, use distance between nearest neighbors
                         (- (elt (funcall key (nth (1+ index) group)) i)
                            (elt (funcall key (nth (1- index) group)) i))))))
                   ;; Lexicase objectives. Average distance across all
                   ;; components.
                 ((vectorp f)
                  (sum (/ (summed-distance [{nth i} key] group)
                          (length f))))))))
    (summed-distance #'fitness (copy-seq *population*))))

(defun pick-least-crowded (candidates &key (predicate *tie-breaker-predicate*))
  "Pick candidate with the greatest crowding distance.

Crowding distance is a fitness sharing metric adapted from NSGA-II. For each
fitness component, it sums the distance between an individual and its nearest
neighbors. Individuals with the greater crowding distance are in sparse areas of
the fitness landscape and should be preferred."

  ;; TODO: This is fairly inefficient because it sorts the population
  ;; for each candidate. If we computed crowding distance for all
  ;; candidates at once we could avoid the extra sorts.
  (extremum candidates predicate :key #'crowding-distance))
