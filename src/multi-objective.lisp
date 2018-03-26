#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
;;; Multi-objective selection based on Pareto dominance.

;; Fitness values should be a list of numeric or vector (lexicase)
;; scores, where each element represents a single objective. All
;; fitness values in the same population must be the same length and
;; have their scores in the same order.

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defvar *pareto-comparison-set-size* nil
  "Size of comparison set for Pareto tournaments.")

(defun multi-objective-scalar (fitness)
  "DOCFIXME"
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
                  ;; Sort is destructive, must use copy
                  (let* ((sorted (sort (copy-seq group) #'<
                                       :key [{elt _ i} key]))
                         (index (position software sorted)))
                    (sum
                     (if (or (zerop index) (eq index (1- (length sorted))))
                         ;; Boundary solutions have infinite distance
                         infinity
                         ;; Otherwise, use distance between nearest neighbors
                         (- (elt (funcall key (nth (1+ index) sorted)) i)
                            (elt (funcall key (nth (1- index) sorted)) i))))))
                   ;; Lexicase objectives. Average distance across all
                   ;; components.
                 ((vectorp f)
                  (sum (/ (summed-distance [{nth i} key] group)
                          (length f))))))))
    (summed-distance #'fitness *population*)))

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

#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
