(defpackage :example
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility))
(in-package :example)

(setf *tournament-selector* #'pareto-selector)
;; Recommended value
(setf *pareto-comparison-set-size* (round (/ *max-population-size* 10)))
(setf *tournament-tie-breaker* #'pick-least-crowded)

(defun test (software)
  ;; Return a list containing scores for each objective.
  (list #| TODO: ... |#))
