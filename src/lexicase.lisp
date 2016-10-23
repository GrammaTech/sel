;;; lexicase selection

;; Fitness values should be a vector of numeric scores.
;; (A key option may be supplied for non-numeric scores.)
;;
;; Where each entry represents a single test case or objective.  All
;; fitness values in the same population must be the same length and
;; have their scores in the same order.

(in-package :software-evolution)

(defvar *lexicase-key* nil
  "Optional key function for components of test vector.")

(defun lexicase-select (population max-size)
  "Choose max-size individuals from the population by lexicase selection.
The same individual may be selected multiple times."
  (assert
   (= 1 (length (remove-duplicates population :key [#'length #'fitness])))
   (population)
   "All fitness vectors must be the same length.")
  (iter (for n below max-size) (collect (lexicase-select-one population))))

(defun lexicase-select-one (group &key (predicate *fitness-predicate*))
  "Choose a single individual by lexicase selection.
Set the value of `*tournament-selector*' to `lexicase-select-one' to
use lexicase-style selection in tournament selection."
  (iter (for which in (shuffle (iota (1- (length (fitness (first group)))))))
        (setf group
              ;; Keep individuals with the highest score on the current test.
              (remove-if-not
               [{= (extremum (mapcar [{elt _ which} #'fitness] group)
                             predicate :key *lexicase-key*)}
                {elt _ which} #'fitness]
               group :key *lexicase-key*))
        ;; Stop when we get down to one individual
        (until (not (cdr group))))
  ;; If there's still a tie after all tests, choose randomly.
  (random-elt group))
