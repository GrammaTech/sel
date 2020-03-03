;;; lexicase.lisp --- Lexicase selection.
;;;
;;; Fitness values should be a vector of numeric scores.
;;; (A key option may be supplied for non-numeric scores.)
;;;
;;; Where each entry represents a single test case or objective.  All
;;; fitness values in the same population must be the same length and
;;; have their scores in the same order.
(defpackage :software-evolution-library/components/lexicase
  (:nicknames :sel/components/lexicase :sel/cp/lexicase)
  (:use :gt/full
        :software-evolution-library)
  (:export :lexicase-select
           :lexicase-select-best
           :*lexicase-key*
           :lexicase-better-p))
(in-package :software-evolution-library/components/lexicase)
(in-readtable :curry-compose-reader-macros)

(defvar *lexicase-key* nil
  "Optional key function for components of test vector.")

(defun lexicase-select (population max-size)
  "Choose max-size individuals from the population by lexicase selection.
The same individual may be selected multiple times."
  (without-compiler-notes
    (assert
     (= 1 (length (remove-duplicates population :key [#'length #'fitness])))
     (population)
     "All fitness vectors must be the same length."))
  (iter (for n below max-size)
        (collect (funcall *tournament-tie-breaker*
                          (lexicase-select-best population)))))

(defun lexicase-select-best (group &key (predicate *fitness-predicate*))
  "Choose best individuals by lexicase selection.

If there is a tie after all tests, return all remaining individuals.

Set the value of `*tournament-selector*' to `lexicase-select-best' to
use lexicase-style selection in tournament selection."
  (iter (for which in (shuffle (iota (1- (length (fitness (first group)))))))
        (setf group
              ;; Keep individuals with the highest score on the current test.
              (remove-if-not
               [{equal (extremum (mapcar [{elt _ which} #'fitness] group)
                                 predicate :key *lexicase-key*)}
                {elt _ which} #'fitness]
               group :key *lexicase-key*))
        ;; Stop when we get down to one individual
        (until (not (cdr group))))
  group)

(defun lexicase-better-p (order fitness-a fitness-b)
  "Compare fitness vectors with a fixed order."

  (iter (for which in order)
        (let ((a (elt fitness-a which))
              (b (elt fitness-b which)))

          (cond
            ((funcall *fitness-predicate* a b) (return t))
            ((funcall *fitness-predicate* b a) (return nil))))))
