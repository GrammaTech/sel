;;; ev.lisp --- evolutionary operations over software objects

;; Copyright (C) 2011  Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(in-package :soft-ev)


;;; Settings
(defvar *population* nil
  "Holds the variant programs to be evolved.")

(defvar *max-population-size* nil
  "Maximum allowable population size.")

(defvar *tournament-size* 2
  "Number of individuals to participate in tournament selection.")

(defvar *fitness-predicate* #'>
  "Whether to favor higher or lower fitness individuals by default.")

(defvar *test-script* "./test.sh"
  "The script to use to evaluate individuals.")

(defvar *pos-test-num* 0
  "Number of positive tests.")

(defvar *neg-test-num* 0
  "Number of negative tests.")

(defvar *pos-test-mult* 1
  "Multiplier for positive test cases")

(defvar *neg-test-mult* 1
  "Multiplier for negative test cases")

(defvar *keep-source* nil
  "Keep intermediate source code files.")

(defvar *cross-chance* 1/5
  "Fraction of new individuals generated using crossover rather than mutation.")

(defvar *fitness-evals* 0
  "Track the total number of fitness evaluations.")

(defvar *running* nil
  "True when evolving, set to nil to stop evolution.")


;;; functions
(defun evaluate (soft &aux (pos 0) (neg 0))
  "Evaluate SOFT setting the fitness."
  (loop for i from 1 to *pos-test-num*
     do (multiple-value-bind (output err-output exit)
            (shell "~a ~a p~d" *test-script* (exe soft) i)
          (declare (ignorable output err-output))
          (when (= exit 0) (incf pos))))
  (loop for i from 1 to *neg-test-num*
     do (multiple-value-bind (output err-output exit)
            (shell "~a ~a n~d" *test-script* (exe soft) i)
          (declare (ignorable output err-output))
          (when (= exit 0) (incf neg))))
  (incf *fitness-evals*)
  (delete-exe soft)
  (+ (* pos *pos-test-mult*)
     (* neg *neg-test-mult*)))

(defun incorporate (soft)
  "Incorporate SOFT into POPULATION, keeping the size of POPULATION constant."
  (push soft *population*)
  (when (and *max-population-size*
             (> (length *population*) *max-population-size*))
    (evict)))

(defun evict ()
  (let ((loser (tournament (complement *fitness-predicate*))))
    (setf *population* (remove loser *population* :count 1))
    loser))

(defun tournament (&optional (predicate *fitness-predicate*) &aux competitors)
  "Select an individual from *POPULATION* with a tournament of size NUMBER."
  (assert *population* (*population*) "Empty population.")
  (car (sort (dotimes (_ *tournament-size* competitors)
               (push (random-elt *population*) competitors))
             predicate :key #'fitness)))

(defun mutate (individual)
  "Mutate the supplied INDIVIDUAL."
  (funcall (case (random 3)
             (0 #'insert)
             (1 #'cut)
             (2 #'swap)) individual)
  individual)

(defun mutant ()
  "Generate a new mutant from a *POPULATION*."
  (mutate (copy (tournament))))

(defun crossed ()
  "Generate a new individual from *POPULATION* using crossover."
  (crossover (tournament) (tournament)))

(defun new-individual ()
  "Generate a new individual from *POPULATION*."
  (if (< (random 1.0) *cross-chance*) (crossed) (mutant)))

(defun evolve (&key max-evals max-time max-inds max-fit min-fit pop-fn ind-fn)
  "Evolves population until an optional stopping criterion is met.

Optional keys are as follows.
  MAX-EVALS ------- quit after this many fitness evaluations
  MAX-INDS -------- quit after this many new individuals have been tried
  MAX-TIME -------- quit after this many seconds
  MAX-FIT --------- quit when an individual achieves this fitness or higher
  MIN-FIT --------- quit when an individual achieves this fitness or lower
  POP-FN ---------- quit when the population satisfies this function
  IND-FN ---------- quit when an individual satisfies this function"
  (let ((start-time (get-internal-real-time))
        (inds 0))
    (setq *fitness-evals* 0)
    (setq *running* t)
    (loop until (or (not *running*)
                    (and max-evals (> *fitness-evals* max-evals))
                    (and max-inds (> inds max-inds))
                    (and max-time (> (/ (- (get-internal-real-time) start-time)
                                        internal-time-units-per-second)
                                     max-time)))
       do (let ((new (new-individual)))
            (incf inds)
            (incorporate new)
            (when (or (and max-fit (>= (fitness new) max-fit))
                      (and min-fit (<= (fitness new) min-fit))
                      (and ind-fn (funcall ind-fn new)))
              (return new))
            (when (and pop-fn (funcall pop-fn *population*))
              (return *population*))))))
