;;; software-evolution.lisp --- Extant Software Evolution

;; Copyright (C) 2011-2012  Eric Schulte

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
(in-package :software-evolution)


;;; Software Object
(defclass software ()
  ((edits   :initarg :edits   :accessor edits   :initform nil)
   (fitness :initarg :fitness :accessor fitness :initform nil)))

(defgeneric genome (software)
  (:documentation "Genotype of the software."))

(defgeneric phenome (software)
  (:documentation "Phenotype of the software."))

(defgeneric evaluate (software)
  (:documentation "Evaluate a the software returning a numerical fitness."))

(defgeneric mutate (software)
  (:documentation "Mutate the software."))

(defgeneric crossover (software-a software-b)
  (:documentation "Crossover two software objects."))

(defgeneric edit-distance (software-a software-b)
  (:documentation "Return the edit distance between two software objects."))


;;; Evolution
(defvar *population* nil
  "Holds the variant programs to be evolved.")

(defvar *max-population-size* nil
  "Maximum allowable population size.")

(defvar *tournament-size* 2
  "Number of individuals to participate in tournament selection.")

(defvar *fitness-predicate* #'>
  "Whether to favor higher or lower fitness individuals by default.")

(defvar *cross-chance* 1/5
  "Fraction of new individuals generated using crossover rather than mutation.")

(defvar *fitness-evals* 0
  "Track the total number of fitness evaluations.")

(defvar *running* nil
  "True when evolving, set to nil to stop evolution.")

(defun incorporate (software)
  "Incorporate SOFTWARE into POPULATION, keeping POPULATION size constant."
  (push software *population*)
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

(defun mutant ()
  "Generate a new mutant from a *POPULATION*."
  (mutate (copy (tournament))))

(defun crossed ()
  "Generate a new individual from *POPULATION* using crossover."
  (crossover (tournament) (tournament)))

(defun new-individual ()
  "Generate a new individual from *POPULATION*."
  (if (< (random 1.0) *cross-chance*) (crossed) (mutant)))

(defun evolve (test
               &key max-evals max-time max-inds max-fit min-fit pop-fn ind-fn)
  "Evolves population until an optional stopping criterion is met.

Optional keys are as follows.
  MAX-EVALS ------- stop after this many fitness evaluations
  MAX-INDS -------- stop after this many new individuals have been tried
  MAX-TIME -------- stop after this many generations
  MAX-FIT --------- stop when an individual achieves this fitness or higher
  MIN-FIT --------- stop when an individual achieves this fitness or lower
  POP-FN ---------- stop when the population satisfies this function
  IND-FN ---------- stop when an individual satisfies this function"
  (let ((start-time (get-internal-real-time))
        (inds 0))
    (setq *fitness-evals* 0)
    (setq *running* t)
    (loop :until (or (not *running*)
                     (and max-evals (> *fitness-evals* max-evals))
                     (and max-inds (> inds max-inds))
                     (and max-time (> (/ (- (get-internal-real-time) start-time)
                                         internal-time-units-per-second)
                                      max-time)))
       :do (let ((new (new-individual)))
             (incf inds)
             (setf (fitness new) (funcall #'test new))
             (assert (numberp (fitness new)))
             (incorporate new)
             (when (or (and max-fit (>= (fitness new) max-fit))
                       (and min-fit (<= (fitness new) min-fit))
                       (and ind-fn (funcall ind-fn new)))
               (return new))
             (when (and pop-fn (funcall pop-fn *population*))
               (return))))))
