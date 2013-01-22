;;; software-evolution.lisp --- Extant Software Evolution

;; Copyright (C) 2011-2012  Eric Schulte

;;; License: GNU General Public License, Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; Software Object
(defclass software ()
  ((edits   :initarg :edits   :accessor edits   :initform nil)
   (fitness :initarg :fitness :accessor fitness :initform nil)))

(defgeneric genome (software)
  (:documentation "Genotype of the software."))

(defgeneric phenome (software &key bin)
  (:documentation "Phenotype of the software."))

(defgeneric cleanup (software &key bin)
  ;; TODO: Figure out how to automatically run this (when defined)
  ;;       after phenome generation.
  (:documentation "Cleanup function to be run after phenome generation."))

(defgeneric evaluate (software)
  (:documentation "Evaluate a the software returning a numerical fitness."))

(defgeneric copy (software)
  (:documentation "Copy the software."))

(defgeneric mutate (software)
  (:documentation "Mutate the software.  May throw a `mutate' error."))

(defgeneric crossover (software-a software-b)
  (:documentation "Crossover two software objects."))

(defgeneric edit-distance (software-a software-b)
  (:documentation "Return the edit distance between two software objects."))

(defgeneric from-file (software file)
  (:documentation "Initialize SOFTWARE with contents of FILE."))

(defgeneric to-file (software file)
  (:documentation "Write SOFTWARE to FILE"))

(defmethod to-file ((software software) file)
  (string-to-file (genome software) file))


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
  (flet ((verify (it) (assert (numberp (fitness it)) (it)
                              "Population member with no fitness") it))
    (assert *population* (*population*) "Empty population.")
    (car (sort (dotimes (_ *tournament-size* competitors)
                 (push (verify (random-elt *population*)) competitors))
               predicate :key #'fitness))))

(defun mutant ()
  "Generate a new mutant from a *POPULATION*."
  (mutate (copy (tournament))))

(defun crossed ()
  "Generate a new individual from *POPULATION* using crossover."
  (crossover (tournament) (tournament)))

(defun new-individual ()
  "Generate a new individual from *POPULATION*."
  (if (< (random 1.0) *cross-chance*) (crossed) (mutant)))

(defun evolve (test &key
                      period period-func
                      max-evals max-time max-fit min-fit pop-fn ind-fn)
  "Evolves population until an optional stopping criterion is met.

Keyword arguments are as follows.
  PERIOD ---------- interval of fitness evaluations to run PERIOD-FUNC
  PERIOD-FUNC ----- function to run every PERIOD fitness evaluations
  MAX-EVALS ------- stop after this many fitness evaluations
  MAX-TIME -------- stop after this many generations
  MAX-FIT --------- stop when an individual achieves this fitness or higher
  MIN-FIT --------- stop when an individual achieves this fitness or lower
  POP-FN ---------- stop when the population satisfies this function
  IND-FN ---------- stop when an individual satisfies this function"
  (let ((start-time (get-internal-real-time)))
    (setq *running* t)
    (loop :until (or (not *running*)
                     (and max-evals (> *fitness-evals* max-evals))
                     (and max-time (> (/ (- (get-internal-real-time) start-time)
                                         internal-time-units-per-second)
                                      max-time)))
       :do (let ((new (new-individual)))
             (setf (fitness new) (funcall test new))
             (incf *fitness-evals*)
             (when (and period (zerop (mod *fitness-evals* period)))
               (funcall period-func))
             (assert (numberp (fitness new)) (new)
                     "Non-numeric fitness: ~S" (fitness new))
             (incorporate new)
             (when (or (and max-fit (>= (fitness new) max-fit))
                       (and min-fit (<= (fitness new) min-fit))
                       (and ind-fn (funcall ind-fn new)))
               (return new))
             (when (and pop-fn (funcall pop-fn *population*))
               (return))))))
