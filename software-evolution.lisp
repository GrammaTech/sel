;;; software-evolution.lisp --- Extant Software Evolution

;; Copyright (C) 2011-2013  Eric Schulte

;;; License: GNU General Public License, Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))


;;; Software Object
(defclass software ()
  ((edits   :initarg :edits   :accessor edits   :initform nil)
   (fitness :initarg :fitness :accessor fitness :initform nil)))

(defgeneric genome (software)
  (:documentation "Genotype of the software."))

(defgeneric phenome (software &key bin)
  (:documentation "Phenotype of the software."))

(defgeneric evaluate (software)
  (:documentation "Evaluate the software returning a numerical fitness."))

(defgeneric copy (software &key edits fitness)
  (:documentation "Copy the software."))

(defgeneric pick-good (software)
  (:documentation "Pick a 'good' index into a software object.
Used to target mutation."))

(defgeneric pick-bad (software)
  (:documentation "Pick a 'bad' index into a software object.
Used to target mutation."))

(defgeneric mutate (software)
  (:documentation "Mutate the software.  May throw a `mutate' error.
Optional argument PICK-GOOD and PICK-BAD may specify functions to
select portions of the genome with desirable and undesirable
properties for targeting of mutation operations."))

(define-condition mutate (error)
  ((text :initarg :text :reader text)
   (obj  :initarg :obj  :reader obj))
  (:report (lambda (condition stream)
             (format stream "Mutation error ~a on ~S"
                     (text condition) (obj condition)))))

(defgeneric apply-mutation (software mutation)
  (:documentation "Apply MUTATION to SOFTWARE."))

(defgeneric crossover (software-a software-b)
  (:documentation "Crossover two software objects."))

(defvar *edit-consolidation-size* (expt 2 7)
  "Number of cons cells at which to consolidate edits")

(defvar *consolidated-edits* nil
  "List used to hold consolidated edits.")

(defvar *edit-consolidation-function*
  (lambda (hash edits) (push (cons hash edits) *consolidated-edits*))
  "Optional function to record consolidated edits.")

(defmethod crossover :around ((a software) (b software))
  (multiple-value-bind (child places) (call-next-method)
    (mapc (lambda (var)
            (when (> (count-cons (edits var)) *edit-consolidation-size*)
              (let ((hash (sxhash (edits var))))
                (when *edit-consolidation-function*
                  (funcall *edit-consolidation-function* hash (edits var)))
                (setf (edits var) (list hash)))))
          (list a b))
    (setf (edits child) (list (cons :crossover places)
                              (list (edits a) (edits b))))
    child))

(defgeneric edit-distance (software-a software-b)
  (:documentation "Return the edit distance between two software objects."))

(defmethod edit-distance ((a software) (b software))
  (edit-distance (genome a) (genome b)))

(defgeneric from-file (software file)
  (:documentation "Initialize SOFTWARE with contents of FILE."))

(defgeneric to-file (software file)
  (:documentation "Write SOFTWARE to FILE"))

(defmethod to-file ((software software) file)
  (string-to-file (genome software) file))

(defgeneric apply-path (software key PATH)
  (:documentation "Apply the execution trace PATH behind KEY in SOFTWARE."))


;;; Evolution
(defvar *population* nil
  "Holds the variant programs to be evolved.")

(defvar *max-population-size* nil
  "Maximum allowable population size.")

(defvar *tournament-size* 2
  "Number of individuals to participate in tournament selection.")

(defvar *fitness-predicate* #'>
  "Function to compare two fitness values to select which is preferred.")

(defvar *cross-chance* 1/5
  "Fraction of new individuals generated using crossover rather than mutation.")

(defvar *fitness-evals* 0
  "Track the total number of fitness evaluations.")

(defvar *running* nil
  "True when evolving, set to nil to stop evolution.")

(defun incorporate (software)
  "Incorporate SOFTWARE into POPULATION, keeping POPULATION size constant."
  (push software *population*)
  (loop :while (and *max-population-size*
                    (> (length *population*) *max-population-size*))
     :do (evict)))

(defun evict ()
  (let ((loser (tournament (complement *fitness-predicate*))))
    (setf *population* (remove loser *population* :count 1))
    loser))

(defun tournament (&optional (predicate *fitness-predicate*) &aux competitors)
  "Select an individual from *POPULATION* with a tournament of size NUMBER."
  (flet ((verify (it) (assert (numberp (fitness it)) (it)
                              "Population member with no fitness") it))
    (assert *population* (*population*) "Empty population.")
    (car (sort (dotimes (no *tournament-size* competitors)
                 (declare (ignorable no))
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

(defun evolve (test &key max-evals max-time target period period-func filter)
  "Evolves population until an optional stopping criterion is met.

Keyword arguments are as follows.
  MAX-EVALS ------- stop after this many fitness evaluations
  MAX-TIME -------- stop after this many generations
  TARGET ---------- stop when an individual passes TARGET-FIT
  PERIOD ---------- interval of fitness evaluations to run PERIOD-FUNC
  PERIOD-FUNC ----- function to run every PERIOD fitness evaluations
  FILTER ---------- only include individual for which FILTER returns true"
  (let ((start-time (get-internal-real-time)))
    (setq *running* t)
    (loop :until (or (not *running*)
                     (and max-evals (> *fitness-evals* max-evals))
                     (and max-time (> (/ (- (get-internal-real-time) start-time)
                                         internal-time-units-per-second)
                                      max-time)))
       :do (handler-case
               (let ((new (new-individual)))
                 (setf (fitness new) (funcall test new))
                 (incf *fitness-evals*)
                 (when (and period (zerop (mod *fitness-evals* period)))
                   (funcall period-func))
                 (assert (numberp (fitness new)) (new)
                         "Non-numeric fitness: ~S" (fitness new))
                 (when (or (null filter)
                           (funcall filter new))
                   (incorporate new))
                 (when (and target-fit (funcall *fitness-predicate*
                                                (fitness new) target-fit))
                   (return new)))
             (mutate (obj) (declare (ignorable obj)) nil)))))
