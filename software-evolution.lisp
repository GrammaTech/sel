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
  ((fitness :initarg :fitness :accessor fitness :initform nil)))

(defmacro define-software (class-name superclasses slots &rest options)
  `(progn
     ;; Define the class
     (defclass ,class-name ,superclasses
       ,(mapcar {plist-drop :copier} slots)
       ,@options)
     ;; Define the copy method
     ,(unless (null slots)
        `(defmethod copy :around ((obj ,class-name))
           (let ((copy (call-next-method)))
             (setf
              ,@(mappend (lambda (accessor copier)
                           (unless (eql copier :none)
                             `((,accessor copy)
                               ,(if copier
                                    `(,copier (,accessor obj))
                                    `(,accessor obj)))))
                         (mapcar #'car slots)
                         (mapcar {plist-get :copier} slots)))
             copy)))))

(defgeneric genome (software)
  (:documentation "Genotype of the software."))

(defgeneric phenome (software &key bin)
  (:documentation "Phenotype of the software."))

(defgeneric evaluate (function software)
  (:documentation "Evaluate the software returning a numerical fitness."))

(defmethod evaluate ((test function) (obj software))
  (multiple-value-bind (fit extra) (funcall test obj)
    (setf (fitness obj) fit)
    (setf (fitness-extra-data obj) extra)
    (values fit extra)))

(defgeneric (setf fitness-extra-data) (extra-data software)
  (:documentation "Pass extra data (optionally) returned by the fitness function
                   to the software object."))

(defmethod (setf fitness-extra-data) (extra-data (obj software)))

(defgeneric copy (software)
  (:documentation "Copy the software."))

(defmethod copy ((obj software))
  (make-instance (class-of obj) :fitness (fitness obj)))

(defgeneric size (software)
  (:documentation "Return the size of the `genome' of SOFTWARE."))

(defmethod size ((software software)) (length (genome software)))

(defgeneric lines (software)
  (:documentation "Return the lines of code of the `genome' of SOFTWARE."))

(defgeneric genome-string (software &optional stream)
  (:documentation "Return a string of the `genome' of SOFTWARE."))

(defgeneric pick (software key &optional func)
  (:documentation "Pick an element of GENOME based on KEY of each element.
KEY is passed to `proportional-pick' to return an index.  Optional
argument FUNC processes the index to return a result."))

(defmethod pick ((sw software) key &optional func)
  (let ((pick (proportional-pick (genome sw) key)))
    (if func (funcall func pick) pick)))

(defgeneric pick-good (software)
  (:documentation "Pick a 'good' index into a software object.
Used to target mutation."))
(defmethod pick-good ((software software)) (random (size software)))

(defgeneric pick-bad (software)
  (:documentation "Pick a 'bad' index into a software object.
Used to target mutation."))
(defmethod pick-bad  ((software software)) (random (size software)))

(defgeneric mutate (software)
  (:documentation "Mutate the software.  May throw a `mutate' error."))

(defgeneric mcmc-step (software)
  (:documentation "Change software in a way amenable to MCMC.
Specifically every step should be reversible, and the resulting walk
should be ergodic."))

(defvar *mcmc-fodder* nil
  "Holds the genome elements which may be used by `mcmc-step'.
Should be initialized to a list of the unique possible genome
elements.")

(defvar *clang-fodder* nil
  "Holds a JSON database of ASTs to be utilized in mutation operations
  with clang-w-fodder objects.")

(define-condition mutate (error)
  ((text :initarg :text :reader text)
   (obj  :initarg :obj  :reader obj))
  (:report (lambda (condition stream)
             (format stream "Mutation error ~a on ~S"
                     (text condition) (obj condition)))))

(defgeneric apply-mutation (software mutation)
  (:documentation "Apply MUTATION to SOFTWARE.
Define an :around method on this function to record mutations."))

(defgeneric crossover (software-a software-b)
  (:documentation "Crossover two software objects.
Define an :around method on this function to record crossovers."))

(defgeneric one-point-crossover (software-a software-b)
  (:documentation "Crossover at a single point."))

(defgeneric two-point-crossover (software-a software-b)
  (:documentation "Crossover between two points."))

(defgeneric from-file (software file)
  (:documentation "Initialize SOFTWARE with contents of FILE."))

(defgeneric to-file (software file)
  (:documentation "Write SOFTWARE to FILE"))

(defmethod to-file ((software software) file)
  (string-to-file (genome software) file))

(defgeneric apply-path (software key PATH) ; TODO: is this used?
  (:documentation "Apply the execution trace PATH behind KEY in SOFTWARE."))


;;; Evolution
(defvar *population* nil
  "Holds the variant programs to be evolved.")

(defvar *max-population-size* nil
  "Maximum allowable population size.")

(defvar *tournament-size* 2
  "Number of individuals to participate in tournament selection.")

(defvar *tournament-eviction-size* 2
  "Number of individuals to participate in eviction tournaments.")

(defvar *fitness-predicate* #'>
  "Function to compare two fitness values to select which is preferred.")

(defvar *cross-chance* 2/3
  "Fraction of new individuals generated using crossover rather than mutation.")

(defvar *mut-rate* 1
  "Chance to mutate a new individual.
If <1, new individuals will be mutated once with change *MUT-RATE*.
If =1, then every new individual will be mutated exactly once.
If >1, then new individuals will be mutated from 1 to *MUT-RATE* times.")

(defvar *fitness-evals* 0
  "Track the total number of fitness evaluations.")

(defvar *running* nil
  "True when a search process is running, set to nil to stop evolution.")

(defmacro incorporate (software &optional
                                  (population '*population*)
                                  (max-population-size '*max-population-size*))
  "Incorporate SOFTWARE into POPULATION, keeping POPULATION size constant."
  `(progn
     (push ,software ,population)
     (loop :while (and ,max-population-size
                       (> (length ,population) ,max-population-size))
        :do (evict))))

(defun evict ()
  (let ((loser (tournament :predicate (complement *fitness-predicate*)
                           :size *tournament-eviction-size*)))
    (setf *population* (remove loser *population* :count 1))
    loser))

(defun tournament
    (&key (predicate *fitness-predicate*) (size *tournament-size*))
  "Select an individual from *POPULATION* with a tournament of size NUMBER."
  (flet ((verify (it)
           (assert (typep it 'software) (it)
                   "Population member is not software object")
           (assert (numberp (fitness it)) (it)
                   "Population member with no fitness")
           it))
    (assert *population* (*population*) "Empty population.")
    (car (sort (loop :for i :below size
                  :collect (verify (random-elt *population*)))
               predicate :key #'fitness))))

(defun mutant (&optional (new (copy (tournament))))
  "Generate a new mutant from a *POPULATION*."
  (cond ((< *mut-rate* 1) (if (< (random 1.0) *mut-rate*) (mutate new) new))
        ((= *mut-rate* 1) (mutate new))
        ((> *mut-rate* 1) (dotimes (n (1+ (floor (random *mut-rate*))) new)
                            (mutate new)))))

(defun crossed (&optional (a (tournament)) (b (tournament)))
  "Generate a new individual from *POPULATION* using crossover."
  (if (< (random 1.0) *cross-chance*)
      (crossover a b)
      (values (copy a) nil nil)))

(defmethod new-individual (&optional (a (tournament)) (b (tournament)))
  "Generate a new individual from *POPULATION*."
  (multiple-value-bind (crossed a-point b-point) (crossed a b)
    (multiple-value-bind (mutant mutation) (mutant crossed)
      (values mutant mutation a-point b-point))))

(defmacro -search (specs step &rest body)
  "Perform a search loop with early termination."
  (destructuring-bind (variant f max-evals max-time target pd pd-fn
                               every-pre-fn every-post-fn
                               filter running fitness-counter)
      specs
    (let* ((time (gensym)) (fit-var (gensym))
           (main
            `(progn
               (setq ,running t)
               (loop :until
                  ,(if (or max-time max-evals)
                       `(or (not ,running)
                            ,@(when max-evals
                                    `((> ,fitness-counter ,max-evals)))
                            ,@(when max-time
                                    `((> (/ (- (get-internal-real-time) ,time)
                                            internal-time-units-per-second)
                                         ,max-time))))
                       `(not ,running))
                  :do (handler-case
                          (let ((,variant (funcall ,step)))
                            ,@(when every-pre-fn
                                    `((funcall ,every-pre-fn ,variant)))
                            (evaluate ,f ,variant)
                            ,@(when every-post-fn
                                    `((funcall ,every-post-fn ,variant)))
                            (incf ,fitness-counter)
                            ,@(when (and pd pd-fn)
                                    `((when (zerop (mod ,fitness-counter ,pd))
                                        (funcall ,pd-fn))))
                            (assert (numberp (fitness ,variant)) (,variant)
                                    "Non-numeric fitness: ~S" (fitness ,variant))
                            ,@(if filter
                                  `((when (funcall ,filter ,variant) ,@body))
                                  body)
                            ,@(when target
                                    `((when (let ((,fit-var (fitness ,variant)))
                                              (or (equal ,fit-var ,target)
                                                  (funcall *fitness-predicate*
                                                           ,fit-var ,target)))
                                        (setq ,running nil)
                                        (return ,variant)))))
                        (mutate (obj) (declare (ignorable obj)) nil))))))
      (when target
        (setf main `(block nil ,main)))
      (when max-time
        (setf main `(let ((,time (get-internal-real-time))) ,main)))
      main)))

(defmacro evolve
    (test &key max-evals max-time target period period-fn
            every-pre-fn every-post-fn
            filter
            (population '*population*)
            (max-population-size '*max-population-size*)
            (running '*running*)
            (fitness-evals '*fitness-evals*))
  "Evolves `*population*' until an optional stopping criterion is met.

Keyword arguments are as follows.
  MAX-EVALS ------- stop after this many fitness evaluations
  MAX-TIME -------- stop after this many seconds
  TARGET ---------- stop when an individual passes TARGET-FIT
  PERIOD ---------- interval of fitness evaluations to run PERIOD-FN
  PERIOD-FN ------- function to run every PERIOD fitness evaluations
  EVERY-PRE-FN ---- function to run before every fitness evaluation
  EVERY-POST-FN --- function to run after every fitness evaluation
  FILTER ---------- only include individual for which FILTER returns true"
  `(-search (new ,test ,max-evals ,max-time ,target ,period ,period-fn
                 ,every-pre-fn ,every-post-fn
                 ,filter ,running ,fitness-evals)
            #'new-individual
            (incorporate new ,population ,max-population-size)))

(defmacro mcmc
    (original test
     &key accept-fn max-evals max-time target period period-fn
       every-pre-fn every-post-fn
       filter
       (running '*running*)
       (fitness-evals '*fitness-evals*))
  "MCMC search from ORIGINAL until an optional stopping criterion is met.

Keyword arguments are as follows.
  ACCEPT-FN ------- function of current and new fitness, returns acceptance
  MAX-EVALS ------- stop after this many fitness evaluations
  MAX-TIME -------- stop after this many seconds
  TARGET ---------- stop when an individual passes TARGET-FIT
  PERIOD ---------- interval of fitness evaluations to run PERIOD-FN
  PERIOD-FN ------- function to run every PERIOD fitness evaluations
  EVERY-PRE-FN ---- function to run before every fitness evaluation
  EVERY-POST-FN --- function to run after every fitness evaluation"
  (let* ((curr (gensym))
         (body
          `(let ((,curr ,original))
             (-search (new ,test ,max-evals ,max-time ,target ,period ,period-fn
                           ,every-pre-fn ,every-post-fn ,filter
                           ,running ,fitness-evals)
                      (mcmc-step ,curr)
                      (when (funcall accept-fn (fitness ,curr) (fitness new))
                        (setf ,curr new))))))
    (if accept-fn
        body
        `(let ((accept-fn
                (lambda (curr new) ;; default to Metropolis Hastings
                  (or (funcall *fitness-predicate* new curr)
                      (< (random 1.0) ;; assumes numeric fitness
                         (if (> new curr) (/ curr new) (/ new curr)))))))
           ,body))))
