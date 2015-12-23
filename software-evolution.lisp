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
             ,@(mappend
                (lambda (accessor copier)
                  (case copier
                    (:none nil)
                    (:direct
                     `((with-slots (,accessor) copy
                         (setf ,accessor
                               (with-slots (,accessor) obj ,accessor)))))
                    (otherwise
                     `((setf (,accessor copy)
                             ,(if copier
                                  `(,copier (,accessor obj))
                                  `(,accessor obj)))))))
                (mapcar #'car slots)
                (mapcar {plist-get :copier} slots))
             copy)))))

(defgeneric genome (software)
  (:documentation "Genotype of the software."))

(defgeneric phenome (software &key bin)
  (:documentation "Phenotype of the software."))

(defgeneric evaluate (function software)
  (:documentation "Evaluate the software returning a numerical fitness."))

(defmethod evaluate ((test function) (obj software))
  (if (fitness obj)
      (values (fitness obj) (fitness-extra-data obj))
      (multiple-value-bind (fit extra) (funcall test obj)
        (assert (numberp fit) (fit)
                "Test ~a returned non-numerical fitness ~a for software ~a."
                test fit obj)
        (setf (fitness obj) fit)
        (setf (fitness-extra-data obj) extra)
        (values fit extra))))

(defgeneric fitness-extra-data (software)
  (:documentation "Hold extra data returned by the fitness function."))

(defmethod fitness-extra-data ((obj software)) nil)

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

(defmethod mutate :before ((obj software))
  ;; Mutation removes previously calculated fitness values.
  (setf (fitness obj) nil))

(defvar *mutation-stats* (make-hash-table
                          #+sbcl :synchronized #+sbcl t)
  "Variable to hold mutation statistics.")

(defvar *crossover-stats* (make-hash-table
                           #+sbcl :synchronized #+sbcl t)
  "Variable to hold crossover statistics.")

(defgeneric analyze-mutation (software mutation
                              software-a cross-point-a
                              crossed
                              software-b cross-point-b
                              &optional test)
  (:documentation "Collect statistics from an applied mutation.
Should return arguments unmodified as values to enable chaining
against `new-individual' with `multiple-value-call'.  Calculated Stats
should be added to the `*mutation-stats*' variable.  This method will
calculate the fitness of SOFTWARE with `evaluate'.

If candidate fitness has not already been evaluated, then optional
argument TEST must be supplied."))

(defmethod analyze-mutation ((obj software) mutation
                             software-a cross-point-a
                             crossed
                             software-b cross-point-b
                             &optional test)
  (labels ((safe-eval (object)
             (or (fitness object)
                 (progn
                   (assert test (object test)
                           "TEST is mandatory if objects have no fitness")
                   (evaluate test object))))
           (classify (new old &optional old-2)
             (let ((fit (safe-eval new))
                   (old-fit (if old-2
                                (extremum (list (safe-eval old)
                                                (safe-eval old-2))
                                          *fitness-predicate*)
                                (safe-eval old))))
               (cond
                 ((= fit (worst)) :dead)
                 ((= fit old-fit) :same)
                 ((funcall (complement *fitness-predicate*) fit old-fit) :worse)
                 ((funcall *fitness-predicate* fit old-fit) :better)))))
    ;; Add information on the mutation to `*mutation-stats*`.
    (let ((effect (classify obj (or crossed software-a))))
      (push (list mutation effect)
            (gethash (mutation-key obj mutation) *mutation-stats*)))
    ;; Add information on the crossover to `*crossover-stats*`.
    (when crossed
      (let ((effect (classify crossed software-a software-b)))
        (push (list mutation effect)
              (gethash (mutation-key obj mutation) *crossover-stats*)))))
  (values
   obj mutation software-a cross-point-a crossed software-b cross-point-b))

(defgeneric mutation-key (software mutation)
  (:documentation "Key used to organize mutations in *mutation-stats*."))

(defmethod mutation-key ((obj software) mutation)
  ;; Default to using the mutation op.
  (declare (ignorable obj)) (car mutation))

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
  ((text :initarg :text :initform nil :reader text)
   (obj  :initarg :obj  :initform nil :reader obj)
   (op   :initarg :op   :initform nil :reader op))
  (:report (lambda (condition stream)
             (if (op condition)
                 (format stream "Mutation error ~a applying ~S to ~S"
                         (text condition) (op condition) (obj condition))
                 (format stream "Mutation error ~a on ~S"
                         (text condition) (obj condition))))))

(defgeneric apply-mutation (software mutation)
  (:documentation "Apply MUTATION to SOFTWARE.
Define an :around method on this function to record mutations."))

(defgeneric crossover (software-a software-b)
  (:documentation "Crossover two software objects.
Define an :around method on this function to record crossovers."))

(defmethod crossover :around ((software-a software) (software-b software))
  ;; Mutation removes previously calculated fitness values.
  (multiple-value-call (lambda (child &rest rest)
                         (setf (fitness child) nil)
                         (apply #'values child rest))
    (call-next-method)))

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

(declaim (inline worst))
(defun worst ()
  (cond ((equal #'< *fitness-predicate*) infinity)
        ((equal #'> *fitness-predicate*) 0)
        (t (error "bad *fitness-predicate* ~a" *fitness-predicate*))))

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
      (values mutant mutation a a-point crossed b b-point))))

(defmacro -search (specs step &rest body)
  "Perform a search loop with early termination."
  (destructuring-bind (variant f max-evals max-time target pd pd-fn
                               every-pre-fn every-post-fn
                               filter running fitness-counter
                               collect-mutation-stats)
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
                  :do (,(if collect-mutation-stats 'handler-bind 'progn)
                        ,@(when collect-mutation-stats
                                `(((mutate
                                    (lambda (err)
                                      (push (list (op err) :error)
                                            (gethash
                                             (mutation-key (obj err) (op err))
                                             *mutation-stats*)))))))
                        (multiple-value-bind
                              (,variant mutation a a-point crossed b b-point)
                            (funcall ,step)
                          ,@(unless collect-mutation-stats
                                    `((declare (ignorable mutation
                                                          a a-point crossed
                                                          b b-point))))
                          ,@(when every-pre-fn
                                  `((funcall ,every-pre-fn ,variant)))
                          (evaluate ,f ,variant)
                          ,@(when collect-mutation-stats
                                  `((funcall #'analyze-mutation
                                             ,variant mutation
                                             a a-point crossed b b-point
                                             ,f)))
                          ,@(when every-post-fn
                                  `((funcall ,every-post-fn ,variant)))
                          (incf ,fitness-counter)
                          ,@(when (and pd pd-fn)
                                  `((when (zerop (mod ,fitness-counter ,pd))
                                      (funcall ,pd-fn))))
                          (assert (numberp (fitness ,variant)) (,variant)
                                  "Non-numeric fitness: ~S"
                                  (fitness ,variant))
                          ,@(if filter
                                `((when (funcall ,filter ,variant) ,@body))
                                body)
                          ,@(when target
                                  `((when (let ((,fit-var (fitness ,variant)))
                                            (or (equal ,fit-var ,target)
                                                (funcall *fitness-predicate*
                                                         ,fit-var ,target)))
                                      (setq ,running nil)
                                      (return ,variant))))))))))
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
            (fitness-evals '*fitness-evals*)
            mutation-stats)
  "Evolves `*population*' until an optional stopping criterion is met.

Keyword arguments are as follows.
  MAX-EVALS ------- stop after this many fitness evaluations
  MAX-TIME -------- stop after this many seconds
  TARGET ---------- stop when an individual passes TARGET-FIT
  PERIOD ---------- interval of fitness evaluations to run PERIOD-FN
  PERIOD-FN ------- function to run every PERIOD fitness evaluations
  EVERY-PRE-FN ---- function to run before every fitness evaluation
  EVERY-POST-FN --- function to run after every fitness evaluation
  FILTER ---------- only include individual for which FILTER returns true
  MUTATION-STATS -- set to non-nil to collect mutation statistics"
  `(-search (new ,test ,max-evals ,max-time ,target ,period ,period-fn
                 ,every-pre-fn ,every-post-fn
                 ,filter ,running ,fitness-evals ,mutation-stats)
            #'new-individual
            (incorporate new ,population ,max-population-size)))

(defmacro mcmc
    (original test
     &key accept-fn max-evals max-time target period period-fn
       every-pre-fn every-post-fn
       filter
       (running '*running*)
       (fitness-evals '*fitness-evals*)
       mutation-stats)
  "MCMC search from ORIGINAL until an optional stopping criterion is met.

Keyword arguments are as follows.
  ACCEPT-FN ------- function of current and new fitness, returns acceptance
  MAX-EVALS ------- stop after this many fitness evaluations
  MAX-TIME -------- stop after this many seconds
  TARGET ---------- stop when an individual passes TARGET-FIT
  PERIOD ---------- interval of fitness evaluations to run PERIOD-FN
  PERIOD-FN ------- function to run every PERIOD fitness evaluations
  EVERY-PRE-FN ---- function to run before every fitness evaluation
  EVERY-POST-FN --- function to run after every fitness evaluation
  MUTATION-STATS -- set to non-nil to collect mutation statistics"
  (let* ((curr (gensym))
         (body
          `(let ((,curr ,original))
             (-search (new ,test ,max-evals ,max-time ,target ,period ,period-fn
                           ,every-pre-fn ,every-post-fn ,filter
                           ,running ,fitness-evals ,mutation-stats)
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
