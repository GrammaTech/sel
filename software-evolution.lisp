;;; software-evolution.lisp --- Extant Software Evolution

;; Copyright (C) 2011-2013  Eric Schulte

;;; License: GNU General Public License, Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros :include-utf8))


;;; Software Object
(define-constant +software-evolution-version+
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (current-git-commit (pathname-directory *default-pathname-defaults*)))
  :test #'equalp
  :documentation
  "Current version of the SOFTWARE-EVOLUTION library.")

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
  (:documentation
   "Phenotype of the software.
Returns multiple values holding in order; (1) the binary path to which
the executable was compiled, (2) the errno, or a numeric indication of
success, of the compilation process, (3) STDERR of the compilation
process, or a string holding error output relevant to phenome
generation, (4) STDOUT of the compilation process, or a string holding
non-error output relevant to phenome generation, (5) the source file
name used during compilation. "))

(defgeneric evaluate (function software)
  (:documentation "Evaluate the software returning a numerical fitness."))

(defmethod evaluate ((test symbol) (obj software))
  (evaluate (symbol-function test) obj))

(defmethod evaluate ((test function) (obj software))
  (if (fitness obj)
      (values (fitness obj) (fitness-extra-data obj))
      (multiple-value-bind (fit extra) (funcall test obj)
        (setf (fitness obj) fit)
        (setf (fitness-extra-data obj) extra)
        (values fit extra))))

(defgeneric fitness-extra-data (software)
  (:documentation "Hold extra data returned by the fitness function."))

(defmethod fitness-extra-data ((obj software)) nil)

(defgeneric (setf fitness-extra-data) (extra-data software)
  (:documentation "Pass extra data (optionally) returned by the fitness function
                   to the software object."))

(defmethod (setf fitness-extra-data) (extra-data (obj software))
  (declare (ignorable extra-data)))

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

(defgeneric (setf genome-string) (text software)
  (:documentation "Set the `genome' of SOFTWARE to the string representation"))

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

(defgeneric pick-mutation-type (software)
  (:documentation "Select a type of mutation to apply to SOFTWARE."))

(defvar *mutation-stats* (make-hash-table
                          :test #'equal
                          #+sbcl :synchronized #+sbcl t
                          #+ccl :shared #+ccl :lock-free)
  "Variable to hold mutation statistics.")

(defvar *max-saved-mutation-improvements* 24
  "Maximum number of mutation improvements to hold on to.")

(defvar *mutation-improvements* nil
  "List of recent mutation improvements cons'd with *fitness-evals*.")

(defvar *crossover-stats* (make-hash-table
                           :test #'equal
                           #+sbcl :synchronized #+sbcl t
                           #+ccl :shared #+ccl :lock-free)
  "Variable to hold crossover statistics.")

(defvar *fitness-evals* 0
  "Track the total number of fitness evaluations.")

(defvar *fitness-predicate* #'>
  "Function to compare two fitness values to select which is preferred.")

(defun worst-numeric-fitness ()
  (cond ((equal #'< *fitness-predicate*) infinity)
        ((equal #'> *fitness-predicate*) 0)
        (t (error "bad *fitness-predicate* ~a" *fitness-predicate*))))

(defun worst-numeric-fitness-p (obj)
  (= (fitness obj)
     (worst-numeric-fitness)))

(defvar *worst-fitness-p* #'worst-numeric-fitness-p
  "Predicate indicating whether an individual has the worst possible fitness.")
(defvar *target-fitness-p* nil
  "Predicate indicating whether an individual has reached the target fitness.")

(defgeneric analyze-mutation (software mutation-info
                              &optional test)
  (:documentation "Collect statistics from an applied mutation.
Should return arguments unmodified as values to enable chaining
against `new-individual' with `multiple-value-call'.  Calculated Stats
should be added to the `*mutation-stats*' variable.  Calculated stats
are also returned as additional values.  This method will calculate
the fitness of SOFTWARE with `evaluate'.  Each mutation will be paired
with one of the tags; :dead, :same, :worse, :better.

If candidate fitness has not already been evaluated, then optional
argument TEST must be supplied."))

(defvar *analyze-mutation-verbose-stream* nil
  "Non-nil to print verbose output when analyzing mutations to value.")

(defmethod analyze-mutation ((obj software) mutation-info
                             &optional test
                             &aux result)
  ;; Mutation info from new-individual
  (destructuring-bind (mutation software-a cross-point-a
                                crossed software-b cross-point-b)
      mutation-info
    (labels ((safe-eval (object)
               (or (fitness object)
                   (progn
                     (assert test (object test)
                             "TEST is mandatory if objects have no fitness")
                     (evaluate test object))))
             (fitness-better-p (fitness-a fitness-b)
               (cond
                 ((and (numberp fitness-a) (numberp fitness-b))
                  (funcall *fitness-predicate* fitness-a fitness-b))
                 ((and (= (length fitness-a) (length fitness-b)))
                  (and (every (lambda (a b)
                                (or (funcall *fitness-predicate* a b)
                                    (= a b)))
                              fitness-a fitness-b)
                       (some *fitness-predicate* fitness-a fitness-b)))
                 (:otherwise (error "Can't compare fitness ~a and fitness ~a"
                                    fitness-a fitness-b))))
             (note (string)
               (when *analyze-mutation-verbose-stream*
                 (format *analyze-mutation-verbose-stream* string)))
             (classify (new old &optional old-2)
               (let ((fit (safe-eval new))
                     (old-fit (if (and old-2 *fitness-predicate*)
                                  (extremum (list (safe-eval old)
                                                  (safe-eval old-2))
                                            #'fitness-better-p)
                                  (safe-eval old))))
                 (values
                  (cond
                    ((not *fitness-predicate*) (note "?") :non-comparable)
                    ((funcall *worst-fitness-p* new) (note "_") :dead)
                    ((and (not (fitness-better-p fit old-fit))
                          (not (fitness-better-p old-fit fit)))
                     (note "=") :same)
                    ((funcall (complement #'fitness-better-p) fit old-fit)
                     (note "-") :worse)
                    ((fitness-better-p fit old-fit)
                     (note "+")
                     (push (cons (mutation-key crossed mutation)
                                 *fitness-evals*)
                           *mutation-improvements*)
                     (when (>= (length *mutation-improvements*)
                               *max-saved-mutation-improvements*)
                       (setf *mutation-improvements*
                             (butlast *mutation-improvements*)))
                     :better))
                  fit old-fit))))
      ;; Add information on the mutation to `*mutation-stats*`.
      (multiple-value-bind (effect fit old-fit)
          (classify obj crossed)
        (push (setf result (list mutation effect *fitness-evals* fit old-fit))
              (gethash (mutation-key crossed mutation) *mutation-stats*)))
      ;; Add information on the crossover to `*crossover-stats*`.
      (when cross-point-a
        (let ((effect (classify crossed software-a software-b)))
          (push (list mutation effect *fitness-evals*)
                (gethash (mutation-key crossed mutation) *crossover-stats*)))))
    (values
     obj mutation software-a cross-point-a crossed software-b cross-point-b
     (first result) (second result) (third result))))

(defgeneric mutation-key (software mutation)
  (:documentation "Key used to organize mutations in *mutation-stats*."))

(defmethod mutation-key ((obj software) mutation)
  ;; Default to using the mutation op.
  (declare (ignorable obj)) (car mutation))

(defun summarize-mutation-stats (&aux results)
  (maphash (lambda (key vals)
             (mapc (lambda (result)
                     (if (aget result (aget (car key) results))
                         (incf (aget result (aget (car key) results)))
                         (setf (aget result (aget (car key) results)) 1)))
                   (mapcar #'second vals)))
           *mutation-stats*)
  results)

(defgeneric mcmc-step (software)
  (:documentation "Change software in a way amenable to MCMC.
Specifically every step should be reversible, and the resulting walk
should be ergodic."))

(defvar *mcmc-fodder* nil
  "Holds the genome elements which may be used by `mcmc-step'.
Should be initialized to a list of the unique possible genome
elements.")

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

(define-condition no-mutation-targets (error)
  ((text :initarg :text :initform nil :reader text)
   (obj  :initarg :obj  :initform nil :reader obj)
   (op   :initarg :op   :initform nil :reader op))
  (:report (lambda (condition stream)
             (if (op condition)
                 (format stream "No targets error ~a applying ~S to ~S"
                         (text condition) (op condition) (obj condition))
                 (format stream "No targets error ~a on ~S"
                         (text condition) (obj condition))))))

(defgeneric apply-mutation (software mutation)
  (:documentation "Apply MUTATION to SOFTWARE, return the resulting software.
Define an :around method on this function to record mutations."))

(defgeneric apply-all-mutations (software mutation)
  (:documentation "Apply MUTATION to every target in SOFTWARE.
Returns the resulting software objects.  Returns a list of the applied
mutations as an optional second value."))

(defgeneric apply-picked-mutations (software mutation n)
  (:documentation "Apply MUTATION to N randomly selected targets in SOFTWARE.
Returns the resulting software objects.  Returns a list of the applied
mutations as an optional second value."))

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

(defgeneric instrument (software &key points functions trace-file print-argv)
  (:documentation
   "Instrument SOFTWARE to produce runtime execution information.
Keyword argument POINTS may hold an alist keyed by program points and
valued with strings to print when that point is executed.  When
present, keyword argument FUNCTIONS will hold a list of functions
which will be called on every instrumented AST returning a fprintf
format string and optional list of variable arguments to fprintf.
Keyword argument TRACE-FILE should hold a string file path to which
the trace will be written.  If TRACE-FILE is not supplied the trace
will be written to STDERR."))

(defgeneric from-string (software string)
  (:documentation "Initialize SOFTWARE with contents of STRING."))

(defgeneric from-file (software file)
  (:documentation "Initialize SOFTWARE with contents of FILE."))

(defgeneric to-file (software file)
  (:documentation "Write SOFTWARE to FILE"))

(defmethod to-file ((software software) file)
  (string-to-file (genome software) file))

(defgeneric apply-path (software key PATH) ; TODO: is this used?
  (:documentation "Apply the execution trace PATH behind KEY in SOFTWARE."))

(defgeneric expression (software what)
  (:documentation "Return WHAT in SOFTWARE as a lisp expression."))


;;; Mutation object
(defmacro define-mutation (class-name superclasses slots &rest options)
  "Like `defclass' but inherits TARGETER slot-options from MUTATION.
Also, ensures MUTATION is a member of superclasses"
  `(defclass ,class-name ,(if (member 'mutation (cons class-name superclasses))
                              superclasses
                              (append superclasses (list 'mutation)))
     ((targeter
       ,@(plist-merge
          (cdr (assoc 'targeter slots))
          (list :initarg :targeter :reader 'targeter
                :initform '(function pick-bad) :type 'function
                :documentation "A function from software -> targets.")))
      (picker
       ,@(plist-merge
          (cdr (assoc 'picker slots))
          (list :initarg :picker :reader 'picker
                :initform '(compose #'random-elt #'pick-bad) :type 'function
                :documentation "A function from software -> random target.")))
      ,@(remove-if {member _ '(targeter picker)} slots :key #'car))
     ,@options))

(defclass mutation ()
  ((object :initarg :object :accessor object :initform nil
           :type software
           :documentation "The software object to be mutated.")
   (targets :initarg :targets :reader get-targets :initform nil
            :type (list * *)
            :documentation "A calculated target set."))
  (:documentation "The base class of all software mutations."))

(defmethod print-object ((mut mutation) stream)
  (print-unreadable-object (mut stream :type t)
    (prin1 (object mut) stream)
    (when (or (get-targets mut) (targeter mut))
      (format stream " ")
      (prin1 (or (get-targets mut)
                 (multiple-value-call [#'third #'list]
                   (function-lambda-expression (targeter mut)))) stream))))

(defmethod targets ((mut mutation))
  (or (get-targets mut)
      (when (object mut)
        (setf (slot-value mut 'targets)
              (funcall (targeter mut) (object mut))))))

(defgeneric at-targets (mutation targets &key)
  (:documentation "Return a copy of MUTATION with `targets' set to TARGETS."))

(defmethod at-targets ((mut mutation) targets &key (object (object mut)))
  (make-instance (type-of mut) :object object :targets targets))

(defmethod mutation-key ((obj software) (mutation mutation))
  (declare (ignorable obj)) (type-of mutation))

(defmethod apply-mutation :before ((obj software) (mut mutation))
  ;; Mutation removes previously calculated fitness values.
  (declare (ignorable mut))
  (setf (fitness obj) nil))

(defmethod apply-all-mutations ((obj software) (mut mutation))
  (setf (object mut) obj)
  (iter (for targeted in (mapcar {at-targets mut} (targets mut)))
        (collect targeted into mutations)
        (collect (apply-mutation (copy obj) targeted) into results)
        (finally (return (values results mutations)))))

(defmethod apply-picked-mutations ((obj software) (mut mutation) n)
  (setf (object mut) obj)
  (iter (for i below n)
        (for picked = (funcall (picker mut) obj))
        (while picked)
        (let ((targeted (at-targets mut picked)))
          (collect targeted into mutations)
          (collect (apply-mutation (copy obj) targeted) into results))
        (finally (return (values results mutations)))))


;;; Evolution
(defvar *population* nil
  "Holds the variant programs to be evolved.")

(defvar *generations* nil
  "Holds the running generation count.")

(defvar *max-population-size* nil
  "Maximum allowable population size.")

(defvar *tournament-size* 2
  "Number of individuals to participate in tournament selection.")

(defvar *tournament-eviction-size* 2
  "Number of individuals to participate in eviction tournaments.")

(defvar *cross-chance* 2/3
  "Fraction of new individuals generated using crossover rather than mutation.")

(defvar *mut-rate* 1
  "Chance to mutate a new individual.
If <1, new individuals will be mutated once with change *MUT-RATE*.
If =1, then every new individual will be mutated exactly once.
If >1, then new individuals will be mutated from 1 to *MUT-RATE* times.")

(defvar *running* nil
  "True when a search process is running, set to nil to stop evolution.")

(defvar *start-time* nil
  "Holds the start time of evolutionary processes.")

(declaim (inline elapsed-time))
(defun elapsed-time () (/ (- (get-internal-real-time) *start-time*)
                          internal-time-units-per-second))

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

(defun default-select-one (group &key (predicate *fitness-predicate*))
  "Return the member of GROUP with most PREDICATE fitness.
Default selection function for `tournament'."
  (extremum group predicate :key #'fitness))

(defvar *tournament-selector* #'default-select-one
  "Function used to select a winner of a tournament.
Should take a group ")

(defun tournament
    (&key (predicate *fitness-predicate*) (size *tournament-size*))
  "Select an individual from *POPULATION* with a tournament."
  (flet ((verify (it)
           (assert (typep it 'software) (it)
                   "Population member is not software object")
           (assert (fitness it) (it)
                   "Population member with no fitness")
           it))
    (assert *population* (*population*) "Empty population.")
    (funcall *tournament-selector*
             (iter (for i below size)
                   (collect (verify (random-elt *population*))))
             :predicate predicate)))

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
    ;; NOTE: This `copy' call is only needed for `analyze-mutation'.
    ;; If it appears to be adding significant overhead, consider two
    ;; alternate implementations of `new-individual' instead of the
    ;; current approach in which `analyze-mutate' "wraps"
    ;; `new-individual'.
    (multiple-value-bind (mutant mutation) (mutant (copy crossed))
      (values mutant
              ;; Mutation info for analyze-mutation
              (list mutation a a-point crossed b b-point)))))

(defmacro -search (specs step &rest body)
  "Perform a search loop with early termination."
  (destructuring-bind (variant f max-evals max-time pd pd-fn
                               every-pre-fn every-post-fn
                               time filter running fitness-counter
                               collect-mutation-stats)
      specs
    (let ((main
           `(progn
              (unless ,time (setq ,time (get-internal-real-time)))
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
                 :do (restart-case
                         (,(if collect-mutation-stats 'handler-bind 'progn)
                           ,@(when collect-mutation-stats
                               `(((mutate
                                   (lambda (err)
                                     (when (and (op err) (obj err))
                                       (push (list (op err) :error)
                                             (gethash
                                              (mutation-key (obj err) (op err))
                                              *mutation-stats*))))))))
                           (multiple-value-bind (,variant mutation-info)
                               (funcall ,step)
                             ,@(unless collect-mutation-stats
                                 `((declare (ignorable mutation-info))))
                             ,@(when every-pre-fn
                                 `((funcall ,every-pre-fn ,variant)))
                             (evaluate ,f ,variant)
                             ,@(when collect-mutation-stats
                                 `((funcall #'analyze-mutation
                                            ,variant mutation-info ,f)))
                             ,@(when every-post-fn
                                 `((funcall ,every-post-fn ,variant)))
                             (incf ,fitness-counter)
                             ,@(when (and pd pd-fn)
                                 `((when (zerop (mod ,fitness-counter ,pd))
                                     (funcall ,pd-fn))))
                             (assert (fitness ,variant) (,variant)
                                     "Variant with no fitness")
                             ,@(if filter
                                   `((when (funcall ,filter ,variant) ,@body))
                                   body)
                             (when (and *target-fitness-p*
                                        (funcall *target-fitness-p* ,variant))
                               (setq ,running nil)
                               (return-from target-reached ,variant))))
                       (ignore-failed-mutation ()
                         :report
                         "Ignore failed mutation and continue evolution")))
              (setq ,running nil))))
      (setf main `(block target-reached ,main))
      main)))

(defmacro mcmc (original test
                &key accept-fn max-evals max-time period period-fn
                  every-pre-fn every-post-fn
                  (time '*start-time*)
                  filter
                  (running '*running*)
                  (fitness-evals '*fitness-evals*)
                  mutation-stats)
  "MCMC search from ORIGINAL until an optional stopping criterion is met.

Use `*target-fitness-p*' to set a target fitness.

Keyword arguments are as follows.
  ACCEPT-FN ------- function of current and new fitness, returns acceptance
  MAX-EVALS ------- stop after this many fitness evaluations
  MAX-TIME -------- stop after this many seconds
  PERIOD ---------- interval of fitness evaluations to run PERIOD-FN
  PERIOD-FN ------- function to run every PERIOD fitness evaluations
  EVERY-PRE-FN ---- function to run before every fitness evaluation
  EVERY-POST-FN --- function to run after every fitness evaluation
  MUTATION-STATS -- set to non-nil to collect mutation statistics"
  (let* ((curr (gensym))
         (body
          `(let ((,curr ,original))
             (-search (new ,test ,max-evals ,max-time ,period ,period-fn
                           ,every-pre-fn ,every-post-fn ,time ,filter
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

(defmacro evolve (test
                  &key max-evals max-time period period-fn
                    every-pre-fn every-post-fn
                    filter
                    (population '*population*)
                    (max-population-size '*max-population-size*)
                    (time '*start-time*)
                    (running '*running*)
                    (fitness-evals '*fitness-evals*)
                    mutation-stats)
  "Evolves `*population*' until an optional stopping criterion is met.

Use `*target-fitness-p*' to set a target fitness.

Keyword arguments are as follows.
  MAX-EVALS ------- stop after this many fitness evaluations
  MAX-TIME -------- stop after this many seconds
  PERIOD ---------- interval of fitness evaluations to run PERIOD-FN
  PERIOD-FN ------- function to run every PERIOD fitness evaluations
  EVERY-PRE-FN ---- function to run before every fitness evaluation
  EVERY-POST-FN --- function to run after every fitness evaluation
  FILTER ---------- only include individual for which FILTER returns true
  MUTATION-STATS -- set to non-nil to collect mutation statistics"
  `(-search (new ,test ,max-evals ,max-time ,period ,period-fn
                 ,every-pre-fn ,every-post-fn
                 ,time ,filter ,running ,fitness-evals ,mutation-stats)
            #'new-individual
            (incorporate new ,population ,max-population-size)))

(defun generational-evolve
    (reproduce evaluate select
     &key
       every-pre-fn every-post-fn mutation-stats test period period-fn
       max-generations max-evals max-time filter)
  "Evolves `*population*' until an optional stopping criterion is met.

Required arguments are as follows:
  REPRODUCE -------- create new individuals from the current population
  EVALUATE --------- evaluate the entire population
  SELECT ----------- select best individuals from the population
Keyword arguments are as follows:
  MAX-GENERATIONS -- stop after this many generations
  MAX-EVALS -------- stop after this many fitness evaluations
  MAX-TIME --------- stop after this many seconds
  PERIOD ----------- interval of generations evaluations to run PERIOD-FN
  PERIOD-FN -------- function to run every PERIOD generations
  EVERY-POST-FN ---- function to run on every new individual before evaluation
  EVERY-POST-FN ---- function to run on every new individual after evaluation
  MUTATION-STATS --- set to non-nil to collect mutation statistics
  TEST ------------- fitness test function for mutation statistics
  FILTER ----------- remove individuals for which FILTER returns false"

  (setq *running* t)
  (setq *start-time* (get-internal-real-time))
  (flet
      ((check-max (current max) (or (not max)
                                    (not current)
                                    (< current max))))
    (prog1
        (loop
         :while (and *running*
                     (check-max *generations* max-generations)
                     (check-max *fitness-evals* max-evals)
                     (check-max (elapsed-time) max-time))
         :do
         (setf *generations* (+ 1 (or *generations* 0)))
         (multiple-value-bind (children mutation-info)
             (funcall reproduce *population*)

           (if every-pre-fn (mapc every-pre-fn children))

           (funcall evaluate children)

           (if every-post-fn (mapc {funcall every-post-fn} children))

           (if filter (setq children (delete-if-not filter children)))
           (setq *population* (append children *population*))

           (if mutation-stats
               (mapcar (lambda (c info) (analyze-mutation c info test))
                       children mutation-info))

           (loop :for child :in children
              :when (funcall *target-fitness-p* child)
              :do
              (setf *running* nil)
              (return-from generational-evolve child)))

         (setq *population*
               (funcall select *population* *max-population-size*))
         (assert (<= (length *population*) *max-population-size*))

         (if (and period period-fn (zerop (mod *generations* period)))
             (funcall period-fn)))
      (setq *running* nil))))

(defun simple-reproduce (population)
  (let (children mutations)
    (iter (for parent in population)
          (restart-case
              (multiple-value-bind (child info)
                  (new-individual parent (random-elt population))
                (push child children)
                (push info mutations))
            (ignore-failed-mutation ()
              :report
              "Ignore failed mutation and continue evolution")))
    (values children mutations)))

(defun simple-evaluate (test new-children)
  (mapc (lambda (child)
          (incf *fitness-evals*)
          (restart-case
              (evaluate test child)
            (worse-for-failed-fitness-evaluation ()
              :report
              "Assign `worst-numeric-fitness' for failed fitness evaluation.")))
        new-children))

(defun simple-select (population max-size &aux new-pop)
  (declare (ignorable population)) ; tournament uses global *population*
  (iter (until (= max-size (length new-pop)))
        (restart-case (push (tournament) new-pop)
          (ignore-failed-selection ()
            :report "Ignore failed `tournament' selection."))))
