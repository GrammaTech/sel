;;; software-evolution-library.lisp --- Extant Software Evolution

;; Copyright (C) 2011-2013  Eric Schulte

;;; License: GNU General Public License, Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution-library)
(enable-curry-compose-reader-macros :include-utf8)


;;; Software Object
(define-constant +software-evolution-library-version+
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (current-git-commit (pathname-directory
                           #.(or *compile-file-truename*
                                 *load-truename*
                                 *default-pathname-defaults*))))
  :test #'equalp
  :documentation
  "Current version of the SOFTWARE-EVOLUTION-LIBRARY.")

(define-constant +software-evolution-library-branch+
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (current-git-branch (pathname-directory
                           #.(or *compile-file-truename*
                                 *load-truename*
                                 *default-pathname-defaults*))))
  :test #'equalp
  :documentation
  "Current branch of the SOFTWARE-EVOLUTION-LIBRARY.")

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
                     `((with-slots (,accessor) copy
                         (setf ,accessor
                               ,(if copier
                                    `(,copier (,accessor obj))
                                    `(,accessor obj))))))))
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

(defgeneric evaluate (function software &rest extra-keys &key &allow-other-keys)
  (:documentation "Evaluate the software returning a numerical fitness."))


(defmethod evaluate ((test symbol) (obj software)
                     &rest extra-keys &key &allow-other-keys)
  (declare (ignorable extra-keys))
  (evaluate (symbol-function test) obj))

(defmethod evaluate ((test function) (obj software)
                     &rest extra-keys &key &allow-other-keys)
  (declare (ignorable extra-keys))
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

(defgeneric pick-bad-good (software &key)
  (:documentation "Pick a 'bad' and a 'good' indexes into a software object.
Used to target mutation."))
(defmethod pick-bad-good ((software software) &key)
  (list (pick-bad software) (pick-good software)))

(defgeneric pick-bad-bad (software &key)
  (:documentation "Pick two 'bad' indexes into a software object.
Used to target mutation."))
(defmethod pick-bad-bad ((software software) &key)
  (list (pick-bad software) (pick-bad software)))

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

(defun fitness-scalar (fitness)
  (cond ((numberp fitness) fitness)
        ((or (listp fitness) (vectorp fitness))
         (reduce #'+ fitness))
        (:otherwise (error "Can't convert fitness ~a to a scalar"
                           fitness))))

(defvar *fitness-scalar-fn* #'fitness-scalar
  "Function to convert fitness to a numeric value")

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

(defun fitness-better-p (fitness-a fitness-b)
  "Check if FITNESS-A is strictly better than FITNESS-B."
  (funcall *fitness-predicate*
           (funcall *fitness-scalar-fn* fitness-a)
           (funcall *fitness-scalar-fn* fitness-b)))

(defun fitness-equal-p (fitness-a fitness-b)
  "Return true if FITNESS-A and FITNESS-B are equal"
  (equalp fitness-a fitness-b))

(defun analyze-mutation (obj mutation-info test &aux result)
  "Default function to collect statistics from an applied mutation.

This function will calculate the improvements to the fitness of SOFTWARE
as the result of crossover and mutation using `evaluate' and TEST.
Each crossover and mutation will be paired with one of the following tags;
:dead, :same, :worse, or :better.  Calculated stats will be added to the
*crossover-stats* and *mutation-stats* variables for analysis."
  ;; Mutation info from new-individual
  (destructuring-bind (mutation software-a cross-point-a
                                crossed software-b cross-point-b)
      mutation-info

    ;; Evaluate software objects to ensure fitness
    (when crossed    (evaluate test crossed))    ; Evaluate for fitness
    (when software-a (evaluate test software-a)) ; Safety - should have fitness
    (when software-b (evaluate test software-b)) ; Safety - should have fitness
    (when obj        (evaluate test obj))        ; Safety - should have fitness

    ;; Add information on the mutation to `*mutation-stats*`.
    (multiple-value-bind (effect fit old-fit)
        (classify obj crossed)
      (when (equal effect :better)
        (push (cons (mutation-key crossed mutation)
                    *fitness-evals*)
              *mutation-improvements*)
        (when (>= (length *mutation-improvements*)
                  *max-saved-mutation-improvements*)
          (setf *mutation-improvements*
                (butlast *mutation-improvements*))))
      (push (setf result (list effect *fitness-evals* fit old-fit))
            (gethash (mutation-key crossed mutation) *mutation-stats*)))

    ;; Add information on the crossover to `*crossover-stats*`.
    (when cross-point-a
      (let ((effect (classify crossed software-a software-b)))
        (push (list effect *fitness-evals*)
              (gethash (mutation-key crossed mutation) *crossover-stats*))))

    (values
     obj mutation
     software-a cross-point-a crossed software-b cross-point-b
     (first result))))

(defmethod classify (new &rest old)
  "Classify the fitness of NEW as :BETTER, :WORSE, :SAME, or :DEAD when
compared to OLD.  NEW and OLD must have fitness populated."
  (let ((fit (fitness new))
        (old-fit (extremum (mapcar {fitness} old)
                           #'fitness-better-p)))
    (values
     (cond
       ((funcall *worst-fitness-p* new) :dead)
       ((and (not (fitness-better-p fit old-fit))
             (not (fitness-better-p old-fit fit)))
        :same)
       ((funcall (complement #'fitness-better-p) fit old-fit)
        :worse)
       ((fitness-better-p fit old-fit)
        :better))
     fit old-fit)))

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
                   (mapcar #'first vals)))
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

(define-condition no-mutation-targets (mutate)
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

(defgeneric apply-mutations (software mutation n)
  (:documentation "Apply MUTATION to the first N targets in SOFTWARE.
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

(defmacro compose-mutations (class-name mutations &rest options)
  "Define a new mutation named CLASS-NAME composing MUTATIONS.
MUTATIONS is a list of the names of mutation classes."
  (with-gensyms ((args args)
                 (mut mut)
                 (software software)
                 (mutation mutation)
                 (target target))
    (flet ((slot-initform (slot-name class)
             (finalize-inheritance (find-class class))
             (slot-definition-initform
              (find-if [{eql slot-name} #'slot-definition-name]
                       (class-slots (find-class class))))))
      `(prog1
           (define-mutation ,class-name
               ,(remove-duplicates
                 (mappend
                  (lambda (obj)
                    (remove-if «or {eql 'standard-object} {eql 'mutation}»
                               (mapcar #'class-name
                                       (class-direct-superclasses
                                        (find-class obj)))))
                  mutations))
             ((targeter
               :initform
               (lambda (&rest ,args)
                 (list ,@(mapcar
                          (lambda (fun) `(apply ,fun ,args))
                          (mapcar {slot-initform 'targeter} mutations))))
               :type 'function
               :documentation
               ,(format nil "Targeters from ~a." mutations))
              (picker
               :initform
               (lambda (targets)
                 (mapcar
                  (lambda (target picker) (funcall picker target))
                  targets
                  (list ,@(mapcar {slot-initform 'picker} mutations))))
               :type 'function
               :documentation
               ,(format nil "Pickers from ~a." mutations)))
             ;; NOTE: Should compose other slots as well.
             ,@options)
         (defmethod build-op ((,mut ,class-name) ,software)
           (mappend
            (lambda (,mutation ,target)
              (build-op (make-instance ,mutation :targets ,target)
                        ,software))
            ',mutations
            (targets ,mut)))))))

(defmacro sequence-mutations  (class-name mut-a mut-b &rest options)
  "Define a new mutation named CLASS-NAME sequencing MUT-A and MUT-B.
MUT-A and MUT-B are instances of mutations.  Instead of collecting
targets for A and then targets for B and then applying A and B as done
by `compose-mutations', `sequence-mutations' first targets and applies A and then targets and applied B."
  (declare (ignorable class-name mut-a mut-b options))
  (error "TODO: Implement `sequence-mutations'."))

(defclass mutation ()
  ((object :initarg :object :accessor object :initform nil
           :type (or software null)
           :documentation "The software object to be mutated.")
   (targets :initarg :targets :reader get-targets :initform nil
            :type (or list fixnum ast-ref)
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
  (apply-mutations obj mut infinity))

(defmethod apply-mutations ((obj software) (mut mutation) n)
  (setf (object mut) obj)
  (iter (for targeted in (mapcar {at-targets mut} (targets mut)))
        (for i below n)
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

(defun incorporate (software)
  "Incorporate SOFTWARE into POPULATION, keeping POPULATION size constant."
  (push software *population*)
  (loop :while (and *max-population-size*
                    (> (length *population*) *max-population-size*))
     :do (evict)))

(defvar *tie-breaker-predicate* #'>
  "Function to compare two tie breaker values to select which is preferred.")

(defun evict ()
  (let ((loser (tournament :predicate (complement *fitness-predicate*)
                           :size *tournament-eviction-size*
                           :tie-breaker-predicate
                              (complement *tie-breaker-predicate*))))
    (setf *population* (remove loser *population* :count 1))
    loser))

(defun default-select-best (group &key (predicate *fitness-predicate*))
  "Return the members of GROUP with most PREDICATE fitness.
Default selection function for `tournament'."
  (remove-if-not [{= (fitness (extremum group predicate :key #'fitness))}
                  #'fitness]
                 group))

(defun default-random-winner (group &key predicate)
  "Choose a random winner from GROUP."
  (declare (ignorable predicate))
  (random-elt group))

(defvar *tournament-selector* #'default-select-best
  "Function used to select winners of a tournament.")

(defvar *tournament-tie-breaker* #'default-random-winner
  "Function used to break ties in a tournament.")

(defun tournament
    (&key (predicate *fitness-predicate*)
       (tie-breaker-predicate *tie-breaker-predicate*)
       (size *tournament-size*))
  "Select an individual from *POPULATION* with a tournament."
  (flet ((verify (it)
           (assert (typep it 'software) (it)
                   "Population member is not software object")
           (assert (fitness it) (it)
                   "Population member with no fitness")
           it))
    (assert *population* (*population*) "Empty population.")
    (funcall *tournament-tie-breaker*
             (funcall *tournament-selector*
                      (iter (for i below size)
                            (collect (verify (random-elt *population*))))
                      :predicate predicate)
             :predicate tie-breaker-predicate)))

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
  "Perform a search loop with early termination.

SPECS should be a list of the following elements.

  (VARIANT MUT-INFO) -- Symbols for variant and mut-info resulting from STEP
  TEST ---------------- Test function used to `evaluate' every VARIANT.
  MAX-EVALS ----------- Maximum number of evaluations to perform.
  MAX-TIME ------------ Maximum time to run.
  PERIOD -------------- Period (in evals) at which to call PERIOD-FN.
  PERIOD-FN ----------- Function to call every Period evals.
  EVERY-PRE-FN -------- Function to call before every evaluation.
  EVERY-POST-FN ------- Function to call after every evaluation.
  FILTER -------------- Function to filter variants from BODY.
  ANALYZE-MUTATION-FN - Function to call to analyze mutation results

The following global variables are implicitly updated by this function
and should be dynamically bound to perform multiple different
simultaneous searches, `*running*', `*start-time*', `*fitness-evals*'.
The global variable `*target-fitness-p*' implicitly defines a stopping
criteria for this search."
  (destructuring-bind (variant mutation-info) (car specs)
    ;; Outside the returned code let-bind unevaluated elements of SPECS.
    (with-gensyms ((test test)
                   (max-evals max-evals)
                   (max-time max-time)
                   (period period)
                   (period-fn period-fn)
                   (every-pre-fn every-pre-fn)
                   (every-post-fn every-post-fn)
                   (filter filter)
                   (analyze-mutation-fn analyze-mutation-fn))
      ;; Inside the returned code let-bind evaluated elements of SPECS.
      `(let ((,test ,(nth 1 specs))
             (,max-evals ,(nth 2 specs))
             (,max-time ,(nth 3 specs))
             (,period ,(nth 4 specs))
             (,period-fn ,(nth 5 specs))
             (,every-pre-fn ,(nth 6 specs))
             (,every-post-fn ,(nth 7 specs))
             (,filter ,(nth 8 specs))
             (,analyze-mutation-fn ,(nth 9 specs)))
         (block search-target-reached
           (unless *start-time* (setq *start-time* (get-internal-real-time)))
           (setq *running* t)
           (loop :until (or (not *running*)
                            (and ,max-evals
                                 (> *fitness-evals* ,max-evals))
                            (and ,max-time
                                 (> (/ (- (get-internal-real-time) *start-time*)
                                       internal-time-units-per-second)
                                    ,max-time))) :do
              (restart-case
                  (multiple-value-bind (,variant ,mutation-info)
                      (funcall ,step)
                    (when ,every-pre-fn
                      (funcall ,every-pre-fn ,variant))
                    (evaluate ,test ,variant)
                    (when ,analyze-mutation-fn
                      (funcall ,analyze-mutation-fn ,variant
                                                    ,mutation-info
                                                    ,test))
                    (when ,every-post-fn
                      (funcall ,every-post-fn ,variant))
                    (incf *fitness-evals*)
                    (when (and ,period ,period-fn
                               (zerop (mod *fitness-evals* ,period)))
                      (funcall ,period-fn))
                    (assert (fitness ,variant) (,variant)
                            "Variant with no fitness")
                    (when (or (not ,filter) (funcall ,filter ,variant))
                      ,@body)
                    (when (and *target-fitness-p*
                               (funcall *target-fitness-p* ,variant))
                      (return-from search-target-reached ,variant)))
                (ignore-failed-mutation ()
                  :report
                  "Ignore failed mutation and continue evolution"))))
         (setq *running* nil)))))

(defmacro mcmc (original test
                &key
                  accept-fn max-evals max-time period period-fn
                  every-pre-fn every-post-fn filter analyze-mutation-fn)
  "MCMC search from ORIGINAL using `mcmc-step' and TEST.
If keyword argument ACCEPT-FN is given it is used to determine when a
newly found candidate replaces the current candidate.  If ACCEPT-FN is
not supplied MCMC defaults to using Metropolis Hastings.

Other keyword arguments are used as defined in the `-search' function."
  (let* ((curr (gensym))
         (body
          `(let ((,curr ,original))
             (-search ((new mut-info)
                       ,test ,max-evals ,max-time ,period ,period-fn
                       ,every-pre-fn ,every-post-fn
                       ,filter ,analyze-mutation-fn)
                      (mcmc-step ,curr)
                      (when (funcall accept-fn (fitness ,curr) (fitness new))
                        (setf ,curr new))))))
    (if accept-fn
        body
        `(let ((accept-fn
                (lambda (curr new) ;; Default to Metropolis Hastings.
                  (or (funcall *fitness-predicate* new curr)
                      (< (random 1.0) ;; Assume a numeric fitness.
                         (if (> new curr) (/ curr new) (/ new curr)))))))
           ,body))))

(defmacro evolve (test
                  &key
                    max-evals max-time period period-fn
                    every-pre-fn every-post-fn filter analyze-mutation-fn)
  "Evolves `*population*' using `new-individual' and TEST.
Keyword arguments are used as defined in the `-search' function."
  `(-search ((new mut-info)
             ,test ,max-evals ,max-time ,period ,period-fn
             ,every-pre-fn ,every-post-fn ,filter ,analyze-mutation-fn)
            #'new-individual
            (incorporate new)))

(defun generational-evolve
    (reproduce evaluate select
     &key
       every-pre-fn every-post-fn analyze-mutation-fn test period period-fn
       max-generations max-evals max-time filter)
  "Evolves `*population*' using REPRODUCE EVALUATE and SELECT.

Required arguments are as follows:
  REPRODUCE ----------- create new individuals from the current population
  EVALUATE ------------ evaluate the entire population
  SELECT -------------- select best individuals from the population
Keyword arguments are as follows:
  MAX-GENERATIONS ----- stop after this many generations
  MAX-EVALS ----------- stop after this many fitness evaluations
  MAX-TIME ------------ stop after this many seconds
  PERIOD -------------- interval of generations evaluations to run PERIOD-FN
  PERIOD-FN ----------- function to run every PERIOD generations
  EVERY-PRE-FN -------- function to run on each new individual before evaluation
  EVERY-POST-FN ------- function to run on each new individual after evaluation
  ANALYZE-MUTATION-FN - function to call to analyze mutation results
  TEST ---------------- fitness test function for mutation statistics
  FILTER -------------- remove individuals for which FILTER returns false"

  (setq *running* t)
  (setq *generations* 0)
  (setq *start-time* (get-internal-real-time))
  (flet
      ((check-max (current max)
         (or (not max) (not current) (< current max))))
    (prog1
        (loop :while (and *running*
                          (check-max *generations* max-generations)
                          (check-max *fitness-evals* max-evals)
                          (check-max (elapsed-time) max-time)) :do
           (incf *generations*)
           (multiple-value-bind (children mutation-info)
               (funcall reproduce *population*)
             (if every-pre-fn (mapc every-pre-fn children))
             (funcall evaluate children)
             (if analyze-mutation-fn
                 (mapcar (lambda (c info)
                           (funcall analyze-mutation-fn c info test))
                         children mutation-info))
             (if every-post-fn (mapc {funcall every-post-fn} children))
             (if filter (setq children (delete-if-not filter children)))
             (setq *population* (append children *population*))
             (loop :for child :in children
                :when (funcall *target-fitness-p* child) :do
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
