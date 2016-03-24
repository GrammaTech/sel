;;; clang software object with dynamically adapting mutation
;;; probabilities based on the results of previous mutations.
(in-package :bed)

(define-constant +initial-mutation-results-queue+
  (make-array 1024
              :element-type '(cons symbol symbol)
              :initial-element (cons :nothing :nothing))
  :test #'equalp
  :documentation
  "Initial value of the *mutation-results-queue*")

(defvar *mutation-results-queue* (copy-seq +initial-mutation-results-queue+)
  "Queue containing pairs (MUTATION-TYPE . MUTATION-RESULT) for
the last *max-mutation-results-queue-length* mutations")

(defvar *mutation-results-queue-next* (the fixnum 0))

(defvar *mutation-results-queue-lock*
  (make-lock "mutation-results-queue"))

(defun queue-mutation (type classification)
  (declare (optimize speed))
  (with-lock-held (*mutation-results-queue-lock*)
    (setf (the (cons symbol symbol)
               (aref *mutation-results-queue* *mutation-results-queue-next*))
          (the (cons symbol symbol) (cons type classification)))
    (incf (the fixnum *mutation-results-queue-next*))
    (when (= (the fixnum (length (the vector *mutation-results-queue*)))
             (the fixnum *mutation-results-queue-next*))
      (setf *mutation-results-queue-next* 0))))

(defvar *bias-toward-dynamic-mutation* 1/2
  "Degree to which dynamic weights are emphasized over default weights.
See `*bed-default-mutation-types*' and `*bed-mutation-types*'.")
(defvar *better-bias* 5/4)
(defvar *same-bias* 1)
(defvar *worse-bias* 1/10)
(defvar *dead-bias* 0)

(define-software clang-w-adaptive-mutation (clang-w-binary clang-w-fodder) ())

(defmethod mutate :around ((obj clang-w-adaptive-mutation))
  (multiple-value-bind (mutant mutation) (call-next-method)
    (when (not (zerop *bias-toward-dynamic-mutation*))
      (queue-mutation (type-of mutation) (classify obj mutant)))
    (values mutant mutation)))

(defun classify (orig mutant)
  "Classify the mutatation from ORIG to MUTANT as :BETTER, :WORSE, :SAME, or
:DEAD dependending on the fitness of ORIG and MUTANT"
  (let ((orig-f   (evaluate *test* orig))
        (mutant-f (evaluate *test* mutant)))
    (cond
      ((equalp mutant-f (worst-fitness)) :dead)
      ((fitness-equal-p orig-f mutant-f) :same)
      ((funcall (complement #'fitness-better-p) mutant-f orig-f) :worse)
      ((fitness-better-p mutant-f orig-f) :better))))

(defun update-mutation-types (mutation-types &aux by-type)
  (flet ((weighted-probability (mutation-results)
           ;; Return a new probability of MUTATION-TYPE occurring by
           ;; examining the results of previous mutations of
           ;; MUTATION-TYPE in MUTATION-RESULTS
           (mean (mapcar (lambda-bind ((type . bias))
                           (* (/ (count-if {equal _ type} mutation-results)
                                 (length mutation-results))
                              bias))
                         `((:better . ,*better-bias*)
                           (:same   . ,*same-bias*)
                           (:worse  . ,*worse-bias*)
                           (:dead   . ,*dead-bias*))))))
    (if (equal :nothing  ; Array is too small to update mutations.
               (car (aref *mutation-results-queue*
                          *mutation-results-queue-next*)))
      mutation-types
      (progn
        ;; Collect our accumulated mutations into association list.
        (map nil (lambda-bind ((type . result)) (push result (aget type by-type)))
             *mutation-results-queue*)
        (->> mutation-types
             (mapcar
              (lambda-bind ((type . prior-probability))
                (cons type
                      (if (aget type by-type)
                          ;; Take the weighted average of the static
                          ;; mutation probability and the dynamically
                          ;; calculated mutation probability.
                          (+ (* (- 1 *bias-toward-dynamic-mutation*)
                                prior-probability)
                             (* *bias-toward-dynamic-mutation*
                                (weighted-probability (aget type by-type))))
                          prior-probability))))
             (normalize-probabilities)
             (cumulative-distribution))))))
