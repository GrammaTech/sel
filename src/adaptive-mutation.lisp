;;;; software object with dynamically adapting mutation probabilities
(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)

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

(defvar *bias-toward-dynamic-mutation* 1/2
  "Degree to which dynamic weights are emphasized over default weights.")
(defvar *better-bias* 5/4)
(defvar *same-bias* 1)
(defvar *worse-bias* 1/10)
(defvar *dead-bias* 0)

(define-software adaptive-mutation (software) ())

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

(defun adaptive-analyze-mutation (obj mutation-info test)
  "Adaptively update mutation probabilites based on the result of the mutation"
  (when (not (zerop *bias-toward-dynamic-mutation*))
    (destructuring-bind (mutation software-a cross-point-a
                                  crossed software-b cross-point-b)
        mutation-info
      (declare (ignorable software-a cross-point-a
                          software-b cross-point-b))
      (evaluate test crossed) ; Evaluate for fitness
      (evaluate test obj)     ; Safety - should have fitness
      (queue-mutation (type-of mutation) (classify obj crossed)))))

(defun update-mutation-types (mutation-types &aux by-type)
  (flet ((dynamic-weight (mutation-results)
           ;; Return a new dynamic mutation probability weight
           ;; for MUTATION-TYPE by examining the results of previous
           ;; mutations of MUTATION-TYPE in MUTATION-RESULTS
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
                                (dynamic-weight (aget type by-type))
                                prior-probability))
                          prior-probability))))
             (normalize-probabilities)
             (cumulative-distribution))))))
