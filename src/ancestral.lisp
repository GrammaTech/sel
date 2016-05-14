;;; ancestral --- class adding ancestry tracking to software
(in-package :se)

(defclass ancestral ()
  ((ancestors :initarg :ancestors :accessor ancestors :initform nil))
  (:documentation "Class adding ancestry tracking to software."))

(defvar *next-ancestry-id* 0
  "Unique identifier for ancestry.")

(defun get-fresh-ancestry-id ()
  (let ((id *next-ancestry-id*))
    (incf *next-ancestry-id*)
    id))

(defmethod from-file :before ((obj ancestral) path)
  (setf (ancestors obj) (list (alist :base path
                                     :how 'from-file
                                     :id (get-fresh-ancestry-id)))))

(defmethod from-string :before ((obj ancestral) string)
  (setf (ancestors obj) (list (alist :base string
                                     :how 'from-string-exactly
                                     :id (get-fresh-ancestry-id)))))

(defmethod apply-mutation :around ((obj ancestral) op)
  (multiple-value-call
      (lambda (variant &rest rest)
        (push (alist :mutant op :id (get-fresh-ancestry-id))
              (ancestors obj)))
    (call-next-method)))

(defmethod crossover :around ((a clang) (b clang))
  (multiple-value-bind (crossed a-point b-point) (call-next-method)
    (when a-point
      (push (alist :cross-with (ancestors b)
                   :crossover '2pt
                   :id (get-fresh-ancestry-id))
            (ancestors crossed)))
    (values crossed a-point b-point)))
