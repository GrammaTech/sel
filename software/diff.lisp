;;; diff.lisp --- Store genomes as differences against a reference genome

;;; Commentary:

;; Classes which inherit from diff will replace their genome structure
;; with a diff which instead of holding a copy of the entire genome
;; only holds a difference against a reference version of the genome.

;;; Code:
(in-package :software-evolution)

(defclass diff (simple)
  ((reference :initarg :reference :accessor reference :initform nil)
   (genome    :initarg :genome    :initform nil)
   (diffs     :initarg :diffs     :accessor diffs     :initform nil))
  (:documentation
   "Alternative to SIMPLE software objects which should use less memory.
Instead of directly holding code in the GENOME, each GENOME is a list
of range references to an external REFERENCE code array.

Similar to the range approach, but striving for a simpler interface."))

(defmethod copy :around ((diff diff))
  ;; TODO: Wrap the next methods to copy reference and diffs instead
  ;; of the genome.
  (let ((ref (reference diff))
        (diffs (diffs diff)))
    (setf (reference diff) nil)
    (setf (diffs diff) (make-instance 'diff::diff))
    (let ((copy (call-next-method)))
      (setf (reference copy) ref)
      (setf (diffs copy) (copy diffs)))))

(defmethod genome ((diff diff))
  ;; Build the genome on the fly from the reference and diffs
  (with-slots (reference diffs) diff
    (apply-seq-diff reference diffs)))

(defmethod (setf genome) ((diff diff) new)
  ;; Convert the genome to a set of diffs against the reference
  (assert (listp new) (new) "Diff genomes must be lists.")
  (with-slots (reference diffs) diff
    (setf diffs (generate-seq-diff 'unified-diff reference new))))
