;;; diff.lisp --- Store genomes as differences against a reference genome

;;; Commentary:

;; Classes which inherit from diff will replace their genome structure
;; with a diff which instead of holding a copy of the entire genome
;; only holds a difference against a reference version of the genome.

;;; Code:
(in-package :software-evolution)

(defclass sw-diff (simple)
  ((reference :initarg :reference :accessor reference :initform nil)
   (genome    :initarg :genome    :initform nil)
   (diffs     :initarg :diffs     :accessor diffs     :initform nil))
  (:documentation
   "Alternative to SIMPLE software objects which should use less memory.
Instead of directly holding code in the GENOME, each GENOME is a list
of range references to an external REFERENCE code array.

Similar to the range approach, but striving for a simpler interface."))

(defvar *in-copy* nil
  "Cheating: set to non-nil only when mid-copy, at which point the
genome methods should have no effect.")

(defmethod copy :around ((diff sw-diff))
  (let ((*in-copy* t))
    (let ((copy (call-next-method)))
      (setf (reference copy) (reference diff))
      (setf (diffs copy)     (diffs diff))
      copy)))

(defmethod genome ((diff sw-diff))
  ;; Build the genome on the fly from the reference and diffs
  (unless *in-copy*
    (with-slots (reference diffs) diff
      (apply-seq-diff reference diffs))))

(defmethod (setf genome) (new (diff sw-diff))
  ;; Convert the genome to a set of diffs against the reference
  (unless *in-copy*
    (with-slots (reference diffs) diff
      (setf diffs (generate-seq-diff 'unified-diff reference new)))))
