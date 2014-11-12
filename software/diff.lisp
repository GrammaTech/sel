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
   (diffs     :initarg :diffs     :accessor diffs     :initform nil)
   ;; save type since all seqs converted to lists internally for diffing
   (type      :initarg :type      :accessor type      :initform nil))
  (:documentation
   "Alternative to SIMPLE software objects which should use less memory.
Instead of directly holding code in the GENOME, each GENOME is a list
of range references to an external REFERENCE code array.

Similar to the range approach, but striving for a simpler interface."))

(defvar *in-copy* nil
  "Cheating: set to non-nil only when mid-copy, at which point the
genome methods should have no effect.")

(defmethod copy :around ((diff diff))
  (let ((*in-copy* t))
    (let ((copy (call-next-method)))
      (setf (reference copy) (reference diff))
      (setf (diffs copy)     (diffs diff))
      (setf (type copy)      (type diff))
      copy)))

(defmethod genome ((diff diff))
  ;; Build the genome on the fly from the reference and diffs
  (unless *in-copy*
    (with-slots (reference diffs type) diff
      (when (and reference diffs type)  ; otherwise uninitialized
        (coerce (apply-seq-diff reference diffs) type)))))

(defmethod (setf genome) (new (diff diff))
  ;; Convert the genome to a set of diffs against the reference
  (unless *in-copy*
    (setf (type diff) (type-of new))
    (let ((list-new (coerce new 'list)))
      (with-slots (reference diffs) diff
        (unless reference (setf reference list-new))
        (setf diffs (generate-seq-diff 'unified-diff reference list-new))))
    new))
