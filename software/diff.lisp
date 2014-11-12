;;; diff.lisp --- Store genomes as differences against a reference genome

;;; Commentary:

;; Classes which inherit from diff will replace their genome structure
;; with a diff which instead of holding a copy of the entire genome
;; only holds a difference against a reference version of the genome.
;; For example, the following will transparently save a single
;; reference version of the genome and each individual in the
;; population of `arm' objects will only hold a pointer to this single
;; reference, and it's own diff against this single reference.
;;
;;    (defclass arm (software-evolution:diff elf-arm)
;;      ((results :initarg :results :accessor results :initform nil)))
;;
;; After some initial experimentation, it does seem that mutations are
;; now noticeably slower, because the differencing operations are not
;; cheap.

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

;; TODO: Handle complex setf's like (setf (nth (genome diff)) ...), as
;;       these currently change the value of the reference.
;;
;;       Ask on #lisp, question could be posed as...
;;
;;       I'd like a method `foo' to support setf forms s.t. *all*
;;       changes to the method are actually persisted in the form of
;;       diffs against some global reference.  I have this working for
;;       the simple (setf foo x) case using `(defmethod (setf foo)
;;       ...)'.  It is possible to support more complex forms like
;;       `(setf (car foo) x)' and if so how?
;;
;;       FWIW, I looked at `define-setf-expander', which seems like
;;       what I need in general, but it's not clear how to make that
;;       do object-based dispatch (which I do need).
