;;;; diff.lisp --- Store genomes as differences against a reference genome
;;;
;;; Classes which inherit from diff will replace their genome structure
;;; with a diff which instead of holding a copy of the entire genome
;;; only holds a difference against a reference version of the genome.
;;; For example, the following will transparently save a single
;;; reference version of the genome and each individual in the
;;; population of `arm' objects will only hold a pointer to this single
;;; reference, and it's own diff against this single reference.
;;;
;;;    (defclass arm (software-evolution-library:diff elf-arm)
;;;      ((results :initarg :results :accessor results :initform nil)))
;;;
;;; After some initial experimentation, it does seem that mutations are
;;; now noticeably slower, because the differencing operations are not
;;; cheap.
;;;
;;; @texi{diff}
(defpackage :software-evolution-library/software/diff
  (:nicknames :sel/software/diff :sel/sw/diff)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/simple)
  (:shadowing-import-from :diff
                          :unified-diff
                          :generate-seq-diff
                          :apply-seq-diff)
  (:export :diff
           :original))
(in-package :software-evolution-library/software/diff)
(in-readtable :curry-compose-reader-macros)

(defclass diff (simple)
  ;; This doesn't use `define-software' because it requires special
  ;; genome handling when copying.
  ((reference :initarg :reference :accessor reference :initform nil)
   (diffs     :initarg :diffs     :accessor diffs     :initform nil)
   ;; save type since all seqs converted to lists internally for diffing
   (diff-type :initarg :diff-type :accessor diff-type :initform nil))
  (:documentation
   "Alternative to SIMPLE software objects which should use less memory.
Instead of directly holding code in the GENOME, each GENOME is a list
of range references to an external REFERENCE code array.

Similar to the range approach, but striving for a simpler interface."))

(defmethod copy ((diff diff) &key)
  "DOCFIXME"
  (let ((copy (make-instance (type-of diff))))
    (setf (fitness copy)   (fitness diff))
    (setf (reference copy) (reference diff))
    (setf (diffs copy)     (diffs diff))
    (setf (diff-type copy) (diff-type diff))
    copy))

(defmethod original ((diff diff))
  "DOCFIXME"
  (let ((copy (copy diff)))
    (setf (diffs copy)
          (make-instance 'unified-diff
            :original-pathname "original"
            :modified-pathname "modified"))
    copy))

(defmethod genome ((diff diff))
  "Build the genome on the fly from DIFF's `reference' and `diffs'."
  (with-slots (reference diffs diff-type) diff
    (when (and reference diffs diff-type)  ; otherwise uninitialized
      (coerce (apply-seq-diff reference diffs) diff-type))))

(defmethod (setf genome) (new (diff diff))
  "Convert the NEW genome to `diffs' against `reference' on DIFF."
  (setf (diff-type diff) (type-of new))
  (let ((list-new (coerce new 'list)))
    (with-slots (reference diffs) diff
      (unless reference (setf reference list-new))
      (setf diffs (generate-seq-diff 'unified-diff reference list-new))))
  new)
