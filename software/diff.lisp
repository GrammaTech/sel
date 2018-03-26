#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
;;; diff.lisp --- Store genomes as differences against a reference genome
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
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defclass diff (simple)
  ;; This doesn't use `define-software' because it requires special
  ;; genome handling when copying.
  ((reference :initarg :reference :accessor reference :initform nil)
   (diffs     :initarg :diffs     :accessor diffs     :initform nil)
   ;; save type since all seqs converted to lists internally for diffing
   (type      :initarg :type      :accessor type      :initform nil))
  (:documentation
   "Alternative to SIMPLE software objects which should use less memory.
Instead of directly holding code in the GENOME, each GENOME is a list
of range references to an external REFERENCE code array.

Similar to the range approach, but striving for a simpler interface."))

(defmethod copy ((diff diff))
  "DOCFIXME"
  (let ((copy (make-instance (type-of diff))))
    (setf (fitness copy)   (fitness diff))
    (setf (reference copy) (reference diff))
    (setf (diffs copy)     (diffs diff))
    (setf (type copy)      (type diff))
    copy))

(defmethod original ((diff diff))
  "DOCFIXME"
  (let ((copy (copy diff)))
    (setf (diffs copy)     (make-instance 'diff:unified-diff
                             :original-pathname "original"
                             :modified-pathname "modified"))
    copy))

(defmethod genome ((diff diff))
  "DOCFIXME"
  ;; Build the genome on the fly from the reference and diffs
  (with-slots (reference diffs type) diff
    (when (and reference diffs type)  ; otherwise uninitialized
      (coerce (apply-seq-diff reference diffs) type))))

(defmethod (setf genome) (new (diff diff))
  "DOCFIXME"
  ;; Convert the genome to a set of diffs against the reference
  (setf (type diff) (type-of new))
  (let ((list-new (coerce new 'list)))
    (with-slots (reference diffs) diff
      (unless reference (setf reference list-new))
      (setf diffs (generate-seq-diff 'diff:unified-diff reference list-new))))
  new)

#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
