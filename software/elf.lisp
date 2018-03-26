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
;;; elf.lisp --- software representation of ELF files
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; elf software objects
(define-software elf (simple)
  ((base :initarg :base :accessor base :initform nil))
  (:documentation "DOCFIXME"))

(defmethod genome-string ((elf elf) &optional stream)
  "DOCFIXME"
  (format stream "~S" (genome elf)))

(defgeneric genome-bytes (elf)
  (:documentation "Return the bytes of the ELF's genome.
The result should be a simple array of type '(UNSIGNED-BYTE 8)."))

(defmethod genome-bytes ((elf elf))
  "DOCFIXME"
  (coerce (apply #'append (lines elf)) '(vector (unsigned-byte 8))))

(defgeneric elf (elf)
  (:documentation "Return the ELF:ELF object associated with ELF.
This takes the `base' of ELF (which should not be changed), copies it,
and applies the changed data in `genome' of ELF."))

(defmethod from-file ((elf elf) path)
  "DOCFIXME"
  (setf (base elf) (read-elf path))
  elf)

(defmethod phenome ((elf elf) &key (bin (temp-file-name)))
  "DOCFIXME"
  #-ccl (declare (values t fixnum string string string))
  (setf bin (ensure-path-is-string bin))
  (write-elf (elf elf) bin)
  (multiple-value-bind (stdout stderr errno) (shell "chmod +x ~a" bin)
    (values bin errno stderr stdout bin)))

(defmethod pick-good ((elf elf))
  "DOCFIXME"
  (random (length (genome elf))))

(defmethod pick-bad ((elf elf))
  "DOCFIXME"
  (random (length (genome elf))))

(defmethod mutate ((elf elf))
  "Randomly mutate ELF."
  (let ((op (case (random-elt '(cut insert swap))
              (cut      `(:cut    ,(pick-bad elf)))
              (insert   `(:insert ,(pick-bad elf) ,(pick-good elf)))
              (swap     `(:swap   ,(pick-bad elf) ,(pick-good elf)))
              )))
    (apply-mutation elf op)
    (values elf op)))

(defgeneric elf-cut (elf s1)
  (:documentation "Cut place S1 from the genome of ELF."))

(defgeneric elf-insert (elf s1 val)
  (:documentation "Insert VAL before S1 in the genome of ELF."))

(defgeneric elfap (elf s1 s2)
  (:documentation "Swap S1 and S2 in genome of ELF."))

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
