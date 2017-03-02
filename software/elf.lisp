;;; elf.lisp --- software representation of ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; elf software objects
(define-software elf (simple)
  ((base :initarg :base :accessor base :initform nil)))

(defmethod genome-string ((elf elf) &optional stream)
  (format stream "~S" (genome elf)))

(defgeneric genome-bytes (elf)
  (:documentation "Return the bytes of the ELF's genome.
The result should be a simple array of type '(UNSIGNED-BYTE 8)."))

(defmethod genome-bytes ((elf elf))
  (coerce (apply #'append (lines elf)) '(vector (unsigned-byte 8))))

(defgeneric elf (elf)
  (:documentation "Return the ELF:ELF object associated with ELF.
This takes the `base' of ELF (which should not be changed), copies it,
and applies the changed data in `genome' of ELF."))

(defmethod from-file ((elf elf) path)
  (setf (base elf) (read-elf path))
  elf)

(defmethod phenome ((elf elf) &key (bin (temp-file-name)))
  #-ccl (declare (values string fixnum string string string))
  (setf bin (ensure-path-is-string bin))
  (write-elf (elf elf) bin)
  (multiple-value-bind (stdout stderr errno) (shell "chmod +x ~a" bin)
    (values bin errno stderr stdout bin)))

(defmethod pick-good ((elf elf)) (random (length (genome elf))))
(defmethod pick-bad ((elf elf)) (random (length (genome elf))))

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
