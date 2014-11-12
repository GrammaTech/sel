;;; elf.lisp --- software representation of ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; elf software objects
(defclass elf (software)
  ((base      :initarg :base      :accessor base      :initform nil)
   (genome    :initarg :genome    :accessor genome    :initform nil)))

(defmethod genome-string ((elf elf) &optional stream)
  (format stream "~S" (genome elf)))

(defmethod copy ((elf elf))
  (make-instance (type-of elf)
    :fitness (fitness elf)
    :genome (copy-tree (genome elf))
    :base (base elf))) ;; <- let elf objects *share* an elf object

(defgeneric elf (elf)
  (:documentation "Return the ELF:ELF object associated with ELF.
This takes the `base' of ELF (which should not be changed), copies it,
and applies the changed data in `genome' of ELF."))

(defmethod phenome ((elf elf) &key (bin (temp-file-name)))
  (write-elf (elf elf) bin)
  (multiple-value-bind (stdout stderr exit) (shell "chmod +x ~a" bin)
    (declare (ignorable stdout stderr))
    (values bin exit)))

(defmethod pick-good ((elf elf)) (random (length (genome elf))))
(defmethod pick-bad ((elf elf)) (random (length (genome elf))))

(defmethod mutate ((elf elf))
  "Randomly mutate ELF."
  (setf (fitness elf) nil)
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
