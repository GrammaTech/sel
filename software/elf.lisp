;;; elf.lisp --- software representation of ELF files
(defpackage :software-evolution-library/software/elf
  (:nicknames :sel/software/elf :sel/sw/elf)
  (:use :gt/full
        :elf
        :software-evolution-library
        :software-evolution-library/software/simple)
  (:shadowing-import-from :common-lisp :type)
  (:shadowing-import-from :software-evolution-library :size)
  (:shadowing-import-from :elf :insert :ordering :data)
  (:export :elf
           :base
           :genome-bytes))
(in-package :software-evolution-library/software/elf)
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
  (setf bin (namestring bin))
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
  (let ((op (case (random-elt '(:cut :insert :swap))
              (:cut      `(:cut    ,(pick-bad elf)))
              (:insert   `(:insert ,(pick-bad elf) ,(pick-good elf)))
              (:swap     `(:swap   ,(pick-bad elf) ,(pick-good elf))))))
    (apply-mutation elf op)
    (values elf op)))

(defgeneric elf-cut (elf s1)
  (:documentation "Cut place S1 from the genome of ELF."))

(defgeneric elf-insert (elf s1 val)
  (:documentation "Insert VAL before S1 in the genome of ELF."))

(defgeneric elfap (elf s1 s2)
  (:documentation "Swap S1 and S2 in genome of ELF."))
