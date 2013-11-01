;;; elf.lisp --- software representation of ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; elf software objects
(defclass elf-sw (software)
  ((base      :initarg :base      :accessor base      :initform nil)
   (genome    :initarg :genome    :accessor genome    :initform nil)))

(defmethod copy ((elf elf-sw))
  (make-instance (type-of elf)
    :fitness (fitness elf)
    :genome (copy-tree (genome elf))
    :base (base elf))) ;; <- let elf objects *share* an elf object

(defgeneric elf (elf-sw)
  (:documentation "Return the elf object associated with ELF.
This takes the `base' of ELF (which should not be changed), copies it,
and applies the changed data in `genome' of ELF."))

(defmethod phenome ((elf elf-sw) &key (bin (temp-file-name)))
  (write-elf (elf elf) bin)
  (shell "chmod +x ~a" bin)
  bin)

(defmethod pick-good ((elf elf-sw)) (random (length (genome elf))))
(defmethod pick-bad ((elf elf-sw)) (random (length (genome elf))))

(defmethod mutate ((elf elf-sw))
  "Randomly mutate ELF."
  (setf (fitness elf) nil)
  (let ((op (case (random-elt '(cut insert swap))
              (cut      `(:cut    ,(pick-bad elf)))
              (insert   `(:insert ,(pick-bad elf) ,(pick-good elf)))
              (swap     `(:swap   ,(pick-bad elf) ,(pick-good elf)))
              )))
    (apply-mutation elf op)
    (values elf op)))

(defmethod apply-mutation ((elf elf-sw) mut)
  (flet ((byte-count (genome)
           (reduce #'+ (mapcar [#'length {aget :bytes}] genome))))
    (let ((starting-bytes (byte-count (genome elf))))
      (setf (genome elf)
            (case (car mut)
              (:cut    (elf-cut (genome elf) (second mut)))
              (:insert (elf-insert (genome elf) (second mut)
                                   (nth (third mut) (genome elf))))
              (:swap   (elf-swap (genome elf) (second mut) (third mut)))))
      (assert (= (byte-count (genome elf)) starting-bytes)
              (elf) "mutation ~S changed size of genome [~S -> ~S]"
              mut starting-bytes (byte-count (genome elf))))))

(defgeneric elf-cut (elf-sw s1)
  (:documentation "Cut place S1 from the genome of ELF-SW."))

(defgeneric elf-insert (elf-sw s1 val)
  (:documentation "Insert VAL before S1 in the genome of ELF-SW."))

(defgeneric elf-swap (elf-sw s1 s2)
  (:documentation "Swap S1 and S2 in genome of ELF-SW."))

(defmethod crossover ((a elf-sw) (b elf-sw))
  "One point crossover."
  (flet ((borders (elf)
           (let ((counter 0))
             (cdr (reverse (reduce (lambda (ac el) (cons (cons (+ el (caar ac))
                                                          (incf counter))
                                                    ac))
                                   (mapcar #'length (genome elf))
                                   :initial-value '((0))))))))
    (let ((point (random-elt (mapcar #'cdr (intersection (borders a) (borders b)
                                                         :key #'car))))
          (new (copy a)))
      (setf (genome new) (append (subseq (genome a) 0 point)
                                 (subseq (genome b) point)))
      new)))

(defmethod apply-path ((elf elf-sw) key addresses &aux applied)
  (loop :for el :in addresses :as i :from 0 :do
     (let* ((addr  (if (consp el) (car el) el))
            (val   (if (consp el) (cdr el) t))
            (place (position addr (addresses elf))))
       (when place
         (push (cons key val) (nth place (genome elf)))
         (push (list i key val) applied))))
  (reverse applied))
