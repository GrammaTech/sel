;;; elf.lisp --- software representation of ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; elf software objects
(defclass elf-sw (software)
  ((base      :initarg :base      :accessor base      :initform nil)
   (genome    :initarg :genome    :accessor genome    :initform nil)
   (addresses :initarg :addresses :accessor addresses :initform nil)))

(defmethod copy ((elf elf-sw) &key
                               (edits (copy-tree (edits elf)))
                               (fitness (fitness elf)))
  (make-instance (type-of elf)
    :edits edits
    :fitness fitness
    :genome (copy-tree (genome elf))
    :base (copy-elf (base elf))))

(defmethod from-file ((elf elf-sw) path)
  (setf (base elf) (read-elf path))
  (let* ((text (named-section (base elf) ".text"))
         (objdump (objdump-parse (objdump text))))
    (setf (genome elf) (by-instruction text objdump))
    (setf (addresses elf) (mapcar #'car (mapcan #'cdr objdump))))
  elf)

(defmethod phenome ((elf elf-sw) &key (bin (temp-file-name)))
  (write-elf (base elf) bin)
  (shell "chmod +x ~a" bin)
  bin)

(defmethod mutate ((elf elf-sw))
  "Randomly mutate ELF."
  (setf (fitness elf) nil)
  (flet ((place () (random (length (data (.text elf))))))
    (let ((mut (case (random-elt '(cut  #|insert swap d-cut d-insert d-swap|#))
                 (cut      `(:cut         ,(place)))
                 (insert   `(:insert      ,(place) ,(place)))
                 (swap     `(:swap        ,(place) ,(place)))
                 ;; (d-cut    `(:data-cut    ,(d-place)))
                 ;; (d-insert `(:data-insert ,(d-place) ,(d-place)))
                 ;; (d-swap   `(:data-swap   ,(d-place) ,(d-place)))
                 )))
      (push mut (edits elf))
      (apply-mutate elf mut)))
  elf)

(defun apply-mutate (elf mut)
  (setf (genome elf)
        (case (car mut)
          (:cut    (elf-cut (genome elf) (second mut)))
          (:insert (elf-insert (genome elf) (second mut)
                               (nth (third mut) (genome elf))))
          (:swap   (elf-swap (genome elf) (second mut) (third mut))))))

(defvar x86-nop #x90)

(defun elf-cut (genome s1)
  (append (subseq genome 0 s1)
          (setf (subseq genome s1)
                (append (mapcar (constantly (list x86-nop)) (nth s1 genome))
                        (cdr (subseq genome s1))))))

(defun elf-insert (genome s1 val)
  (let ((to-remove (length val))
        (expanded-length (1+ (length genome))))
    (setf genome (setf (subseq genome s1) (cons val (subseq genome s1))))
    ;; bookkeeping
    (loop :for i :upto (max s1 (- (length genome) s1))
       :while (> to-remove 0) :do
       (let ((forward  (+ s1 i))
             (backward (- s1 i)))
         (when (and (< forward expanded-length)
                    (tree-equal (list x86-nop) (nth forward genome)))
           (decf to-remove)
           (setf genome (elf-cut genome forward)))
         (when (and (> to-remove 0) (> backward 0)
                    (tree-equal (list x86-nop) (nth backward genome)))
           (decf to-remove)
           (setf genome (elf-cut genome backward)))))
    (unless (zerop to-remove) (error 'mutate :text "size change" :obj genome))
    genome))

(defun elf-swap (genome s1 s2)
  (mapcar (lambda-bind ((point . value))
            (setf genome (elf-cut genome point))
            (setf genome (elf-insert genome point value)))
          (sort (mapcar #'cons
                        (list s1 s2)
                        (mapcar [#'copy-tree {nth _ genome}] (list s2 s1)))
                #'< :key [#'length #'cdr])))

(defmethod crossover ((a elf-sw) (b elf-sw))
  "Two point crossover."
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

(defun by-instruction (section
                       &optional (objdump (objdump-parse (objdump section))))
  (let ((data (data section))
        (offsets (mapcar [{- _ (address (sh section))} #'car]
                         (mapcan #'cdr objdump))))
    (mapcar (lambda (start end) (coerce (subseq data start end) 'list))
            offsets
            (append (cdr offsets) (list nil)))))

(defmethod apply-path ((elf elf-sw) key addresses &aux applied)
  (loop :for el :in addresses :as i :from 0 :do
     (let* ((addr (if (consp el) (car el) el))
            (val (if (consp el) (cdr el) t))
            (place (position addr (addresses elf))))
       (when place
         (push (cons key val) (aref (genome elf) place))
         (push (list i key val) applied))))
  (reverse applied))
