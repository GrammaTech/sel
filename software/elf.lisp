;;; elf.lisp --- software representation of ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; elf software objects
(defclass elf-sw (software)
  ((addr-map :initarg :addr-map :accessor addr-map :initform nil)
   (base     :initarg :base     :accessor base     :initform nil)))

(defvar *rar* (make-instance 'elf-sw :base (read-elf "rar")))

(defmethod .text ((elf-sw elf-sw))
  (named-section (base elf-sw) ".text"))

(defmethod .rodata ((elf-sw elf-sw))
  (named-section (base elf-sw) ".rodata"))

(defmethod copy ((sw elf-sw) &key
                               (edits (copy-tree (edits sw)))
                               (fitness (fitness sw)))
  (make-instance (type-of sw)
    :edits edits
    :addr-map (copy-tree (addr-map sw))
    :base (copy-elf (base sw))))

(defmethod from-file ((sw elf-sw) path)
  (setf (base sw) (read-elf path))
  sw)

(defmethod phenome ((sw elf-sw) &key (bin (temp-file-name)))
  (write-elf (base sw) bin)
  (shell "chmod +x ~a" bin)
  bin)

(defmethod mutate ((sw elf-sw))
  "Randomly mutate SW."
  (setf (fitness sw) nil)
  (flet ((place () (random (length (data (.text sw)))))
         (place () (random (length (data (.rodata sw))))))
    (let ((mut (case (random-elt '(cut insert swap d-cut d-insert d-swap))
                 (cut      `(:cut         ,(place)))
                 (insert   `(:insert      ,(place) ,(place)))
                 (swap     `(:swap        ,(place) ,(place)))
                 (d-cut    `(:data-cut    ,(d-place)))
                 (d-insert `(:data-insert ,(d-place) ,(d-place)))
                 (d-swap   `(:data-swap   ,(d-place) ,(d-place))))))
      (push mut (edits sw))
      (apply-mutate sw mut)))
  sw)

(defun apply-mutate (sw op)
  "Apply OP to SW."
  ;; TODO: mutate w/bookkeeping
  )

(defmethod crossover ((a elf-sw) (b elf-sw))
  "Two point crossover."
  ;; TODO: see elfrep.ml
  )

;; TODO: borrow some memory mapping and oprofile from the ASM level
