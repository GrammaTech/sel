;;; elf-arm.lisp --- software representation of arm ELF files

;; Copyright (C) 2014  GrammaTech Inc.

;;; Code:
(in-package :software-evolution)

;; http://en.wikipedia.org/wiki/NOP
;; http://infocenter.arm.com/help/index.jsp?
;;   topic=/com.arm.doc.dui0170b/Caccegih.html
(defvar arm-op-width (expt 2 1)
  "Size of chunks modified to ARM mutations in bytes.")

(defun arm-nop ()
  "Return the appropriate NOP opcode based on the `arm-op-width'."
  (coerce (int-to-bytes
           (ecase arm-op-width
             (1 #x0)
             (2 #x46C0)
             (4 #xE1A00000))
           arm-op-width)
          'list))

(defclass elf-arm (elf-risc)
  ((nop :initarg :nop :accessor nop :initform (arm-nop))))

(defun group-genome-bytes (genome width)
  (map 'vector
       [#'list {cons :code} {mappend #'cdar} {coerce _ 'list}]
       (chunks genome width)))

(defmethod from-file ((elf elf-arm) path)
  (setf (base elf) (read-elf path))
  (setf (genome elf) (group-genome-bytes (risc-genome-from-elf (base elf))
                                         arm-op-width))
  elf)

(defmethod (setf lines) :around ((elf elf-arm) path)
  (call-next-method)
  (setf (genome elf) (group-genome-bytes (genome elf) arm-op-width)))
