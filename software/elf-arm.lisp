;;; elf-arm.lisp --- software representation of arm ELF files

;; Copyright (C) 2014  GrammaTech Inc.

;;; Code:
(in-package :software-evolution)

;; http://en.wikipedia.org/wiki/NOP
;; http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0170b/Caccegih.html
(defvar arm-nop #xE1A00000)

(defclass elf-arm (elf-risc)
  ((nop :initarg :nop :accessor nop :initform arm-nop)))
