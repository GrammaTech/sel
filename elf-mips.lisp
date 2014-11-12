;;; elf-risc.lisp --- software representation of risc ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)

(defclass elf-mips-sw (elf-risc-sw) ())

(defvar mips-nop risc-nop)
