;;; elf-mips.lisp --- software representation of mips ELF files

;; Copyright (C) 2014  GrammaTech Inc.

;;; Code:
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defclass elf-mips (elf-risc) ())
