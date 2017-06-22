;;; elf-mips.lisp --- software representation of mips ELF files

;; Copyright (C) 2014  GrammaTech Inc.

;;; Code:
(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)

(defclass elf-mips (elf-risc) ())
