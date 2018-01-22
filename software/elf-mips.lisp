;;; elf-mips.lisp --- software representation of mips ELF files
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(define-software elf-mips (elf-risc)
  ()
  (:documentation
   "Executable Linkable Format (ELF) binaries in MIPS architectures."))
