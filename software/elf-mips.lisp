;;; elf-mips.lisp --- software representation of mips ELF files
(defpackage :software-evolution-library/software/elf-mips
  (:nicknames :sel/software/elf-mips :sel/sw/elf-mips)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility))
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(define-software elf-mips (elf-risc)
  ()
  (:documentation
   "Executable Linkable Format (ELF) binaries in MIPS architectures."))
