;; Copyright (C) 2011-2013  Eric Schulte
(defpackage :software-evolution
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :split-sequence
   :cl-ppcre
   :diff
   :elf
   :software-evolution-utility)
  (:shadow :elf :size :type :magic-number :diff :insert)
  (:export
   ;; software objects
   :software
   :define-software
   :edits
   :fitness
   :genome
   :phenome
   :evaluate
   :copy
   :size
   :lines
   :genome-string
   :pick
   :pick-good
   :pick-bad
   :mutate
   :apply-mutation
   :crossover
   :one-point-crossover
   :two-point-crossover
   :*edit-consolidation-size*
   :*consolidated-edits*
   :*edit-consolidation-function*
   :edit-distance
   :from-file
   :clang-w-fodder-from-file
   :load-json-db
   :to-file
   :apply-path
   :pick-json
   ;; global variables
   :*population*
   :*max-population-size*
   :*tournament-size*
   :*tournament-eviction-size*
   :*fitness-predicate*
   :*cross-chance*
   :*mut-rate*
   :*fitness-evals*
   :*running*
   ;; evolution functions
   :incorporate
   :evict
   :tournament
   :mutant
   :crossed
   :new-individual
   :evolve
   :mcmc
   ;; software backends
   :simple
   :light
   :sw-range
   :diff
   :original
   :asm
   :*asm-linker*
   :elf
   :elf-cisc
   :elf-csurf
   :elf-x86
   :elf-arm
   :elf-risc
   :elf-mips
   :genome-bytes
   :pad
   :nop-p
   :forth
   :lisp
   :clang
   :clang-w-fodder
   :cil
   :llvm
   :linker
   :flags
   :elf-risc-max-displacement
   :ops                      ; <- might want to fold this into `lines'
   ;; software backend specific methods
   :reference
   :base
   :disasm
   :addresses
   :instrument
   ))
