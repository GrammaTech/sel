;; Copyright (C) 2011-2013  Eric Schulte
(defpackage :software-evolution
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :split-sequence
   :cl-ppcre
   :elf
   :software-evolution-utility)
  (:shadow :elf :size :type :magic-number)
  (:export
   ;; software objects
   :software
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
   :to-file
   :apply-path
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
   :asm
   :*asm-linker*
   :elf
   :elf-x86
   :elf-risc
   :elf-mips
   :elf-arm
   :arm-op-width
   :forth
   :lisp
   :clang
   :cil
   :llvm
   :linker
   :flags
   :elf-risc-max-displacement
   ;; software backend specific methods
   :reference
   :base
   :addresses
   :instrument
   ))
