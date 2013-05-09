;; Copyright (C) 2011-2013  Eric Schulte
(defpackage :software-evolution
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :elf
   :cl-ppcre
   :software-evolution-utility)
  (:shadow :size :type :magic-number)
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
   :*fitness-predicate*
   :*cross-chance*
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
   ;; software backends
   :asm
   :*asm-linker*
   :elf-sw
   :lisp
   :clang
   :cil
   :llvm
   :linker
   :flags
   ;; elf methods
   :base
   :addresses
   ))
