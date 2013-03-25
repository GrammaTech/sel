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
  (:shadow :type :magic-number)
  (:export
   ;; software objects
   :software
   :edits
   :fitness
   :genome
   :phenome
   :cleanup
   :evaluate
   :copy
   :mutate
   :apply-mutation
   :crossover
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
   ;; genome operations
   :inds
   :ind
   :del-ind
   :size
   :average-keys
   :edit-distance
   :cut
   :insert
   :swap
   :to-tree
   :to-list
   ;; software backends
   :asm
   :asm-linker
   :elf-sw
   :lisp
   :clang
   :cil
   ;; elf methods
   :base
   :addresses
   :pick ; <- generalize later
   ))
