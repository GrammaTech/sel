;; Copyright (C) 2011  Eric Schulte
(defpackage #:software-evolution
  (:use
   :common-lisp
   :alexandria
   :split-sequence
   :metabang-bind
   :curry-compose-reader-macros
   :trivial-shell
   :cl-ppcre
   :elf)
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
   :crossover
   :edit-distance
   :from-file
   :to-file
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
   :elf
   :lisp
   :clang
   :cil))
