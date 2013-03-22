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
   ;; particular software back ends
   :lisp-from-file
   :asm-from-file
   :clang-from-file
   :cil-from-file
   :elf-from-file
   :to-tree
   :to-list))
