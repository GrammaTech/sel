;;;; tree-sitter-base.lisp --- base package for including all tree-sitter
;;;;                           requirements.
;;; Dummy Package
(defpackage :software-evolution-library/software/tree-sitter-base
  (:nicknames :sel/software/ts-base :sel/sw/ts-base)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/tree-sitter-code-gen
        :software-evolution-library/software/tree-sitter-general))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)
