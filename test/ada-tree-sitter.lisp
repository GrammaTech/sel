;;;; ada-tree-sitter.lisp --- Ada tree-sitter representation.
(defpackage :software-evolution-library/test/ada-tree-sitter
  (:nicknames :sel/test/ada-tree-sitter :sel/test/ada-ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/ada
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-ada-tree-sitter))
(in-package :software-evolution-library/test/ada-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-ada-tree-sitter "Ada tree-sitter representation."
  (ada-tree-sitter-available-p))


;;; Utility


;;; Round Trip Tests


;;; Rule Substitution Tests


;;; Static analysis tests.
