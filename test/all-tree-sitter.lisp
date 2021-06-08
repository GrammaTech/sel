;;;; all-tree-sitter.lisp --- C tree-sitter representation.
(defpackage :software-evolution-library/test/all-tree-sitter
  (:nicknames :sel/test/all-tree-sitter :sel/test/all-ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/all-tree-sitter
   :software-evolution-library/test/util-clang
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting))
(in-package :software-evolution-library/test/all-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-all-tree-sitter "All tree-sitter languages representation." nil)
