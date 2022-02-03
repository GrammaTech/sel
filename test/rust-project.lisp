;;;; rust-project.lisp --- Project tests.
(defpackage :software-evolution-library/test/rust-project
  (:nicknames :sel/test/rust-project)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/simple
   :software-evolution-library/software/compilable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/rust
   :software-evolution-library/software/parseable
   :software-evolution-library/software/project
   :software-evolution-library/software/rust-project)
  #-windows (:shadowing-import-from :osicat
                                    :file-permissions :pathname-as-directory)
  (:export :test-rust-project))
(in-package :software-evolution-library/test/rust-project)
(in-readtable :curry-compose-reader-macros)

(defsuite test-rust-project
    "Rust project representation."
  (rust-tree-sitter-available-p))

;;; TODO:
