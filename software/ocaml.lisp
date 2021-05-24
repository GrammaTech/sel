(defpackage :software-evolution-library/software/ocaml
  (:nicknames :sel/software/ocaml :sel/sw/ocaml)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "ocaml/ocaml")
(create-tree-sitter-language "ocaml/interface")
;;;===================================================
