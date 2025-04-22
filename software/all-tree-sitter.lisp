;;;
;;; Load all (installed) languages supported by tree-sitter.
;;;
(defpackage :software-evolution-library/software/all-tree-sitter
  (:nicknames :sel/software/all-tree-sitter :sel/sw/all-ts)
  (:use
   :gt/full
   :software-evolution-library
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/ada
   :software-evolution-library/software/agda
   :software-evolution-library/software/bash
   :software-evolution-library/software/c
   :software-evolution-library/software/c-sharp
   :software-evolution-library/software/cpp
   :software-evolution-library/software/css
   :software-evolution-library/software/go
   :software-evolution-library/software/html
   :software-evolution-library/software/java
   :software-evolution-library/software/javascript
   :software-evolution-library/software/julia
   :software-evolution-library/software/jsdoc
   :software-evolution-library/software/json
   :software-evolution-library/software/ocaml
   :software-evolution-library/software/php
   :software-evolution-library/software/python
   :software-evolution-library/software/ql
   :software-evolution-library/software/regex
   :software-evolution-library/software/ruby
   :software-evolution-library/software/rust
   :software-evolution-library/software/scala
   :software-evolution-library/software/typescript
   ))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)
