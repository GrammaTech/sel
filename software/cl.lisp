(defpackage :software-evolution-library/software/cl
  (:documentation "Common Lisp ASTs based on tree-sitter")
  (:nicknames
   :sel/software/cl
   :sel/sw/cl)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;; To build:

;; git clone https://github.com/theHamsta/tree-sitter-commonlisp.git
;; cd src
;; clang -std=c99 -fPIC parser.c -c
;; clang -shared parser.o -o /usr/lib/tree-sitter-commonlisp.so
;; mkdir -p /usr/share/tree-sitter/commonlisp/
;; cp -f grammar.json node-types.json /usr/share/tree-sitter/commonlisp

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "commonlisp")
;;;===================================================

(define-language-alias-mappings lisp ("asd" "cl" "common lisp" "lisp"))
