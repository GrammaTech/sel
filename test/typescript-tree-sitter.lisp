;;;; typescript-tree-sitter.lisp --- Typescript tree-sitter representation.
(defpackage :software-evolution-library/test/typescript-tree-sitter
  (:nicknames :sel/test/typescript-tree-sitter :sel/test/ts-ts)
  (:use
    :gt/full
    :software-evolution-library/test/util
    :stefil+
    :software-evolution-library
    :software-evolution-library/software/parseable
    :software-evolution-library/software/tree-sitter
    :software-evolution-library/software/typescript
    :software-evolution-library/components/file
    :software-evolution-library/components/formatting)
  (:export :test-typescript-tree-sitter))
(in-package :software-evolution-library/test/typescript-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-typescript-tree-sitter "Typescript tree-sitter representation."
  (typescript-tree-sitter-available-p))


;;; Parsing tests
(deftest test-typescript-can-parse ()
  (is (typep (genome (from-string 'typescript-ts "1+1"))
             'ast)))

(deftest test-tsx-can-parse ()
  (is (typep (genome (from-string 'typescript-tsx "1+1"))
             'ast)))

(deftest test-typescript-blank-regression ()
  "Test that we can parse TypeScript rules that contain choices where
BLANK precedes the other branches."
  (is (typep (genome (from-string 'typescript-ts "return 1;"))
             'ast)))

;;; Representation tests.

(deftest test-arrow-function-parameter/s ()
  "Test that function-parameters works on arrow functions with one
unparenthesized argument."
  (is (length=
       1
       (function-parameters (typescript-ts "(x) => 1"))
       (finishes
        (assure list
          (function-parameters (typescript-ts "x => 1"))))))
  (is (length=
       1
       (function-parameters (typescript-tsx "(x) => 1"))
       (finishes
        (assure list
          (function-parameters (typescript-tsx "x => 1")))))))
