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
(defun regression-parse-test (source)
  "Test that SOURCE can successfully be converted to a javascript-ast and
back to SOURCE."
  (is (equal (source-text (convert 'typescript-ts-ast source)) source))
  (is (equal (source-text (convert 'typescript-tsx-ast source)) source)))

(deftest test-typescript-can-parse ()
  (regression-parse-test "1+1"))

(deftest test-typescript-blank-regression ()
  "Test that we can parse TypeScript rules that contain choices where
BLANK precedes the other branches."
  (regression-parse-test "return 1;"))

(deftest test-typescript-readonly-regression ()
  "Test that the string \"readonly\" doesn't precede a parameter."
  (regression-parse-test "function (x) { }"))

(deftest test-typescript-export-match ()
  (regression-parse-test "export { x } from './file';"))

(deftest test-const-declarations-persist ()
  "Test that const doesn't become let (or v.v.)."
  (regression-parse-test "const x = 1")
  (regression-parse-test "let x = 1"))

(deftest test-typescript-for-of-const ()
  ;; It already works with `var' and `let', here to make sure we don't
  ;; break anything.
  (regression-parse-test "for (let x of xs) {}")
  (regression-parse-test "for (var x of xs) {}")
  (regression-parse-test "for (const x of xs) {}"))

(deftest test-typescript-export-statement-substitution ()
  "The substitution for export statement parses and
reproduces source text."
  (regression-parse-test "export default function myfun() {
  return true;
}"))


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
