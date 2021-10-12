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

(deftest test-typescript-readonly-regression ()
  "Test that the string \"readonly\" doesn't precede a parameter."
  (let ((string "function (x) { }"))
    (is (equal string (source-text (typescript-ts string))))))

(deftest test-typescript-export-match ()
  (is (typep
       (genome
        (from-string 'typescript "export { x } from './file';"))
       'ast)))

(deftest test-const-declarations-persist ()
  "Test that const doesn't become let."
  (is (equal "const x = 1"
             (source-text
              (typescript-ts "const x = 1"))))
  (is (equal "let x = 1"
             (source-text
              (typescript-ts "let x = 1")))))

(deftest test-typescript-for-of-const ()
  ;; It already works with `var' and `let', here to make sure we don't
  ;; break anything.
  (is (find-if (of-type 'typescript-ts-for-in-statement)
               (genome (from-string 'typescript "for (let x of xs) {}"))))
  (is (find-if (of-type 'typescript-ts-for-in-statement)
               (genome (from-string 'typescript "for (var x of xs) {}"))))
  (is (find-if (of-type 'typescript-ts-for-in-statement)
               (genome (from-string 'typescript "for (const x of xs) {}")))))


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
