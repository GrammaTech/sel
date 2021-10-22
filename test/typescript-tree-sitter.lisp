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
  (:import-from :cmd)
  (:export :test-typescript-tree-sitter))
(in-package :software-evolution-library/test/typescript-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-typescript-tree-sitter "Typescript tree-sitter representation."
  (typescript-tree-sitter-available-p))

(def +ts-dir+
  (asdf:system-relative-pathname
   :software-evolution-library
   #p"test/etc/typescript/"))


;;; Parsing tests
(defgeneric regression-parse-test (orig)
  (:documentation
   "Test that ORIG can successfully be converted to a javascript-ast and
back to ORIG.")
  (:method ((orig string))
    (declare (optimize debug))
    (let* ((ast (convert 'typescript-ts-ast orig))
           (new (source-text ast)))
      (is (equal orig new)))
    (let* ((ast (convert 'typescript-tsx-ast orig))
           (new (source-text ast)))
      (is (equal orig new))))
  (:method ((orig pathname))
    (regression-parse-test (read-file-into-string orig))))

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

(deftest test-typescript-export-variants ()
  (regression-parse-test "export = fn;")
  (regression-parse-test "export type x = typeof y;")
  (regression-parse-test "export as namespace foo;"))

(deftest test-multiple-signatures ()
  (regression-parse-test
   (path-join +ts-dir+ #p"multiple-signatures-regression.ts")))

(deftest test-setters ()
  (regression-parse-test
   (path-join +ts-dir+ #p"setter-regression.ts")))

(deftest test-public-field ()
  (regression-parse-test
   (path-join +ts-dir+ #p"public-field-regression.ts")))

(deftest test-parameter-type-round-trip ()
  (regression-parse-test "function (x: string) {}"))

(deftest test-arrow-function-round-trip ()
  (regression-parse-test "(x) => 1")
  (regression-parse-test "x => 1"))

(deftest (test-typescript-source-ranges :long-running t) ()
  (let ((files
         (append (expand-wildcard
                  (path-join +ts-dir+ #p"../javascript/*/*.js"))
                 (expand-wildcard (path-join +ts-dir+ "*.ts")))))
    (test-ast-source-ranges-for-files
     'typescript-ts files :ignore-indentation t)
    (test-ast-source-ranges-for-files
     'typescript-tsx files :ignore-indentation t)))


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

(deftest test-parameter-type ()
  (is (string$= "string"
                (source-text
                 (parameter-type
                  (find-if (of-type 'typescript-ts-required-parameter)
                           (typescript-ts "function (x: string) {}")))))))


;;; Auxiliary functions for parsing entire projects.

(defun get-ts-files (dir)
  (mapcar #'pathname
          (lines (cmd:$cmd "find" (ensure-directory-pathname dir)
                           "-name '*.ts' | grep -Ev"
                           (list "node_modules|/test|\.d\.ts$")))))

(defun test-project-parsing (dir)
  (dolist (file (get-ts-files dir))
    (with-simple-restart (continue "Next")
      (stefil:finishes
       (source-text (genome (from-file 'typescript file)))))))

(defun faulty-classes (dir)
  (hash-table-alist
   (frequencies
    (with-collectors (collect)
      (handler-bind ((sel/sw/ts::parse-tree-matching-error
                      (lambda (e)
                        (collect
                         (sel/sw/ts::parse-tree-matching-error-superclass
                          e))
                        (invoke-restart 'continue)))
                     (sel/sw/ts::rule-matching-error
                      (lambda (e)
                        (collect
                         (type-of
                          (sel/sw/ts::rule-matching-error-ast
                           e)))
                        (invoke-restart 'continue))))
        (test-project-parsing dir))))))
