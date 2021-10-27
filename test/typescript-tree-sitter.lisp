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

(def +ts-dir+ (make-pathname :directory +typescript-dir+))
(def +js-dir+ (make-pathname :directory +javascript-dir+))


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
  "Test that we can parse exports with destructuring."
  (regression-parse-test "export { x } from './file';"))

(deftest test-const-declarations-persist ()
  "Test that const doesn't become let (or v.v.)."
  (regression-parse-test "const x = 1")
  (regression-parse-test "let x = 1"))

(deftest test-typescript-for-of-const ()
  "Test that for loops preserve the distinction between let/const."
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
  "Test support for TypeScript-specific export syntax."
  (regression-parse-test "export = fn;")
  (regression-parse-test "export type x = typeof y;")
  (regression-parse-test "export as namespace foo;")
  (regression-parse-test "export * as module from 'file';")
  (regression-parse-test "export * from 'file' as module;"))

(deftest test-multiple-signatures ()
  "Test support for multiple method signatures one after another."
  (regression-parse-test
   (path-join +ts-dir+ #p"multiple-signatures-regression.ts")))

(deftest test-setters ()
  "Test support for setter methods."
  (regression-parse-test
   (path-join +ts-dir+ #p"setter-regression.ts")))

(deftest test-public-field ()
  "Test support for `readonly abstract' public fields."
  (regression-parse-test
   (path-join +ts-dir+ #p"public-field-regression.ts")))

(deftest test-public-field-2 ()
  "Test support for `protected override readonly' public fields."
  (regression-parse-test
   (path-join +ts-dir+ #p"public-field-regression-2.ts")))

(deftest test-parameter-type-round-trip ()
  "Test support for type parameters."
  (regression-parse-test "function (x: string) {}"))

(deftest test-arrow-function-round-trip ()
  "Test that we preserve the distinction between single-argument arrow
functions with and without parentheses."
  (regression-parse-test "(x) => 1")
  (regression-parse-test "x => 1"))

(deftest test-property-signature-round-trip ()
  "Test that we don't lose the ? in a optional property in an object
type in a property signature."
  (regression-parse-test "export namespace FsContentRequest {
        export const type: RequestType<{ uri: string; encoding?: string; }, string, any> = new RequestType('fs/content');
}"))

(deftest test-member-expression-round-trip ()
  "Test that we don't lose ? in member expressions."
  (regression-parse-test "a?")
  (regression-parse-test "a?.b")
  (regression-parse-test "a.b()?.c?.d")
  (regression-parse-test "const clientMain = extensions.getExtension('vscode.css-language-features')?.packageJSON?.main || '';"))

(deftest test-object-type ()
  "Can we round-trip an object type that uses commas as separators?"
  ;; Make sure we don't break semicolons.
  (regression-parse-test "let languageModels: { version: number; languageId: string; cTime: number; languageModel: T }")
  (regression-parse-test "let languageModels: { version: number, languageId: string, cTime: number, languageModel: T }"))

(deftest test-optional-chaining-round-trip ()
  "Make sure we don't lose ? in optional chaining."
  (regression-parse-test "const x = a?.b"))

(deftest test-optional-element-access ()
  "Make sure we don't lose ? in optional element access."
  (regression-parse-test "return emojiMap?.[code]"))

(deftest test-optional-call ()
  "Make sure we don't lose ? in optional calls."
  (regression-parse-test "fn?.(arg)"))

(deftest test-interface-readonly-round-trip ()
  "Make sure we don't lose ? in optional interface elements."
  (regression-parse-test "export interface RuntimeEnvironment {
        readonly file?: RequestService;
}"))

(deftest test-declare-readonly ()
  "Make sure we don't lose `declare' in `declare readonly'."
  (regression-parse-test "class C {
    declare readonly _serviceBrand: undefined;
}"))

(deftest test-async-arrow-round-trip ()
  "Make sure we don't lose `async' on an arrow function."
  (regression-parse-test "async p => {}"))

(deftest test-async-function-round-trip ()
  "Make sure we don't lose `async' on a function expression."
  (regression-parse-test "async function () {}"))

(deftest test-async-function-declaration-round-trip ()
  "Make sure we don't lose `async' on a function declaration."
  (regression-parse-test "async function myfun () {}"))

(deftest test-async-function*-declaration-round-trip ()
  "Make sure we don't lose `async' on a generator function declaration."
  (regression-parse-test "async function* myfun () {}"))

(deftest test-async-function-signature-round-trip ()
  "Make sure we don't lose `async' on a function signature."
  (regression-parse-test "async function fn(): any;"))

(deftest test-export-async-function-declaration-round-trip ()
  "Make sure we don't lose `async' on an exported function declaration."
  (regression-parse-test "export async function fn(): Promise<string | undefined>;"))

(deftest test-static-async-method ()
  "Make sure we can parse `static async' methods."
  (regression-parse-test "class myclass {
    static async fn() {};
}"))

(deftest test-trailing-comma-displacement ()
  "Make sure trailing commas don't get displaced."
  (regression-parse-test "return fn(arg,);")
  (regression-parse-test "return fn(arg,
                        );")
  (regression-parse-test "{
    key: value,
}"))

(deftest test-optional-member-round-trip ()
  "Make sure we don't lose the ? in optional class members."
  (regression-parse-test "class File {
        data?: Uint8Array;
}")
  (regression-parse-test "class File {
        private data?: Uint8Array;
}"))

(deftest test-constructor-private-readonly-round-trip ()
  "Make sure we support `private readonly' class members."
  (regression-parse-test "class myclass {
        constructor(
                private readonly extensionRoot: vscode.Uri
        ) { }
} "))

(deftest test-interface-readonly ()
  "Make sure we keep `readonly' in interface properties."
  (regression-parse-test "export interface I {
                readonly content: string;
}"))

(deftest test-enum-const ()
  "Make sure a `const enum' keeps it's `const'."
  (regression-parse-test "const enum CharCode {
        Backspace = 8,
        LineFeed = 10
}"))

;;; TODO
#+(or)
(deftest test-spurious-semicolon-in-class ()
  "Make sure semicolons aren't inserted in the wrong place when semicolons and ASI are mixed in a class body."
  (regression-parse-test
   "class C {
    is(item) {}

    readonly ref: string;
}"))

(deftest (test-typescript-source-ranges :long-running t) ()
  (let ((files
         (append (expand-wildcard (path-join +js-dir+ #p"*/*.js"))
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
