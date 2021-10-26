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
  (regression-parse-test "export as namespace foo;")
  (regression-parse-test "export * as module from 'file';")
  (regression-parse-test "export * from 'file' as module;"))

(deftest test-multiple-signatures ()
  (regression-parse-test
   (path-join +ts-dir+ #p"multiple-signatures-regression.ts")))

(deftest test-setters ()
  (regression-parse-test
   (path-join +ts-dir+ #p"setter-regression.ts")))

(deftest test-public-field ()
  (regression-parse-test
   (path-join +ts-dir+ #p"public-field-regression.ts")))

(deftest test-public-field-2 ()
  (regression-parse-test
   (path-join +ts-dir+ #p"public-field-regression-2.ts")))

(deftest test-parameter-type-round-trip ()
  (regression-parse-test "function (x: string) {}"))

(deftest test-arrow-function-round-trip ()
  (regression-parse-test "(x) => 1")
  (regression-parse-test "x => 1"))

(deftest test-property-signature-round-trip ()
  (regression-parse-test "export namespace FsContentRequest {
        export const type: RequestType<{ uri: string; encoding?: string; }, string, any> = new RequestType('fs/content');
}"))

(deftest test-member-expression-round-trip ()
  (regression-parse-test "a?")
  (regression-parse-test "a?.b")
  (regression-parse-test "a.b()?.c?.d")
  (regression-parse-test "const clientMain = extensions.getExtension('vscode.css-language-features')?.packageJSON?.main || '';"))

(deftest test-object-type-round-trip ()
  (regression-parse-test "let languageModels: { [uri: string]: { version: number, languageId: string, cTime: number, languageModel: T } } = {};")
  )

(deftest test-optional-chaining-round-trip ()
  (regression-parse-test "const x = a?.b"))

(deftest test-optional-element-access ()
  (regression-parse-test "return emojiMap?.[code]"))

(deftest test-optional-call ()
  (regression-parse-test "fn?.(arg)"))

(deftest test-interface-readonly-round-trip ()
  (regression-parse-test "export interface RuntimeEnvironment {
        readonly file?: RequestService;
}"))

(deftest test-async-arrow-round-trip ()
  (regression-parse-test "async p => {}"))

(deftest test-async-function-round-trip ()
  (regression-parse-test "async function () {}"))

(deftest test-async-function-declaration-round-trip ()
  (regression-parse-test "async function myfun () {}"))

(deftest test-async-function*-declaration-round-trip ()
  (regression-parse-test "async function* myfun () {}"))

(deftest test-async-function-signature-round-trip ()
  (regression-parse-test "async function fn(): any;"))

(deftest test-export-async-function-declaration-round-trip ()
  (regression-parse-test "export async function fn(): Promise<string | undefined>;"))

(deftest test-static-async-method ()
  (regression-parse-test "class myclass {
    static async fn() {};
}")
  )

(deftest test-trailing-comma-displacement ()
  (regression-parse-test "return fn(arg,);")
  (regression-parse-test "return fn(arg,
                        );")
  (regression-parse-test "{
    key: value,
}"))

(deftest test-optional-member-round-trip ()
  (regression-parse-test "class File {
        data?: Uint8Array;
}")
  (regression-parse-test "class File {
        private data?: Uint8Array;
}"))

(deftest test-constructor-private-readonly-round-trip ()
  (regression-parse-test "class myclass {
constructor(
                private readonly extensionRoot: vscode.Uri
        ) { }
} "))

(deftest test-interface-readonly ()
  (regression-parse-test "export interface I {
                readonly content: string;
}"))

(deftest test-enum-const ()
  (regression-parse-test "const enum CharCode {
        Backspace = 8,
        LineFeed = 10
}"))

(deftest test-spurious-semicolon-in-class ()
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
