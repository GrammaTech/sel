;;;; rust-tree-sitter.lisp --- Rust tree-sitter representation.
(defpackage :software-evolution-library/test/rust-tree-sitter
  (:nicknames :sel/test/rust-tree-sitter :sel/test/rust-ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/rust
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:local-nicknames (:attrs :functional-trees/attrs)
                    (:ts :software-evolution-library/software/tree-sitter))
  (:export :test-rust-tree-sitter))
(in-package :software-evolution-library/test/rust-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-rust-tree-sitter "Rust tree-sitter representation."
  (rust-tree-sitter-available-p))


;;; Utility

(deftest constant-fold-rust ()
  (is (eql 2 (constant-fold (rust* "2"))))
  (is (eql 4 (constant-fold (rust* "2 + 2"))))
  (is (null (constant-fold (rust* "2 + x"))))
  (is (eql 4 (constant-fold (rust* "2 + { 2 }"))))
  (is (eql 5 (constant-fold (rust* "(2+2)+1"))))
  (is (eql 5 (constant-fold (rust* "{ 2+2 }+1"))))
  (is (eql 7 (constant-fold (rust* "2*3+1")))))

(deftest test-parse-rust-integer ()
  "Parsing Rust integers should handle delimiters, type suffixes, and
base prefixes."
  (flet ((all-equal? (xs)
           (same #'=
                 (mapcar (op (convert 'integer (rust* _)))
                         xs))))
    (every (op (is (all-equal? _)))
           '(("123" "123i32" "123u32" "123_u32")
             ("0xff" "0xff_u8")
             ("0o70" "0x70_i16")
             ("0b1111_1111_1001_0000"
              "0b1111_1111_1001_0000i64")
             ("0" "0usize")))))

(deftest test-convert-rust-negative-literal ()
  "Converting negative literals should work."
  (let ((int-lit (rust* "-1")))
    (is (typep (convert 'rust-negative-literal int-lit)
               'rust-negative-literal))
    (is (source-text= (convert 'rust-negative-literal int-lit) "-1")))
  (let ((float-lit (rust* "-1.0")))
    (is (typep (convert 'rust-negative-literal float-lit)
               'rust-negative-literal))
    (is (source-text= (convert 'rust-negative-literal float-lit)
                      "-1.0"))))

(deftest test-insert-negative-literal ()
  "Test we can insert negative numbers into match patterns.
Match patterns use a different AST type (`rust-negative-literal') to
represent negative numbers than elsewhere in Rust (where they are
simply of type `rust-unary-expression')."
  (let* ((match1 (rust* "match i { 1 => { \"negative!\" }}"))
         (int (rust* "-1"))
         (match2 (with match1
                       (is (find-if (of-type 'rust-integer-literal)
                                    match1))
                       int)))
    (is (typep int 'rust-unary-expression))
    (is (source-text= match2 (rust* "match i { -1 => { \"negative!\" }}")))))

(deftest test-insert-negative-literal-disjunction ()
  "Test we can insert negative numbers into match pattern disjunctions.
Match patterns use a different AST type (`rust-negative-literal') to
represent negative numbers than elsewhere in Rust (where they are
simply of type `rust-unary-expression')."
  (let* ((match1 (rust* "match i { 1 | -2 => { \"negative!\" }}"))
         (int (rust* "-1"))
         (match2 (with match1
                       (is (find-if (of-type 'rust-integer-literal)
                                    match1))
                       int)))
    (is (typep int 'rust-unary-expression))
    (is (source-text= match2 (rust* "match i { -1 | -2 => { \"negative!\" }}")))))


;;; Analysis test

(deftest test-implicit-return-exit-control-flow ()
  "An implicit return expression has the surrounding block in its exit control flow."
  (let* ((rust-block (rust* "{ x }"))
         (rust-return (is (find-if (of-type 'rust-implicit-return-expression)
                                   rust-block))))
    (is (typep rust-block 'rust-block))
    (attrs::with-attr-table rust-block
      (is (equal (list rust-block) (exit-control-flow rust-return))))))

(deftest test-try-expression-control-flow-in-named-function ()
  "A try expression should have the surrounding function in its exit control flow."
  (let* ((rust (rust* "fn myfun() -> Result<(), &'static string> {
    fallible_step1()?;
    infallible_step2()?;
}"))
         (expr (is (find-if (of-type 'rust-try-expression) rust))))
    (attrs::with-attr-table rust
      (is (member rust (exit-control-flow expr))))))

(deftest test-try-expression-control-flow-in-closure ()
  "A try expression should have the surrounding closure in its exit control flow."
  (let* ((rust (rust* "|| -> Result<(), &'static string> {
    fallible_step1()?;
    infallible_step2()?;
}"))
         (expr (is (find-if (of-type 'rust-try-expression) rust))))
    (attrs::with-attr-table rust
      (is (member rust (exit-control-flow expr))))))

(deftest test-control-flow-in-else-clause ()
  "We follow control flow into an else clause."
  (let* ((rust (rust* "|| -> mytype {
if test() {
    do_something();
} else {
    return mytype{};
}
do_something_else();
}"))
         (if-ast (is (find-if (of-type 'if-ast) rust))))
    (attrs::with-attr-table rust
      (let ((exits (exit-control-flow if-ast)))
        ;; It could fall through or short-circuit.
        (is (length= exits 2))
        (is (member rust exits))))))

(deftest test-fallthrough-to-implicit-return ()
  "A statement before an implicit return expression should fall through to it."
  (let* ((rust (rust* "{
    function_call();
    value
}"))
         (statement (is (find-if (of-type 'statement-ast) rust)))
         (return-ast (is (find-if (of-type 'return-ast) rust))))
    (attrs::with-attr-table rust
      (is (member return-ast (exit-control-flow statement))))))

(deftest test-panic-control-flow ()
  "A panic! call should terminate control flow."
  (let* ((rust (rust* "{
panic!(\"Disco!\");
other_function();
}"))
         (stmt (find-if (of-type 'rust-macro-invocation) rust)))
    (attrs:with-attr-table rust
      (is (null (exit-control-flow stmt))))))

(deftest test-self-parameter-names ()
  "Calling `parameter-names' should work on a self parameter."
  (is (equal "self"
             (parameter-name
              (car (children
                    (rust-parameters (rust* "fn myfun(&mut self) {}"))))))))


;;; Symbol table

(deftest test-macro-invocation-has-no-outer-declarations ()
  "Macro invocation should not add to the symbol table."
  (is (null (ts::outer-declarations (rust* "mymacro!()")))))

(deftest test-rust-closure-inner-declarations ()
  "Closure parameters should appear in the symbol table of their
bodies."
  (let* ((rust (rust* "let expensive_closure = |num: u32| -> u32 {
        println!(\"calculating slowly...\");
        thread::sleep(Duration::from_secs(2));
        num
    };"))
         (parameter (is (find-if (of-type 'parameter-ast) rust)))
         (var (lastcar (collect-if (of-type 'identifier-ast) rust))))
    (attrs:with-attr-table rust
      (is (eql parameter (get-declaration-ast :variable var))))))

(deftest test-variables-shadow-functions ()
  "Variables should shadow functions."
  (let* ((rust (rust* "fn add(x: u32, y: u32) -> u32 {
    return x+y;
}

fn main() {
    let add:i32 = 0;
    println!(\"Sum: {}\", add(2,2));
}"))
         (id (lastcar (collect-if (of-type 'identifier-ast) rust))))
    (attrs:with-attr-table rust
      (is (typep (get-declaration-ast :variable id)
                 'rust-let-declaration)))))

(deftest test-rust-definition-name-ast ()
  "We should be able to extract the definition-name-ast from Rust ASTs."
  (let ((rust-exprs
          (list
           (rust* "enum Foo {}")
           (rust* "fn Foo() {}")
           (rust* "fn Foo();")
           (rust* "let Foo")
           (rust* "let Foo = 1")
           (rust* "mod Foo {}")
           (rust* "struct Foo {}")
           (rust* "trait Foo {}")
           (rust* "union Foo {}"))))
    (dolist (expr rust-exprs)
      (is (equal "Foo" (source-text (definition-name-ast expr)))))))

(deftest test-rust-declaration-type ()
  "We should be able to extract the declaration type from Rust ASTs."
  (let ((rust-exprs
          (list
           (rust* "let x: Foo")
           (rust* "let x: Foo = Foo::new()")
           (find-if (of-type 'rust-parameter)
                    (rust* "fn Fn(x: Foo) {}")))))
    (dolist (expr rust-exprs)
      (is (equal "Foo" (source-text (declaration-type expr)))))))

(deftest test-function-inner-declarations ()
  "Functions should add their parameters (including their type
parameters) to the symbol tables of their bodies."
  (let* ((fn (rust* "fn Foo<T>(x: T) { x }"))
         (x-use (lastcar (collect-if (of-type 'identifier-ast) fn))))
    (is (equal "x" (source-text x-use)))
    (attrs:with-attr-table fn
      (is (typep (get-declaration-ast :variable x-use) 'rust-parameter))
      (is (equal "T" (source-text (infer-type x-use)))))))


;;; Round Trip Tests
(deftest rust-can-round-trip-_-pattern ()
  "The '_' shows up in source-text."
  (let ((source "let _ = 100i;"))
    (is (equal (source-text (convert 'rust-ast source))
               source))))

(deftest rust-can-round-trip-match ()
  (let ((file
         (asdf:system-relative-pathname
          :software-evolution-library
          #p"test/etc/rust/match.rs")))
    (is (equal (read-file-into-string file)
               (source-text
                (from-file 'rust file))))))


;;; Rule Substitution Tests
(deftest rust-match-block-rule-substitution ()
  "Match block can be parsed."
  (let ((source "
/// Returns the path to the ripgrep executable.
pub fn a(&self) -> Thing {
    match next_thing() {
        None => Thing::new(),
        Some(thing) => {
            thing
        }
    }
}"))
    ;; NOTE: only care if it fails.
    (convert 'rust-ast source)))

(deftest rust-function-modifiers-substitution ()
  "Function modifiers can be parsed."
  ;; TODO: maybe actually check for the relevant modifiers in a slot.
  (let* ((source "
pub unsafe fn auto() -> MmapChoice {
    MmapChoice(MmapChoiceImpl::Auto)
}")
         (root (convert 'rust-ast source))
         (target-ast (find-if (of-type 'rust-function-modifiers) root)))
    (is (find-if (of-type 'rust-unsafe)
                 (rust-modifiers target-ast)))))

(deftest rust-_-substitution ()
  "'_' pattern can be parsed."
  (let* ((source "let _ = 100i;")
         (root (convert 'rust-ast source)))
    (is (find-if (of-type 'rust-_) root))))

(deftest rust-type-arguments-substitution ()
  "Ensure that the substitution is not considered a computed-text AST."
  (let* ((source "let x:Vec<T>;")
         (root (convert 'rust-ast source))
         (target-ast (find-if (of-type 'rust-type-arguments) root)))
    (is (typep target-ast '(not computed-text)))))

(deftest rust-let-mut-specifier ()
  "Is there a slot for let mut specifiers?"
  (is (typep (rust-mutable-specifier
              (convert 'rust-ast "let mut x = 1;" :deepest t))
             'rust-mutable-specifier))
  (is (null (rust-mutable-specifier
             (convert 'rust-ast "let x = 1;" :deepest t)))))

(deftest rust-ref-mut-specifier ()
  "Is there a slot for let mut specifiers?"
  (let ((ast1 (find-if (of-type 'rust-reference-expression) (rust "&mut x;")))
        (ast2 (find-if (of-type 'rust-reference-expression) (rust "&x;"))))
    (is (typep (rust-mutable-specifier ast1) 'rust-mutable-specifier))
    (is (null (rust-mutable-specifier ast2)))))

(deftest rust-param-mut-specifier ()
  "Is there a slot for let mut specifiers?"
  (let ((ast1 (find-if (of-type 'parameter-ast)
                       (rust "fn myfun(mut x: i32) {}")))
        (ast2 (find-if (of-type 'parameter-ast)
                       (rust "fn myfun(x: i32) {}"))))
    (is (typep ast1 'parameter-ast))
    (is (typep ast2 'parameter-ast))
    (is (typep (rust-mutable-specifier ast1) 'rust-mutable-specifier))
    (is (null (rust-mutable-specifier ast2)))))

(deftest rust-ref-param-mut-specifier ()
  "Is there a slot for mut specifiers?"
  (let ((ast1 (find-if (of-type 'rust-reference-type)
                       (rust "fn myfun(x: &mut i32) {}")))
        (ast2 (find-if (of-type 'rust-reference-type)
                       (rust "fn myfun(x: &i32) {}"))))
    (is (typep ast1 'type-ast))
    (is (typep ast2 'type-ast))
    (is (typep (rust-mutable-specifier ast1) 'rust-mutable-specifier))
    (is (null (rust-mutable-specifier ast2)))))

(deftest rust-block-substitution ()
  "The implicit return expression is not followed by a semicolon."
  (let* ((source "{(x, y)}")
         (root (convert 'rust-ast source))
         (target-ast (find-if (of-type 'rust-block) root)))
    (is (not (find #\; (source-text target-ast))))))

(deftest rust-tuple-expression-substitution ()
  "rust-tuple-expression does not prefer a trailing comma over a blank."
  (let* ((source "{(x, y)}")
         (root (convert 'rust-ast source))
         (target-ast (find-if (of-type 'rust-tuple-expression) root)))
    (is (eql 1 (count #\, (source-text target-ast))))))

(deftest rust-self-parameter-substitution ()
  "rust-self-parameter does not drop the borrow syntax from the source text."
  (let* ((source "pub fn f(&mut self) {}")
         (root (convert 'rust-ast source)))
    (is (equal source (source-text root)))))

(deftest rust-closure-expression-substitution ()
  "rust-closure-expression does not drop 'move' from the source text."
  (let* ((source "let x = f(move || write(&thing));")
         (root (convert 'rust-ast source)))
    (is (equal source (source-text root)))))

(deftest rust-generic-type-with-turbofish-substitution ()
  "rust-generic-type does not drop turbofish information."
  (let* ((source "let x = X::<Y>::new();")
         (root (convert 'rust-ast source)))
    (is (equal source (source-text root)))))

(deftest rust-round-trip-dereference ()
  "Is the operator preserved in a Rust unary expression?"
  (is (source-text= "*x;" (rust "*x;"))))

(deftest rust-empty-argument-list ()
  "Does an empty argument list print as () and not (,)?"
  (let ((ast (make 'rust-call-expression
                   :rust-function
                   (make 'rust-identifier :text "myfun")
                   :rust-arguments
                   (make 'rust-arguments
                         :children nil))))
    (is (source-text= "myfun()" ast))))

(deftest rust-one-source-text-fragment-in-argument-list ()
  "An argument list with a single source text fragment should have a
valid output transformation."
  (let ((args (make 'rust-arguments
                    :children (list
                               (make 'rust-integer-literal :text "1")))))
    (finishes (source-text args))
    (finishes
     (source-text
      (with args
            (is (find-if (of-type 'rust-integer-literal) args))
            (make 'rust-source-text-fragment-variation-point))))))

(deftest rust-one-source-text-fragment-in-tuple-expression ()
  "A tuple expression with a single source text fragment should have a
valid output transformation."
  (finishes
    (source-text
     (make 'rust-tuple-expression
           :children
           (list (make 'rust-source-text-fragment-variation-point))))))

(deftest rust-range-operators ()
  (let* ((sources
          '("1..2;"
            "3..;"
            "..4;"
            "..;"
            "5..=6;"
            ;; This parses as an assignment!
            #+(or) "..=7;"))
         (roots (mapcar (op (convert 'rust-ast _)) sources)))
    (iter (for source in sources)
          (for root in roots)
          (is (string= source (source-text root))))))

(deftest rust-range-expression-subclasses ()
  (is (typep (rust* "0..10")  'rust-range-expr))
  (is (typep (rust* "..")     'rust-range-full-expr))
  (is (typep (rust* "0..")    'rust-range-from-expr))
  (is (typep (rust* "..10")   'rust-range-to-expr)))

(deftest rust-self-parameter-is-parameter ()
  (is (every (of-type 'parameter-ast)
             (children
              (rust-parameters
               (convert 'rust-ast
                        "fn myfun(&self, x:i32) -> {}"
                        :deepest t))))))

(deftest rust-tuple-type-trailing-comma-single-element ()
  "A tuple type with a single element should maintain a trailing comma in its
source text."
  (let ((expected "(i32,)")
        (tuple-type (find-if (of-type 'rust-tuple-type)
                             (convert 'rust-ast "impl F<(i32,)> for Obj {}"))))
    (is (source-text= tuple-type expected))))

(deftest rust-tuple-pattern-trailing-comma-single-element ()
  "A tuple pattern with a single element should maintain a trailing comma in its
source text."
  (let ((expected "(i32,)")
        (tuple-type
          (find-if (of-type 'rust-tuple-type)
                   (convert 'rust-ast "fn f((a,): (i32,)) -> Self {}"))))
    (is (source-text= tuple-type expected))))


;;; Parsing tests.

(deftest test-rust-unary-expression ()
  (let ((rust (rust* "-a")))
    (is (eql (operator rust) :-))
    (is (typep (argument rust) 'rust-identifier))))

(deftest test-rust-token-tree-delimiters ()
  (flet ((test-delimiters (delims ast)
           (let ((tree (find-if (of-type 'rust-token-tree) ast)))
             (is (equal delims
                        (mapconcat #'source-text
                                   (mapcar (op (funcall _ tree))
                                           (list #'rust-left-delimiter
                                                 #'rust-right-delimiter))
                                   ""))))))
    (test-delimiters "()" (rust* "macro!()"))
    (test-delimiters "[]" (rust* "macro![]"))
    (test-delimiters "{}" (rust* "macro!{}"))
    (test-delimiters "()" (rust* "macro!(x)"))
    (test-delimiters "[]" (rust* "macro![x]"))
    (test-delimiters "{}" (rust* "macro!{x}"))))


;;; Whitespace tests.

(defun check-patch-whitespace (rust)
  (let ((ast (convert 'rust-ast rust :deepest t)))
    (is (not (typep ast 'source-text-fragment)))
    (is (equal rust
               (source-text (patch-whitespace ast :prettify t))))))

(deftest test-rust-patch-whitespace ()
  ;; No space before semicolon.
  (check-patch-whitespace "x;")
  ;; No spaces around dots.
  (check-patch-whitespace "x.y;")
  ;; No whitespace before arguments.
  (check-patch-whitespace "x(y);")
  ;; No spaces around colon for a primitive type.
  (check-patch-whitespace "let x:u64 = y;")
  ;; No spaces around colon for a user-defined type.
  (check-patch-whitespace "let x:mytype = y;")
  ;; No spaces around colon for a generic type, or betwen the type
  ;; identifier and the type arguments.
  (check-patch-whitespace "let x: Vec<T> = y;")
  ;; Generic with a primitive type argument.
  (check-patch-whitespace "let x: Vec<i32> = y;")
  ;; No spaces around :: for a class method.
  (check-patch-whitespace "Point::new();")
  ;; No space before &.
  (check-patch-whitespace "myfn(&y);")
  ;; But a space after &mut.
  (check-patch-whitespace "myfn(&mut y);")
  ;; The convention that there is no space after : in a let, but there
  ;; is in a function parameter.
  (check-patch-whitespace "fn myfn(x: i32) {}")
  (check-patch-whitespace "struct MyStruct { x: f64 }")
  (check-patch-whitespace "let x = y;")
  ;; NB: While the Rust docs are inconsistent in the spacing of struct
  ;; expressions, this is the way rustfmt likes it.
  (check-patch-whitespace "Type { x: 1, y: 2 };")
  (check-patch-whitespace "struct Type<T> { }")
  (check-patch-whitespace "#[derive(Clone)]
struct MyType<T> { x: T, y: T }")
  (check-patch-whitespace "impl<T> Foo<T> {}")
  (check-patch-whitespace "Point::<f32>::new();")
  (check-patch-whitespace "x[1];")
  (check-patch-whitespace "for i in 0..n {}")
  (check-patch-whitespace "for i in 0..=n {}")
  (check-patch-whitespace "fn myfn(&self) {}")
  (check-patch-whitespace "fn myfn(&mut x<'_>) {}")
  (check-patch-whitespace "let raw = &mut x as *mut i32;")
  (check-patch-whitespace "#[allow(unused_imports)]")
  (check-patch-whitespace "{};")
  ;; Don't add newlines around a single expression.
  (check-patch-whitespace "{ x }")
  (check-patch-whitespace (fmt "{~%~4tx();~%~4ty~%}"))
  (check-patch-whitespace "fun()?;")
  (check-patch-whitespace "impl From<(i32, i32)> for Foo {}")
  (check-patch-whitespace "const x: f64 = 42.0;")
  (check-patch-whitespace "let mut x: foo<bar>;"))
