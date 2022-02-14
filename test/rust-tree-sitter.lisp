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
  (:export :test-rust-tree-sitter))
(in-package :software-evolution-library/test/rust-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-rust-tree-sitter "Rust tree-sitter representation."
  (rust-tree-sitter-available-p))


;;; Utility


;;; Round Trip Tests
(deftest rust-can-round-trip-_-pattern ()
  "The '_' shows up in source-text."
  (let ((source "let _ = 100i;"))
    (is (equal (source-text (convert 'rust-ast source))
               source))))


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


;;; Parsing tests.

(deftest rust-empty-argument-list ()
  "Does an empty argument list print as () and not (,)?"
  (let ((ast (make 'rust-call-expression
                   :rust-function
                   (make 'rust-identifier :text "myfun")
                   :rust-arguments
                   (make 'rust-arguments
                         :children nil))))
    (is (source-text= "myfun()" ast))))

(deftest rust-round-trip-dereference ()
  "Is the operator preserved in a Rust unary expression?"
  (is (source-text= "*x;" (rust "*x;"))))


;;; Whitespace tests.

(defun check-patch-whitespace (rust)
  (is (source-text= rust
                    (patch-whitespace
                     (convert 'rust-ast rust :deepest t)
                     :prettify t))))

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
  (check-patch-whitespace "let x:Vec<T> = y;")
  ;; Generic with a primitive type argument.
  (check-patch-whitespace "let x:Vec<i32> = y;")
  ;; No spaces around :: for a class method.
  (check-patch-whitespace "Point::new();")
  ;; No space before &.
  (check-patch-whitespace "myfn(&y);")
  ;; But a space after &mut.
  (check-patch-whitespace "myfn(&mut y);")
  ;; The convention that there is no space after : in a let, but there
  ;; is in a function parameter.
  (check-patch-whitespace "fn myfn(x: i32){}")
  ;; TODO Whitespace between an identifier and a equal signs. This
  ;; doesn't currently work because the output transformation looks
  ;; like \(<rust-identifier :text "x"> "" "="), which means working
  ;; pairwise patch-whitespace doesn't know that the identifier and
  ;; the equal sign are next to each other.
  #+(or) (check-patch-whitespace "let x = y;"))
