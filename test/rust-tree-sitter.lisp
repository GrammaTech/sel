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
