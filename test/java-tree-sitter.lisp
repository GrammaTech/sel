;;;; java-tree-sitter.lisp --- Java tree-sitter representation.
(defpackage :software-evolution-library/test/java-tree-sitter
  (:nicknames :sel/test/java-tree-sitter :sel/test/java-ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/java
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-java-tree-sitter))
(in-package :software-evolution-library/test/java-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-java-tree-sitter "Java tree-sitter representation."
  (java-tree-sitter-available-p))


;;; Utility


;;; Round Trip Tests


;;; Rule Substitution Tests
(deftest java-empty-statement-substitution-1 ()
  "Semicolons are replaced with java-empty-statement in classes that use
statements."
  (let ((source ";"))
    (is (equal source (source-text (convert 'java-ast source))))))

(deftest java-empty-statement-substitution-2 ()
  "Semicolons are replaced with java-empty-statement in classes that use
_class_body_declaration."
  (let ((source "public class X { ; ; }"))
    (is (equal source (source-text (convert 'java-ast source))))))

(deftest java-empty-statement-substitution-3 ()
  "Semicolons are replaced with java-empty-statement after the first semicolon
in an enum body."
  (let* ((source "enum a { ; ; }")
         (root (convert 'java-ast source))
         (target-ast (find-if (of-type 'java-enum-body-declarations) root)))
    (is (equal source (source-text (convert 'java-ast source))))
    (is (eql 1 (length (direct-children target-ast))))))

(deftest java-modifiers-substitution ()
  "Modifiers are placed in their own slot and reproduced correctly."
  (let* ((source "abstract native static public class X {}")
         (root (convert 'java-ast source))
         (target-ast (find-if (of-type 'java-modifiers) root)))
    (is (java-modifiers target-ast))
    (is (equal source (source-text root)))))

(deftest java-module-declaration-substitution ()
  "The 'open' specifier is stored so that it can be reproduced."
  (let* ((source "open module X {}")
         (root (convert 'java-ast source))
         (target-ast (find-if (of-type 'java-module-declaration) root)))
    (is (java-open target-ast))
    (is (equal source (source-text root)))))


;;; Static analysis tests.

(deftest java-var-placeholder-type ()
  (is (placeholder-type-p
       (declaration-type
        (convert 'java-ast "var x = 1;" :deepest t)))))
