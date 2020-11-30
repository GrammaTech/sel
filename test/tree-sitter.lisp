;;;; tree-sitter.lisp --- tree-sitter representations.
(defpackage :software-evolution-library/test/tree-sitter
  (:nicknames :sel/test/tree-sitter :sel/test/ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-tree-sitter)
  (:import-from :software-evolution-library/software/tree-sitter
                :position-after-leading-newline))
(in-package :software-evolution-library/test/tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-tree-sitter "tree-sitter representations.")


;;; Tests
(deftest tree-sitter-newline-post-processing-1 ()
  (is (eql (position-after-leading-newline "") nil)
      "position-after-leading-newline on empty string"))

(deftest tree-sitter-newline-post-processing-2 ()
  (is (eql (position-after-leading-newline "x") nil)
      "position-after-leading-newline on string with no whitespace or newline"))

(deftest tree-sitter-newline-post-processing-3 ()
  (is (eql (position-after-leading-newline "   ") nil)
      "position-after-leading-newline on string with whitespace only, no newline"))

(deftest tree-sitter-newline-post-processing-4 ()
  (is (eql (position-after-leading-newline " x") nil)
      "position-after-leading-newline on string with whitespace, no newline"))

(deftest tree-sitter-newline-post-processing-5 ()
  (is (eql (position-after-leading-newline (string #\Newline)) 1)
      "position-after-leading-newline on newline"))

(deftest tree-sitter-newline-post-processing-6 ()
  (is (eql (position-after-leading-newline
               (concatenate 'string (string #\Newline) "x"))
              1)
      "position-after-leading-newline on newline + other stuff"))

(deftest tree-sitter-newline-post-processing-7 ()
  (is (eql (position-after-leading-newline
               (concatenate 'string (string #\Newline) "// foo "))
              1)
      "position-after-leading-newline on newline, comment"))

(deftest tree-sitter-newline-post-processing-8 ()
  (is (equalp (position-after-leading-newline
               "  // foo ")
              nil)
      "position-after-leading-newline on comment"))

(deftest tree-sitter-newline-post-processing-9 ()
  (is (equalp (position-after-leading-newline "/")
              nil)
      "position-after-leading-newline slash at EOL not a comment"))

(deftest tree-sitter-newline-post-processing-10 ()
  (is (equalp (position-after-leading-newline " / ")
              nil)
      "position-after-leading-newline slash not at EOL not a comment"))
