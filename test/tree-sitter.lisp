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
  (:shadow :simple-ast)
  (:export :test-tree-sitter)
  (:import-from :software-evolution-library/software/tree-sitter
                :structured-text
                :position-after-leading-newline
                :inner-parent
                :surrounding-text-transform))
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

(deftest test-null-before-text ()
  (is (equal "" (before-text (make 'structured-text :before-text nil)))))

(deftest test-null-after-text ()
  (is (equal "" (after-text (make 'structured-text :after-text nil)))))

(deftest test-inner-parent-children ()
  (is (null (children (make 'inner-parent :children '())))))

(defclass simple-ast (structured-text functional-tree-ast)
  ((children
    :initform nil
    :initarg :children
    :accessor children)
   (child-slots
    :reader child-slots
    :initform '((children . 0))
    :allocation :class)))

(deftest test-can-copy-with-surrounding-asts ()
  (let* ((node (make 'simple-ast
                     :before-text (make 'conflict-ast)
                     :after-text (make 'conflict-ast)
                     :children nil))
         (parent
          (make 'simple-ast :children
                (list (make 'simple-ast))))
         (tree
          (with parent '(0) node)))
    (is (equal? (list node) (children tree)))
    (is (= 2 (count-if (of-type 'conflict-ast)
                       (output-transformation node))))))

(deftest test-asts-are-always-traversed ()
  (is (length= 3
               (collect-if (of-type 'conflict-ast)
                           (make 'simple-ast
                                 :before-text (make 'conflict-ast)
                                 :after-text (make 'conflict-ast)
                                 :text (make 'conflict-ast)
                                 :children nil)))))

(deftest test-fragment-surrounding-text-transform ()
  (let ((fragment (allocate-instance (find-class 'source-text-fragment))))
    (setf (text fragment) "")
    (is (equal "" (surrounding-text-transform fragment)))))
