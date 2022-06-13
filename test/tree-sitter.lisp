;;;; tree-sitter.lisp --- tree-sitter representations.
(defpackage :software-evolution-library/test/tree-sitter
  (:nicknames :sel/test/tree-sitter :sel/test/ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/c
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
                :surrounding-text-transform
                :preserve-properties
                :evolution-candidate-ast-p
                :check-ast-swappable
                :operation-matches-rule-p))
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

(defclass simple-ast (structured-text indentation functional-tree-ast)
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

(deftest test-inherited-child-slot-overrides ()
  "Regression test for a bug where `conflict-ast` and `text-fragment`
could not be copied because they inherited a value for
`ft:child-slot-specifiers' containing the `children' slot, which they
do not have."
  (finishes (copy (make 'conflict-ast)))
  (finishes (copy (make 'text-fragment))))


;;; Mutations
#+ignore

((tree-sitter-replace . 1)
      (tree-sitter-cut . 1)
      (tree-sitter-nop . 1))

(deftest test-tree-sitter-insert-1 ()
  "Insert a tree-sitter-ast into a tree-sitter software object."
  (if-let* ((c (is (from-string (make-instance 'c)
                                "int foo() { int a = 10; return 0; }")
                   "Create c software failed"))
            (ast (is (first (children (convert 'c-ast "int x = 20;")))
                     "Failed to create insertion ast"))
            (compound-ast (is (find-if
                               (lambda (x) (typep x 'compound-ast)) c)
                              "compound-ast not found"))
            (insert-before (is (@ compound-ast 0) "Could not find target ast"))
            (mut (is (make-instance
                      'tree-sitter-insert
                      :object c
                      :targets
                      (list
                       insert-before
                       ast))
                     "Mutation could not be created"))
            (v (is (ignore-errors (apply-mutation c mut))
                   "Apply-mutation failed"))
            (result (is
                        (find-if
                         (lambda (x)
                           (and (typep x 'number-ast) (equal (text x) "20")))
                         v)
                        "Resulting object does not contain inserted item")))
    t))

(deftest test-tree-sitter-swap-1 ()
  "Swap 2 asts in a tree-sitter software object."
  (if-let* ((c (is (from-string
                    (make-instance 'c)
                    "int foo() { int a = 10; int b = 20; return 0; }")
                   "Create c software failed"))
            (compound-ast (is (find-if (lambda (x) (typep x 'compound-ast)) c)
                              "compound-ast not found"))
            (ast1 (@ compound-ast 0))
            (ast2 (@ compound-ast 1))
            (mut (is (make-instance
                      'tree-sitter-swap
                      :object c
                      :targets
                      (list
                       ast1
                       ast2))
                     "Mutation could not be created"))
            (v (is (ignore-errors (apply-mutation c mut))
                   "Apply-mutation failed"))
            (source (is (source-text (genome c)) "Source-text failed"))
            (ast2pos (is (search "int b = 20" source)
                         "ast2 missing from result"))
            (ast1pos (is (search "int a = 10" source)
                         "ast1 missing from result"))
            (result (is (and ast2pos ast1pos (> ast1pos ast2pos))
                        "Swap mutation failed")))
    t))

(deftest test-tree-sitter-move-1 ()
  "Move ast in a tree-sitter software object."
  (if-let* ((c (is (from-string
                    (make-instance 'c)
                    "int foo() { int a = 1; int b = 2; int c = 3; return 0; }")
                   "Create c software failed"))
            (compound-ast (is (find-if (lambda (x) (typep x 'compound-ast)) c)
                              "compound-ast not found"))
            (ast1 (@ compound-ast 0))
            (ast2 (@ compound-ast 2))
            (mut (is (make-instance
                      'tree-sitter-move
                      :object c
                      :targets
                      (list
                       ast1
                       ast2))
                     "Mutation could not be created"))
            (v (is (ignore-errors (apply-mutation c mut))
                   "Apply-mutation failed"))
            (source (is (source-text (genome c)) "Source-text failed"))
            (ast2pos (is (search "int b = 2" source)
                         "ast2 missing from result"))
            (ast1pos (is (search "int c = 3" source)
                         "ast1 missing from result"))
            (result (is (and ast2pos ast1pos (< ast1pos ast2pos))
                        "Move mutation failed")))
    t))

(deftest test-tree-sitter-replace-1 ()
  "Replace a tree-sitter-ast in a tree-sitter software object."
  (if-let* ((c (is (from-string (make-instance 'c)
                                "int foo() { int a = 10; return 0; }")
                   "Create c software failed"))
            (new-ast (is (first (children (convert 'c-ast "int x = 20;")))
                     "Failed to create replacement ast"))
            (compound-ast (is (find-if
                               (lambda (x) (typep x 'compound-ast)) c)
                              "compound-ast not found"))
            (old-ast (is (@ compound-ast 0) "Could not find target ast"))
            (mut (is (make-instance
                      'tree-sitter-replace
                      :object c
                      :targets
                      (list
                       old-ast
                       new-ast))
                     "Mutation could not be created"))
            (v (is (ignore-errors (apply-mutation c mut))
                   "Apply-mutation failed"))
            (result (is
                        (find-if
                         (lambda (x)
                           (and (typep x 'number-ast) (equal (text x) "20")))
                         v)
                        "Resulting object does not contain replaced item")))
    t))

(deftest test-tree-sitter-cut-1 ()
  "Cut (delete) ast from a tree-sitter software object."
  (if-let* ((c (is (from-string
                    (make-instance 'c)
                    "int foo() { int a = 1; int b = 2; int c = 3; return 0; }")
                   "Create c software failed"))
            (compound-ast (is (find-if (lambda (x) (typep x 'compound-ast)) c)
                              "compound-ast not found"))
            (old-ast (@ compound-ast 1))
            (mut (is (make-instance
                      'tree-sitter-cut
                      :object c
                      :targets
                      (list old-ast))
                     "Mutation could not be created"))
            (v (is (ignore-errors (apply-mutation c mut))
                   "Apply-mutation failed"))
            (source (is (source-text (genome c)) "Source-text failed"))
            (result (is (null (search "int b = 2" source))
                              "old-ast not removed from result")))
    t))


;;; Preserve Properties
(deftest tree-sitter-preserve-properties-1 ()
  "Preserve-properties returns the before and after slots and the
indentation slots."
  (let ((result (preserve-properties (make-instance 'simple-ast
                                                    :before-text "x"
                                                    :after-text "y"
                                                    :indent-children 10
                                                    :indent-adjustment 10)))
        (expected-result
          '((:before-text . "x")
            (:after-text . "y")
            (:before-asts) (:after-asts)
            (:indent-children . 10)
            (:indent-adjustment . 10))))
    (iter
      (for (key . value) in result)
      (is (equal value (aget key expected-result))))))

(deftest tree-sitter-preserve-properties-2 ()
  "Preserve-properties returns the before and after slots and the
indentation slots in :before and :after groupings."
  (let ((result (preserve-properties
                 (make-instance 'simple-ast
                                :before-text "x"
                                :after-text "y"
                                :indent-children 10
                                :indent-adjustment 10)
                 :group-by-position t))
        (expected-result
          '((:before (:before-text . "x")
             (:before-asts)
             (:indent-children . 10)
             (:indent-adjustment . 10))
            (:after
             (:after-text . "y")
             (:after-asts)))))
    (iter
      (for (outer-key . outer-value) in result)
      (for outer-expected-value = (aget outer-key expected-result))
      (iter
        (for (key . value) in outer-value)
        (is (equal value (aget key outer-expected-value)))))))


;;; Mutation target selection
(deftest tree-sitter-evolution-candidate-ast-p ()
  (is (nest (equal 6)
            (length)
            (remove-if-not #'evolution-candidate-ast-p)
            (convert 'list)
            (genome)
            (from-string (make-instance 'c) "void foo() { /* comment */ }"))))

(deftest tree-sitter-mutation-signals-no-mutation-targets ()
  (let ((c (from-string (make-instance 'c) ";")))
    (is (signals no-mutation-targets
                 (nest (apply-mutation c)
                       (make-instance 'tree-sitter-swap :object c))))))

(deftest tree-sitter-check-ast-swappable ()
  (let ((root (convert 'c-ast "void foo() { int a; int b; }")))
    (is (not (check-ast-swappable root
                                  (stmt-with-text root "int a;")
                                  (stmt-with-text root "a"))))
    (is (not (check-ast-swappable root
                                  (stmt-with-text root "a")
                                  (stmt-with-text root "int a;"))))
    (is (check-ast-swappable root
                             (stmt-with-text root "int a;")
                             (stmt-with-text root "int b;")))))

(deftest tree-sitter-operation-matches-rule-p ()
  (let ((root (convert 'c-ast "void foo() { int a; int b; }")))
    (is (operation-matches-rule-p #'less root
                                  (stmt-with-text root "int a;")))
    (is (operation-matches-rule-p #'with root
                                  (stmt-with-text root "int b;")
                                  (tree-copy (stmt-with-text root "int a;"))))
    (is (not (operation-matches-rule-p #'less root
                                       (stmt-with-text root "()"))))
    (is (not (operation-matches-rule-p #'with root
                                       (stmt-with-text root "()")
                                       (tree-copy (stmt-with-text root
                                                                  "int a;")))))))


;;; Variation Point (Errors and Source-text-fragments)
(deftest tree-sitter-error-variation-point ()
  "Error variation points are created when an error occurs and contain the
correct source text."
  ;; NOTE: this test can fail if the parser or tree-sitter itself change how
  ;;       errors are created in the parse tree.
  (let* ((source "int")
         (target-ast (find-if (of-type 'error-variation-point)
                              (convert 'c-ast source))))
    (is (source-text= source target-ast))))

(deftest tree-sitter-source-text-fragment-variation-point ()
  "Source-text-fragment variation points are created when an error occurs and
contain the correct source text."
  ;; NOTE: this test can fail if the parser or tree-sitter itself change how
  ;;       zero-width tokens are created in the parse tree.
  (let* ((source "int i")
         (target-ast (find-if (of-type 'source-text-fragment-variation-point)
                              (convert 'c-ast source))))
    (is (source-text= source target-ast))))

(deftest tree-sitter-variation-point-trees ()
  "*use-variation-point-tree* can be used to set either a tree representation
or a non-tree representation for error and source-text-fragment variation
points."
  (let* ((*use-variation-point-tree* nil)
         (source "int i")
         (root (convert 'c-ast source)))
    (is (find-if (of-type 'c-source-text-fragment)
                 root))
    (is (not (find-if (of-type 'c-source-text-fragment-tree)
                      root)))
    (is (source-text= source root)))
  (let* ((*use-variation-point-tree* t)
         (source "int i")
         (root (convert 'c-ast source)))
    (is (find-if (of-type 'c-source-text-fragment-tree)
                 root))
    (is (not (find-if (of-type 'c-source-text-fragment)
                      root)))))

(deftest tree-sitter-variation-point-trees-contain-tokens ()
  "*use-variation-point-tree* can be used to set either a tree representation
or a non-tree representation for error and source-text-fragment variation
points."
  (let* ((*use-variation-point-tree* t)
         (source "int i")
         (root (convert 'c-ast source)))
    (is (source-text=
         "int"
         (find-if (of-type 'c-primitive-type)
                  root)))
    (is (source-text=
         "i"
         (find-if (of-type 'c-identifier)
                  root))))
  (let* ((*use-variation-point-tree* t)
         (source "for (;;) {")
         (root (convert 'c-ast source)))
    (is (find-if (of-type '|C-{|)
                 root))))

(deftest tree-sitter-source-text-fragment-trees-zero-width-tokens ()
  "Source-text-fragment variation point trees don't contain zero-width tokens."
  (let ((*use-variation-point-tree* t)
         (source "int i"))
    (is (not (find-if (of-type '|C-;|)
                      (convert 'c-ast source))))))
