;;;; conflict-ast.lisp --- Conflict ast tests.
(defpackage :software-evolution-library/test/conflict-ast
  (:nicknames :sel/test/conflict-ast)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/c)
  (:export :test-conflict-ast))
(in-package :software-evolution-library/test/conflict-ast)
(in-readtable :curry-compose-reader-macros)
(defsuite test-conflict-ast "Conflict ast tests.")

(deftest conflict-ast.1 ()
  (let ((c1 (make-instance 'conflict-ast
             :child-alist '((1 a) (2 b))
             :default-children '(c)))
        (c2 (make-instance 'conflict-ast
             :child-alist '((2 d) (3 e))
             :default-children '(f))))
    (let ((c (combine-conflict-asts c1 c2)))
      (is (equalp (conflict-ast-child-alist c)
                  '((1 a f) (2 b d) (3 c e)))
          "conflict ast alists are merged")
      (is (equalp (conflict-ast-default-children c) '(c f))
          "conflict ast defaults are merged"))))

(deftest test-conflict-surrounding-text ()
  "Conflict ASTs can produce source-text when in a surrounding text slot."
  (let ((ast (convert 'c-ast "    int a = 0;"))
        (conflict (make-instance 'conflict-ast
                                 :child-alist '((:my "  ")
                                                (:your "   ")
                                                (:old "    ")))))
    (setf (before-text (find-if (of-type 'statement-ast) ast)) conflict)
    (is (stringp (source-text ast)))))
