(defpackage :software-evolution-library/test/template
  (:use :gt/full
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/parseable
        :software-evolution-library/software/template
        :software-evolution-library/software/python
        :software-evolution-library/test/util
        :stefil+)
  (:export))
(in-package :software-evolution-library/test/template)
(in-readtable :curry-compose-reader-macros)
(defsuite test-template "tree-sitter representations.")

(deftest test-substitute-names ()
  (equal "foo = bar"
         (source-text
          (ast-template "{{x}} = {{y}}"
                        'python-ast
                        :x "foo"
                        :y "bar"))))

(deftest test-substitute-tree-rhs ()
  (equal "four = 2 + 2"
         (nest
          (source-text)
          (ast-template "{{x}} = {{y}}" 'python-ast :x "four" :y)
          (ast-template "{{x}} + {{y}}" 'python-ast :x 2 :y 2))))
