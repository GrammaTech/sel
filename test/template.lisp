(defpackage :software-evolution-library/test/template
  (:use :gt/full
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/parseable
        :software-evolution-library/software/template
        :software-evolution-library/software/python
        :software-evolution-library/software/javascript
        :software-evolution-library/software/c
        :software-evolution-library/test/util
        :stefil+)
  (:export))
(in-package :software-evolution-library/test/template)
(in-readtable :curry-compose-reader-macros)
(defsuite test-templates "tree-sitter representations.")

(deftest test-template-as-shorthand ()
  (is (equal? (convert 'python-ast "pass" :deepest t)
              (python "pass")))
  (is (equal? (convert 'python-ast "return x" :deepest t)
              (python "return x"))))

(deftest test-leading-newline ()
  (equal? (python "
def x(): x")
          (python "def x(): x")))

(deftest test-substitute-names ()
  (equal "foo = bar"
         (source-text
          (python "{{x}} = {{y}}" :x "foo" :y "bar"))))

(deftest test-substitute-tree-rhs ()
  (equal "four = 2 + 2"
         (nest
          (source-text)
          (python "{{x}} = {{y}}" :x "four" :y)
          (python "{{x}} + {{y}}" :x 2 :y 2))))

(deftest test-duplication ()
  (is (equal
       (fmt "~
def read_foo():
    global foo
    return foo")
       (source-text
        (python "
def {{read-name}}():
    global {{name}}
    return {{name}}"
                :read-name "read_foo"
                :name "foo")))))
