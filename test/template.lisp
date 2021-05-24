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
  (is (equal? (python "
def x(): x")
              (python "def x(): x"))))

(deftest test-substitute-names ()
  (is (equal "foo = bar"
             (source-text
              (python "$X = $Y" :x "foo" :y "bar")))))

(deftest test-extract-names ()
  (is (equal (list "foo" "bar")
             (mapcar #'source-text
                     (match (python "foo = bar")
                       ((python "$X = $Y" :x x :y y)
                        (list x y)))))))

(deftest test-substitute-tree-rhs ()
  (is (equal "four = 2 + 2"
             (nest
              (source-text)
              (python "$X = $Y" :x "four" :y)
              (python "$X + $Y" :x 2 :y 2)))))

(deftest test-extract-tree-rhs ()
  (is (equal (list "four" "2" "2")
             (mapcar #'source-text
                     (match (python "four = 2 + 2")
                       ((python "$VAR = $EXPR"
                                :var var
                                :expr (python "$X + $Y" :x x :y y))
                        (list var x y)))))))

(deftest test-duplication ()
  (is (equal
       (fmt "~
def read_foo():
    global foo
    return foo")
       (source-text
        (python "
def $READ_NAME():
    global $NAME
    return $NAME"
                :read-name "read_foo"
                :name "foo")))))

(deftest test-extract-duplicates ()
  (match (python (fmt "~
def read_foo():
    global foo
    return foo"))
    ((python "
def $READ_NAME():
    global $NAME1
    return $NAME2"
             :read-name read-name
             :name1 name1
             :name2 name2)
     (is (equal? name1 name2))
     (is (equal (source-text read-name) "read_foo"))
     (is (equal (source-text name1) "foo")))))
