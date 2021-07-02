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
  (is (equal* "foo = bar"
              (source-text
               (python "$X = $Y" :x "foo" :y "bar"))
              (source-text
               (python "$1 = $2" "foo" "bar")))))

(deftest test-extract-names ()
  (is (equal* (list "foo" "bar")
              (mapcar #'source-text
                      (match (python "foo = bar")
                        ((python "$X = $Y" :x x :y y)
                         (list x y))))
              (mapcar #'source-text
                      (match (python "foo = bar")
                        ((python "$1 = $2" x y)
                         (list x y)))))))

(deftest test-substitute-tree-rhs ()
  (is (equal* "four = 2 + 2"
              (nest
               (source-text)
               (python "$X = $Y" :x "four" :y)
               (python "$X + $Y" :x 2 :y 2))
              (nest
               (source-text)
               (python "$1 = $2" "four")
               (python "$1 + $2" 2 2)))))

(deftest test-extract-tree-rhs ()
  (is (equal* (list "four" "2" "2")
              (mapcar #'source-text
                      (match (python "four = 2 + 2")
                        ((python "$VAR = $EXPR"
                                 :var var
                                 :expr (python "$X + $Y" :x x :y y))
                         (list var x y))))
              (mapcar #'source-text
                      (match (python "four = 2 + 2")
                        ((python "$1 = $2" var (python "$1 + $2" x y))
                         (list var x y)))))))

(deftest test-duplication ()
  (is (equal*
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
                :name "foo"))
       (source-text
        (python "
def $1():
    global $2
    return $2"
                "read_foo"
                "foo")))))

(deftest test-duplicate-with-asts ()
  (equal
   (source-text
    (python "$X = fn($X)" :x (make 'python-identifier :text "var")))
   "var = fn(var)"))

(deftest test-extract-duplicates ()
  (multiple-value-bind (matched? read-name name1 name2)
      (match (python "
def read_foo():
    global foo
    return foo")
        ((python "
def $READ_NAME():
    global $NAME1
    return $NAME2"
                 :read-name read-name
                 :name1 name1
                 :name2 name2)
         (values t read-name name1 name2)))
    (is matched?)
    (is (equal? name1 name2))
    (is (equal (source-text read-name) "read_foo"))
    (is (equal (source-text name1) "foo"))))

(deftest test-ast-from-template ()
  (is (typep (ast-from-template "$1;" 'cpp-ast "\"Foo: %d\"")
             'cpp-string-literal)))

(deftest test-ellipsis-match ()
  (is (equal "fn"
             (source-text
              (match (python "fn(1)")
                ((python "$FN(...)" :fn fn)
                 fn)))))
  (is (equal "fn"
             (source-text
              (match (python "fn(1, 2, 3)")
                ((python "$FN(...)" :fn fn)
                 fn)))))
  (is (equal "fn"
             (source-text
              (match (javascript "fn(1, 2, 3)")
                ((javascript "$FN(...)" :fn fn)
                 fn))))))

(deftest test-longer-name-first ()
  (is (equal "x = y"
             (source-text
              (python "$ARG1 = $ARG" :arg "y" :arg1 "x")))))

(deftest test-list-match ()
  (is (equal '("fn" "1")
             (match (python "fn(1)")
               ((python "$FN(@ARGS)" :fn fn :args args)
                (mapcar #'source-text (cons fn args))))))
  (is (equal '("fn" "1" "2" "3")
             (match (python "fn(1, 2, 3)")
               ((python "$FN(@ARGS)" :fn fn :args args)
                (mapcar #'source-text (cons fn args))))))
  (is (equal '("fn" "1" "2" "3")
             (match (python "fn(1, 2, 3)")
               ((python "$FN($ARG, @ARGS)" :fn fn :arg arg :args args)
                (mapcar #'source-text (list* fn arg args))))))
  (is (equal '("fn" "1" "2" "3")
             (match (python "fn(1, 2, 3)")
               ((python "$FN($ARG1, $ARG2, @ARGS)" :fn fn :arg1 arg1
                                                   :arg2 arg2
                                                   :args args)
                (mapcar #'source-text (list* fn arg1 arg2 args))))))
  (is (equal '("fn" "1" "2" "3")
             (match (javascript "fn(1, 2, 3)")
               ((javascript "$FN(@ARGS)" :fn fn :args args)
                (mapcar #'source-text (cons fn args))))))
  (is (equal '("x" "y" "z" "z" "y" "x")
             (mapcar #'source-text
                     (match (python "lambda x, y, z: fn(z, y, x)")
                       ((python "lambda @PARAMS: fn(@ITEMS)"
                                :params params :items items)
                        (append params items)))))))

(deftest test-insert-list ()
  (is (equal*
       "fn(1, 2, 3)"
       (source-text (python "fn(@ARGS)" :args '(1 2 3)))
       (source-text (python "fn(1, @ARGS)" :args '(2 3)))
       (source-text (python "fn(1, 2, @ARGS)" :args '(3)))
       (source-text
        (python "fn(@ARGS)"
                :args
                (mapcar (op (make 'python-integer
                                  :text (princ-to-string _)))
                        '(1 2 3))))))

  (is (equal*
       "fn(1, 2, 3)"
       (source-text (javascript "fn(@ARGS)" :args '(1 2 3)))
       (source-text (javascript "fn(@1)" '(1 2 3)))
       (source-text (javascript "fn(1, @ARGS)" :args '(2 3)))
       (source-text (javascript "fn(1, 2, @ARGS)" :args '(3)))
       (source-text
        (javascript "fn(@ARGS)"
                    :args
                    (mapcar (op (make 'javascript-number
                                      :text (princ-to-string _)))
                            '(1 2 3))))))

  (is (equal? (python "lambda x, y, z: fn(x, y, z)")
              (python "lambda @PARAMS: fn(@ITEMS)"
                      :params '("x" "y" "z")
                      :items '("z" "y" "x"))))

  (is (equal* (source-text (javascript "[x, y, z]"))
              (source-text (javascript "[@ITEMS]" :items (list "x" "y" "z")))
              (source-text (javascript "[x, @ITEMS]" :items (list "y" "z"))))))

(deftest test-insert-empty-list ()
  (is (equal "foo()" (source-text (python "$1(@2)" "foo" nil))))
  (is (equal "foo()" (source-text (javascript "$1(@2)" "foo" nil))))
  (is (equal "lambda : 1" (source-text (python "lambda @PARAMS: 1" :params nil)))))

(deftest test-string-parsing-behavior ()
  (is (find-if (of-type 'python-default-parameter)
               (python "def fn(@ARGS): pass" :args "x=1")))
  (is (find-if (of-type 'python-default-parameter)
               (python "def fn($ARG1, $ARG2): pass" :arg1 "x=1" :arg2 "y=2")))
  (signals error
           (python "def fn(@ARGS): pass" :args '("x=1" "y=2"))))

(deftest test-compile-source-fragment-signals-errors ()
  (signals error
    (expand-macro-recursively '(python "def foo(@1): " '(1 2 3)))))

(deftest test-terminal-symbol-as-string ()
  (let ((ast (c "1 $OP_ARG 2;" :op-arg (make-instance 'c-+))))
    (is (find-if (of-type 'c-binary-expression) ast))
    (is (equal (source-text ast) "1 + 2;"))))

(deftest test-template-predecessor-whitespace ()
  "Test for two bugs: (1) the wrong whitespace because predecessor
returns the previous AST and (2) there not being a finger in the
predecessor AST."
  ;; This would invalidate the test.
  (is (not (stringp (template-subtree 'python-ast
                                      (make 'python-identifier :text "x")))))
  (is (equal "x.y"
             (source-text
              (python "$X.$Y"
                      ;; The bugs only happens when the args are ASTs.
                      :x (make 'python-identifier :text "x")
                      :y (make 'python-identifier :text "y"))))))
