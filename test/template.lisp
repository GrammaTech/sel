(defpackage :software-evolution-library/test/template
  (:use :gt/full
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/parseable
        :software-evolution-library/software/template
        :software-evolution-library/software/python
        :software-evolution-library/software/javascript
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/rust
        :software-evolution-library/test/util
        :stefil+)
  (:import-from
    :software-evolution-library/software/tree-sitter
    :internal-ast?)
  (:local-nicknames (:attrs :functional-trees/attrs)))
(in-package :software-evolution-library/test/template)
(in-readtable :curry-compose-reader-macros)
(defsuite test-templates "tree-sitter representations."
    (and (python-tree-sitter-available-p)
         (javascript-tree-sitter-available-p)
         (c-tree-sitter-available-p)
         (cpp-tree-sitter-available-p)
         (rust-tree-sitter-available-p)))

#+(and :TREE-SITTER-CPP :TREE-SITTER-C
       :TREE-SITTER-JAVASCRIPT :TREE-SITTER-PYTHON
       :TREE-SITTER-RUST)
(progn

(deftest test-template-as-shorthand ()
  (is (equal? (convert 'python-ast "pass" :deepest t)
              (python "pass")))
  (is (equal? (convert 'python-ast "return x" :deepest t)
              (python "return x"))))

(deftest test-leading-newline ()
  "Newlines are conserved when creating an AST via the template interface."
  (is (not (equal? (python "
def x(): x")
                   (python "def x(): x")))))

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
                      :items '("x" "y" "z"))))

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
  (is (not (stringp (template-subtree 'python-ast
                                      (make 'python-identifier :text "x")))))
  (is (equal "x.y"
             (source-text
              (python "$X.$Y"
                      ;; The bugs only happens when the args are ASTs.
                      :x (make 'python-identifier :text "x")
                      :y (make 'python-identifier :text "y"))))))

(deftest test-insert-semicolons ()
  (is (typep (cpp* "1") 'cpp-number-literal)))

(deftest test-insert-newline ()
  (is (typep (cpp* "#define EMPTY") 'cpp-preproc-def)))

(deftest test-insert-semicolons-in-patterns ()
  (is (equal '("1" "2")
             (match (cpp* "1 + 2")
               ((cpp* "$X + $Y" :x x :y y)
                (list (source-text x) (source-text y)))))))

(defun make-derive (traits)
  (rust "#[derive(@ARGS)]"
        :args
        (mapcar (op (make 'rust-meta-item
                          :children
                          (list (make 'rust-identifier :text (source-text _)))))
                traits)))

(deftest test-match-not-invalid ()
  "Regression test that ast-for-match isn't used when expanding templates."
  (is (match (cpp* "true")
        ((cpp* "true") t))))

(deftest test-starred-template-variant ()
  (is (typep
       (ematch (cpp* "($EXPR)" :expr 1)
         ((and ast (cpp* "($EXPR)" :expr expr))
          (is (typep ast 'cpp-parenthesized-expression))
          expr))
       'cpp-number-literal)))

(deftest test-starred-template-target ()
  (is (typep (cpp* "void print() const {}") 'cpp-function-definition))
  (is (typep (cpp* "struct MyStruct { double x; int y; }") 'cpp-struct-specifier)))

(deftest test-template-type ()
  (is (typep (cpp* "void") 'cpp-primitive-type)))

(deftest test-template-no-inner-fragment ()
  (is (typep (rust* "type Output = Self") 'rust-type-item)))

(deftest test-template-match-ignores-whitespace ()
  (is (source-text= "x"
                    (match (cpp* "x   = 1")
                      ((cpp* "$NAME=1" :name name)
                       name))))
  (dolist (variant '("int* x = &y;"
                     "int *x = &y;"
                     "int * x = &y;"))
    (is (source-text=
         "x"
         (match (convert 'cpp-ast variant :deepest t)
           ((cpp* "int *$VAR = &y" :var var)
            var))))))

(deftest test-templates-dont-copy-unless-needed ()
  ;; Note the absence of whitespace is to avoid copying to insert
  ;; around-text.
  (let* ((ast (cpp "int x=1;"))
         (expr (stmt-with-text ast "1"))
         (new-expr (cpp* "$N+1" :n expr))
         (new-ast (with ast expr new-expr)))
    (is (eql expr (lookup new-expr (ast-path new-expr expr))))
    (is (eql expr (lookup new-ast (ast-path new-ast expr))))
    (let* ((new-expr (cpp* "$N+$N" :n expr))
           (new-ast (with ast expr new-expr))
           (instances (collect-if (of-type 'literal-ast) new-ast)))
      (is (member expr instances))
      (is (notevery (eqls expr) instances)))))

(deftest test-template-ignore-single-underscore ()
  (is (source-text= "x"
                    (is (match (cpp "int x = 1;")
                          ((cpp "$_ $VAR = 1;" :var var)
                           var))))))

(deftest test-template-ignore-multiple-underscores ()
  (is (source-text= "x"
                    (is (match (cpp "int x = 1;")
                          ((cpp "$_ $VAR = $_;" :var var)
                           var))))))

(deftest test-templates-unify ()
  (flet ((args-match? (ast)
           (attrs:with-attr-table ast
             (match ast
               ((cpp* "$FN($ARG, $ARG)" :fn fn :arg arg)
                (mapcar #'source-text (list fn arg)))))))
    (is (equal '("fn" "x") (args-match? (cpp* "fn(x, x)"))))
    (is (null (args-match? (cpp* "fn(x, y)"))))))

(deftest test-detect-unbound-positional-metavariables ()
  (signals error
    (eval '(match (python "fn(x,x)")
            ((python "$1($2, $2)") t))))
  (signals error
    (eval '(match (python "fn(x,x)")
            ((python "$FN($ARG, $ARG)" :fn fn) t))))
  )

(deftest test-only-ignored-variables ()
  (is (match (cpp* "container.begin()")
        ((cpp* "$_.begin()") t))))

(deftest test-unify-aliases ()
  (let* ((cpp (sel:from-string 'cpp (fmt "~
int p1 = 0;
int& r = p1;
int* p = &r;
fun(r, p);")))
         (ast (find-if (of-type 'cpp-call-expression) cpp)))
    (attrs:with-attr-table cpp
      (is (match ast
            ((cpp* "$_($ARG, $ARG)" :arg arg)
             arg))))))

(deftest test-internal-ast-predicate ()
  (is (internal-ast? 'cpp-internal-asts-0))
  (is (internal-ast? '(cpp-internal-asts-0 0))))

) ; #+(AND :TREE-SITTER-CPP :TREE-SITTER-C
  ;        :TREE-SITTER-JAVASCRIPT :TREE-SITTER-PYTHON :TREE-SITTER-RUST)
