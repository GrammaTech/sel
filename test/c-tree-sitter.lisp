;;;; c-tree-sitter.lisp --- C tree-sitter representation.
(defpackage :software-evolution-library/test/c-tree-sitter
  (:nicknames :sel/test/c-tree-sitter :sel/test/c-ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/c
   :software-evolution-library/test/util-clang
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:shadowing-import-from :cl-tree-sitter :parse-string)
  (:export :test-c-tree-sitter))
(in-package :software-evolution-library/test/c-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-c-tree-sitter "C tree-sitter representation."
  (c-tree-sitter-available-p))


;;; Utility
(define-constant +c-tree-sitter-dir+ (append +etc-dir+ (list "c-tree-sitter"))
  :test #'equalp
  :documentation "Path to directory holding C files for tree-sitter tests.")

(defixture unicode
  (:setup
   (setf *soft*
         (from-file (make-instance 'c)
                    (make-pathname :name "unicode"
                                   :type "c"
                                   :directory +unicode-dir+)))))

(defixture w/while
  (:setup
   (nest
    (setf *soft*)
    (from-file (make-instance 'c))
    (asdf:system-relative-pathname :software-evolution-library)
    "test/etc/c-fragments/short.c")))


  

;;; Tests
(deftest test-deepest-sans-semicolon ()
  (is (typep (convert 'c-ast "int x = y" :deepest t) 'c-ast)))

(deftest function-name-on-c-tree-sitter1 ()
  (with-fixture w/while
    (is (equal "main"
               (function-name (find-if (of-type 'function-ast) *soft*))))))

(deftest function-name-on-c-tree-sitter2 ()
  (let ((root (convert 'c-ast "char* foo(char * line) { return line; }")))
    (is (equal "foo"
               (function-name (find-if (of-type 'function-ast) root))))))

(deftest function-name-on-c-tree-sitter3 ()
  (let ((root (convert 'c-ast "int (*foo(int * node))(int32) { return 0; }")))
    (is (equal "foo"
               (function-name (find-if (of-type 'function-ast) root))))))

(deftest parameter-type-on-c-tree-sitter ()
  (is (equalp (parameter-type
               (first (function-parameters
                       (@ (convert 'c-ast "void bar(const char ***it){}") 0))))
              '("char" 3 "const")))
  (is (equalp (parameter-type
               (first (function-parameters
                       (@ (convert 'c-ast "void foo(int baz){}") 0))))
              '("int" 0))))

(deftest parameter-name-on-c-tree-sitter ()
  (is (string= (parameter-name
               (first (function-parameters
                       (@ (convert 'c-ast "void bar(const char ***it){}") 0))))
               "it"))
  (is (equalp (parameter-name
               (first (function-parameters
                       (@ (convert 'c-ast "void foo(int baz){}") 0))))
              "baz")))

(deftest comment-inheritance-works-as-expected ()
  (is (subtypep 'c-comment 'comment-ast)))

(deftest c-tree-sitter-can-parse-file ()
  (finishes
    (from-file
     (make-instance 'c)
     (make-pathname :name "test"
                    :type "c"
                    :directory +c-tree-sitter-dir+))))

#+broken
(deftest c-tree-sitter-parses-with-errors ()
  "c-tree-sitter-ast parses ASTs even if there's errors."
  (let ((ast (convert 'c-ast "a = 1")))
    (is (find-if {typep _ 'c-error} ast))
    (is (find-if {typep _ 'c-assignment-expression} ast))))

(deftest c-tree-sitter-parses-from-list ()
  (let ((ast (convert
              'c-ast
              '((:class . :declaration)
                (:declarator
                 ((:class . :init-declarator)
                  (:declarator
                   (:class . :identifier)
                   (:text "a")
                   (:before-text . " ")
                   (:after-text . " "))
                  (:value
                   (:class . :number-literal)
                   (:text "0")
                   (:before-text . " "))))
                (:type
                 (:class . :primitive-type)
                 (:text "int"))))))
    (is (find-if {typep _ 'c-declaration} ast))
    (is (find-if {typep _ 'c-init-declarator} ast))
    (is (find-if {typep _ 'c-identifier} ast))
    (is (find-if {typep _ 'c-number-literal} ast))
    (is (equal "int a = 0;" (source-text ast)))))

(deftest c-tree-sitter-handles-unicode ()
  "c-ast successfully parses unicode."
  (with-fixture unicode
    (is (stmt-with-text *soft* "int x = 0" :at-start t))
    (is (stmt-with-text *soft* "\"2 bytes: Δ\"" :at-start t))
    (is (stmt-with-text *soft* "int y = 1" :at-start t))
    (is (not (find-if {typep _ 'c-error} (genome *soft*))))))

(deftest test-c-source-ranges ()
  ;; There are a lot of C source files and parsing them is slow
  ;; so set a limit. Note the files actually tested are chosen at
  ;; random from the set of all files.
  (uiop::with-current-directory
      ((asdf:system-relative-pathname :software-evolution-library "test/etc/"))
    (let* ((c-files (expand-wildcard #p"*/*.c"))
           ;; FIXME: There is a performance issue in `ast-source-ranges'
           ;;        (at least on CCL maybe SBCL as well) so we remove
           ;;        the largest C source files.
           (large-c-files '("obstack" "search" "getopt" "kwset" "grep" "dfa" "regex"))
           ;; NOTE: these are files which have parse errors due to unsupported
           ;;       features in the tree-sitter parser.
           (error-files '("typeof" "typeof2" "bug8" "varargs2" "variety" "variety2" "nested"))
           (c-files (remove-if [{member _ (append large-c-files error-files)
                               :test #'string=}
                               #'pathname-name]
                               c-files)))
      (test-ast-source-ranges-for-files
       'c c-files :limit 10 :ignore-indentation t))))

(deftest tree-sitter-shows-source-text-for-a-while ()
  (with-fixture w/while
    (is (stringp (source-text (find-if {typep _ 'c-while-statement} *soft*))))))


;;;; tree-sitter rule substitution tests

(deftest preprocessor-test-1 () ;; address sel issue 136
  "Ensure that #ifndef is not converted to #ifdef"
  (let ((*soft* (from-string (make-instance 'c) 
"#ifndef BSD4_1
#define HAVE_GETPAGESIZE
#endif
")))
    (is (typep *soft* 'c))
    (is (typep (genome *soft*) 'c-translation-unit))
    (is (typep (elt (children (genome *soft*)) 0) 'c-preproc-ifdef))
    (is (starts-with-subseq
         "#ifndef" (source-text (elt (children (genome *soft*)) 0))))))

(deftest preprocessor-test-2 () ;; address sel issue 136
  "Ensure that #ifdef is not converted to #ifndef"
  (let ((*soft* (from-string (make-instance 'c) 
"#ifdef BSD4_1
#define HAVE_GETPAGESIZE
#endif
")))
    (is (typep *soft* 'c))
    (is (typep (genome *soft*) 'c-translation-unit))
    (is (typep (elt (children (genome *soft*)) 0) 'c-preproc-ifdef))
    (is (starts-with-subseq
         "#ifdef" (source-text (elt (children (genome *soft*)) 0))))))

(deftest compound-operator-test-1 () ;; sel issue #137
  (let ((*soft* (from-string (make-instance 'c) 
"{ int x = 1; 
   x *= 4; 
   x += 2; 
   x -= 1; 
   x /= 5; }")))
    (is (typep *soft* 'c))
    (let ((g (genome *soft*)))
      (is (typep g 'c-translation-unit))
      (is (typep (@ g 0) 'c-compound-statement))
      (is (typep (@ g '(0 1)) 'c-expression-statement))
      (is (typep (@ g '(0 1 0)) 'c-assignment-expression))
      (is (string-equal
           (source-text (@ g '(0 1 0 1)))
           "*="))
      (is (string-equal
           (source-text (@ g '(0 2 0 1)))
           "+="))
      (is (string-equal
           (source-text (@ g '(0 3 0 1)))
           "-="))
      (is (string-equal
           ; 0 4 0 1
           (source-text (@ g '(0 4 0 1)))
           "/=")))))
      
(deftest field-expression-test-1 () ;; sel issue #142
  "Ensure that '.' and '->' are handled correctly"
  (let ((*soft* (from-string (make-instance 'c) 
"int main ()
{
    typedef struct { int f1; int f2; } X;
    X a[3];
    int i;
    X *p = a;
    p->f1 = 1;
    i = p->f1;
    return i;
}
")))
    (is (typep *soft* 'c))
    (let ((g (genome *soft*)))
      (is (typep g 'c-translation-unit))
      (is (typep (@ g 0) 'c-function-definition))
      (is (typep (@ g '(0 2)) 'c-compound-statement))
      (is (typep (@ g '(0 2 4)) 'c-expression-statement))
      (is (typep (@ g '(0 2 4 0)) 'c-assignment-expression))
      (is (typep (@ g '(0 2 4 0 0)) 'c-field-expression))
      (is (string-equal (source-text  (@ g '(0 2 4 0 0 1)))
                        "->"))
      (is (typep (@ g '(0 2 5)) 'c-expression-statement))
      (is (typep (@ g '(0 2 5 0)) 'c-assignment-expression))
      (is (typep (@ g '(0 2 5 0 2)) 'c-field-expression))
      (is (string-equal (source-text  (@ g '(0 2 5 0 2 1)))
                        "->")))))

(deftest c-type-descriptor-1 ()
  "type descriptors should maintain the correct order of type qualifiers."
  (let ((source "x const* a = (x const *) m->n->o[i];"))
    (is (equal source (source-text (convert 'c-ast source))))))


;;;; Parsing test

(defun parsing-test-dir (path)
  (merge-pathnames-as-file
   (make-pathname :directory (append +c-tree-sitter-dir+
                                     (list "parsing")))
   path))

(defun parse-test (path error-check? &rest ast-types)
  (let ((soft (from-file (make-instance 'c)
                         (parsing-test-dir path))))
    (is (not (zerop (size soft))))
    (is (equal (genome-string soft)
               (file-to-string (parsing-test-dir path))))
    (when error-check?
      (is (not (find-if {typep _ 'c-error} (genome soft)))))
    (is (find-if
         (lambda (ast)
           (typep ast `(and ,@ast-types)))
         (genome soft))
        (format nil "Found ASTS of types (and ~{~a~^ ~})" ast-types))
    soft))

(deftest (c-tree-sitter-parsing-test :long-running) ()
  (mapc {apply {parse-test _ t}}
        '((#P"abstract-array-declarator.c" c-abstract-array-declarator)
          (#P"abstract-pointer-declarator.c" c-abstract-pointer-declarator)
          (#P"argument-list.c" c-argument-list)
          (#P"array-declarator.c" c-array-declarator)
          (#P"assignment-expression.c" c-assignment-expression)
          (#P"attribute-specifier.c" c-attribute-specifier)
          (#P"binary-expression.c" c-binary-expression)
          (#P"bitfield-clause.c" c-bitfield-clause)
          (#P"break-statement.c" c-break-statement statement-ast)
          (#P"call-expression.c" c-call-expression)
          (#P"case-statement.c" c-case-statement statement-ast)
          (#P"cast-expression.c" c-cast-expression)
          (#P"char-literal.c" c-char-literal)
          (#P"comma-expression.c" c-comma-expression)
          (#P"compound-literal-expression.c" c-compound-literal-expression)
          (#P"compound-statement.c" c-compound-statement)
          (#P"concatenated-string.c" c-concatenated-string)
          (#P"conditional-expression.c" c-conditional-expression)
          (#P"continue-statement.c" c-continue-statement statement-ast)
          (#P"declaration.c" c-declaration)
          (#P"do-statement.c" c-do-statement statement-ast)
          (#P"enum.c" c-enum-specifier)
          (#P"enum.c" c-enumerator)
          (#P"enum.c" c-enumerator-list)
          (#P"binary-expression.c" c-expression-statement statement-ast)
          (#P"bitfield-clause.c" c-field-declaration)
          (#P"bitfield-clause.c" c-field-declaration-list)
          (#P"field-designator.c" c-field-designator)
          (#P"field-expression.c" c-field-expression)
          (#P"for-statement.c" c-for-statement statement-ast)
          (#P"function-declarator.c" c-function-declarator)
          (#P"function-definition.c" c-function-definition statement-ast)
          (#P"goto-statement.c" c-goto-statement)
          (#P"if-statement.c" c-if-statement statement-ast)
          (#P"field-designator.c" c-init-declarator)
          (#P"field-designator.c" c-initializer-list)
          (#P"field-designator.c" c-initializer-pair)
          (#P"goto-statement.c" c-labeled-statement statement-ast)
          (#P"function-definition.c" c-parameter-declaration)
          (#P"function-definition.c" c-parameter-list)
          (#P"parenthesized-declarator.c" c-parenthesized-declarator)
          (#P"parenthesized-expression.c" c-parenthesized-expression)
          (#P"pointer-declarator.c" c-pointer-declarator)
          (#P"pointer-expression.c" c-pointer-expression)
          ;; NOTE: currently an issue with having just a #define alone
          ;;       in a file. This is probably a tree-sitter issue?
          (#P"preproc-def.c" c-preproc-def)
          (#P"preproc-defined.c" c-preproc-defined)
          (#P"preproc-if.c" c-preproc-elif)
          (#P"preproc-if.c" c-preproc-else)
          (#P"preproc-function-def.c" c-preproc-function-def)
          (#P"preproc-if.c" c-preproc-if)
          (#P"preproc-ifdef.c" c-preproc-ifdef)
          (#P"preproc-include.c" c-preproc-include)
          (#P"preproc-function-def.c" c-preproc-params)
          (#P"function-definition.c" c-return-statement statement-ast)
          (#P"sized-type-specifier.c" c-sized-type-specifier)
          (#P"sizeof-expression.c" c-sizeof-expression)
          (#P"storage-class-specifier.c" c-storage-class-specifier)
          (#P"concatenated-string.c" c-string-literal)
          (#P"bitfield-clause.c" c-struct-specifier)
          (#P"subscript-designator.c" c-subscript-designator)
          (#P"subscript-expression.c" c-subscript-expression)
          (#P"case-statement.c" c-switch-statement statement-ast)
          (#P"type-definition.c" c-type-definition)
          (#P"cast-expression.c" c-type-descriptor)
          (#P"unary-expression.c" c-unary-expression)
          (#P"union-specifier.c" c-union-specifier)
          (#P"update-expression.c" c-update-expression)
          (#P"continue-statement.c" c-while-statement statement-ast)
          (#P"variadic-function.c" c-parameter-declaration)
          (#P"variadic-macro.c" c-parameter-declaration))))

(deftest (c-tree-sitter-round-trip-parsing-test :long-running) ()
  ;; Use this test when a round-trip could contain error ASTs.
  (mapc {apply {parse-test _ nil}}
        '((#P"language-redefining-macro.c")
          ;; inner-asts round trip tests
          ;;   These test that whitespace and comments aren't lost
          ;;   between terminal tokens.
          (#P"inner-asts-for-statement.c")
          ;; TODO: figure out how to work around issues with the top-most rule.
          (#P"inner-asts-enum.c")
          (#P"inner-asts-if-defined.c"))))

(defixture factorial.c
  (:setup (setf *soft* (from-file (make-instance 'c)
                                  (asdf:system-relative-pathname
                                   :software-evolution-library
                                   "test/etc/factorial.c"))))
  (:teardown (setf *soft* nil)))

(deftest test-comments-for ()
  (with-fixture factorial.c
    (is (= 3 (length (comments-for *soft* (find-if {typep _ 'c-while-statement} *soft*)))))
    (is (= 1 (length (comments-for *soft* (stmt-with-text *soft* "printf" :at-start t)))))))

(deftest c-tree-sitter-inserts-comments-in-correct-order ()
  (let ((source "{ a /*  */ /* */ ;}"))
    (is (equal
         source
         (source-text (genome (from-string (make-instance 'c) source)))))))

(deftest c-source-text-fragments-are-created ()
  (let* ((source
           "
if (x == 1)
    return 1;
#ifdef CHECK2
else if (x == 2)
    return 2;
#endif;
return 0;
")
         (ast (convert 'c-ast source)))
    (is (equal source (source-text ast)))
    (is (find-if (of-type 'c-source-text-fragment) ast))))

(deftest c-source-text-fragments-nested-errors ()
  "source-text-fragments should successfully be created for nested error nodes."
  ;; NOTE: this will become an invalid test if the parser is changed such that
  ;;       the parse tree doesn't have nested errors.
  (labels ((has-nested-error-p (parse-tree)
             "Return T if PARSE-TREE contains a nested error node."
             (walk-tree
              (lambda (subtree)
                (and (consp subtree)
                     (eql (car subtree) :error)
                     (find :error (caddr subtree) :key #'car)
                     (return-from has-nested-error-p t)))
              parse-tree)))
    (let* ((source "a b :, c AmfServiceClient {")
           (parse-tree (parse-string :c source :produce-cst t))
           (ast (convert 'c-ast source)))
      (is (has-nested-error-p parse-tree))
      (is (equal source (source-text ast)))
      (is (find-if (of-type 'c-source-text-fragment) ast)))))

(deftest c-can-change-class-on-error ()
  "ASTs can dynamically change their subclass to the first one that matches with
the current state of the AST."
  (let* ((source "if (x) {
 return 0;
} else {
 return 1;
}")
         (if-statement (find-if (of-type 'c-if-statement)
                                (convert 'c-ast source)))
         (modified-if-statement (copy if-statement :c-alternative nil)))
    (is (equal (source-text if-statement) source))
    (is (equal (source-text modified-if-statement) "if (x) {
 return 0;
} "))
    (is (not (equal (type-of if-statement) (type-of modified-if-statement))))))

(deftest c-newlines-arent-dropped ()
  "Rules which have newline tokens do not drop the newlines."
  ;; This works around a bug in tree-sitter.
  (let ((source (format nil "#define a ~%")))
    (is (equal source (source-text (convert 'c-ast source))))))

(deftest c-source-text-uses-the-correct-root ()
  "source-text doesn't signal a no-ast-path error when ancestor-check is called."
  ;; regression test
  (let* ((source (format nil "#if defined(A)~%int x() { }~%#endif~%")))
    (is (equal source (source-text (convert 'c-ast source))))))

(deftest c-includes-maintain-trailing-newlines ()
  "Preprocessor includes don't drop empty lines following them."
  (let ((source "#include <stdio.h>


int f () {}"))
    (is (equal source (source-text (convert 'c-ast source))))))

(deftest c-errors-in-named-slots ()
  "Errors which occur in named slots don't cause an error when source-text is
called."
  (let ((source "
int fun(int x) {
  switch(x) {
  case COMPILER$VAR:
    return -1;
  default:
    return 0;
  }
}
"))
    (is (equal (source-text (convert 'c-ast source))
               source))))


;;;; SCOPES tests

(deftest c-test-scopes ()
  (let* ((c (sel:from-string (make 'c) (fmt "~
int main () {
  int x = 1;
  int z;
  y();
}")))
         (scopes (scopes c (find-if (of-type 'call-ast) c)))
         (bindings (apply #'append scopes))
         (x-binding (find "x" bindings
                          :test #'equal
                          :key {assocdr :name}))
         (z-binding (find "z" bindings
                          :test #'equal
                          :key {assocdr :name})))
    (is x-binding)
    (is z-binding)
    (is (equal "1" (source-text (rhs (assocdr :decl x-binding)))))))

(deftest c-scopes-1 ()
  "scopes gets the bindings from 'for' statements."
  (let* ((source "for (int i = 0; i < 10; i++) {
  return;
}")
         (genome (convert 'c-ast source))
         (software (make 'c :genome genome))
         (i-alist
           (scopes-contains-string-p
            (scopes software (find-if (of-type 'c-return-statement) genome))
            "i"))
         (expected-declaration (find-if (of-type 'c-init-declarator) genome))
         (expected-scope (find-if (of-type 'c-for-statement) genome)))
    (is (eq (aget :decl i-alist) expected-declaration))
    (is (eq (aget :scope i-alist) expected-scope))))

(deftest c-scopes-2 ()
  "scopes gets the bindings from a variable declaration."
  (let* ((source "int i = 0; ;")
         (genome (convert 'c-ast source))
         (software (make 'c :genome genome))
         (i-alist
           (scopes-contains-string-p
            (scopes software (find-if (of-type 'c-expression-statement)
                                      genome))
            "i"))
         (expected-declaration (find-if (of-type 'c-init-declarator)
                                        genome)))
    (is (eq (aget :decl i-alist) expected-declaration))
    (is (eq (aget :scope i-alist) genome))))

(deftest c-scopes-3 ()
  "scopes gets bindings from a function declaration."
  (let* ((source "void i () {
  return;
}")
         (genome (convert 'c-ast source))
         (software (make 'c :genome genome))
         (i-alist
           (scopes-contains-string-p
            (scopes software (find-if (of-type 'c-return-statement) genome))
            "i"))
         (expected-declaration (find-if (of-type 'c-function-declarator) genome)))
    (is (eq (aget :decl i-alist) expected-declaration))
    (is (eq (aget :scope i-alist) genome))))

(deftest c-scopes-4 ()
  "scopes gets the bindings from a parameter list."
  (labels ((is-parameter-p (scopes name-string)
             "Test that NAME-STRING exists in SCOPES and has the expected
              scope and declaration."
             (let ((parameter-alist
                     (scopes-contains-string-p scopes name-string)))
               (is (equal (aget :name parameter-alist) name-string))
               (is (typep (aget :scope parameter-alist) 'c-function-definition))
               (is (equal name-string
                          (source-text
                           (find-if (of-type 'identifier-ast)
                                    (aget :decl parameter-alist))))))))
    (let* ((source "void i (int a, void b, float *c) {
  return;
}")
           (genome (convert 'c-ast source))
           (software (make 'c :genome genome))
           (scopes (scopes software (find-if (of-type 'c-return-statement)
                                             genome))))
      (is (scopes-contains-string-p scopes "i"))
      (is-parameter-p scopes "a")
      (is-parameter-p scopes "b")
      (is-parameter-p scopes "c"))))

(deftest c-scopes-5 ()
  "scopes gets bindings from array declarations."
  (let* ((source "int i[1][1]; return;")
         (genome (convert 'c-ast source))
         (software (make 'c :genome genome))
         (i-alist
           (scopes-contains-string-p
            (scopes software (find-if (of-type 'c-return-statement) genome))
            "i"))
         (expected-declaration (find-if (of-type 'c-array-declarator) genome)))
    (is (eq (aget :decl i-alist) expected-declaration))
    (is (eq (aget :scope i-alist) genome))))

(deftest c-scopes-6 ()
  "scopes gets bindings from pointer declarations."
  (let* ((source "int **i; return;")
         (genome (convert 'c-ast source))
         (software (make 'c :genome genome))
         (i-alist
           (scopes-contains-string-p
            (scopes software (find-if (of-type 'c-return-statement) genome))
            "i"))
         (expected-declaration (find-if (of-type 'c-pointer-declarator) genome)))
    (is (eq (aget :decl i-alist) expected-declaration))
    (is (eq (aget :scope i-alist) genome))))


;;;; Equality tests
(deftest c-equal?-surrounding-text ()
  "equal? considers surrounding text when checking for equality."
  (let ((ast (make-instance 'c-ast)))
    (is (not (equal? ast (copy ast :before-text " "))))
    (is (not (equal? ast (copy ast :after-text " "))))
    (is (not (equal? ast (copy ast :before-text " " :after-text " "))))))


;;;; With Property tests
(defun test-the-with-property (ast node)
  "Given NODE in the genome of AST, test that the tree remains equal
(under tree equality) if NODE is replaced by itself.  Return true
if property holds, false if not."
  (let* ((path (ast-path ast node))
         (new-ast (with ast path node)))
    (equal? ast new-ast)))

(defgeneric with-property-fails-on-some-node (ast &key &allow-other-keys)
  (:documentation
  "Check that the with property applies to all nodes of an ast.
If any fails, return that node.  Otherwise, return NIL.")
  (:method ((ast functional-trees:node) &key)
    (block done
      (let ((count 0))
        (declare (ignorable count))
        (mapc (lambda (n)
                #+trace-with-property
                (when (eql (nth-value 1 (floor (incf count) 100)) 0)
                  (format t " ~a" count)
                  (finish-output))
                (unless (test-the-with-property ast n)
                  (return-from done n)))
            ast))
      nil))
  (:method ((sw software) &key)
    (with-property-fails-on-some-node (genome sw)))
  (:method ((s string) &key (lang 'c))
    (with-property-fails-on-some-node (from-string (make-instance lang) s)))
  (:method ((pn pathname) &key (lang 'c))
    (with-property-fails-on-some-node (from-file (make-instance lang) pn))))

(deftest with-property ()
  (let ((paths
          (directory (make-pathname :directory (append +c-tree-sitter-dir+
                                                       '(:up :wild-inferiors))
                                    :name "odd-even"
                                    :type "c"))))
    (is (equal
         (iter (for pn in paths)
               (let ((result (with-property-fails-on-some-node pn)))
                 (when result
                   (collect (list pn result)))))
         nil))))


;;;; GET-UNBOUND-VALS tests
(deftest c-get-unbound-vals-1 ()
  "get-unbound-vals handles variable shadowing."
  (let* ((source "int i = 10;

for (int i = i; i < i; i++) {}")
         (software (make 'c :genome (convert 'c-ast source)))
         (unbound-vals
           (get-unbound-vals software (find-if (of-type 'c-for-statement)
                                               (genome software)))))
    (is (member "i" unbound-vals :key #'source-text :test #'equal))))

(deftest c-get-unbound-vals-2 ()
  "get-unbound-vals gets variables that aren't defined in an AST."
  (let* ((source "int x = 0;
int y = 0;

for (int i = 0; i < x; i++) {
  x = i;
  y = x;
  i += i;
}")
         (software (make 'c :genome (convert 'c-ast source)))
         (for-statement (find-if (of-type 'c-for-statement) (genome software)))
         (unbound-vals (get-unbound-vals software for-statement)))
    (is (equal (collect-if (op (equal "x" (source-text _))) for-statement)
               (remove-if-not {equal "x"} unbound-vals :key #'source-text)))
    (is (equal (list (find-if {equal "y"} for-statement :key #'source-text))
               (remove-if-not {equal "y"} unbound-vals :key #'source-text)))
    ;; Doesn't contain unexpected identifiers.
    (is (null (remove-if {member _ '("x" "y") :test #'equal} unbound-vals
                         :key #'source-text)))))

(deftest c-get-unbound-vals-3 ()
  "get-unbound-vals doesn't return unbound function call identifiers."
  (let* ((source "x ();")
         (software (make 'c :genome (convert 'c-ast source)))
         (unbound-vals (get-unbound-vals software (genome software))))
    (is (null unbound-vals))))

(deftest field-name-test ()
  (is (equal (field-name (find-if (of-type 'c/cpp-field-declaration)
                                  (genome (from-string
                                           (make-instance 'c)
                                           "struct { int x; };"))))
             "x"))
  (is (null (field-name (genome (from-string (make-instance 'c) "int x;"))))))


;;;; variable-use-p tests
(defun bulk-variable-use-p (obj variables)
  "Return a list of variable-use-p results for every node in obj which has
source text equal to any value in variables."
  (mapcar {variable-use-p obj}
          (collect-if
           (op (member _ variables :test #'equal))
           (genome obj) :key #'source-text)))

(deftest c-variable-use-p-test ()
  "variable-use-p returns T on variable uses and NIL on non-variables."
  (iter
    (for (source failure-output variable-use-names non-variable-names) in
         '(("x->y;" "Field Expression" ("x") ("y"))
           ("x [y];" "Subscript Expression" ("x" "y"))
           ("x + y;" "Binary Expression" ("x" "y"))
           ("(x);" "Parenthesized Expression" ("x"))
           ("x++;" "Update Expression" ("x"))
           ("!x;" "Unary Expression" ("x"))
           ("x;" "Expression Statement" ("x"))
           ("*x;" "Pointer Expression" ("x"))
           ("int x = y;" "Init Declarator" ("y") ("x"))
           ("struct x y = { .a = m, .b = n };" "Initializer Pair"
            ("m" "n")
            ("x" "y" "a" "b"))
           ("int x [y];" "Array Declarator" ("y") ("x"))
           ("return x;" "Return Statement" ("x"))))
    (let* ((obj (make 'c :genome source))
           (variable-use-result (bulk-variable-use-p obj variable-use-names))
           (non-variable-result (bulk-variable-use-p obj non-variable-names)))
      (when variable-use-names
        ;; Ensure results
        (is (consp variable-use-result)
            "~a: variable uses empty result."
            failure-output)
        (is (notany #'null variable-use-result)
            "~a: variable uses failure."
            failure-output))
      (when non-variable-names
        ;; Ensure results
        (is (consp non-variable-result)
            "~a: non-variables empty result."
            failure-output)
        (is (every #'null non-variable-result)
            "~a: non-variables failure."
            failure-output)))))


;;;; collect-var-uses tests
(deftest c-collect-var-uses-1 ()
  "collect-var-uses doesn't collect shadowed variable uses."
  (let* ((source "void f (int a) {
  a += 1;

  for (char a = 0; a < 0; a++) {
    return a;
  }
}")
         (software (make 'c :genome source))
         (target-ast
           (second
            (collect-if {equal "a"} (genome software) :key #'source-text)))
         (var-uses (collect-var-uses software target-ast)))
    (is (eql 1 (length var-uses)))
    (is (eq target-ast (car var-uses)))))

(deftest c-collect-var-uses-2 ()
  "collect-var-uses gets uses of a variable."
  (let* ((source "void f (int a) {
  a += 1;

  for (a = 0; a < 0; a++) {
    if (a == a) {
      break;
    }

    return a;
  }

  return a;
}")
         (software (make 'c :genome source))
         (genome (genome software))
         (target-ast
           (find-if {equal "a"} genome :key #'source-text))
         (var-uses (collect-var-uses software target-ast)))
    (is (eql 8 (length var-uses)))
    (is (equal var-uses
               (cdr (collect-if {equal "a"} genome :key #'source-text))))))


;;;; patch-whitespace tests
(defun strip-surrounding-text (ast)
  "Destructively strip all before and after text from AST."
  (setf (before-text ast) ""
        (after-text ast) "")
  (iter
    (for child in (children ast))
    (strip-surrounding-text child))
  ast)

(defun prettify-software-dir (path)
  (merge-pathnames-as-file
   (make-pathname :directory (append +c-tree-sitter-dir+
                                     (list "prettify")))
   path))

(defun prettify-software-test (path)
  "Test that the file at PATH can have its AST reproduced by prettify-software
when its surrounding text is removed."
  (let* ((software (from-file (make 'c) (prettify-software-dir path)))
         ;; Get the string directly before #'genome turns it into an AST.
         (original-source (slot-value software 'genome))
         (software-copy (make 'c :genome (strip-surrounding-text
                                          (tree-copy (genome software))))))
    (is (equal (trim-whitespace original-source)
               (source-text
                (genome
                 (prettify-software
                  (make 'c-style-indentation)
                  software-copy)))))))

(deftest c-round-trip-prettify-software ()
  "prettify-software will prettify C."
  (mapc #'prettify-software-test
        ;; NOTE: these files can be modified as needed but
        ;;       should have somewhat presentable source.
        '(#P"comments.c"
          #P"if-statement.c"
          #P"indentation.c"
          #P"loops.c"
          #P"preproc.c"
          #P"string.c")))

(deftest do-not-duplicate-keys-on-copy ()
  (let* ((it (c "if(x){ return 0; }else{ return 1; }"))
         (annotations (slot-value (copy (copy (copy it))) 'sel/sw/ts::annotations)))
    ;; All keys are unique.
    (is (= (length (mapcar #'car annotations))
           (length (remove-duplicates (mapcar #'car annotations)))))))


;;;; Rule Substitution tests
(deftest c-labeled-statement-rule-substitution ()
  (let ((labeled-statement (convert 'c-ast "label: break;" :deepest t)))
    (is (equal "label" (source-text (c-label labeled-statement))))
    (is (equal "break;" (source-text (c-statement labeled-statement))))))

(deftest c-for-statement-rule-substitution ()
  (let* ((source "void foo() { for (int i = 0; i<5; i++) { i=i+2; } }")
         (root (convert 'c-ast source))
         (for-statement (stmt-with-text root "for" :at-start t)))
    (is (typep (body for-statement) 'compound-ast))))

(deftest c-case-statement-rule-substitution ()
  (let* ((source "void foo(int i) { switch (i) { case 1: i++; break; } }")
         (root (convert 'c-ast source))
         (case-statement (stmt-with-text root "case" :at-start t)))
    (is (= 2 (length (c-statements case-statement))))))
