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

(deftest function-name-on-c-tree-sitter ()
  (with-fixture w/while
    (string= "main" (function-name (find-if {typep _ 'function-ast} *soft*)))))

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

(defun parsing-test-dir (path)
  (merge-pathnames-as-file
   (make-pathname :directory (append +c-tree-sitter-dir+
                                     (list "parsing")))
   path))

(defun parse-test (path &rest ast-types)
  (let ((soft (from-file (make-instance 'c)
                         (parsing-test-dir path))))
    (is (not (zerop (size soft))))
    (is (equal (genome-string soft)
               (file-to-string (parsing-test-dir path))))
    (is (not (find-if {typep _ 'c-error} (genome soft))))
    (is (find-if
         (lambda (ast)
           (typep ast `(and ,@ast-types)))
         (genome soft))
        (format nil "Found ASTS of types (and ~{~a~^ ~})" ast-types))
    soft))

(deftest (c-tree-sitter-parsing-test :long-running) ()
  (mapc {apply #'parse-test}
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
          (#P"variadic-macro.c" c-parameter-declaration)
          ;; NOTE: round trip.
          (#P"declaration-specifiers.c")
          ;; NOTE: round trip.
          (#P"language-redefining-macro.c"))))

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

(deftest test-scopes ()
  (let* ((c (sel:from-string (make 'c) (fmt "~
int main () {
  int x = 1;
  int z;
  y();
}")))
         (scopes (software-evolution-library/software/parseable:scopes
                  c
                  (find-if (of-type 'call-ast) c)))
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
