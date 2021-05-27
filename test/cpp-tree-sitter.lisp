;;;; cpp-tree-sitter.lisp --- C tree-sitter representation.
(defpackage :software-evolution-library/test/cpp-tree-sitter
  (:nicknames :sel/test/cpp-tree-sitter :sel/test/cpp-ts)
  (:use
    :gt/full
    :software-evolution-library/test/util
    :stefil+
    :software-evolution-library
    :software-evolution-library/software/parseable
    :software-evolution-library/software/tree-sitter
    :software-evolution-library/software/cpp
    :software-evolution-library/test/util-clang
    :software-evolution-library/components/file
    :software-evolution-library/components/formatting)
  (:export :test-cpp-tree-sitter))
(in-package :software-evolution-library/test/cpp-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-cpp-tree-sitter "C tree-sitter representation."
  (cpp-tree-sitter-available-p))

(deftest test-scopes ()
  (let* ((c (sel:from-string (make 'cpp) (fmt "~
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

(deftest test-reference-return ()
  (is (equal "foo"
             (function-name
              (convert 'cpp-ast "int& foo() {}" :deepest t))))
  (let* ((file (asdf:system-relative-pathname
                "software-evolution-library"
                "test/etc/cpp-tree-sitter/reference.cc"))
         (software (sel:from-file 'cpp file))
         (fn (find-if (of-type 'function-ast) (genome software))))
    (is (equal "FileDescriptorTables::GetEmptyInstance"
               (function-name fn)))))

(deftest preprocessor-test-1 () ;; address sel issue 136
  "Ensure that #ifndef is not converted to #ifdef"
  (let ((*soft* (from-string (make-instance 'cpp) 
"#ifndef BSD4_1
#define HAVE_GETPAGESIZE
#endif
")))
    (is (typep *soft* 'cpp))
    (is (typep (genome *soft*) 'cpp-translation-unit))
    (is (typep (elt (children (genome *soft*)) 0) 'cpp-preproc-ifdef))
    (is (starts-with-subseq
         "#ifndef" (source-text (elt (children (genome *soft*)) 0))))))

(deftest preprocessor-test-2 () ;; address sel issue 136
  "Ensure that #ifdef is not converted to #ifndef"
  (let ((*soft* (from-string (make-instance 'cpp) 
"#ifdef BSD4_1
#define HAVE_GETPAGESIZE
#endif
")))
    (is (typep *soft* 'cpp))
    (is (typep (genome *soft*) 'cpp-translation-unit))
    (is (typep (elt (children (genome *soft*)) 0) 'cpp-preproc-ifdef))
    (is (starts-with-subseq
         "#ifdef" (source-text (elt (children (genome *soft*)) 0))))))

(deftest compound-operator-test-1 () ;; sel issue #137
  (let ((*soft* (from-string (make-instance 'cpp) 
"{ int x = 1; 
   x *= 4; 
   x += 2; 
   x -= 1; 
   x /= 5; }")))
    (is (typep *soft* 'cpp))
    (let ((g (genome *soft*)))
      (is (typep g 'cpp-translation-unit))
      (is (typep (@ g 0) 'cpp-compound-statement))
      (is (typep (@ (@ g 0) 1) 'cpp-expression-statement))
      (is (typep (@ (@ (@ g 0) 1) 0) 'cpp-assignment-expression))
      (is (string-equal
           (source-text (@ (@ (@ (@ g 0) 1) 0) 1))
           "*="))
      (is (string-equal
           (source-text (@ (@ (@ (@ g 0) 2) 0) 1))
           "+="))
      (is (string-equal
           (source-text (@ (@ (@ (@ g 0) 3) 0) 1))
           "-="))
      (is (string-equal
           (source-text (@ (@ (@ (@ g 0) 4) 0) 1))
           "/=")))))
      
(deftest field-expression-test-1 () ;; sel issue #142
  "Ensure that '.' and '->' are handled correctly"
  (let ((*soft* (from-string (make-instance 'cpp) 
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
    (is (typep *soft* 'cpp))
    (let ((g (genome *soft*)))
      (is (typep g 'cpp-translation-unit))
      (is (typep (@ g 0) 'cpp-function-definition))
      (is (typep (@ (@ g 0) 2) 'cpp-compound-statement))
      (is (typep (@ (@ (@ g 0) 2) 4) 'cpp-expression-statement))
      (is (typep (@ (@ (@ (@ g 0) 2) 4) 0) 'cpp-assignment-expression))
      (is (typep (@ (@ (@ (@ (@ g 0) 2) 4) 0) 0) 'cpp-field-expression))
      (is (string-equal (source-text  (@ (@ (@ (@ (@ (@ g 0) 2) 4) 0) 0) 1))
                        "->"))
      (is (typep (@ (@ (@ g 0) 2) 5) 'cpp-expression-statement))
      (is (typep (@ (@ (@ (@ g 0) 2) 5) 0) 'cpp-assignment-expression))
      (is (typep (@ (@ (@ (@ (@ g 0) 2) 5) 0) 2) 'cpp-field-expression))
      (is (string-equal (source-text  (@ (@ (@ (@ (@ (@ g 0) 2) 5) 0) 2) 1))
                        "->")))))
