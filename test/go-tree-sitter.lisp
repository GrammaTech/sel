;;;; go-tree-sitter.lisp -- Go tree-sitter representation
(defpackage :software-evolution-library/test/go-tree-sitter
  (:nicknames :sel/test/go-tree-sitter :sel/test/go-ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/go
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting
   :functional-trees/attrs)
  (:import-from :software-evolution-library/software/tree-sitter
                :outer-declarations
                :inner-declarations)
  (:export :test-go-tree-sitter))
(in-package :software-evolution-library/test/go-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-go-tree-sitter "Go tree-sitter representation."
  (go-tree-sitter-available-p))


;;; Utility

(defmacro with-source-attr ((root-var source) &body body)
  `(let* ((source ,source)
          (,root-var (convert 'golang-ast source)))
     (with-attr-table ,root-var
       ,@body)))

(defun assert-declarations-equal (declarations-fn target-type root expected-list
                              &optional (expected-namespace-list
                                         nil
                                         expected-namespaces-supplied-p))
  "Check if EXPECTED-LIST is equal to the value returned by DECLARATIONS-FN when
called on the first AST of TARGET-TYPE in ROOT. Optionally checks if the
namespace list is EXPECTED-NAMESPACE-LIST"
  (mvlet ((declarations
           namespaces
           (funcall declarations-fn
                    (find-if (of-type target-type)
                             root))))
    (is (equal expected-list
               (mapcar #'source-text declarations)))
    (when expected-namespaces-supplied-p
      (is (equal namespaces expected-namespace-list)))))


;;; General tests

;;; Confirm that go-specific methods for three language-independent generic
;;; functions are doing the right thing
(deftest function-declaration.1 ()
  ;;; Test of function-name, function-parameters, and function-body on
  ;;; golang-function-declaration nodes
  (let* ((a (convert 'golang-ast "func f(x int32, y int32) int32 { return x+y; g(); x=1; continue; break; }"))
         (fd (find-if (of-type 'golang-function-declaration) a)))
    (is (equal (function-name fd) "f"))
    (let ((p (function-parameters fd)))
      (is (eql (length p) 2))
      (is (every (of-type 'golang-parameter-declaration) p))
      (is (equal (source-text (car p)) "x int32"))
      (is (equal (source-text (cadr p)) "y int32")))
    (let ((b (function-body fd)))
      (is (typep b 'golang-block))
      (is (eql (length (direct-children b)) 5))
      (is (typep (@ b 0) 'golang-return-statement))
      ;; Also, tests of no-fallthrough
      ;; no-fallthrough should be true if the statement transfer control elsewhere
      (is (no-fallthrough b))
      (is (no-fallthrough (@ b 0)))
      (is (not (no-fallthrough (@ b 1))))
      (is (not (no-fallthrough (@ b 2))))
      (is (no-fallthrough (@ b 3)))
      (is (no-fallthrough (@ b 4))))))

(deftest method-declaration.1 ()
  ;;; Test of function-name and function-parameters on
  ;;; golang-method-declaration nodes
  (let* ((a (convert 'golang-ast "func (p *Pointer) f(x int32, z *uint32) { }"))
         (fd (find-if (of-type 'golang-method-declaration) a)))
    (is (equal (function-name fd) "f"))
    (let ((p (function-parameters fd)))
      (is (eql (length p) 2))
      (is (every (of-type 'golang-parameter-declaration) p))
      (is (equal (source-text (car p)) "x int32"))
      (is (equal (source-text (cadr p)) "z *uint32")))))

;;; Tests of field-names on golang-field-declarations
(deftest field-names.1 ()
  (let* ((str (format nil "type foo struct {~% f1 uint32~% f2 *A~% }~%"))
         (a (convert 'golang-ast str))
         (fdl (find-if (of-type 'golang-field-declaration-list) a))
         (fds (direct-children fdl)))
    (is (eql (length fds) 2))
    (is (equal (field-names (car fds)) '("f1")))
    (is (equal (field-names (cadr fds)) '("f2")))))


;;; Inner Declarations

(deftest go-inner-declarations-function-1 ()
  "inner-declarations returns the name and the parameters of a function."
  (with-source-attr (root
                     "
package pkg

func fn (x, y int, a, b bool) {
  return
}
")
    (assert-declarations-equal
     #'inner-declarations
     'golang-function-declaration
     root
     '("fn" "x" "y" "a" "b"))))

(deftest go-inner-declarations-function-2 ()
  "inner-declarations returns the return parameters of a function."
  (with-source-attr (root
                     "
package pkg

func fn () (x, y int) {
  x = 1
  y = 2

  return
}
")
    (assert-declarations-equal
     #'inner-declarations
     'golang-function-declaration
     root
     '("fn" "x" "y"))))

(deftest go-inner-declarations-function-3 ()
  "inner-declarations returns the type parameters of a function."
  (with-source-attr (root
                     "
package pkg

func fn[T X, A, B C] () {
  return
}
")
    (assert-declarations-equal
     #'inner-declarations
     'golang-function-declaration
     root
     '("T" "A" "B" "fn")
     '(:type :type :type :function))))

(deftest go-inner-declarations-method-1 ()
  "outer-declarations returns the receiver of a method."
  (with-source-attr (root
                     "
package pkg

type X int

func (x X) fn () X {
  return x
}
")
    (assert-declarations-equal
     #'inner-declarations
     'golang-method-declaration
     root
     '("x" "fn")
     '(:variable :method))))

(deftest go-inner-declarations-for-1 ()
  "inner-declarations returns the variables defined in a 'for' init section."
  (with-source-attr (root
                     "
package pkg

func fn () {
  for i := 0; i < 10; i++ {
    return
  }
}
")
    (assert-declarations-equal
     #'inner-declarations
     'golang-for-statement
     root
     '("i"))))

(deftest go-inner-declarations-if-1 ()
  "inner-declarations returns the variables defined in an 'if' init section."
  (with-source-attr (root
                     "
package pkg

func fn () {
  if i := 0; i < 0 {
    return
  }
}
")
    (assert-declarations-equal
     #'inner-declarations
     'golang-if-statement
     root
     '("i"))))

(deftest go-inner-declarations-type-switch-1 ()
  "inner-declarations returns the variables defined in a type switch init
section and the alias."
  (with-source-attr (root
                     "
package pkg

func fn () {
  switch x, y := 0, 0; v := x.(type) {
    case int:
      return y
  }
}
")
    (assert-declarations-equal
     #'inner-declarations
     'golang-type-switch-statement
     root
     '("v" "x" "y"))))

(deftest go-inner-declarations-expression-switch-1 ()
  "inner-declarations returns the variables defined in an expression switch init
section."
  (with-source-attr (root
                     "
package pkg

func fn () {
  switch x, y := 0, 0; x {
    case 0:
      return y
  }
}
")
    (assert-declarations-equal
     #'inner-declarations
     'golang-expression-switch-statement
     root
     '("x" "y"))))


;;; Outer Declarations

(deftest go-outer-declarations-function-1 ()
  "outer-declarations returns the name of a function."
  (with-source-attr (root
                     "
package pkg

func fn (x int) {
  return
}
")
    (assert-declarations-equal
     #'outer-declarations
     'golang-function-declaration
     root
     '("fn"))))

(deftest go-outer-declarations-method-1 ()
  "outer-declarations returns the name of a method."
  (with-source-attr (root
                     "
package pkg

type X int

func (x X) fn () X {
  return x
}
")
    (assert-declarations-equal
     #'outer-declarations
     'golang-method-declaration
     root
     '("fn")
     '(:method))))

(deftest go-outer-declarations-var-1 ()
  "outer-declarations returns the variables declared by var declarations."
  (with-source-attr (root
                     "
package pkg

var x, y int
")
    (assert-declarations-equal
     #'outer-declarations
     'golang-var-declaration
     root
     '("x" "y"))))

(deftest go-outer-declarations-var-2 ()
  "outer-declarations returns the variables declared by var declarations."
  (with-source-attr (root
                     "
package pkg

var (
  x, y int
  a, b bool
)
")
    (assert-declarations-equal
     #'outer-declarations
     'golang-var-declaration
     root
     '("x" "y" "a" "b"))))

(deftest go-outer-declarations-short-var-1 ()
  "outer-declarations returns the variables declared by short var declarations."
  (with-source-attr (root
                     "
package pkg

x, y := 10, 20
")
    (assert-declarations-equal
     #'outer-declarations
     'golang-short-var-declaration
     root
     '("x" "y"))))

(deftest go-outer-declarations-import-declaration-1 ()
  "outer-declarations returns the named imports from import statements."
  (with-source-attr (root
                     "
package pkg

import thing \"thing\"
")
    (assert-declarations-equal
     #'outer-declarations
     'golang-import-declaration
     root
     '("thing")
     '(:variable))))


;;; Symbol Table


