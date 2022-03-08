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
   :software-evolution-library/components/formatting)
  (:export :test-go-tree-sitter))
(in-package :software-evolution-library/test/go-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-go-tree-sitter "Go tree-sitter representation."
  (go-tree-sitter-available-p))

;;; Utility

(deftest function-declaration.1 ()
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
      (is (no-fallthrough b))
      (is (no-fallthrough (@ b 0)))
      (is (not (no-fallthrough (@ b 1))))
      (is (not (no-fallthrough (@ b 2))))
      (is (no-fallthrough (@ b 3)))
      (is (no-fallthrough (@ b 4))))))

(deftest method-declaration.1 ()
  (let* ((a (convert 'golang-ast "func (p *Pointer) f(x int32, z *uint32) { }"))
         (fd (find-if (of-type 'golang-method-declaration) a)))
    (is (equal (function-name fd) "f"))
    (let ((p (function-parameters fd)))
      (is (eql (length p) 2))
      (is (every (of-type 'golang-parameter-declaration) p))
      (is (equal (source-text (car p)) "x int32"))
      (is (equal (source-text (cadr p)) "z *uint32")))))

(deftest field-names.1 ()
  (let* ((str (format nil "type foo struct {~% f1 uint32~% f2 *A~% }~%"))
         (a (convert 'golang-ast str))
         (fdl (find-if (of-type 'golang-field-declaration-list) a))
         (fds (direct-children fdl)))
    (is (eql (length fds) 2))
    (is (equal (field-names (car fds)) '("f1")))
    (is (equal (field-names (cadr fds)) '("f2")))))
                            
    
         
    
      
    
