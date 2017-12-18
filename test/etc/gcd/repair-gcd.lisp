;; repair using software evolution
(load #P"repair.lisp")
(in-package :repair)
(in-readtable :curry-compose-reader-macros)

(defvar *clang* (from-file (make-instance 'clang) "gcd.c"))
(defvar *cil*   (from-file (make-instance 'cil)   "gcd.c"))
(defvar *llvm*  (from-file (make-instance 'llvm) "gcd.ll"))

(defun brute-force-ast (ast)
  (assert (or (eq (type-of ast) 'cil)
              (eq (type-of ast) 'clang)
              (eq (type-of ast) 'llvm)) (ast)
              "AST should be either a cil or clang object")
  (let ((fitness (test-suite ast)))
    (assert (= 10 fitness) (ast)
            "AST should pass 10/11 test cases but instead passes ~a/11"
            fitness))
  (let ((num (size ast)))
    (store
     (block repair
       (flet ((mut (op)
                (let ((new (copy ast)))
                  (apply-mutation new op)
                  (when (= 11 (test-suite new))
                    (format t "repair found!~%")
                    (return-from repair new)))))
         ;; replace
         (when (eq (type-of ast) 'llvm)
           (loop :for left :below num :do
              (loop :for right :below num :do
                 (format t "replace ~d ~d~%" left right)
                 (mut (list :replace left right)))))
         ;; delete
         (loop :for id :below num :do
            (mut (list :cut id)))
         (when (eq (type-of ast) 'clang)
           (error 'brute-force "should have found it by now (:CUT 42)"))
         ;; insert
         (loop :for left :below num :do
            (loop :for right :below num :do
               (mut (list :insert left right))))
         (when (eq (type-of ast) 'cil)
           (error 'brute-force "should have found it by now (:INSERT 0 9)"))
         ;; swap
         (loop :for left :below num :do
            (loop :for right :below num :do
               (mut (list :swap left right))))))
     "results.store")))

#+run
((lambda (ast)
   (let ((*population* (loop :for i :below 100 :collect (copy ast)))
         (*max-population-size* 100))
     (store (evolve #'test-suite :max-fit 12) "results-ast.store")))
 *cil*)
