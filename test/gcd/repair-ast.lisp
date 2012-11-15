;; repair using software evolution
(require :software-evolution)
(in-package :software-evolution)

(load #P"repair.lisp")

(defvar *base*  (file-to-string "gcd.c"))
(defvar *clang* (from-file (make-instance 'clang) "gcd.c"))
(defvar *cil*   (from-file (make-instance 'cil) "gcd.c"))

(defun brute-force-ast (ast)
  (assert (or (eq (type-of ast) 'cil)
              (eq (type-of ast) 'clang)) (ast)
              "AST should be either a cil or clang object")
  (let ((num (num-ids ast)))
    (store
     (block repair
       (flet ((mut (op)
                (let ((it (make-instance (type-of ast)
                            :base *base*
                            :edits (list op)
                            :c-flags (case (type-of ast)
                                       (cil nil)
                                       (clang (list "2>/dev/null"))))))
                  (when (= 12 (test-suite it))
                    (format t "repair found!~%")
                    (return-from repair it)))))
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
   (let ((*population* (repeatedly 100 (copy ast)))
         (*max-population-size* 100))
     (store (evolve #'test-suite :max-fit 12) "results-ast.store")))
 *cil*)
