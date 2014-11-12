;; repair using software evolution
(require :software-evolution)
(load #P"test/gcd/repair.lisp")
(in-package :repair)

(defvar *path*  "test/gcd/gcd.c")
(defvar *clang* (from-file (make-instance 'clang) *path*))
(defvar *cil*   (from-file (make-instance 'cil) *path*))

(defun brute-force-ast (ast)
  "Perform a brute force search for the repair to gcd.c.
Prints updates on the progress of the search, saves the discovered
repair to \"results.store\", and prints errors if known repairs are
not found."
  (assert (or (eq (type-of ast) 'cil)
              (eq (type-of ast) 'clang)) (ast)
              "AST should be either a cil or clang object")
  (let ((num (size ast)))
    (store
     (block repair
       (flet ((mut (op)
                (let* ((it (copy ast)))
                  (apply-mutation it op)
                  (setf (fitness it) (test-suite it))
                  (if (= 11 (fitness it))
                      (progn (format t "repair found!~%")
                             (return-from repair it))
                      (format t "    ~2d ~a~%" (fitness it) op)))))
         ;; insert
         (loop :for left :below num :do
            (loop :for right :below num :do
               (mut (list :insert left right))))
         (when (eq (type-of ast) 'cil)
           (error "should have found it by now (:INSERT 0 9)"))
         ;; delete
         (loop :for id :below num :do
            (mut (list :cut id)))
         (when (eq (type-of ast) 'clang)
           (error "should have found it by now (:CUT 42)"))
         ;; swap
         (loop :for left :below num :do
            (loop :for right :below num :do
               (mut (list :swap left right))))))
     "results.store")))

#+(or )
((lambda (ast)
   (let ((*population* (loop :for i :below 100 :collect (copy ast)))
         (*max-population-size* 100))
     (store (evolve #'test-suite :max-fit 12) "results-ast.store")))
 *cil*)
