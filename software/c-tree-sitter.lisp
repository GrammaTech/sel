;;;; c-tree-sitter.lisp --- C software representation with tree-sitter backend.
(uiop:define-package :software-evolution-library/software/c-tree-sitter
  (:nicknames :sel/software/c-tree-sitter :sel/sw/c-tree-sitter
              :sel/software/c-ts :sel/sw/c-ts)
  (:use :gt/full
        :babel
        :cl-json
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/parseable
        :software-evolution-library/software/non-homologous-parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting))
(in-package :software-evolution-library/software/c-tree-sitter)
(in-readtable :curry-compose-reader-macros)

(define-software c-tree-sitter (tree-sitter) ()
  (:documentation "C tree-sitter software representation."))


;;; Shared object set-up
(register-tree-sitter-language "tree-sitter-c" :c 'c-tree-sitter-ast)


;;; C tree-sitter classes
(eval-always
  ;; TODO: maybe figure out a way to roll this into the automatically
  ;;       defined classes?
  (defclass c-tree-sitter-ast (tree-sitter-ast)
    ()
    (:documentation "AST for C from input via tree-sitter."))

  (defmethod inconsistent-production-p
      ((language (eql :c)) (production-name (eql :update-expression)))
    t))

;;; TODO: work on ease-of-use macros for tree-sitter.
(define-tree-sitter-classes ()
  ;; TODO: throw these in a variable that actually looks
  ;;       at the project path.
  "~/quicklisp/local-projects/sel/software/tree-sitter/c/node-types.json"
  "~/quicklisp/local-projects/sel/software/tree-sitter/c/grammar.json"
  :c
  c-tree-sitter-ast)

(defmethod update-child-order ((language (eql :c)) (ast c-update-expression))
  ;; TODO: we will get this before the ast-ranges are removed.
  ;;       Check both fields. whichever happens first, that
  ;;       slot should occur first.
  ;; NOTE: cl-tree-sitter trims out the operator field? This
  ;;       causes problems?
  #+nil
  (let* ((operator-range (ast-annotation :range-start (slot-value ast 'operator)))
         (field-range (ast-annotation :range-start (slot-value ast 'argument)))
         (operator-col (car operator-range))
         (operator-row (cdr operator-range))
         (field-col (car field-range))
         (field-row (cdr field-range))
         (operator-first-p
           (cond
             ((= operator-row field-row)
              (< operator-col field-col))
             ((< operator-row  field-row)
              t))))
    (push
     (if operator-first-p
         '(:child-order ((operator . 1)) ((argument . 1)))
         '(:child-order ((argument . 1)) ((operator . 1))))
     (slot-value ast 'annotations)))
  ;; NOTE: the child order annotation indicates an index in a list.
  ;;       Convert the slot into a list to accomodate this
  (setf (slot-value ast 'argument)
        (list (slot-value ast 'argument)))
  (push
   '(:child-order ((argument . 0)))
   (slot-value ast 'annotations)))
