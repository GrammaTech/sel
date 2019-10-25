;;; clang-expression.lisp --- calculate lisp expressions from clang ASTs
(defpackage :software-evolution-library/software/clang-expression
  (:nicknames :sel/software/clang-expression :sel/sw/clang-expression)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :cl-ppcre
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang
        :software-evolution-library/software/expression)
  (:export :expression-intern
           :expression-to-c))
(in-package :software-evolution-library/software/clang-expression)
(in-readtable :curry-compose-reader-macros)

(defun expression-intern (string)
  "Intern STRING for symbolic use in an expression.
This is used to intern string names by `expression'."
  (make-keyword (string-upcase string)))

(defmethod expression ((obj clang-base) (ast clang-ast-base))
  "Convert AST to an expression tree.
* OBJ clang software object containing AST
* AST the AST to convert
"
  ;; TODO: The following AST types currently pull information from the
  ;;        source text.  This conversion may be made more robust by
  ;;        extending more ASTs types with additional information like
  ;;        opcode, e.g. the following.
  ;;        - UnaryExprOrTypeTraitExpr :: sizeof | alignof | vec_step
  ;;        - UnaryOperator :: opcode
  ;;        - MemberExpr :: field name
  (flet ((over-children (elt)
           (cons elt (mapcar {expression obj}
                             (get-immediate-children obj ast))))
         (only-child ()
           (expression obj (first (get-immediate-children obj ast)))))
    (switch ((ast-class ast))
      (:BinaryOperator (over-children (expression-intern (ast-opcode ast))))
      (:CompoundAssignOperator (->> (ast-opcode ast)
                                    (expression-intern)
                                    (over-children)))
      (:DeclRefExpr (expression-intern (peel-bananas (source-text ast))))
      (:ImplicitCastExpr (only-child))
      (:IntegerLiteral
       (handler-bind ((parse-number
                       (expression-intern (source-text ast))))
         (parse-number (source-text ast))))
      (:FloatingLiteral
       (handler-bind ((parse-number
                       (expression-intern (source-text ast))))
         (parse-number (source-text ast))))
      (:ParenExpr (only-child))
      (:ArraySubscriptExpr (over-children :|[]|))
      (:CallExpr (mapcar {expression obj} (get-immediate-children obj ast)))
      (:UnaryExprOrTypeTraitExpr
       (let* ((src (source-text ast))
              (operator (cond
                          ((scan "\s*sizeof" src)   :sizeof)
                          ((scan "\s*alignof" src)  :alignof)
                          ((scan "\s*vec_step" src) :vec_step)
                          (t (error
                              "Unmatched UnaryExprOrTypeTraitExpr ~s." src)))))
         (if (get-immediate-children obj ast)
             (over-children operator)
             ;; Otherwise the argument is a type.
             (multiple-value-bind (matchp matches)
                 (scan-to-strings "\\(([^\\)]+)\\)" src)
               (if matchp
                   (list operator (expression-intern (aref matches 0)))
                   (error "Unmatched UnaryOperator ~s." src))))))
      (:UnaryOperator
       (over-children (let ((src (source-text ast)))
                        (cond
                          ((scan "\s*\\*" src) :unary-*)
                          (t (error "Unmatched UnaryOperator ~s." src))))))
      (:MemberExpr
       (let ((src (source-text ast)))
         (let ((match-data (multiple-value-list (scan "->([\\w\\a_]+)" src))))
           (if (first match-data)
               (list :->
                     (expression obj (first (get-immediate-children obj ast)))
                     (expression-intern (subseq src
                                                (aref (third match-data) 0)
                                                (aref (fourth match-data) 0))))
               (error "Unmatched MemberExpr ~S." src)))))
      (t :unimplemented))))

(defun expression-to-c (expression)
  "Format EXPRESSION as C source code.
* EXPRESSION an expression tree
"
  (cond
    ((listp expression)
     (assert (= 3 (length expression)))
     (format nil "(~a ~a ~a)"
             (expression-to-c (second expression))
             (symbol-name (car expression))
             (expression-to-c (third expression))))
    ((numberp expression) (format nil "~a" expression))
    ((symbolp expression) (symbol-name expression))))


;;;; Evaluation
(define-software clang-expression (expression)
  ((scope :initarg :scope :accessor scope :initform nil :copier :direct
          :documentation "List of in-scope variable names."))
  (:documentation
   "C arithmetic expressions represented as trees to allow direct evaluation."))

(defmethod operator-to-function ((obj clang-expression) operator)
  "Return a function which evaluates OPERATOR on two expressions.
* OBJ a CLANG-EXPRESSION object
* OPERATOR a keyword symbol representing the operator (e.g. :+)
"
  (labels
      ((pointerp (val)
         (eq (char (second val) 0) #\*))
       (operator (op a b)
         (let ((ta (second a))
               (tb (second b)))
           (list (funcall op (car a) (car b))
                 ;; Highly approximate type propagation
                 ;; We really only care if values are pointers or not.
                 (cond
                   ((string= ta tb) ta) ; preserve equal types
                   ((pointerp a) ta)
                   ((pointerp b) tb)
                   (t "literal")))))
       (operator-no-pointers (op a b)
         (if (or (pointerp a) (pointerp b))
             (error (make-condition 'eval-error
                      :text "Not allowed on pointer values."
                      :expr operator))
             (operator op a b))))

    (case operator
      (:+ {operator #'+})
      (:- {operator #'-})
      (:/ {operator-no-pointers #'truncate}) ; assume integer division
      (:* {operator-no-pointers #'*})
      (otherwise
       (error (make-condition 'eval-error
                :text "Unknown operator"
                :expr operator))))))

(defmethod evaluate-expression ((obj clang-expression) free-vars
                                &optional expression)
  "Evaluate expression over FREE-VARS, returning result and max interior value.
* OBJ a CLANG-EXPRESSION object
* FREE-VARS an alist mapping variable names to values
* EXPRESSION an expression tree
"
  (multiple-value-bind (result interior-max)
      (cond
        ((listp expression)
         ;; TODO: support functions/operators with different arity
         (unless (= (length expression) 3)
           (error (make-condition 'eval-error
                    :text (format nil
                                  "Wrong number of arguments. Expected 2 got ~a"
                                  (1- (length expression)))
                    :expr expression)))
         (let ((args (mapcar (lambda (a)
                               (multiple-value-list
                                (evaluate-expression obj free-vars a)))
                             (cdr expression))))
           (values (apply (operator-to-function obj (car expression))
                          (mapcar #'first args))
                   (apply #'max (mapcar #'second args)))))
        ((numberp expression)
         (list expression "int"))
        ((symbolp expression)
         (or (aget expression free-vars)
             (error (make-condition 'eval-error
                      :text "Undefined variable:"
                      :expr expression))))
        (t (error (make-condition 'eval-error
                    :text "Unrecognized expression:"
                    :expr expression))))
    (values result
            (max (car result) (or interior-max 0)))))

(defmethod operators ((obj clang-expression))
  "DOCFIXME
* OBJ DOCFIXME
"
  '(:+ :- :* :/))
