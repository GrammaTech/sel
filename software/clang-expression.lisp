#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
;;; clang-expression.lisp --- calculate lisp expressions from clang ASTs
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defun expression-intern (string)
  "Intern STRING for symbolic use in an expression.
This is used to intern string names by `expression'."
  (make-keyword (string-upcase string)))

(defmethod expression ((obj clang) (ast ast-ref))
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

#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
