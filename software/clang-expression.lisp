;;; clang-expression.lisp --- calculate lisp expressions from clang ASTs
(in-package :software-evolution)

(defun expression-intern (string)
  "Intern STRING for symbolic use in an expression.
This is used to intern string names by `expression'."
  (make-keyword (string-upcase string)))

#+(or )                ; NOTE: Looks like this isn't really necessary.
(defun clang-expression-opcode (raw-opcode)
  (switch (raw-opcode :test #'string=)
    ("=" :=!)
    (t (expression-intern raw-opcode))))

(defmethod expression ((obj clang) (id integer))
  (expression obj (get-ast obj id)))

(defmethod expression ((obj clang) (ast list))
  ;; TODO: The following AST types currently pull information from the
  ;;        source text.  This conversion may be made more robust by
  ;;        extending more ASTs types with additional information like
  ;;        opcode, e.g. the following.
  ;;        - UnaryExprOrTypeTraitExpr :: sizeof | alignof | vec_step
  ;;        - UnaryOperator :: opcode
  ;;        - MemberExpr :: field name
  (flet ((over-children (elt)
           (cons elt (mapcar {expression obj} (aget :children ast))))
         (only-child ()
           (expression obj (first (aget :children ast)))))
    (switch ((aget :ast-class ast) :test #'string=)
      ("BinaryOperator" (over-children (expression-intern (aget :opcode ast))))
      ("DeclRefExpr" (expression-intern (peel-bananas (aget :src-text ast))))
      ("ImplicitCastExpr" (only-child))
      ("IntegerLiteral" (parse-integer (aget :src-text ast)))
      ("ParenExpr" (only-child))
      ("ArraySubscriptExpr" (over-children :|[]|))
      ("CallExpr" (mapcar {expression obj} (aget :children ast)))
      ("UnaryExprOrTypeTraitExpr"
       (let* ((src (aget :src-text ast))
              (operator (cond
                          ((scan "\s*sizeof" src)   :sizeof)
                          ((scan "\s*alignof" src)  :alignof)
                          ((scan "\s*vec_step" src) :vec_step)
                          (t (error
                              "Unmatched UnaryExprOrTypeTraitExpr ~s." src)))))
         (if (aget :children ast)
             (over-children operator)
             ;; Otherwise the argument is a type.
             (multiple-value-bind (matchp matches)
                 (scan-to-strings "\\(([^\\)]+)\\)" src)
               (if matchp
                   (list operator (expression-intern (aref matches 0)))
                   (error "Unmatched UnaryOperator ~s." src))))))
      ("UnaryOperator"
       (over-children (let ((src (aget :src-text ast)))
                        (cond
                          ((scan "\s*\\*" src) :unary-*)
                          (t (error "Unmatched UnaryOperator ~s." src))))))
      ("MemberExpr"
       (let ((src (aget :src-text ast)))
         (let ((match-data (multiple-value-list (scan "->([\\w\\a_]+)" src))))
           (if (first match-data)
               (list :->
                     (expression obj (first (aget :children ast)))
                     (expression-intern (subseq src
                                            (aref (third match-data) 0)
                                            (aref (fourth match-data) 0))))
               (error "Unmatched MemberExpr ~S." src)))))
      (t :unimplemented))))

(defvar *math-operators* '(:+ :- :* :/))

;; Operator replacement
(define-mutation change-operator (mutation)
  ((targeter :initform (lambda (lisp)
                         (list (random-elt (operator-subtrees lisp))
                               (random-elt *math-operators*))))))

(defmethod operator-subtrees ((lisp lisp))
  (remove-if-not [{member _ *math-operators*} #'car {subtree (genome lisp)}]
                 (range 0 (1- (size lisp)))))

(defmethod apply-mutation ((lisp lisp) (mutation change-operator))
  (bind (((tree operator) (targets mutation)))
    (with-slots (genome) lisp
      (rplaca (subtree genome tree) operator)))
  lisp)

      (with-slots (genome) lisp
        (rplaca (subtree genome tree) operator)))
    lisp)
