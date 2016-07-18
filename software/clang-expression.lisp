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

(defun expression-to-c (expression)
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
(define-condition eval-error (error) ())

(defun operator-to-function (operator)
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
                   (t "int")))))
       (operator-no-pointers (op a b)
         (if (or (pointerp a) (pointerp b))
             (error (make-condition 'eval-error
                                    "Not allowed on pointer values."))
             (operator op a b))))

   (case operator
     (:+ {operator #'+})
     (:- {operator #'-})
     (:/ {operator-no-pointers #'truncate}) ; assume integer division
     (:* {operator-no-pointers #'*})
     (otherwise
      (error (make-condition 'eval-error
                             "Unknown operator: ~s" operator))))))

(defun evaluate-expression (expression free-vars)
  (multiple-value-bind (result interior-max)
      (cond
        ((listp expression)
         (let ((args (mapcar (lambda (a)
                               (multiple-value-list
                                (evaluate-expression a free-vars)))
                             (cdr expression))))
           ;; TODO: support functions/operators with different arity
           (unless (= (length args) 2)
             (error (make-condition 'eval-error
                      "Wrong number of arguments. Expected 2, got ~a"
                      (length args))))
           (values (apply (operator-to-function (car expression))
                          (mapcar #'first args))
                   (apply #'max (mapcar #'second args)))))
        ((numberp expression) (list expression "int"))
        ((symbolp expression)
         (or (aget expression free-vars)
             (error (make-condition 'eval-error
                      "Undefined variable: ~s" expression))))
        (t (error (make-condition 'eval-error
                    "Unrecognized expression: ~s" expression))))
    (values result
            (max (car result) (or interior-max 0)))))

(defun expression-unbound-vars (expression)
  (cond
    ((listp expression)
     (remove-duplicates (apply #'append
                               (mapcar #'expression-unbound-vars
                                       (cdr expression)))))
    ((symbolp expression) (list expression))
    (t nil)))


;;;; Targeted mutations
(defvar *math-operators* '(:+ :- :* :/))
(define-mutation change-operator (mutation)
  ((targeter :initform (lambda (lisp)
                         (let ((operators (operator-subtrees lisp)))
                           (when operators
                             (list (random-elt operators)
                                   (random-elt *math-operators*))))))))

(defmethod operator-subtrees ((lisp lisp))
  (filter-subtrees [{member _ *math-operators*} #'car]
                   lisp))

(defmethod apply-mutation ((lisp lisp) (mutation change-operator))
  (when (targets mutation)
    (bind (((tree operator) (targets mutation)))
      (with-slots (genome) lisp
        (rplaca (subtree genome tree) operator))))
  lisp)

;;; Constant replacement
(define-mutation change-constant (mutation)
  ((targeter :initform (lambda (lisp)
                         (list (random-elt (constant-subtrees lisp))
                               (random-elt '(:double :halve :negate
                                             :increment :decrement
                                             :one :zero :negative-one)))))))

(defmethod constant-subtrees ((lisp lisp))
  (filter-subtrees [#'numberp #'car] lisp))

(defmethod apply-mutation ((lisp lisp) (mutation change-constant))
  (bind (((tree transformation) (targets mutation)))
    (with-slots (genome) lisp
      (rplaca (subtree genome tree)
              (let ((value (car (subtree genome tree))))
                (ecase transformation
                  (:double (* 2 value))
                  (:halve (floor value 2))
                  (:negate (* -1 value))
                  (:increment (+ value 1))
                  (:decrement (- value 1))
                  (:one 1)
                  (:zero 0)
                  (:negative-one -1))))))
  lisp)

;;; Specialized mutations
(defgeneric pick-bad-binop-left (software)
  (:documentation
   "Pick a binary operation for demotion via `demote-binop-left'."))

(defmethod pick-bad-binop-left ((obj lisp))
  (flet ((binopp (subtree) (and (listp subtree) (= 3 (length subtree)))))
    (->> (genome obj)
         (remove-if-not (lambda (subtree)
                          (and (binopp subtree) (binopp (second subtree)))))
         (random-elt))))

(defgeneric pick-bad-binop-right (software)
  (:documentation
   "Pick a binary operation for demotion via `demote-binop-right'."))

(defmethod pick-bad-binop-right ((obj lisp))
  (flet ((binopp (subtree) (and (listp subtree) (= 3 (length subtree)))))
    (->> (genome obj)
         (remove-if-not (lambda (subtree)
                          (and (binopp subtree) (binopp (third subtree)))))
         (random-elt))))

(define-mutation demote-binop-left (mutation)
  ((targeter :initform #'pick-bad-binop-left)))

(defmethod apply-mutation ((lisp lisp) (mutation demote-binop-left))
  (destructuring-bind (op (l-op l-left l-right) right) (targets mutation)
    (with-slots (genome) lisp
      (setf (subtree genome (targets mutation))
            (list l-op (list op l-left right) l-right))))
  lisp)

(define-mutation demote-binop-right (mutation)
  ((targeter :initform #'pick-bad-binop-right)))

(defmethod apply-mutation ((lisp lisp) (mutation demote-binop-right))
  (destructuring-bind (op left (r-op r-left r-right)) (targets mutation)
    (with-slots (genome) lisp
      (setf (subtree genome (targets mutation))
            (list r-op r-left (list op r-right left)))))
  lisp)

;;; Semantics preserving mutations
(define-mutation mult-divide (mutation)
  ((targeter :initform #'pick-bad)))

(defmethod apply-mutation ((lisp lisp) (mutation mult-divide))
  (let ((s (targets mutation)))
    (with-slots (genome) lisp
      (setf (subtree genome s)
            `(:/ (:* ,(copy-tree (car (subtree genome s))) 2) 2))))
  lisp)

(define-mutation add-subtract (mutation)
  ((targeter :initform #'pick-bad)))

(defmethod apply-mutation ((lisp lisp) (mutation add-subtract))
  (let ((s (targets mutation)))
    (with-slots (genome) lisp
      (setf (subtree genome s)
            `(:- (:+ ,(copy-tree (car (subtree genome s))) 1) 1))))
  lisp)

(define-mutation subtract-add (mutation)
  ((targeter :initform #'pick-bad)))

(defmethod apply-mutation ((lisp lisp) (mutation subtract-add))
  (let ((s (targets mutation)))
    (with-slots (genome) lisp
      (setf (subtree genome s)
            `(:+ (:- ,(copy-tree (car (subtree genome s))) 1) 1))))
  lisp)

(define-mutation double-half (mutation)
  ((targeter :initform #'pick-bad)))

(defmethod apply-mutation ((lisp lisp) (mutation double-half))
  (let ((s (targets mutation)))
    (with-slots (genome) lisp
      (setf (subtree genome s)
            `(:- (:+ ,(copy-tree (car (subtree genome s)))
                     ,(copy-tree (car (subtree genome s))))
                 ,(copy-tree (car (subtree genome s)))))))
  lisp)
