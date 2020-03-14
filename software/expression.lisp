;;; expression.lisp --- evolve single expressions
(defpackage :software-evolution-library/software/expression
  (:nicknames :sel/software/expression :sel/sw/expression)
  (:use :gt/full
        :metabang-bind
        :software-evolution-library
        :software-evolution-library/software/sexp)
  (:import-from :arrow-macros :some->>) ; FIXME: Remove.
  (:export :expression
           :constant-fold
           :random-subtree
           :change-operator
           :change-constant
           :scope
           :mult-divide
           :add-subtract
           :subtract-add
           :add-subtract-tree
           :subtract-add-tree
           :add-subtract-scope
           :evaluate-expression
           :demote-binop-left
           :demote-binop-right
           :eval-error))
(in-package :software-evolution-library/software/expression)
(in-readtable :curry-compose-reader-macros)

(define-software expression (sexp)
  ((scope :initarg :scope :accessor scope :initform nil :copier :direct
          :documentation "List of in-scope variable names."))
  (:documentation "DOCFIXME"))

(defmethod operators ((obj expression))
  "DOCFIXME"
  (values '(:+ :- :* :/ :! :c :f :m :n :x :e)
          '(2  2  2  2  1  2  1  2  2  2  2)))

(defmethod operator-to-function ((obj expression) operator)
  "DOCFIXME

* OBJ DOCFIXME
* OPERATOR DOCFIXME
"
  (case operator
    (:+ #'+)
    (:- #'-)
    (:/ #'/)
    (:* #'*)
    (:! #'factorial)
    (:c #'choose)
    (:f #'floor)
    (:m #'mod)
    (:n #'min)
    (:x #'max)
    (:e #'expt)
    (otherwise
     (error (make-condition 'eval-error
              :text "Unknown operator"
              :expr operator)))))

(defmethod constants ((obj expression)
                      &optional (num-random 8) (random-max 256))
  "DOCFIXME

* OBJ DOCFIXME
* NUM-RANDOM DOCFIXME
* RANDOM-MAX DOCFIXME
"
  `(0 1 2 ,pi ,(exp 1.0d0) ,@(iter (for i below num-random)
                                   (collect (random random-max)))))


;;; Mutations
(defvar *expression-mutation-types*
  (cumulative-distribution
   (normalize-probabilities
    '((change-operator        . 25)     ; Out of 100
      (change-constant        . 25)
      ;; Demotion.
      (demote-binop-left      . 10)
      (demote-binop-right     . 10)
      ;; Semantics preserving.
      (mult-divide            . 1)
      (double-half            . 1)
      (subtract-add-tree      . 1)
      (add-subtract           . 0.5)
      (subtract-add           . 0.5)
      (add-subtract-tree      . 0.5)
      (add-subtract-scope     . 0.5)
      ;; Random subtree.
      (random-subtree         . 25))))
  "Cumulative distribution of normalized probabilities of weighted mutations.")

(defmethod pick-mutation-type ((obj expression))
  "DOCFIXME"
  (random-pick *expression-mutation-types*))

;;; Operator mutation.
(define-mutation change-operator (mutation)
  ((targeter :initform #'target-operator))
  (:documentation "DOCFIXME"))

(defmethod target-operator ((obj expression))
  "DOCFIXME"
  (let ((operators (operator-subtrees obj)))
    (when operators
      (list (random-elt operators)
            (random-elt (operators obj))))))

(defmethod operator-subtrees ((obj expression))
  "DOCFIXME"
  (filter-subtrees [{member _ (operators obj)} #'car] obj))

(defmethod apply-mutation ((obj expression) (mutation change-operator))
  "DOCFIXME
* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (when (targets mutation)
    (bind (((tree operator) (targets mutation)))
      (with-slots (genome) obj
        (rplaca (subtree genome tree) operator))))
  obj)

;;; Constant replacement
(define-mutation change-constant (mutation)
  ((targeter :initform (lambda (obj)
                         (list (random-elt (constant-subtrees obj))
                               (random-elt '(:double :halve :negate
                                             :increment :decrement
                                             :one :zero :negative-one))))))
  (:documentation "DOCFIXME"))

(defmethod constant-subtrees ((obj expression))
  "DOCFIXME"
  (filter-subtrees [#'numberp #'car] obj))

(defmethod apply-mutation ((obj expression) (mutation change-constant))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (bind (((tree transformation) (targets mutation)))
    (with-slots (genome) obj
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
  obj)

;;; Specialized mutations
(defgeneric pick-bad-binop-left (software)
  (:documentation
   "Pick a binary operation for demotion via `demote-binop-left'."))

(defmethod pick-bad-binop-left ((obj expression))
  "DOCFIXME"
  (flet ((binopp (subtree) (and (listp subtree) (= 3 (length subtree)))))
    (some->> (iter (for i below (size obj))
               (collect (list i (subtree (genome obj) i))))
             (remove-if-not (lambda-bind ((i subtree))
                              (declare (ignorable i))
                              (and (binopp subtree) (binopp (second subtree)))))
             (random-elt))))

(defgeneric pick-bad-binop-right (software)
  (:documentation
   "Pick a binary operation for demotion via `demote-binop-right'."))

(defmethod pick-bad-binop-right ((obj expression))
  "DOCFIXME"
  (flet ((binopp (subtree) (and (listp subtree) (= 3 (length subtree)))))
    (some->> (iter (for i below (size obj))
                   (collect (list i (subtree (genome obj) i))))
             (remove-if-not (lambda-bind ((i subtree))
                              (declare (ignorable i))
                              (and (binopp subtree) (binopp (third subtree)))))
             (random-elt))))

;; TODO: Combine with `demote-binop-right' implementation.
(define-mutation demote-binop-left (mutation)
  ((targeter :initform #'pick-bad-binop-left))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((obj expression) (mutation demote-binop-left))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (with-slots (targets) mutation
    (when targets
      (destructuring-bind (subtree-id (op (l-op l-left l-right) right)) targets
        (with-slots (genome) obj
          (setf (subtree genome (1+ subtree-id))
                (list l-op (list op l-left right) l-right))))))
  obj)

(define-mutation demote-binop-right (mutation)
  ((targeter :initform #'pick-bad-binop-right))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((obj expression) (mutation demote-binop-right))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (with-slots (targets) mutation
    (when targets
      (destructuring-bind (subtree-id (op left (r-op r-left r-right))) targets
        (with-slots (genome) obj
          (setf (subtree genome (1+ subtree-id))
                (list r-op r-left (list op r-right left)))))))
  obj)

;;; Semantics preserving mutations
(define-mutation mult-divide (mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((obj expression) (mutation mult-divide))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (let ((s (targets mutation)))
    (with-slots (genome) obj
      (setf (subtree genome s)
            `(:/ (:* ,(copy-tree (car (subtree genome s))) 2) 2))))
  obj)

(define-mutation add-subtract (mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((obj expression) (mutation add-subtract))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (let ((s (targets mutation)))
    (with-slots (genome) obj
      (setf (subtree genome s)
            `(:- (:+ ,(copy-tree (car (subtree genome s))) 1) 1))))
  obj)

(define-mutation subtract-add (mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((obj expression) (mutation subtract-add))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (let ((s (targets mutation)))
    (with-slots (genome) obj
      (setf (subtree genome s)
            `(:+ (:- ,(copy-tree (car (subtree genome s))) 1) 1))))
  obj)

(define-mutation add-subtract-tree (mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((obj expression) (mutation add-subtract-tree))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (let ((s (targets mutation))
        (r (pick-good obj)))
    (with-slots (genome) obj
      (setf (subtree genome s)
            `(:- (:+ ,(copy-tree (car (subtree genome s)))
                     ,(copy-tree (subtree genome r)))
                 ,(copy-tree (subtree genome r))))))
  obj)

(define-mutation add-subtract-scope (mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((obj expression) (mut add-subtract-scope))
  "DOCFIXME

* OBJ DOCFIXME
* MUT DOCFIXME
"
  (with-slots (genome scope) obj
    (let ((s (targets mut))
          (r (random-elt scope)))
      (setf (subtree genome s)
            `(:- (:+ ,(copy-tree (car (subtree genome s)))
                     ,r)
                 ,r))))
  obj)

(define-mutation subtract-add-tree (mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((obj expression) (mutation subtract-add-tree))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (let ((s (targets mutation))
        (r (pick-good obj)))
    (with-slots (genome) obj
      (setf (subtree genome s)
            `(:+ (:- ,(copy-tree (car (subtree genome s)))
                     ,(copy-tree (subtree genome r)))
                 ,(copy-tree (subtree genome r))))))
  obj)

(define-mutation double-half (mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((obj expression) (mutation double-half))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (let ((s (targets mutation)))
    (with-slots (genome) obj
      (setf (subtree genome s)
            `(:- (:+ ,(copy-tree (car (subtree genome s)))
                     ,(copy-tree (car (subtree genome s))))
                 ,(copy-tree (car (subtree genome s)))))))
  obj)

;;; Random subtree mutations.
(define-mutation random-subtree (mutation)
  ((targeter :initform #'target-subtree))
  (:documentation "DOCFIXME"))

(defmethod target-subtree ((obj expression))
  "DOCFIXME"
  (let ((operators (all-subtrees obj)))
    (when operators
      (list (random-elt operators)
            (random-elt (operators obj))))))

(defmethod random-subtree ((obj expression) &optional (depth 3))
  (assert (>= depth 0))
  (if (zerop depth)
      (random-elt (if (zerop (random 2))
                      (constants obj)
                      (scope obj)))
      (bind (((op arity) (random-elt
                          (apply #'mapcar #'list
                                 (multiple-value-list (operators obj))))))
        (cons op (iter (for i below arity)
                       (collect (random-subtree obj (random depth))))))))

(defmethod all-subtrees ((obj expression))
  "DOCFIXME"
  (filter-subtrees {constantly t} obj))

(defmethod apply-mutation ((obj expression) (mutation random-subtree))
  "DOCFIXME

* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (when (targets mutation)
    (bind (((tree operator) (targets mutation)))
      (with-slots (genome) obj
        (rplaca (subtree genome tree) operator))))
  obj)

(defmethod constant-fold ((obj expression))
  "DOCFIXME"
  (labels ((fold (expr)
             (if (and (listp expr) (keywordp (car expr)))
                 (let ((args (mapcar #'fold (cdr expr))))
                   (if (every #'numberp args)
                       (apply (operator-to-function obj (car expr)) args)
                       (cons (car expr) args)))
                 expr)))
    (setf (genome obj) (fold (genome obj)))
    obj))


;;;; Evaluation
(define-condition eval-error (error)
  ((text :initarg :text :initform nil :reader text)
   (expr :initarg :expr :initform nil :reader expr))
  (:report (lambda (condition stream)
             (format stream "Eval error ~a on ~a"
                     (text condition) (expr condition))))
  (:documentation "DOCFIXME"))

(defun choose (set subset)
  "Number of ways to choose SUBSET elements from SET."
  (declare (optimize speed))
  (without-compiler-notes
    (/ (factorial set)
       (* (factorial subset) (factorial (- set subset))))))

(defgeneric evaluate-expression (object environment &optional expression)
  (:documentation "Evaluate OBJECT's expression in ENVIRONMENT."))

(defmethod evaluate-expression ((obj expression) free-vars
                                &optional (expression (genome obj)))
  (cond
    ((listp expression)
     (apply (operator-to-function obj (car expression))
            (mapcar {evaluate-expression obj free-vars} (cdr expression))))
    ((numberp expression) expression)
    ((symbolp expression) (or (aget expression free-vars)
                              (error (make-condition 'eval-error
                                       :text "Undefined variable:"
                                       :expr expression))))
    (t (error (make-condition 'eval-error
                :text "Unrecognized expression:"
                :expr expression)))))

(defun expression-unbound-vars (expression)
  "DOCFIXME"
  (cond
    ((listp expression)
     (remove-duplicates (apply #'append
                               (mapcar #'expression-unbound-vars
                                       (cdr expression)))))
    ((symbolp expression) (list expression))
    (t nil)))
