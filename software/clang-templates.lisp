;;; Mutation templates or clang objects

;; IP note: developed for BRASS.

(in-package :software-evolution)

(defclass clang-template-mutation (clang-mutation) ())

(defmethod initialize-instance :after ((mutation clang-template-mutation))
  (if (not (targets mutation))
      (setf (slot-value mutation 'targets)
            (pick-template-targets mutation (object mutation)))))

;; refine-condition: add an additional boolean clause to an if condition
(defclass refine-condition (clang-template-mutation) ())

(defmethod pick-template-targets ((mutation refine-condition) software)
  (list (pick-target-condition software)
        (random-elt '("&&" "||"))
        (pick-condition-expr software)))

(defmethod build-op ((mutation refine-condition) software)
  (bind (((target connector expr) (targets mutation))
         (target-ast (get-ast software target))
         (expr-ast (get-ast software expr)))
    `((:set (:stmt1 . ,target)
            (:value1 .
                     ((:src--text . ,(format nil "(~a) ~a (~a)"
                                             (aget :src--text target-ast)
                                             connector
                                             (aget :src--text expr-ast)))
                      (:unbound--vals .
                           ,(merge-lists (aget :unbound--vals target-ast)
                                         (aget :unbound--vals expr-ast)))
                      (:unbound--funs .
                           ,(merge-lists (aget :unbound--funs target-ast)
                                         (aget :unbound--funs expr-ast)))))))))

;; add-condition: wrap a statement in an if
(defclass add-condition (clang-mutation) ())

(defmethod pick-template-targets ((mutation add-condition) software)
  (list (aget :counter (random-elt (full-stmt-filter (asts software))))
        (pick-condition-expr software)))

(defmethod build-op ((mutation add-condition) software)
  (bind (((target expr) (targets mutation))
         (target-ast (get-ast software target))
         (expr-ast (get-ast software expr)))
    `((:set (:stmt1 . ,target)
            (:value1 .
                     ((:src--text . ,(format nil "if (~a) {~%~a~%}"
                                             (aget :src--text expr-ast)
                                             (aget :src--text target-ast)))
                      (:unbound--vals .
                           ,(merge-lists (aget :unbound--vals target-ast)
                                         (aget :unbound--vals expr-ast)))
                      (:unbound--funs .
                           ,(merge-lists (aget :unbound--funs target-ast)
                                         (aget :unbound--funs expr-ast)))))))))

;;; Helper functions
(defun merge-lists (a b)
  (remove-duplicates (append a b) :test #'equal))

;; TODO: error handling if pick fails
(defun pick-target-condition (software)
  "Pick a condition to target for refinement."
  (let ((if-stmts (with-class-filter "IfStmt" (asts software))))
    (aget :counter
          (random-elt (mapcar [#'first {get-immediate-children software}]
                              if-stmts)))))

(defun pick-condition-expr (software)
  "Pick an expression to use as a condition."
  ;; TODO: other sources of conditions (boolean operators, in-scope
  ;; variables, fodder?)
  (pick-target-condition software))
