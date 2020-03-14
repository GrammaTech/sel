;;; sexp.lisp --- software representation of S-expressions
;;;
;;; A simple software object for the manipulation of S-expressions.
;;; This functionality is not used independently but is used by
;;; sel/sw/clang-expression and sel/sw/coq.
;;;
;;; @texi{sexp}
(defpackage :software-evolution-library/software/sexp
  (:nicknames :sel/software/sexp :sel/sw/sexp)
  (:use :gt/full
        :metabang-bind
        :software-evolution-library
        :software-evolution-library/software/simple)
  (:export :sexp
           :sexp-cut
           :sexp-replace
           :sexp-swap
           :subtree
           :tree-size
           :filter-subtrees
           :*sexp-mutation-types*))
(in-package :software-evolution-library/software/sexp)
(in-readtable :curry-compose-reader-macros)


;;; Tree actions
(defun tree-size (tree)
  "Return the number of cons cells in TREE."
  (if (and tree (consp tree))
      (+ 1 (tree-size (car tree)) (tree-size (cdr tree)))
      0))

(defun subtree (tree index)
  "Return the INDEX cons cell in TREE in depth first order."
  (if (zerop index)
      (values tree index)
      (flet ((descend (branch)
               (when (consp branch)
                 (multiple-value-bind (new-tree new-index)
                     (subtree branch (1- index))
                   (if (= new-index 0)
                       (return-from subtree (values new-tree new-index))
                       (setf index new-index))))))
        (descend (car tree))
        (descend (cdr tree))
        (values nil index))))

(defun find-subtree-if (predicate tree)
  "Return the subtree of TREE matching PREDICATE."
  (when (and tree (listp tree))
    (if (funcall predicate tree)
        tree
        (or (find-subtree-if predicate (car tree))
            (find-subtree-if predicate (cdr tree))))))

(defmacro set-subtree (tree index new)
  "Feels like some heuristics here (why `rplaca' instead of `rplacd')."
  `(if (zerop ,index)
       (setf ,tree ,new)
       (rplaca (subtree ,tree ,index) ,new)))

(defsetf subtree set-subtree)


;;; Sexp software object
(define-software sexp (simple)
  ((genome :initarg :genome :accessor genome :initform nil :copier copy-tree))
  (:documentation "Common Sexp source represented naturally as lists of code."))

(defmethod from-file ((sexp sexp) file)
  (with-open-file (in file)
    (setf (genome sexp)
          (loop :for form := (read in nil :eof)
             :until (eq form :eof)
             :collect form)))
  sexp)

(defmethod genome-string ((sexp sexp) &optional stream)
  (format stream "~&~{~S~^~%~}~%" (genome sexp)))

(defmethod to-file ((sexp sexp) path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (genome-string sexp out)))

(defmethod size ((sexp sexp))
  (tree-size (genome sexp)))


(defgeneric filter-subtrees (predicate tree)
  (:documentation "Return a list of subtrees of TREE satisfying PREDICATE.")
  (:method (predicate (sexp sexp))
    (remove-if-not [predicate {subtree (genome sexp)}]
                   (iter (for i below (size sexp)) (collect i)))))


;;; Mutations
(define-mutation sexp-cut (mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "Cut an element from a sexp object."))

(define-mutation sexp-replace (mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation
   "Replace an element from a sexp object with another element."))

(define-mutation sexp-swap (mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "Swap an element from a sexp object with another element."))

(defvar *sexp-mutation-types*
  ;; TODO: Fix `sexp-cut' before adding back to this list.
  '(sexp-replace sexp-swap)
  "List of mutations available for use against sexp software objects.")

(defmethod pick-mutation-type ((obj sexp))
  (random-elt *sexp-mutation-types*))

(defmethod mutate ((sexp sexp))
  (unless (> (size sexp) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj sexp)))
  (let ((mutation (make-instance (pick-mutation-type sexp)
                                 :object sexp)))
    (apply-mutation sexp mutation)
    (values sexp mutation)))

(defmethod apply-mutation ((sexp sexp) (mutation sexp-cut))
  ;; TODO: Fix.
  (with-slots (genome) sexp
    (let* ((s1 (targets mutation))
           (st (subtree genome s1)))
      (let ((prev (find-subtree-if [{eq st} #'cdr] genome)))
        (if prev
            ;; Middle of a subtree: snap cdr to remove
            (setf (cdr prev) (cdr st))
            ;; Beginning of a subtree.
            (setf (subtree genome s1)
                  (copy-tree (cdr (subtree genome s1))))))))
  sexp)

(defmethod apply-mutation ((sexp sexp) (mutation sexp-replace))
  (bind (((s1 s2) (targets mutation)))
    (with-slots (genome) sexp
      (setf (subtree genome s1)
            (copy-tree (car (subtree genome s2))))))
  sexp)

(defmethod apply-mutation ((sexp sexp) (mutation sexp-swap))
  (bind (((s1 s2) (targets mutation)))
    (let ((s1 (max s1 s2))
          (s2 (min s1 s2)))
      (with-slots (genome) sexp
        (let ((left  (car (subtree genome s1)))
              (right (car (subtree genome s2))))
          (setf (subtree genome s1) (copy-tree right))
          (setf (subtree genome s2) (copy-tree left))))))
  sexp)

(defmethod crossover ((a sexp) (b sexp))
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((points (sort (loop :for i :below 2 :collect (random range)) #'<))
              (new (copy a)))
          (setf (subtree (genome new) (first points))
                (copy-tree (subtree (genome b) (second points))))
          (values new points))
        (values (copy a) nil))))
