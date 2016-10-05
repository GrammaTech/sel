;;; lisp.lisp --- software representation of lisp code

;; Copyright (C) 2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :software-evolution)


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


;;; Lisp software object
(define-software lisp (simple)
  ((genome :initarg :genome :accessor genome :initform nil :copier copy-tree)))

(defmethod from-file ((lisp lisp) file)
  (with-open-file (in file)
    (setf (genome lisp)
          (loop :for form := (read in nil :eof)
             :until (eq form :eof)
             :collect form)))
  lisp)

(declaim (inline genome-string))
(defmethod genome-string ((lisp lisp) &optional stream)
  (format stream "~&~{~S~^~%~}~%" (genome lisp)))

(defmethod to-file ((lisp lisp) path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (genome-string lisp out)))

(defmethod size ((lisp lisp)) (tree-size (genome lisp)))

(defmethod filter-subtrees (predicate (lisp lisp))
  (remove-if-not [predicate {subtree (genome lisp)}]
                 (iter (for i below (size lisp)) (collect i))))


;;; Mutations
(defun pick-bad-good-lisp (lisp)
  (list (pick-bad lisp) (pick-good lisp)))

(define-mutation lisp-cut (mutation)
  ((targeter :initform #'pick-bad)))

(define-mutation lisp-replace (mutation)
  ((targeter :initform #'pick-bad-good-lisp)))

(define-mutation lisp-swap (mutation)
  ((targeter :initform #'pick-bad-good-lisp)))

(defvar *lisp-mutation-types*
  ;; TODO: Fix `lisp-cut' before adding back to this list.
  '(lisp-replace lisp-swap))

(defmethod pick-mutation-type ((obj lisp))
  (random-elt *lisp-mutation-types*))

(defmethod mutate ((lisp lisp))
  (unless (> (size lisp) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj lisp)))
  (let ((mutation (make-instance (pick-mutation-type lisp)
                                 :object lisp)))
    (apply-mutation lisp mutation)
    (values lisp mutation)))

(defmethod apply-mutation ((lisp lisp) (mutation lisp-cut))
  ;; TODO: Fix.
  (with-slots (genome) lisp
    (let* ((s1 (targets mutation))
           (st (subtree genome s1)))
      (let ((prev (find-subtree-if [{eq st} #'cdr] genome)))
        (if prev
            ;; Middle of a subtree: snap cdr to remove
            (setf (cdr prev) (cdr st))
            ;; Beginning of a subtree.
            (setf (subtree genome s1)
                  (copy-tree (cdr (subtree genome s1))))))))
  lisp)

(defmethod apply-mutation ((lisp lisp) (mutation lisp-replace))
  (bind (((s1 s2) (targets mutation)))
    (with-slots (genome) lisp
      (setf (subtree genome s1)
            (copy-tree (car (subtree genome s2))))))
  lisp)

(defmethod apply-mutation ((lisp lisp) (mutation lisp-swap))
  (bind (((s1 s2) (targets mutation)))
    (let ((s1 (max s1 s2))
          (s2 (min s1 s2)))
      (with-slots (genome) lisp
        (let ((left  (car (subtree genome s1)))
              (right (car (subtree genome s2))))
          (setf (subtree genome s1) (copy-tree right))
          (setf (subtree genome s2) (copy-tree left))))))
  lisp)

(defmethod crossover ((a lisp) (b lisp))
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((points (sort (loop :for i :below 2 :collect (random range)) #'<))
              (new (copy a)))
          (setf (subtree (genome new) (first points))
                (copy-tree (subtree (genome b) (second points))))
          (values new points))
        (values (copy a) nil))))
