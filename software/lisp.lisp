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

(defmacro set-subtree (tree index new)
  "Feels like some heuristics here (why `rplaca' instead of `rplacd')."
  `(if (zerop ,index)
       (setf ,tree ,new)
       (rplaca (subtree ,tree (1- ,index)) ,new)))

(defsetf subtree set-subtree)


;;; Lisp software object
(defclass lisp (simple) ())

(defmethod from-file ((lisp lisp) file)
  (with-open-file (in file)
    (setf (genome lisp)
          (loop :for form = (read in nil :eof)
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

(defmethod mutate ((lisp lisp))
  (unless (> (size lisp) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj lisp)))
  (let ((op (case (random-elt '(cut insert swap))
              (cut    `(:cut    ,(pick-bad lisp)))
              (insert `(:insert ,(pick-bad lisp) ,(pick-good lisp)))
              (swap   `(:swap   ,(pick-bad lisp) ,(pick-good lisp))))))
    (apply-mutation lisp op)
    (values lisp op)))

(defmethod apply-mutation ((lisp lisp) mutation)
  (let ((op (first mutation))
        (s1 (second mutation))
        (s2 (third mutation)))
    (with-slots (genome) lisp
      (case op
        (:cut    (setf (subtree genome s1)
                       (copy-tree (cdr (subtree genome s1)))))
        (:insert (setf (cdr (subtree genome s1))
                       (cons (copy-tree (subtree genome s1))
                             (copy-tree (cdr (subtree genome s1))))))
        (:swap (let ((left  (copy-tree (subtree genome s1)))
                     (right (copy-tree (subtree genome s2))))
                 (setf (subtree genome s1) right)
                 (setf (subtree genome s2) left)))))))

(defmethod crossover ((a lisp) (b lisp))
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((points (sort (loop :for i :below 2 :collect (random range)) #'<))
              (new (copy a)))
          (setf (subtree (genome new) (first points))
                (copy-tree (subtree (genome b) (second points))))
          (values new points))
        (values (copy a) nil))))
