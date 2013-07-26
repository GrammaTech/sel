;;; cons.lisp --- support for cons-cell genomes

;; Copyright (C) 2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :software-evolution)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defmethod inds ((genome cons))
  (unless (null genome)
    (flet ((follow (dir list)
             (mapcar (lambda (el) (cons dir el))
                     (if (consp list) (inds list) '(())))))
      (append '(()) (follow :a (car genome)) (follow :d (cdr genome))))))

(defmethod ind ((genome list) index)
  (flet ((get-at (list dir) (case dir (:a (car list)) (:d (cdr list)))))
    (if (cdr index)
        (ind (get-at genome (car index)) (cdr index))
        (get-at genome (car index)))))

(defmethod (setf ind) (new (genome list) index)
  (if (cdr index)
      (setf (ind (case (car index)
                   (:a (car genome))
                   (:d (cdr genome)))
                 (cdr index))
            new)
      (case (car index)
        (:a (rplaca genome new) new)
        (:d (rplacd genome (if (and (proper-list-p genome) new)
                               (cons new nil)
                               new)) new))))

(defmethod del-ind ((genome cons) index)
  (if (cddr index)
      (del-ind (case (car index) (:a (car genome)) (:d (cdr genome)))
               (cdr index))
      (case (car index)
        (:a (if (cdr index)
                (rplaca genome
                        (case (cadr index)
                          (:a (cdar genome))
                          (:d (caar genome))))
                (progn (rplaca genome (cadr genome))
                       (rplacd genome (cddr genome)))))
        (:d (rplacd genome
                    (case (cadr index)
                      (:a (cddr genome))
                      (:d (cadr genome))))))))


;;; Cons-cell Methods
(defmethod copy ((genome list))
  (copy-tree genome))

(defmethod size ((genome list))
  (if (consp genome)
      (+ (size (car genome)) (size (cdr genome)))
      1))

(defmethod average-keys ((genome list) place)
  (let ((inds (list (butlast place) place
                    (append place '(:a)) (append place '(:d)))))
    (dolist (key *genome-averaging-keys*)
      (let ((new (/ (apply #'+ (mapcar (lambda (el)
                                         (or (cdr (assoc key (ind genome el)))
                                             0))
                                       inds))
                    4)))
        (if (assoc key (ind genome place))
            (setf (cdr (assoc key (ind genome place))) new)
            (push (cons key new) (ind genome place)))))
    genome))

(defun ensure-proper-list (lisp)
  (unless (proper-list-p lisp) (setf (cdr lisp) (cons (cdr lisp) nil)))
  lisp)

(defmethod insert ((genome list) dup ins)
  (let ((new (copy-tree genome)))
    (setf (ind new ins)
          (cons (copy-tree (ind genome ins))
                (copy-tree (ind genome dup))))
    (values (ensure-proper-list new) (list ins dup))))

(defmethod cut ((genome list) del)
  (let ((new (copy-tree genome)))
    (del-ind new del)
    (values (ensure-proper-list new) del)))

(defmethod swap ((genome list) a b)
  (let ((ordered (sort (list a b) #'< :key #'length))
        (new (copy-tree genome)))
    (setf (ind new (second ordered)) (copy-tree (ind genome (first ordered))))
    (setf (ind new (first ordered))  (copy-tree (ind genome (second ordered))))
    (values (ensure-proper-list new) ordered)))

(defmethod crossover ((a list) (b list))
  (let* ((inds-a (inds a))
         (inds-b (inds b))
         (points-in-common (remove-if-not (lambda (it) (member it inds-a)) inds-b))
         (point (random-elt points-in-common))
         (new (copy-tree a)))
    (setf (ind new point) (ind b point))
    (values (ensure-proper-list new) point)))
