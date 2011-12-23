;;; genomes.lisp --- generic methods for genome manipulation

;; Copyright (C) 2011  Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Each genome must have the following properties
;; - every element of the genome must be either raw data or a
;;   subgenome of the same type
;; - it must be indexable
;; - raw data consists of an alist

;;; Code:
(defgeneric inds (genome)
  (:documentation "Return a list of the indexes of GENOME."))

(defgeneric ind (genome ind)
  (:documentation "Return the element located at IND in GENOME."))

(defgeneric (setf ind) (genome ind new)
  (:documentation "Set the element located at IND in GENOME to NEW."))


;;; list genomes
(defmethod inds ((genome list))
  (loop for element in genome as i from 0 collect i))

(defmethod ind ((genome list) ind)
  (nth ind genome))

(defmethod (setf ind) (new (genome list) ind)
  (setf (nth ind genome) new))


;;; tree genomes
(defstruct (tree (:copier tree-copier))
  (data nil)
  (branches nil))

(defun map-tree (type fun tree)
  (let ((first (funcall fun tree))
        (rest (mapcar (lambda (branch) (map-tree type fun branch))
                      (tree-branches tree))))
    (case type
      (tree (make-tree :data first :branches rest))
      (list (if rest (cons first rest) first)))))

(defmethod inds ((genome tree) &aux (counter -1))
  (map-tree 'list (lambda (subtree) (incf counter)) genome))

(defmethod ind ((genome tree) index &aux (counter -1) result)
  (map-tree 'tree (lambda (current)
                    (when (= (incf counter) index)
                      (setq result current))) genome)
  result)

(defun accessors (tree)
  (cons 'it
        (let ((ind -1))
          (mapcan (lambda (branch)
                    (incf ind)
                    (mapcar (lambda (ac) `(nth ,ind (tree-branches ,ac)))
                            (accessors branch)))
                  (tree-branches tree)))))

(defmethod (setf ind) (new (genome tree) ind)
  (if (= ind 0)
      (progn
        (setf (tree-data genome) (tree-data new))
        (setf (tree-branches genome) (tree-branches new)))
      (let ((ac (nth ind (accessors genome))))
        (eval `((lambda (it) (setf ,ac ,new)) ,genome)))))

(setf (ind *tree* 3) (make-tree :data 2))
(identity *tree*)
