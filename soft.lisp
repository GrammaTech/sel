;;; soft.lisp --- general representation of an instance of software

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

;;; Code:
(in-package :soft-ev)


;;; Software Object
(defclass soft ()
  ((genome  :initarg :genome  :accessor genome      :initform nil)
   (fitness :initarg :fitness :accessor raw-fitness :initform nil)
   (history :initarg :history :accessor history     :initform nil)))

(defgeneric copy (soft)
  (:documentation "Return a copy of the software."))

(defgeneric fitness (soft)
  (:documentation "Return the fitness of the software."))

(defmethod (setf fitness) (new (soft soft))
  (setf (raw-fitness soft) new))

(defgeneric from (soft stream)
  (:documentation "Read a software object from a file."))

(defgeneric to (soft stream)
  (:documentation "Write a software object to a file."))

(defgeneric executable (soft path)
  (:documentation "Generate an executable from a software object."))

(defgeneric insert (soft)
  (:documentation "Duplicate and insert an element of the genome of SOFT"))

(defgeneric cut (soft)
  (:documentation "Delete an element of the genome of SOFT."))

(defgeneric swap (soft)
  (:documentation "Swap two elements of the genome of SOFT."))

(defgeneric crossover (soft-a soft-b)
  (:documentation "Crossover between the genomes of SOFT-A and SOFT-B."))

(defmethod copy ((soft soft))
  (let ((new (make-instance (type-of soft))))
    (setf (genome new) (genome soft))
    (setf (history new) (history soft))
    (setf (fitness new) (fitness soft))
    new))

(defmethod fitness ((soft soft))
  (or (raw-fitness soft)
      (setf (raw-fitness soft) (evaluate soft))))

(defmethod insert ((soft soft))
  (multiple-value-bind (genome place)
      (insert (genome soft))
    (setf (genome soft) (genome-average-keys genome place))
    place))

(defmethod insert :around ((soft soft))
  (let ((place (call-next-method)))
    (push (cons :insert place) (history soft))
    (setf (fitness soft) nil)
    soft))

(defmethod cut ((soft soft))
  (multiple-value-bind (genome place)
      (cut (genome soft))
    (setf (genome soft) genome)
    place))

(defmethod cut :around ((soft soft))
  (let ((place (call-next-method)))
    (push (cons :cut place) (history soft))
    (setf (fitness soft) nil)
    soft))

(defmethod swap ((soft soft))
  (multiple-value-bind (genome places)
      (swap (genome soft))
    (setf (genome soft)
          (reduce (lambda (g p) (genome-average-keys g p))
                  places :initial-value genome))
    places))

(defmethod swap :around ((soft soft))
  (let ((places (call-next-method)))
    (push (cons :swap places) (history soft))
    (setf (fitness soft) nil)
    soft))

(defmethod crossover ((a soft) (b soft))
  (let ((new (make-instance (type-of a))))
    (multiple-value-bind (genome place)
        (crossover (genome a) (genome b))
      (setf (genome new) genome)
      (values new place))))

(defmethod crossover :around ((a soft) (b soft))
  (multiple-value-bind (new place) (call-next-method)
    (setf (fitness new) nil)
    (setf (history new) (list (cons :crossover place)
                              (cons (history a) (history b))))
    new))

(defvar *genome-averaging-keys* nil
  "Keys whose value should be averaged with neighbors after genome operations.")

(defmethod genome-average-keys ((genome list) place)
  (let ((above (unless (= place (- (length genome) 1))
                 (nth (+ place 1) genome)))
        (below (unless (= place 0)
                 (nth (- place 1) genome)))
        (middle (nth place genome)))
    (dolist (key *genome-averaging-keys*)
      (setf (cdr (assoc key (nth place genome)))
            (/ (apply #'+ (mapcar (lambda (el) (cdr (assoc key el)))
                                  (list above below middle)))
               3)))
    genome))

(defmethod insert ((genome list))
  (let* ((size (length genome))
         (dup-place (random size))
         (ins-place (random (+ 1 size))))
    (values (cond
              ((> dup-place ins-place)
               (append (subseq genome 0 ins-place)
                       (list (nth dup-place genome))
                       (subseq genome ins-place dup-place)
                       (subseq genome dup-place)))
              ((> ins-place dup-place)
               (append (subseq genome 0 dup-place)
                       (subseq genome dup-place ins-place)
                       (list (nth dup-place genome))
                       (subseq genome ins-place)))
              (:otherwise genome))
            ins-place)))

(defmethod cut ((genome list))
  (let ((place (random (length genome))))
    (values (append (subseq genome 0 place)
                    (subseq genome (+ 1 place)))
            place)))

(defmethod swap ((genome list))
  (let* ((size (length genome))
         (a (random size))
         (b (random size))
         (temp (nth a genome)))
    (setf (nth a genome) (nth b genome))
    (setf (nth b genome) temp)
    (values genome (list a b))))

(defmethod crossover ((a list) (b list))
  (let ((point (random (min (length a) (length b)))))
    (append (subseq a 0 point) (subseq b point))))
