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
  ((genome  :initarg :genome  :accessor genome)
   (fitness :initarg :fitness :accessor fitness)
   (history :initarg :history :accessor history)))

(defgeneric copy (soft)
  (:documentation "Return a copy of the software."))

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


;;; Genome
(defvar *genome-averaging-keys* nil
  "Keys whose value should be averaged with neighbors after genome operations.")

(defmethod genome-average-keys ((genome list) place)
  (let ((above (unless (= ins-place (- size 1))
                 (nth (+ ins-place 1) genome)))
        (below (unless (= ins-place 0)
                 (nth (- ins-place 1) genome)))
        (middle (nth ins-place genome)))
    (dolist (key *genome-averaging-keys*)
      (setf (cdr (assoc key (nth ins-place result)))
            (/ (apply #'+ (mapcar (lambda (el) (cdr (assoc key el)))
                                  (list above below middle)))
               3)))))

(defmethod insert ((genome list))
  (let* ((size (length genome))
         (dup-place (random size))
         (ins-place (random (+ 1 size))))
    (cond
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
      (:otherwise genome))))

(defmethod cut ((genome list))
  (let ((place (random (length genome))))
    (append (subseq genome 0 place)
            (subseq genome (+ 1 place)))))

(defmethod swap ((genome list))
  (let* ((size (length genome))
         (a (random size))
         (b (random size))
         (temp (nth a genome)))
    (setf (nth a genome) (nth b genome))
    (setf (nth b genome) temp))
  genome)
