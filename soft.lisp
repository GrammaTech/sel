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
  ((exe     :initarg :exe     :accessor raw-exe     :initform nil)
   (genome  :initarg :genome  :accessor genome      :initform nil)
   (fitness :initarg :fitness :accessor raw-fitness :initform nil)
   (history :initarg :history :accessor history     :initform nil)))

(defgeneric copy (soft)
  (:documentation "Return a copy of the software."))

(defgeneric fitness (soft)
  (:documentation "Return the fitness of the software. (caching)"))

(defmethod (setf fitness) (new (soft soft))
  (setf (raw-fitness soft) new))

(defmethod fitness :around ((soft soft))
  (or (raw-fitness soft) (setf (fitness soft) (call-next-method))))

(defgeneric exe (soft &optional place)
  (:documentation
   "Return the path to an executable of the software. (caching)"))

(defmethod (setf exe) (new (soft soft))
  (setf (raw-exe soft) new))

(defmethod exe :around ((soft soft) &optional place)
  (declare (ignorable place))
  (or (raw-exe soft) (setf (exe soft) (or (call-next-method) :failed))))

(defgeneric delete-exe (soft)
  (:documentation
   "Delete any external executables associated with the software."))

(defmethod delete-exe ((soft soft))
  (when (raw-exe soft)
    (when (and (not (eq :failed (raw-exe soft))) (probe-file (exe soft)))
      (delete-file (exe soft)))
    (setf (exe soft) nil)))

(defgeneric from (soft stream)
  (:documentation "Read a software object from a file."))

(defgeneric to (soft stream)
  (:documentation "Write a software object to a file."))

(defgeneric from-bytes (bytes)
  (:documentation "Read a software object from a byte array."))

(defgeneric to-bytes (soft)
  (:documentation "Write a software object to a byte array."))

(defgeneric good-ind (soft)
  (:documentation "Return a random \"good\" index in the genome."))

(defgeneric bad-ind (soft)
  (:documentation "Return a random \"bad\" index in the genome."))

(defgeneric good-place (soft)
  (:documentation
   "Return a random \"good\" place (between indices) in the genome."))

(defgeneric bad-place (soft)
  (:documentation
   "Return a random \"bad\" place (between indices) in the genome."))

(defgeneric insert (soft)
  (:documentation "Duplicate and insert an element of the genome of SOFT"))

(defgeneric cut (soft)
  (:documentation "Delete an element of the genome of SOFT."))

(defgeneric swap (soft)
  (:documentation "Swap two elements of the genome of SOFT."))

(defgeneric crossover (soft-a soft-b)
  (:documentation "Crossover between the genomes of SOFT-A and SOFT-B."))


;;; generic methods defined for software
(defmethod copy ((soft soft))
  (make-instance (type-of soft)
    :genome (genome soft)
    :history (history soft)
    :fitness (fitness soft)))

(defmethod from-bytes ((bytes vector))
  (let ((tmp (temp-file-name)))
    (with-open-file (out tmp :direction :output :element-type '(unsigned-byte 8))
      (dotimes (n (length bytes))
        (write-byte (aref bytes n) out)))
    (prog1 (restore tmp)
      (delete-file tmp))))

(defmethod to-bytes ((soft soft))
  (let ((tmp (temp-file-name))
        (bytes (make-array '(0)
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0 :adjustable t)))
    (store soft tmp)
    (with-open-file (in tmp :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte in  nil)
         while byte do (vector-push-extend byte bytes)))
    (delete-file tmp)
    bytes))

(defmethod fitness ((soft soft))
  (evaluate soft))

(defmethod good-ind ((soft soft))
  (good-ind (genome soft)))

(defmethod bad-ind ((soft soft))
  (bad-ind (genome soft)))

(defmethod good-place ((soft soft))
  (good-place (genome soft)))

(defmethod bad-place ((soft soft))
  (bad-place (genome soft)))

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


;;; generic methods defined for lists
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

(defun weighted-pick (weights &aux (counter 0))
  (let* ((cumulative (reverse (reduce (lambda (acc el)
                                        (incf counter el)
                                        (cons counter acc))
                                      weights :initial-value nil)))
         (point (random (float counter))))
    (loop for weight in cumulative as i from 0
       if (> weight point) do (return i))))

(defun weighted-ind (list key)
  (weighted-pick (mapcar key list)))

(defun weighted-place (list key &aux (last 0))
  (weighted-pick
   (mapcar (lambda (el) (prog1 (/ ( + el last) 2) (setf last el)))
           (append (mapcar key list) (list 0)))))

(defun random-ind (genome)
  (random (length genome)))

(defmethod good-ind ((genome list))
  (random-ind genome))

(defmethod bad-ind ((genome list))
  (random-ind genome))

(defun random-place (genome)
  (random (+ 1 (length genome))))

(defmethod good-place ((genome list))
  (random-place genome))

(defmethod bad-place ((genome list))
  (random-place genome))

(defmethod insert ((genome list))
  (let ((dup-place (good-ind genome))
        (ins-place (bad-place genome)))
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
  (let ((ind (bad-ind genome)))
    (values (append (subseq genome 0 ind)
                    (subseq genome (+ 1 ind)))
            ind)))

(defmethod swap ((genome list))
  (let* ((a (good-ind genome))
         (b (good-ind genome))
         (temp (nth a genome)))
    (setf (nth a genome) (nth b genome))
    (setf (nth b genome) temp)
    (values genome (list a b))))

(defmethod crossover ((a list) (b list))
  (let ((point (random (min (length a) (length b)))))
    (append (subseq a 0 point) (subseq b point))))
