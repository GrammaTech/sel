;;; simple.lisp --- simple software rep. to manipulate lines of code

;; Copyright (C) 2013  Eric Schulte

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
(in-package :software-evolution)


;;; simple software objects
(defclass simple (software)
  ((genome :initarg :genome :accessor genome :initform nil)))

(declaim (inline lines))
(defmethod lines ((simple simple))
  (remove nil (mapcar {aget :line} (genome simple))))

(declaim (inline genome-string))
(defmethod genome-string ((simple simple))
  (format nil "~{~a~%~}" (lines simple)))

(defun file-to-simple-genome-list (filespec)
  (with-open-file (in filespec)
    (loop :for line = (read-line in nil) :while line
       :collect (list (cons :line line)))))

(defmethod from-file ((simple simple) path)
  (setf (genome simple) (file-to-simple-genome-list path))
  simple)

(defun common-subseq (paths)
  (coerce (loop :for i :below (apply #'min (mapcar #'length paths))
             :while (every [{equal (aref (car paths) i)} {aref _ i}] paths)
             :collect (aref (car paths) i))
          'string))

(defmethod from-file ((simple simple) (paths list))
  (let ((base (common-subseq paths)))
    (setf (genome simple)
          (mapcan (lambda (path)
                    (cons (list (cons :path (subseq path (length base))))
                          (file-to-simple-genome-list path)))
                  paths)))
  simple)

(defmethod to-file ((simple simple) file)
  ;; handle multi-file individuals differently
  (if (assoc :path (car (genome simple)))
      ;; if multi-file, then assume FILE is a directory path
      (let ((base (unless (equal #\/ (aref file (1- (length file))))
                    (concatenate 'string file "/")))
            path lines paths)
        (flet ((flush ()
                 (prog1 (push (string-to-file
                               (format nil "~{~a~%~}" (nreverse lines))
                               (concatenate 'string base path)) paths)
                   (setf lines nil path nil))))
          (loop :for el :in (genome simple) :do
             (if (assoc :path el)
                 (progn
                   ;; write accumulated lines to path
                   (when (and lines path) (flush))
                   (setf path (cdr (assoc :path el))))
                 (push (cdr (assoc :line el)) lines)))
          (flush)))
      ;; if single-file, then assume FILE is a file path
      (string-to-file (genome-string simple) file)))

(defmethod mutate ((simple simple))
  "Randomly mutate SIMPLE."
  (unless (> (size simple) 0) (error 'mutate :text "No valid IDs" :obj simple))
  (setf (fitness simple) nil)
  (let ((mut (case (random-elt '(cut insert swap))
               (cut    `(:cut    ,(pick-bad simple)))
               (insert `(:insert ,(pick-bad simple) ,(pick-good simple)))
               (swap   `(:swap   ,(pick-bad simple) ,(pick-good simple))))))
    (push mut (edits simple))
    (apply-mutation simple mut))
  simple)

(defmethod apply-mutation ((simple simple) mutation)
  (let ((op (first mutation))
        (s1 (second mutation))
        (s2 (third mutation)))
    (with-slots (genome) simple
      (setf genome (case op
                     (:cut (append (subseq genome 0 s1)
                                   (subseq genome (1+ s1))))
                     (:insert (append (subseq genome 0 s1)
                                      (list (nth s2 genome))
                                      (subseq genome s1)))
                     (:swap (let ((tmp (nth s1 genome)))
                              (setf (nth s1 genome) (nth s2 genome))
                              (setf (nth s2 genome) tmp))
                            genome))))))


;;; Crossover
(defmethod crossover ((a simple) (b simple)) (two-point-crossover a b))

#|
(defun align-for-crossover (a b &key (test equal) key)
  "Return two offsets which align genomes A and B for maximum similarity.
TEST may be used to test for similarity and should return a boolean (number?)."
  (values 0 0))
|#

(defmethod two-point-crossover ((a simple) (b simple))
  ;; Two point crossover
  (let* ((range (min (size a) (size b)))
         (points (sort (loop :for i :below 2 :collect (random range)) #'<))
         (new (copy a)))
    (setf (genome new)
          (copy-tree (append (subseq (genome b) 0 (first points))
                             (subseq (genome a) (first points) (second points))
                             (subseq (genome b) (second points)))))
    (values new points)))

(defmethod one-point-crossover ((a simple) (b simple))
  (let* ((range (min (size a) (size b)))
         (point (random range))
         (new (copy a)))
    (setf (genome new)
          (copy-tree (append (subseq (genome b) 0 point)
                             (subseq (genome a) point))))
    (values new point)))


;;; light software objects
;;
;; A refinement of the "simple" software representation, using a
;; simplified genome data structure which require fewer cons cells but
;; does not allow tagging of lines of code with information.
(defclass light (simple) ())

(defmethod lines ((light light)) (genome light))

(defmethod from-file ((light light) path)
  (setf (genome light) (split-sequence #\Newline (file-to-string path)))
  light)
