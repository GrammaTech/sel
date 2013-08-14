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
(defmethod (setf lines) (new (simple simple))
  (setf (genome simple) (mapcar [#'list {cons :line}] new)))

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
  (unless (> (size simple) 0) (error 'mutate :text "No valid IDs" :obj simple))
  (setf (fitness simple) nil)
  (apply-mutation simple
    (case (random-elt '(cut insert swap))
      (cut    `(:cut    ,(pick-bad simple)))
      (insert `(:insert ,(pick-bad simple) ,(pick-good simple)))
      (swap   `(:swap   ,(pick-bad simple) ,(pick-good simple)))))
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

(defmethod mcmc-step ((simple simple))
  (let ((point (random (size simple))))
    (with-slots (genome) simple
      (setf genome
            (if (zerop (random 2))
                ;; delete an element
                (append (subseq genome 0 point)
                        (subseq genome (1+ point)))
                ;; insert an element
                (append (subseq genome 0 point)
                        (list (random-elt *mcmc-fodder*))
                        (subseq genome point)))))))


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
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((points (sort (loop :for i :below 2 :collect (random range)) #'<))
              (new (copy a)))
          (setf (genome new)
                (copy-tree (append
                            (subseq (genome b) 0 (first points))
                            (subseq (genome a) (first points) (second points))
                            (subseq (genome b) (second points)))))
          (values new points))
        (values (copy a) nil))))

(defmethod one-point-crossover ((a simple) (b simple))
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((point (random range))
              (new (copy a)))
          (setf (genome new)
                (copy-tree (append (subseq (genome b) 0 point)
                                   (subseq (genome a) point))))
          (values new point))
        (values (copy a) nil))))


;;; light software objects
;;
;; A refinement of the "simple" software representation, using a
;; simplified genome data structure which require fewer cons cells but
;; does not allow tagging of lines of code with information.
(defclass light (simple) ())

(defmethod lines ((light light)) (genome light))
(defmethod (setf lines) (new (light light)) (setf (genome light) new))

(defmethod from-file ((light light) path)
  (setf (genome light) (split-sequence #\Newline (file-to-string path)))
  light)


;;; range software objects
;;
;; An software object which uses even less memory, but adds some
;; complexity to many genome manipulation methods.
;;
;; This class
;;
(defclass range (simple)
  ((reference :initarg :reference :initform nil))
  (:documentation
   "Alternative to SIMPLE software objects which should use less memory.
Instead of directly holding code in the GENOME, each GENOME is a list
of range references to an external REFERENCE code array."))

(defgeneric reference (software))
(defgeneric (setf reference) (software new)
  (:documentation "Set the value of REFERENCE to NEW, and update the GENOME."))

(defmethod reference ((range range)) (slot-value range 'reference))

(defmethod (setf reference) (new (range range))
  (declare (ignorable new))
  (assert (typep new 'vector) (new) "Reference must be a vector.")
  (setf (slot-value range 'reference) new)
  (setf (genome range) (list (cons 0 (1- (length new))))))

(defmethod from-file ((range range) path)
  (declare (ignorable path))
  (error "RANGE individuals may not be initialized directly from files.
First construct an array of lines of code from PATH and use this to
initialize the RANGE object."))

(defmethod copy ((range range))
  (with-slots (reference genome) range
    (make-instance (class-of range)
      :reference (reference range)
      :genome (copy-tree (genome range))
      :fitness (fitness range))))

(declaim (inline range-size))
(defun range-size (range) (1+ (- (cdr range) (car range))))

(defmethod size ((range range))
  (reduce #'+ (mapcar #'range-size (genome range))))

(defmethod lines ((range range))
  (mapcan (lambda-bind ((start . end))
            (mapcar {aref (reference range)}
                    (loop :for i :from start :to end :collect i)))
          (genome range)))

(defmethod (setf lines) (new (range range))
  (setf (reference range) (coerce new 'vector))
  (setf (genome range) (list (cons 0 (1- (length new))))))

(defun range-nth (index genome)
  "Return the reference index of the INDEX line specified in RANGE."
  (block nil (mapc (lambda (r)
                     (let ((size (range-size r)))
                       (if (> size index)
                           (return (+ (car r) index))
                           (decf index size))))
                   genome)
         nil))

(defun range-cut (genome place)
  "Delete PLACE from GENOME."
  (remove nil
    (mapcan (lambda (range)
              (let ((size (range-size range)))
                (if (and place (> size place))
                    (if (> size 1)
                        (let* ((start (car range))
                               (del (+ place start))
                               (end (cdr range)))
                          (prog1
                              (cond
                                ((= start del)
                                 (list (cons (1+ start) end)))
                                ((= del end)
                                 (list (cons start (1- end))))
                                (t
                                 (list (cons start (1- del))
                                       (cons (1+ del) end))))
                            (setf place nil)))
                        nil)
                    (prog1 (list range) (if place (decf place size))))))
            genome)))

(defun range-insert (genome to val)
  "Insert VAL before TO in GENOME."
  (mapcan (lambda (range)
            (let ((size (range-size range)))
              (if (and to (> size to))
                  (prog1 (let* ((start (car range))
                                (ins   (+ to start))
                                (end   (cdr range)))
                           (cond
                             ((= start ins) (list (cons val val) range))
                             ((= ins end)   (list range (cons val val)))
                             (t             (list (cons start (1- ins))
                                                  (cons val val)
                                                  (cons ins end)))))
                    (setf to nil))
                  (prog1 (list range) (if to (decf to size))))))
          genome))

(defun range-swap (genome s1 s2 val1 val2)
  "Set value of S1 to VAL2 and S2 to VAL1 in GENOME."
  (flet ((rep (r s1 val) ;; replace S1 with VAL
           (let* ((start (car r))
                  (ins   (+ s1 start))
                  (end   (cdr r)))
             (cond
               ((= start end ins) (list (cons val val)))
               ((= start ins)     (list (cons val val) (cons (1+ start) end)))
               ((= ins end)       (list (cons start (1- end)) (cons val val)))
               (t                 (list (cons start (1- ins))
                                        (cons val val)
                                        (cons (1+ ins) end)))))))
    (mapcan
     (lambda (range)
       (let ((size (range-size range)))
         (if (and s2 (> size s2))
             (prog1 (rep range s2 val1) (setf s2 nil))
             (prog1 (list range) (if s2 (decf s2 size))))))
     (mapcan
      (lambda (range)
        (let ((size (range-size range)))
          (if (and s1 (> size s1))
              (prog1 (rep range s1 val2) (setf s1 nil))
              (prog1 (list range) (if s1 (decf s1 size))))))
      genome))))

(defmethod apply-mutation ((range range) mutation)
  (let ((op (first mutation))
        (s1 (second mutation))
        (s2 (third mutation)))
    (with-slots (genome) range
      (setf genome (case op
                     (:cut    (range-cut    genome s1))
                     (:insert (range-insert genome s1 (range-nth s2 genome)))
                     (:swap   (range-swap   genome s1 s2
                                            (range-nth s1 genome)
                                            (range-nth s2 genome))))))))

(defun range-subseq (range start &optional end)
  (flet ((from (c range)
           (remove nil
             (mapcan (lambda (r)
                       (let ((size (range-size r)))
                         (cond
                           ((and c (> c size)) (prog1 nil (decf c size)))
                           ((and c (= c size)) (prog1 nil (setf c nil)))
                           ((and c (zerop c))  (prog1 (list r) (setf c nil)))
                           ((and c (< c size))
                            (prog1 (list (cons (+ (car r) c) (cdr r)))
                              (setf c nil)))
                           ((not c) (list r)))))
                     range)))
         (to (c range)
           (remove nil
             (mapcan (lambda (r)
                       (let ((size (range-size r)))
                         (cond
                           ((and c (> c size)) (prog1 (list r) (decf c size)))
                           ((and c (= c size)) (prog1 (list r) (setf c nil)))
                           ((and c (zerop c))  (prog1 nil (setf c nil)))
                           ((and c (< c size))
                            (prog1 (list (cons (car r) (+ (car r) (1- c))))
                              (setf c nil)))
                           ((not c) nil))))
                     range))))
    (if end (to (- end start) (from start range)) (from start range))))

(defmethod one-point-crossover ((a range) (b range))
  (assert (eq (reference a) (reference b)) (a b)
          "Can not crossover range objects with unequal references.")
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((point (random range))
              (new (copy a)))
          (setf (genome new)
                (copy-tree (append (range-subseq (genome a) 0 point)
                                   (range-subseq (genome b) point))))
          (values new point))
        (values (copy a) 0))))

(defmethod two-point-crossover ((a range) (b range))
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((points (sort (loop :for i :below 2 :collect (random range)) #'<))
              (new (copy a)))
          (setf (genome new)
                (copy-tree
                 (append
                  (range-subseq (genome b) 0 (first points))
                  (range-subseq (genome a) (first points) (second points))
                  (range-subseq (genome b) (second points)))))
          (values new points))
        (values (copy a) nil))))
