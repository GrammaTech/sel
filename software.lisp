;;; software.lisp --- general representation of an instance of software

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
(in-package :software-evolution)


;;; Genome Object
(defgeneric inds (genome)
  (:documentation "Return a list of the indexes of GENOME."))

(defgeneric ind (genome ind)
  (:documentation "Return the element located at IND in GENOME."))

(defgeneric (setf ind) (new genome ind)
  (:documentation "Set the element located at IND in GENOME to NEW."))

(defgeneric places (genome)
  (:documentation "Returns a list of the places in GENOME.  Places can
  be thought of as the slots /between/ the indices."))

(defgeneric place (genome place)
  (:documentation "Return a list of the neighbors of PLACE in GENOME."))

(defgeneric (setf place) (new genome place)
  (:documentation "Insert NEW into GENOME at PLACE."))


;;; Vector Genomes
(defmethod inds ((genome vector))
  (loop :for i :from 0 :to (1- (length genome)) collect i))

(defmethod ind ((genome vector) ind)
  (aref genome ind))

(defmethod (setf ind) (new (genome vector) ind)
  (setf (aref genome ind) new))


;;; Tree Genomes
(defstruct (tree (:copier tree-copier))
  (data nil)
  (branches nil))

(defun to-tree (item)
  (if (consp item)
      (make-tree
       :data (car item)
       :branches (mapcar #'to-tree (cdr item)))
      (make-tree :data item)))

(defun to-list (tree)
  (if (tree-branches tree)
      (cons (tree-data tree)
            (mapcar #'to-list (tree-branches tree)))
      (tree-data tree)))

(defun map-tree (type fun tree)
  (let ((first (funcall fun tree))
        (rest (mapcar (lambda (branch) (map-tree type fun branch))
                      (tree-branches tree))))
    (case type
      (tree (make-tree :data first :branches rest))
      (list (if rest (cons first rest) first)))))

(defun accessors (tree &aux (ind -1))
  "Return a list of accessors to subtrees in BFS order."
  (cons 'it
        (mapcan (lambda (branch)
                  (incf ind)
                  (mapcar (lambda (ac) `(nth ,ind (tree-branches ,ac)))
                          (accessors branch)))
                (tree-branches tree))))

(defmethod inds ((genome tree) &aux (counter -1) inds)
  (map-tree 'list (lambda (_) (declare (ignorable _))
                     (push (incf counter) inds))
            genome)
  (reverse inds))

(defmethod ind ((genome tree) index &aux (counter -1) result)
  (map-tree 'tree (lambda (current)
                    (when (= (incf counter) index)
                      (setq result current))) genome)
  result)

(defmethod (setf ind) (new (genome tree) index)
  (if (= index 0)
      (progn
        (setf (tree-data genome) (tree-data new))
        (setf (tree-branches genome) (tree-branches new)))
      (let ((ac (nth index (accessors genome))))
        (eval `((lambda (it) (setf ,ac ,new)) ,genome)))))


;;; Cons-cell Genomes
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
      (setf
       (ind (case (car index) (:a (car genome)) (:d (cdr genome))) (cdr index))
       new)
      (case (car index) (:a (rplaca genome new)) (:d (rplacd genome new)))))

(defun del-ind (genome index)
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


;;; Software Object
(defclass software ()    ; TODO: REMOVE the `exe' member because it is not
                     ; general enough and only really applies to
                     ; compiled software objects.  The to and from
                     ; file methods are support enough.
  ((exe     :initarg :exe     :accessor raw-exe     :initform nil)
   (genome  :initarg :genome  :accessor genome      :initform nil)
   (fitness :initarg :fitness :accessor raw-fitness :initform nil)
   (history :initarg :history :accessor history     :initform nil)))

(defgeneric copy (software)
  (:documentation "Return a copy of the software."))

(defgeneric fitness (software)
  (:documentation "Return the fitness of the software. (caching)"))

(defmethod (setf fitness) (new (software software))
  (setf (raw-fitness software) new))

(defmethod fitness :around ((software software))
  (or (raw-fitness software) (setf (fitness software) (call-next-method))))

(defgeneric exe (software &optional place)
  (:documentation
   "Return the path to an executable of the software. (caching)"))

(defmethod (setf exe) (new (software software))
  (setf (raw-exe software) new))

(defmethod exe :around ((software software) &optional place)
  (declare (ignorable place))
  (or (raw-exe software) (setf (exe software) (or (call-next-method) :failed))))

(defgeneric delete-exe (software)
  (:documentation
   "Delete any external executables associated with the software."))

(defmethod delete-exe ((software software))
  (when (raw-exe software)
    (when (and (not (eq :failed (raw-exe software)))
               (probe-file (exe software)))
      (delete-file (exe software)))
    (setf (exe software) nil)))

(defgeneric from (software stream)
  (:documentation "Read a software object from a file."))

(defgeneric to (software stream)
  (:documentation "Write a software object to a file."))

(defgeneric from-bytes (bytes) ;; TODO: REMOVE
  (:documentation "Read a software object from a byte array."))

(defgeneric to-bytes (software) ;; TODO: REMOVE
  (:documentation "Write a software object to a byte array."))

(defgeneric random-ind (software)
  (:documentation "Return a random index in the genome."))

(defgeneric good-ind (software)
  (:documentation "Return a random \"good\" index in the genome."))

(defgeneric bad-ind (software)
  (:documentation "Return a random \"bad\" index in the genome."))

(defgeneric random-place (software)
  (:documentation "Return a random place in the genome."))

(defgeneric good-place (software)
  (:documentation
   "Return a random \"good\" place (between indices) in the genome."))

(defgeneric bad-place (software)
  (:documentation
   "Return a random \"bad\" place (between indices) in the genome."))

(defgeneric insert (software)
  (:documentation "Duplicate and insert an element of the genome of SOFT"))

(defgeneric cut (software)
  (:documentation "Delete an element of the genome of SOFT."))

(defgeneric swap (software)
  (:documentation "Swap two elements of the genome of SOFT."))

(defgeneric crossover (software-a software-b)
  (:documentation "Crossover between the genomes of SOFT-A and SOFT-B."))

(defvar *genome-averaging-keys* nil
  "Keys whose value should be averaged with neighbors after genome operations.")

(defgeneric genome-average-keys (genome place)
  (:documentation "Average the keys in *GENOME-AVERAGING-KEYS* around PLACE."))


;;; Software Methods
(defmethod copy ((software software))
  (make-instance (type-of software)
    :genome (genome software)
    :history (history software)
    :fitness (raw-fitness software)))

(defmethod from-bytes ((bytes vector))
  (let ((tmp (temp-file-name)))
    (with-open-file (out tmp :direction :output :element-type '(unsigned-byte 8))
      (dotimes (n (length bytes))
        (write-byte (aref bytes n) out)))
    (prog1 (restore tmp)
      (delete-file tmp))))

(defmethod to-bytes ((software software))
  (let ((tmp (temp-file-name))
        (bytes (make-array '(0)
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0 :adjustable t)))
    (store software tmp)
    (with-open-file (in tmp :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte in  nil)
         while byte do (vector-push-extend byte bytes)))
    (delete-file tmp)
    bytes))

(defmethod fitness ((software software))
  (evaluate software))

(defmethod random-ind (software)
  (random-elt (inds software)))

(defmethod random-ind ((software software))
  (random-ind (genome software)))

(defmethod good-ind (software)
  (random-ind software))

(defmethod good-ind ((software software))
  (good-ind (genome software)))

(defmethod bad-ind (software)
  (random-ind software))

(defmethod bad-ind ((software software))
  (bad-ind (genome software)))

(defmethod random-place ((software software))
  (random-place (genome software)))

(defmethod good-place (software)
  (random-place software))

(defmethod good-place ((software software))
  (random-place (genome software)))

(defmethod bad-place (software)
  (random-place software))

(defmethod bad-place ((software software))
  (random-place (genome software)))

(defmethod insert ((software software))
  (multiple-value-bind (genome place)
      (insert (genome software))
    (setf (genome software) (genome-average-keys genome place))
    place))

(defmethod insert :around ((software software))
  (let ((place (call-next-method)))
    (push (cons :insert place) (history software))
    (setf (fitness software) nil)
    software))

(defmethod cut ((software software))
  (multiple-value-bind (genome place)
      (cut (genome software))
    (setf (genome software) genome)
    place))

(defmethod cut :around ((software software))
  (let ((place (call-next-method)))
    (push (cons :cut place) (history software))
    (setf (fitness software) nil)
    software))

(defmethod swap ((software software))
  (multiple-value-bind (genome places)
      (swap (genome software))
    (setf (genome software)
          (reduce (lambda (g p) (genome-average-keys g p))
                  places :initial-value genome))
    places))

(defmethod swap :around ((software software))
  (let ((places (call-next-method)))
    (push (cons :swap places) (history software))
    (setf (fitness software) nil)
    software))

(defmethod crossover ((a software) (b software))
  (let ((new (make-instance (type-of a))))
    (multiple-value-bind (genome place)
        (crossover (genome a) (genome b))
      (setf (genome new) genome)
      (values new place))))

(defmethod crossover :around ((a software) (b software))
  (multiple-value-bind (new place) (call-next-method)
    (setf (fitness new) nil)
    (setf (history new) (list (cons :crossover place)
                              (cons (history a) (history b))))
    new))


;;; Vector Methods
(defmethod genome-average-keys ((genome vector) place)
  (let ((above (unless (= place (- (length genome) 1))
                 (aref genome (+ place 1))))
        (below (unless (= place 0)
                 (aref genome (- place 1))))
        (middle (aref genome place)))
    (dolist (key *genome-averaging-keys*)
      (let ((new (/ (apply #'+ (mapcar (lambda (el) (or (cdr (assoc key el)) 0))
                                       (list above below middle)))
                    3)))
        (if (assoc key (aref genome place))
            (setf (cdr (assoc key (aref genome place))) new)
            (push (cons key new) (aref genome place)))))
    genome))

(defun weighted-pick (weights &aux (counter 0))
  "Weighted select of an index into a list of weights."
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

(defmethod random-place ((genome vector))
  (random (+ 1 (length genome))))

(defmethod insert ((genome vector))
  (let ((dup-place (good-ind genome))
        (ins-place (bad-place genome)))
    (values (cond
              ((> dup-place ins-place)
               (concatenate 'vector
                 (subseq genome 0 ins-place)
                 (vector (aref genome dup-place))
                 (subseq genome ins-place dup-place)
                 (subseq genome dup-place)))
              ((> ins-place dup-place)
               (concatenate 'vector
                 (subseq genome 0 dup-place)
                 (subseq genome dup-place ins-place)
                 (vector (aref genome dup-place))
                 (subseq genome ins-place)))
              (:otherwise
               (concatenate 'vector
                 (subseq genome 0 dup-place)
                 (vector (aref genome dup-place))
                 (subseq genome dup-place))))
            ins-place)))

(defmethod cut ((genome vector))
  (let ((ind (bad-ind genome)))
    (values (concatenate 'vector
              (subseq genome 0 ind)
              (subseq genome (+ 1 ind)))
            ind)))

(defmethod swap ((genome vector))
  (let* ((a (good-ind genome))
         (b (good-ind genome))
         (temp (aref genome a)))
    (setf (aref genome a) (aref genome b))
    (setf (aref genome b) temp)
    (values genome (list a b))))

(defmethod crossover ((a vector) (b vector))
  (let ((point (random (min (length a) (length b)))))
    (values (concatenate 'vector (subseq a 0 point) (subseq b point))
            point)))


;;; Cons-cell Methods
(defmethod random-place ((genome list))
  (random-ind genome))

(defmethod genome-average-keys ((genome list) place)
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

(defmethod insert ((genome list))
  (let ((dup-ind (good-ind genome))
        (ins-place (good-place genome)))
    (setf (ind genome ins-place)
          (cons (ind genome ins-place)
                (ind genome dup-ind)))
    (values genome (list ins-place dup-ind))))

(defmethod cut ((genome list))
  (let ((del-ind (bad-ind genome)))
    (del-ind genome del-ind)
    (values genome del-ind)))

(defmethod swap ((genome list))
  (let* ((a (good-ind genome))
         (b (good-ind genome))
         (ordered (sort (list a b) #'< :key #'length))
         (tmp (ind genome (second ordered))))
    (setf (ind genome (second ordered)) (ind genome (first ordered)))
    (setf (ind genome (first ordered)) tmp)
    (values genome ordered)))

(defmethod crossover ((a list) (b list))
  (let* ((inds-a (inds a))
         (inds-b (inds b))
         (points-in-common (remove-if-not (lambda (it) (member it inds-a)) inds-b))
         (point (random-elt points-in-common))
         (new (copy-seq a)))
    (setf (ind new point) (ind b point))
    (values new point)))
