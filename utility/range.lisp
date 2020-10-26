(defpackage software-evolution-library/utility/range
  (:nicknames :sel/utility/range)
  (:use :gt)
  (:export
   :source-location
   :line
   :column
   :source-range
   :range
   :begin
   :end
   :source-<
   :source-<=
   :source->
   :source->=
   :contains
   :intersects
   :source-location->position
   :position->source-location))
(in-package :software-evolution-library/utility/range)


;;;; Source and binary locations and ranges.
;;;
;;; Classes for representing locations and ranges in sequences
;;; (typically text source code files or binary executable bytes).
;;; Functions for comparing locations and ranges, as well as
;;; determining "contains" and "intersects" relationships between
;;; locations and ranges.
;;;
;;; @texi{locations-and-ranges}
(defclass source-location ()
  ((line :initarg :line :accessor line :type fixnum)
   (column :initarg :column :accessor column :type fixnum)))

(defclass source-range ()
  ((begin :initarg :begin :accessor begin :type source-location)
   (end   :initarg :end   :accessor end   :type source-location)))

(defclass range ()
  ((begin :initarg :begin :accessor begin :type fixnum)
   (end   :initarg :end   :accessor end   :type fixnum)))

(defmethod print-object ((obj source-location) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (line obj) stream)
    (format stream ":")
    (prin1 (column obj) stream)))

(defmethod print-object ((obj source-range) stream)
  (flet ((p1-range (range)
           (prin1 (line range) stream)
           (format stream ":")
           (prin1 (column range) stream)))
    (print-unreadable-object (obj stream :type t)
      (p1-range (begin obj))
      (format stream " to ")
      (p1-range (end obj)))))

(defmethod print-object ((obj range) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (begin obj) stream)
    (format stream " to ")
    (prin1 (end obj) stream)))

(defgeneric source-< (a b)
  (:documentation "Return true if source location A comes strictly before B.")
  (:method ((a source-location) (b source-location))
    (or (< (line a) (line b))
        (and (= (line a) (line b))
             (< (column a) (column b))))))

(defgeneric source-<= (a b)
  (:documentation "Return true if source location A is equal to or comes
before B.")
  (:method ((a source-location) (b source-location))
    (or (< (line a) (line b))
        (and (= (line a) (line b))
             (<= (column a) (column b))))))

(defgeneric source-> (a b)
  (:documentation "Return true if source location A comes strictly after B.")
  (:method ((a source-location) (b source-location))
    (or (> (line a) (line b))
        (and (= (line a) (line b))
             (> (column a) (column b))))))

(defgeneric source->= (a b)
  (:documentation "Return true if source location A is equal to or comes
after B.")
  (:method ((a source-location) (b source-location))
    (or (> (line a) (line b))
        (and (= (line a) (line b))
             (>= (column a) (column b))))))

(defgeneric contains (range location)
  (:documentation "Return true if RANGE fully subsumes LOCATION.")
  (:method ((range source-range) (location source-location))
    (and (source-<= (begin range) location)
         (source->= (end range) location)))
  (:method ((a-range source-range) (b-range source-range))
    (and (source-<= (begin a-range) (begin b-range))
         (source->= (end a-range) (end b-range))))
  (:method ((range range) point)
    (and (<= (begin range) point) (>= (end range) point)))
  (:method ((a-range range) (b-range range))
    (and (<= (begin a-range) (begin b-range))
         (>= (end a-range) (end b-range)))))

(defgeneric intersects (a-range b-range)
  (:documentation "Return true if A-RANGE and B-RANGE intersect.")
  (:method ((a-range source-range) (b-range source-range))
    (and (source-< (begin a-range) (end b-range))
         (source-> (end a-range) (begin b-range))))
  (:method ((a-range range) (b-range range))
    (and (< (begin a-range) (end b-range))
         (> (end a-range) (begin b-range)))))

(defun position->source-location (text pos)
  (declare (array-index pos)
           (optimize speed)
           (inline cl:count))
  (with-string-dispatch () text
    ;; We pretend there is a newline at "position" -1.
    (let* ((lines (1+ (cl:count #\Newline text :end pos)))
           (columns (- pos
                       (or (cl:position #\Newline text
                                        :end pos
                                        :from-end t)
                           -1))))
      (make 'source-location
            :line lines
            :column columns))))

;;; TODO Make a test
(flet ((loc (line col) (make 'source-location :line line :column col)))
  (assert (equal? (loc 1 1)
                  (position->source-location "" 0)))
  (let ((s (fmt "~%")))
    (assert (equal? (loc 1 1) (position->source-location s 0)))
    (assert (equal? (loc 2 1) (position->source-location s 1))))
  (let ((s (fmt "~%~%")))
    (assert (equal? (loc 1 1) (position->source-location s 0)))
    (assert (equal? (loc 2 1) (position->source-location s 1)))
    (assert (equal? (loc 3 1) (position->source-location s 2))))
  (let ((s (fmt "~%a~%b")))
    (assert (equal? (loc 1 1) (position->source-location s 0)))
    (assert (equal? (loc 2 1) (position->source-location s 1)))
    (assert (equal? (loc 2 2) (position->source-location s 2)))
    (assert (equal? (loc 3 1) (position->source-location s 3)))
    (assert (equal? (loc 3 2) (position->source-location s 4)))))
