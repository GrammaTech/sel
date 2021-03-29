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
   :source-=
   :contains
   :intersects
   :source-location->position
   :precompute-newline-offsets
   :position->source-location
   :source-range-subseq))
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
  ((begin :initarg :begin :accessor begin :type source-location
          :initarg :start)
   (end   :initarg :end   :accessor end   :type source-location)))

(defclass range ()
  ((begin :initarg :begin :accessor begin :type fixnum
          :initarg :start)
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

(defgeneric source-= (a b)
  (:documentation "Return true if source location A is equal to B.")
  (:method ((a source-location) (b source-location))
    (and (= (line a) (line b))
         (= (column a) (column b))))
  (:method ((a source-range) (b source-range))
    (and (source-= (begin a) (begin b))
         (source-= (end a) (end b)))))

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

(defun precompute-newline-offsets (string)
  "Precompute a cache of newline offsets in STRING.
Can be used by `position->source-location' and
`source-location->position'."
  (lret ((v
          (make-array 10
                      :fill-pointer 0
                      :adjustable t
                      :element-type 'array-index)))
    (with-string-dispatch () string
      (iter (for i below (length string))
            (when (eql #\Newline (vref string i))
              (vector-push-extend i v))))))

(defun position->source-location (string pos &optional newlines)
  "Translate POS, a position in STRING, into a source location object.
If NEWLINES is provided, it should be the value returned for STRING by
`precompute-newline-offsets'."
  (declare (array-index pos)
           (optimize speed)
           (inline cl:count))
  ;; We would like this to be fast, so specialize it on different
  ;; string subtypes.
  (with-string-dispatch () string
    ;; Source locations are 1-based so we pretend there is a newline
    ;; at "position" -1.
    (let* ((lines
            (1+ (if newlines
                    (bisect-left newlines pos #'<)
                    (cl:count #\Newline string
                              :start 0
                              :end pos))))
           (columns (- pos
                       (or (cl:position #\Newline string
                                        :end pos
                                        :from-end t)
                           -1))))
      (declare (array-index lines))
      (make 'source-location
            :line lines
            :column columns))))

;;; TODO Should this be a utility?
(defun nth-position (n item seq
                     &rest args
                     &key (start 0)
                     &allow-other-keys)
  "Return the position of the Nth occurrence of ITEM in SEQ.
If there are fewer than N+1 occurrences, return the difference as a
second value."
  (nlet rec ((n n)
             (start start)
             (last-pos nil))
    (if (minusp n) (values last-pos 0)
        (let ((next-pos (apply #'position item seq :start start args)))
          (if (null next-pos) (values last-pos (1+ n))
              (rec (1- n)
                   (1+ next-pos)
                   next-pos))))))

(defun source-location->position (text location &optional newlines)
  "Translate LOCATION, a source location, into a position in TEXT.
Note the position may actually point beyond TEXT if the source
location has an extra newline, which can happen because `source-range`
addresses a node that ends in a newline as (n+1,1).

If NEWLINES is provided it should be the value returned for TEXT by
`precompute-newline-offsets'."
  (mvlet* ((line (assure (integer 1 *) (line location)))
           (column (column location))
           ;; When translating from a source location to a position, we treat
           ;; excess newlines as extending beyond the end of the string.
           (line-start-pos remaining
            (cond ((= line 1) (values -1 0))
                  (newlines
                   (if (> line (length newlines))
                       (values (last-elt newlines)
                               (- line (length newlines) 1))
                       (values (aref newlines (- line 2))
                               0)))
                  (t
                   (multiple-value-bind (pos rem)
                       (nth-position (- line 2) #\Newline text)
                     (if (null pos) (values -1 0)
                         (values pos rem)))))))
    ;; Note that we don't have to adjust the column as the extra
    ;; offset is "absorbed" by the newline.
    (+ line-start-pos column remaining)))

(defgeneric source-range-subseq (string source-range &optional newlines)
  (:documentation
   "Get the subsequence of STRING corresponding to SOURCE-RANGE.
If NEWLINES is provided it should be the value returned for STRING by
`precompute-newline-offsets'.")
  (:method ((string string) source-range &optional newlines)
    ;; Use `slice' to avoid having to worry about the possible extra
    ;; newline.
    (slice string
           (source-location->position string (begin source-range) newlines)
           (source-location->position string (end source-range) newlines)))
  (:method ((line-octets vector) source-range &optional newlines
            &aux (start (begin source-range))
              (end (end source-range)))
    "Get the subsequence of LINE-OCTETS corresponding to SOURCE-RANGE."
    (declare (ignore newlines))
    (let ((max-line (length line-octets)))
      (with-slots ((start-column column) (start-line line)) start
        (with-slots ((end-column column) (end-line line)) end
          (coerce
           (cond
             ((= start-line end-line)
              (subseq (aref line-octets (clamp start-line 0 max-line)) start-column end-column))
             ((< start-line end-line)
              (concatenate
               'vector
               (subseq (aref line-octets (clamp start-line 0 max-line)) start-column)
               (iter
                 (for i from (clamp (1+ start-line) 0 max-line) to (clamp (1- end-line) 0 max-line))
                 (reducing (aref line-octets i)
                           by (lambda (total octets)
                                (concatenate 'vector total octets))
                           initial-value #()))
               ;; NOTE: tree-sitter will add +1 past the number of lines in a
               ;;       file when a newline is considered a token for a rule.
               (unless (= end-column 0)
                 (subseq (aref line-octets (clamp end-line 0 max-line)) 0 end-column))))
             (t #()))
           '(vector (unsigned-byte 8))))))))
