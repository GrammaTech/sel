;;; simple.lisp --- simple software rep. to manipulate lines of code
(defpackage :software-evolution-library/software/simple
  (:nicknames :sel/software/simple :sel/sw/simple)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :split-sequence
        :software-evolution-library
        :software-evolution-library/software/file
        :software-evolution-library/utility)
  (:shadowing-import-from :uiop :ensure-directory-pathname)
  (:import-from :asdf-encodings :detect-file-encoding)
  (:export :simple
           :light
           :sw-range
           :reference
           :range-nth
           :range-subseq
           :*simple-mutation-types*
           :simple-mutation
           :simple-cut
           :simple-insert
           :simple-swap
           :double-cut))
(in-package :software-evolution-library/software/simple)
(in-readtable :curry-compose-reader-macros)


;;; simple software objects
(define-software simple (software file)
  ((genome :initarg :genome :accessor genome :initform nil
           :copier enhanced-copy-seq))
  (:documentation "The simplest base software object."))

(defun sel-copy-array (array)
  (let* ((element-type (array-element-type array))
         (fill-pointer (and (array-has-fill-pointer-p array)(fill-pointer array)))
         (adjustable (adjustable-array-p array))
         (new (make-array (array-dimensions array)
                          :element-type element-type
                          :adjustable adjustable
                          :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array) new)
      (setf (row-major-aref new i)(row-major-aref array i)))))

(defun enhanced-copy-seq (sequence)
  "Copies any type of array (except :displaced-to) and lists. Otherwise returns NIL."
  (if (arrayp sequence)
      (sel-copy-array sequence)
      (if (listp sequence)
          (copy-list sequence))))

(defmethod lines ((simple simple))
  (remove nil (map 'list {aget :code} (genome simple))))

(defmethod (setf lines) (new (simple simple))
  (setf (genome simple) (mapcar [#'list {cons :code}] new)))

(defmethod size ((obj simple))
  (length (lines obj)))

(defmethod genome-string ((simple simple) &optional stream)
  (format stream "~{~a~%~}" (lines simple)))

(defmethod from-file ((simple simple) path)
  (setf (genome simple)
        (with-open-file (in path :external-format (detect-file-encoding path))
          (loop :for line := (read-line in nil) :while line
             :collect (list (cons :code line)))))
  simple)

(defmethod to-file ((simple simple) file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (genome-string simple out)))

(defvar *simple-mutation-types*
  (cumulative-distribution
   (normalize-probabilities
    '((simple-cut    . 1)
      (simple-insert . 1)
      (simple-swap   . 1))))
  "Default list of mutations to apply to simple software objects.")

(defmethod pick-mutation-type ((obj simple))
  (random-pick *simple-mutation-types*))

(defmethod mutate ((simple simple))
  (unless (> (size simple) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj simple)))
  (restart-case
      (let ((mutation (make-instance (pick-mutation-type simple)
                                     :object simple)))
        (apply-mutation simple mutation)
        (values simple mutation))
    (try-another-mutation ()
      :report "Try another mutation"
      (mutate simple))))

(defclass simple-mutation (mutation)
  ()
  (:documentation "DOCFIXME"))

(define-mutation simple-cut (simple-mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((simple simple) (mutation simple-cut))
  "DOCFIXME"
  ;; NOTE: it is important here that elements of genome are not
  ;;       changed, rather the genome should *only* be changed by
  ;;       setting the genome *accessor* directly.  I.e., avoid
  ;;       forms like the following as they will change the
  ;;       reference value of diff objects (see diff.lisp).
  ;;
  ;;           (setf (car (genome simple)) ...)
  (let ((cut (targets mutation))
        (genome (genome simple)))
    (setf (genome simple)
          (concatenate (class-of genome)
                       (subseq genome 0 cut)
                       (subseq genome (1+ cut))))
    simple))

(define-mutation double-cut (simple-mutation)
  ((targeter :initform #'pick-bad-bad))
  (:documentation "Remove two random elements of the genome."))

(defmethod apply-mutation ((simple simple) (mutation double-cut))
  "Cut two random elements of the genome.
 If the two elements indices are equal, this just becomes one simple-cut.
 Otherwise it is implemented as two simple-cut mutations,
 so derived mutations should only need to implement simple-cut."
  (let ((first-cut (first (targets mutation)))
        (second-cut (second (targets mutation)))
        mutated)
    (assert (and (integerp first-cut) (integerp second-cut)) (mutation)
            "Requires mutations targets to be a list of two integers.")
    (setf mutated
          (apply-mutation
           simple
           (make-instance 'simple-cut
             :targets (max first-cut second-cut))))
    (unless (= first-cut second-cut)
      (setf mutated
            (apply-mutation
             mutated
             (make-instance 'simple-cut
               :targets (min first-cut second-cut)))))
    mutated))

(define-mutation simple-insert (simple-mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((simple simple) (mutation simple-insert))
  "DOCFIXME"
  (let ((bad-good (targets mutation))
        (genome (genome simple)))
    (assert (listp bad-good) (mutation)
            "Requires mutations targets to be a list of two elements.")
    (let ((insertion-pt (first bad-good))
          (copy-pt (second bad-good)))
      (setf (genome simple)
            (concatenate (class-of genome)
                         (subseq genome 0 insertion-pt)
                         (list (elt genome copy-pt))
                         (subseq genome insertion-pt)))
      simple)))

(define-mutation simple-swap (simple-mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "DOCFIXME"))

(defmethod apply-mutation ((simple simple) (mutation simple-swap))
  "DOCFIXME"
  (let ((bad-good (targets mutation))
        (genome (genome simple)))
    (assert (listp bad-good) (mutation)
            "Requires mutations targets to be a list of two elements.")
    (let ((pt1 (first bad-good))
          (pt2 (second bad-good))
          (tmp (copy-seq genome)))
      (setf (elt tmp pt1) (elt genome pt2))
      (setf (elt tmp pt2) (elt genome pt1))
      (setf (genome simple) tmp)
      simple)))

(defmethod mcmc-step ((simple simple))
  "DOCFIXME"
  (let ((point (random (size simple))))
    (let ((genome (genome simple)))
      (setf genome
            (if (zerop (random 2))
                ;; delete an element
                (concatenate (class-of genome)
                  (subseq genome 0 point)
                  (subseq genome (1+ point)))
                ;; insert an element
                (concatenate (class-of genome)
                  (subseq genome 0 point)
                  (list (random-elt *mcmc-fodder*))
                  (subseq genome point)))))))


;;; Crossover
(defmethod crossover ((a simple) (b simple))
  "DOCFIXME"
  (two-point-crossover a b))

#|
(defun align-for-crossover (a b &key (test equal) key)
  "Return two offsets which align genomes A and B for maximum similarity.
TEST may be used to test for similarity and should return a boolean (number?)."
  (values 0 0))
|#

(defmethod two-point-crossover ((a simple) (b simple))
  "DOCFIXME"
  ;; Two point crossover
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((points (sort (loop :for i :below 2 :collect (random range)) #'<))
              (new (copy a)))
          (setf (genome new)
                (copy-seq (concatenate (class-of (genome a))
                           (subseq (genome b) 0 (first points))
                           (subseq (genome a) (first points) (second points))
                           (subseq (genome b) (second points)))))
          (values new points))
        (values (copy a) nil))))

(defmethod one-point-crossover ((a simple) (b simple))
  "DOCFIXME"
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((point (random range))
              (new (copy a)))
          (setf (genome new)
                (copy-seq (concatenate (class-of (genome a))
                            (subseq (genome b) 0 point)
                            (subseq (genome a) point))))
          (values new point))
        (values (copy a) nil))))

(defun context (list i size)
  "DOCFIXME"
  (loop :for j :from (max 0 (- i size)) :to (min (1- (length list)) (+ i size))
     :collect (nth j list)))

(defun contexts (list size)
  "Return lists of contexts of LIST of radius SIZE."
  (loop :for i :below (length list) :collect (context list i size)))

(defun synapsing-points (a b &key
                               (context 2)
                               (test (lambda (a b) (if (tree-equal a b) 1 0)))
                               (key 'identity))
  "Return points from a and b which have similar context.
First select a point from B at random, then select a point from A
proportionately based on the similarity of context.  CONTEXT
determines the number of elements on either side of the points to
consider.  TEST should take two elements and return a similarity
metric between 0 and 1.  KEY may be a function of one argument whose
value is passed to TEST."
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((ap (random (length a)))
         (a-ctx (let ((ctx (subseq a
                                   (max 0 (- ap context))
                                   (min (length a) (+ ap context 1)))))
                  (if key (mapcar key ctx) ctx)))
         (bp
          (position-extremum-rand (contexts b context) #'>
                                  [#'mean {mapcar test a-ctx} {mapcar key}])))
    (list ap bp)))

(defmethod synapsing-crossover ((a simple) (b simple)
                                &key
                                  (key {aget :code})
                                  (context 2)
                                  (test (lambda (a b) (if (tree-equal a b) 1 0))))
  "DOCFIXME"
  (let* ((starts (synapsing-points (genome a) (genome b)
                                   :key key :context context :test test))
         (ends (mapcar #'+ starts
                       (synapsing-points (subseq (genome a) (first starts))
                                         (subseq (genome b) (second starts))
                                         :key key :context context :test test)))
         (new (copy a)))
    (setf (genome new)
          (copy-seq (concatenate (class-of (genome a))
                      (subseq (genome a) 0 (first starts))
                      (subseq (genome b) (second starts) (second ends))
                      (subseq (genome a) (first ends)))))
    (values new (mapcar #'cons starts ends))))

(defmethod similarity-crossover ((a simple) (b simple)
                                 &key
                                   (key {aget :code})
                                   (test (lambda (a b) (if (tree-equal a b) 1 0))))
  "DOCFIXME"
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let* ((new (copy a))
               ;; random subset of A
               (pts (sort (loop :for i :below 2 :collect (random range)) #'<))
               (size (- (second pts) (first pts)))
               (base (mapcar key (apply #'subseq (genome a) pts)))
               ;; beginning of a subset of B similar to random subset of A
               (bs
                (proportional-pick
                 (chunks (genome b) size)
                 [{+ (/ 0.1 size)} #'mean {mapcar test base} {mapcar key}]))
               (b-pts (list bs (+ bs (apply #'- (reverse pts))))))
          (setf (genome new)
                (concatenate (class-of (genome a))
                  (subseq (genome a) 0 (first pts))
                  (apply #'subseq (genome b) b-pts)
                  (subseq (genome a) (second pts))))
          (values new pts))
        (values (copy a) nil))))


;;;; Light software objects
;;;
;;; A refinement of the `simple' software representation, using a
;;; simplified genome data structure which require fewer cons cells
;;; but does not allow tagging of lines of code with information.
;;;
;;; @texi{light}
(defclass light (simple)
  ()
  (:documentation "DOCFIXME"))

(defmethod lines ((light light))
  "DOCFIXME"
  (genome light))

(defmethod (setf lines) (new (light light))
  "DOCFIXME"
  (setf (genome light) new))

(defmethod from-file ((light light) path)
  "DOCFIXME"
  (setf (genome light) (split-sequence #\Newline (file-to-string path)))
  light)


;;;; Range software objects
;;;
;;; An software object which uses even less memory than the `light' or
;;; `simple' software object, but adds some complexity to many genome
;;; manipulation methods.  Genomes are stored as references to ranges
;;; in the original reference genome.  This representation does not
;;; permit wholly new genome elements, only re-orderings of existing
;;; genome elements.
;;;
;;; @texi{range}
(define-software sw-range (simple)
  ((reference :initarg :reference :initform nil))
  (:documentation
   "Alternative to SIMPLE software objects which should use less memory.
Instead of directly holding code in the GENOME, each GENOME is a list
of range references to an external REFERENCE code array."))

(defgeneric reference (software)
  (:documentation "The original \"reference\" genome.
The sw-range SOFTWARE genome is a list of references to this genome.")
  (:method ((range sw-range)) (slot-value range 'reference)))
(defgeneric (setf reference) (software new)
  (:documentation "Set the value of REFERENCE to NEW, and update the GENOME.")
  (:method (new (range sw-range))
    (assert (typep new 'vector) (new) "Reference must be a vector.")
    (setf (slot-value range 'reference) new)
    (setf (genome range) (list (cons 0 (1- (length new)))))))

(defmethod from-file ((range sw-range) path)
  "DOCFIXME"
  (declare (ignorable path))
  (error "RANGE individuals may not be initialized directly from
files.  First construct an array of code (lines or bytes) from PATH
and use this to initialize the RANGE object."))

(declaim (inline range-size))
(defun range-size (range)
  "DOCFIXME"
  (1+ (- (cdr range) (car range))))

(defmethod size ((range sw-range))
  "DOCFIXME"
  (reduce #'+ (mapcar #'range-size (genome range))))

(defmethod lines ((range sw-range))
  "DOCFIXME"
  (mappend (lambda (pair)
             (destructuring-bind (start . end) pair
               (mapcar {aref (reference range)}
                       (loop :for i :from start :to end :collect i))))
           (genome range)))

(defmethod (setf lines) (new (range sw-range))
  "DOCFIXME"
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

(defmethod apply-mutation ((range sw-range) (mutation simple-cut))
  "DOCFIXME"
  (with-slots (genome) range
    (setf genome (range-cut genome (targets mutation)))
    range))

(defmethod apply-mutation ((range sw-range) (mutation simple-insert))
  "DOCFIXME"
  (let ((bad-good (targets mutation)))
    (assert (listp bad-good) (mutation)
            "Requires mutations targets to be a list of two elements.")
    (let ((insertion-pt (first bad-good))
          (copy-pt (second bad-good)))
      (with-slots (genome) range
        (setf genome
              (range-insert genome insertion-pt (range-nth copy-pt genome)))
        range))))

(defmethod apply-mutation ((range sw-range) (mutation simple-swap))
  "DOCFIXME"
  (let ((bad-good (targets mutation)))
    (assert (listp bad-good) (mutation)
            "Requires mutations targets to be a list of two elements.")
    (let ((pt1 (first bad-good))
          (pt2 (second bad-good)))
      (with-slots (genome) range
        (setf genome
              (range-swap genome pt1 pt2
                          (range-nth pt1 genome)
                          (range-nth pt2 genome)))
        range))))

(defun range-subseq (range start &optional end)
  "DOCFIXME"
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

(defmethod one-point-crossover ((a sw-range) (b sw-range))
  "DOCFIXME"
  (assert (eq (reference a) (reference b)) (a b)
          "Can not crossover range objects with unequal references.")
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((point (random range))
              (new (copy a)))
          (setf (genome new)
                (copy-seq (append (range-subseq (genome a) 0 point)
                                  (range-subseq (genome b) point))))
          (values new point))
        (values (copy a) 0))))

(defmethod two-point-crossover ((a sw-range) (b sw-range))
  "DOCFIXME"
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((points (sort (loop :for i :below 2 :collect (random range)) #'<))
              (new (copy a)))
          (setf (genome new)
                (copy-seq
                 (append
                  (range-subseq (genome b) 0 (first points))
                  (range-subseq (genome a) (first points) (second points))
                  (range-subseq (genome b) (second points)))))
          (values new points))
        (values (copy a) nil))))
