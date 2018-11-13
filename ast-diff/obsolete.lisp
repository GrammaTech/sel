(in-package :software-evolution-library/ast-diff)

(in-readtable :curry-compose-reader-macros)

;;; Obsolete code moved moved here rather than being deleted
;;; entirely.  Some may be used for development testing.

;;; Supporting structures.

(defstruct (costed (:conc-name c))
  "Objects with a cost and length."
  (obj nil)
  (orig nil)
  (length 0 :type fixnum)
  (cost 0 :type fixnum)
  (hash nil :type (or null fixnum)))

;; (declaim (inline reduce-on))
(defun reduce-on (combining-fn tail-fn x &key (recur-p #'consp) (next #'cdr))
  "Use COMBINING-FN to combine, last to first, the values obtained by
calling NEXT zero or more times on X, until RECUR-P is false, at which
point TAIL-FN is applied instead to that last value."
  (let ((stack nil))
    (iter (while (funcall recur-p x))
          (push x stack)
          (setf x (funcall next x)))
    (let ((result (funcall tail-fn x)))
      (iter (while stack)
            (setf result (funcall combining-fn (pop stack) result)))
      result)))

#+nil
(defun to-costed (obj)
  (labels ((recurse (obj)
             (reduce-on
              (lambda (obj cdr) (ccons (recurse (car obj)) cdr :orig obj))
              (lambda (obj) (make-costed :obj obj :orig obj :cost (ast-cost obj)))
              obj)))
    (recurse obj)))

#+nil
(defmethod from-costed ((costed costed))
  (labels ((recurse (costed)
             (reduce-on
              (lambda (costed rest) (cons (recurse (ccar costed)) rest))
              (lambda (x) (if (costed-p x) (or (corig x) (recurse (cobj x))) x))
              costed
              :recur-p (lambda (x) (and (costed-p x) (not (corig x)) (cconsp x)))
              :next #'ccdr)))
    (values (recurse costed) (ccost costed))))


#+nil
(defmethod ccons ((car costed) (cdr costed) &key orig cost)
  (make-costed :obj (cons car cdr)
               :orig orig
               :length (1+ (clength cdr))
               :cost (or cost (+ (ccost car) (ccost cdr)))))

#+nil
(defmethod cconsp ((costed costed))
  (consp (cobj costed)))

#+nil
(defmethod cstringp ((costed costed))
  (stringp (cobj costed)))

#+nil
(defmethod cnull ((clist costed))
  (null (cobj clist)))

#+nil
(defmethod ccar ((costed costed))
  (car (cobj costed)))

#+nil
(defmethod ccdr ((costed costed))
  (cdr (cobj costed)))

#+nil
(defmethod creverse ((costed costed))
  (labels ((helper (it acc)
             (if (cconsp it)
                 (helper (ccdr it) (ccons (ccar it) acc))
                 (if (cnull it)
                     acc
                     (ccons it acc)))))
    (helper costed (make-costed))))

#+nil
(defmethod clast ((costed costed))
  (iter (while (cconsp costed))
        (setf costed (ccdr costed)))
  costed)

#+nil
(defmethod ast-equal-p ((ast-a costed) (ast-b costed))
  (ast-equal-p (cobj ast-a) (cobj ast-b)))


#+nil
(defmethod ast-cost ((ast costed))
  (ast-cost (cobj ast)))

#+nil
(defmethod ast-can-recurse ((ast-a costed) (ast-b costed))
  (ast-can-recurse (cobj ast-a) (cobj ast-b)))


#+nil
(defmethod ast-on-recurse ((ast costed))
  (ast-on-recurse (cobj ast)))

#+nil
(defmethod ast-text ((ast costed))
  (ast-text (cobj ast)))


  #+nil
  (defmethod ast-hash ((c costed))
    (or (chash c)
        (setf (chash c)
              (if (consp (cobj c))
                  (ast-hash
                   (nconc
                    (iter (while (and (typep c 'costed)
                                      (typep (cobj c) 'cons)))
                          (collect (car (cobj c)))
                          (setf c (cdr (cobj c))))
                    c))
                  (ast-hash (cobj c))))))

#+nil
(defun chunk (o-a o-b &aux chunks stable unstable leftp)
  "Group two diffs against the same original into stable and unstable chunks.
See http://www.cis.upenn.edu/%7Ebcpierce/papers/diff3-short.pdf."
  (labels
      ((two (sym-a sym-b)
         (or (setf leftp (and (eql sym-a (caar o-a)) (eql sym-b (caar o-b))))
             (and (eql sym-a (caar o-b)) (eql sym-b (caar o-a)))))
       (flush-unstable ()
         (when unstable
           (appendf chunks (list (cons :unstable unstable)))
           (setf unstable nil)))
       (flush-stable ()
         (when stable
           (appendf chunks (list (cons :stable stable)))
           (setf stable nil))))
    (iter (while (and o-a o-b))
          (cond
            ;; Stable
            ((two :same :same)
             (flush-unstable)
             (appendf stable (list (list (pop o-a) (pop o-b)))))
            ((two :recurse :recurse)
             (flush-stable) (flush-unstable)
             (appendf chunks
                      (list (cons :recurse
                                  (chunk (cdr (pop o-a))
                                         (cdr (pop o-b)))))))
            ;; Unstable
            ((or (two :same :delete)
                 (two :recurse :delete)
                 (two :same :recurse)
                 (two :delete :delete)
                 (two :insert :insert))
             (flush-stable)
             (appendf unstable (list (list (pop o-a) (pop o-b)))))
            ((or (two :same :insert)
                 (two :recurse :insert))
             (flush-stable)
             (appendf unstable (list (if leftp
                                         (list nil (pop o-b))
                                         (list (pop o-a) nil)))))
            (t (error "Unanticipated state when chunking: ~a ~a."
                      (caar o-a) (caar o-b)))))
    (flush-stable) (flush-unstable)
    chunks))

#+nil
(defun diff3 (original branch-a branch-b)
  (labels ((map-chunks (function chunks)
             (when chunks
               (cons (if (eql :recurse (caar chunks))
                         (map-chunks function (cdar chunks))
                         (funcall function (car chunks)))
                     (map-chunks function (cdr chunks))))))
    (map-chunks
     (lambda (chunk)
       (format t "~a:~a~%" (car chunk) (mapcar {mapcar #'car} (cdr chunk)))
       (ecase (car chunk)
         (:stable (cons :stable (cdaadr chunk))) ; Return the text of chunk.
         (:unstable
	  (format t "~A~%" chunk)
          ;; TODO: Unstable Cases:
          ;; - changed only in A
          ;; - changed only in B
          ;; - falsely conflicting
          ;; - truly conflicting
          chunk)))               ; Already labeled :unstable.
     (chunk (ast-diff original branch-a)
            (ast-diff original branch-b)))))

;;; Find "good" common subsequences of two sequences.
;;; This is intended to run in linear time.

(defun good-common-subsequences (s1 s2 &key (test #'eql))
  "Find good common subsequences of two lists s1 and s2, under
equality operation TEST.  Return a list of triples (start1
start2 len), where 0 <= start1 < (length s1), 0 <= start2
< (length s2), and (every test (subseq s1 start1 len) (subseq s2
start2 len)) is true. The list is sorted into increasing order
by the CADDR of the elements.  TEST must be suitable for use
as the test of a hash table."
  (assert (listp s1))
  (assert (listp s2))
  (let ((table1 (make-hash-table :test test))
        (table2 (make-hash-table :test test)))
    ;; POS1 and POS2 are hash tables mapping the
    ;; elements of s1 and s2 to the positions they
    ;; have in each list.  Arrange so the indices
    ;; in each list are in increasing order.
    (flet ((%collect (s h)
             (iter (for x in s) (for i from 0)
                   (push i (gethash x h))))
           (%order (h)
             (iter (for (x l) in-hashtable h)
                   (setf (gethash x h) (nreverse l)))))
      (%collect s1 table1)
      (%collect s2 table2)
      (%order table1)
      (%order table2))
    ;; Greedy algorithm that advances through s1 and s2
    ;; When a common subsequence can be constructed, do so
    ;; until it runs out.  When elements unique to one list
    ;; are found, skip them.  When two non-unique elements
    ;; are found that are not the same, discard the one that
    ;; requires the largest rejection of elements in the other
    ;; list.
    ;;
    ;; This algorithm will fail if there are many elements
    ;; that are equal to two or more elements in the other sequence,
    ;; or a great deal of reordering.  In general, however, for
    ;; diffs of programs this doesn't much matter, as the chance
    ;; of hash collisions for unequal trees should be very small.
    (let ((result nil)
          (pos1 0)
          (pos2 0)
          (start1 0)
          (start2 0)
          (len 0)
          (p1 s1)
          (p2 s2))
      (labels ((cut ()
                 "Terminate the current common segment"
                 (when (> len 0)
                   (push (list start1 start2 len) result)
                   (setf len 0)))
               (chop1 ()
                 "Advance the cursor in s1"
                 (let ((i (pop (gethash (car p1) table1))))
                   (assert (eql i pos1)))
                 (incf pos1)
                 (pop p1))
               (chop2 ()
                 "Advance the cursor in s2"
                 (let ((i (pop (gethash (car p2) table2))))
                   (assert (eql i pos2)))
                 (incf pos2)
                 (pop p2)))
        (loop
           (cond ((null p1) (return))
                 ((null p2) (return))
                 ((null (gethash (car p1) table2))
                  (cut)
                  (chop1))
                 ((null (gethash (car p2) table1))
                  (cut)
                  (chop2))
                 ;; Both (car p1) and (car p2) occur in the other
                 ;; list
                 ((funcall test (car p1) (car p2))
                  ;; Extended the common subsequence, or start
                  ;; one if none is in progress
                  (if (= len 0)
                      (setf start1 pos1 start2 pos2 len 1)
                      (incf len))
                  (chop1)
                  (chop2))
                 ;; Cannot continue the sequence, but both
                 ;; elements occur in the other sequence after
                 ;; this point.  Skip the element whose next match
                 ;; in the other list is farthest away.  It might
                 ;; be best to skip both, but that's not greedy.
                 (t
                  (cut)
                  (let* ((h1 (gethash (car p1) table2))
                         (h2 (gethash (car p2) table1))
                         (l1 (length h1))
                         (l2 (length h2))
                         (m1 (car h1))
                         (m2 (car h2)))
                    (cond
                      ((< l1 l2) (chop1))
                      ((< l2 l1) (chop2))
                      ((<= (- m2 pos1) (- m1 pos2))
                       (chop1))
                      (t (chop2)))))))
        (if (> len 0)
            (nreconc result (list (list start1 start2 len)))
            (nreverse result))))))

(defun merge-diffs (orig-a orig-b &aux (o-a orig-a) (o-b orig-b))
  ;; Derived from CHUNK, but a bit smarter, and
  ;; produce an actual diff not a list of chunks
  (append
   (iter (while (and o-a o-b))
	 (let ((sym-a (caar o-a))
	       (sym-b (caar o-b)))
	   (case sym-a
	     (:same
	      (ecase sym-b
		((nil :same)
		 (if (equalp (car o-a) (car o-b))
		     (collect (progn (pop o-a) (pop o-b)))
		     (push (list :unstable (pop o-a) (pop o-b)) *unstable*)))
		(:insert
		 (appending (iter (while (eql (caar o-b) :insert))
				  (collect (pop o-b)))))
		(:delete
		 ;; If this is not the same as :SAME
		 ;; then it's an unstable merge
		 (unless (equalp (cdar o-a) (cdar o-b))
		   (push (list (car o-a) (car o-b)) *unstable*))
		 (pop o-a)
		 (collect (pop o-b)))
		(:recurse
		 (pop o-a)
		 (collect (pop o-b)))))
	     (:insert
	      (ecase sym-b
		(:insert
		 (cond
		   ((equalp (car o-a) (car o-b))
		    (pop o-b)
		    (collect (pop o-a)))
		   (t
		    (push (list (car o-a) (car o-b)) *unstable*)
		    (collect (pop o-a))
		    (collect (pop o-b)))))
		((:delete :recurse nil)
		 ;; (push (list (car o-a) (car o-b)) *unstable*)
		 (collect (pop o-a)))
		(:same
		 (appending (iter (while (eql (caar o-a) :insert))
				  (collect (pop o-a)))))))
	     (:delete
	      (ecase sym-b
		(:delete
		 (cond
		   ((equalp (car o-a) (car o-b))
		    (pop o-b)
		    (collect (pop o-a)))
		   (t
		    (push (list (car o-a) (car o-b)) *unstable*)
		    (collect (pop o-a))
		    (collect (pop o-b)))))
		((:insert nil)
		 (push (list (car o-a) (car o-b)) *unstable*)
		 (pop o-a))
		(:recurse
		 (push (list (car o-a) (car o-b)) *unstable*)
		 (pop o-a)
		 (collect (pop o-b)))
		(:same
		 ;; Should be the same
		 (unless (equalp (cdar o-a) (cdar o-b))
		   (push (list (car o-a) (car o-b)) *unstable*))
		 (pop o-b)
		 (collect (pop o-a)))))
	     
	     (:recurse
	      (ecase sym-b
		((:insert)
		 (collect (pop o-b)))
		((:delete nil)
		 (push (list (car o-a) (car o-b)) *unstable*)
		 (pop o-b)
		 (collect (pop o-a)))
		(:recurse
		 (collect
		     (cons :recurse (merge-diffs (cdr (pop o-a)) (cdr (pop o-b))))))
		(:same
		 (pop o-b)
		 (collect (pop o-a)))))
	     
	     (nil
	      (collect (pop o-b))
	      (pop o-a)))))
   o-a o-b))

;;; Profiling Information for `ast-diff':
;;
;; Run the below form to test performance at increasing sizes (results
;; table below as well).  The result is n^2 performance until memory
;; pressure kicks in (between 500 and 1000 on SBCL).
;;
;;   (CCL is ~2× as slow as SBCL, but uses much less memory.)
;;
;; Top functions by percent runtime:
;;  ~20% make-costed
;;  ~10% ccons
;; ~0.2% to-costed
;; --------------------
;; ~95% memoized-compute-diff
;; ~85% recursive-diff
;; ~40% recurse
;;
