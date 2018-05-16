;;; ast-diff.lisp --- diffs between ASTs and other tree structures
;;
;; From http://thume.ca/2017/06/17/tree-diffing/#a-tree-diff-optimizer
;;
;;; TODO:
;; Skipping identical prefixes and suffixes
;;
;; Add cost of switch, two grids
;;
;; Lazy “switch” creation
;;
;; Profile
;;
;; A-star
;;
;; Sub-diff for text (customizable)
;;
;; Lisp differ
;; - collect comments
;; - is diff in comments
;; - command line ex’s suitable for use by git
;;
;; C differ
;; - command line ex’s suitable for use by git
(defpackage :software-evolution-library/ast-diff
  (:nicknames :sel/ast-diff)
  (:use
   :common-lisp
   :software-evolution-library/utility
   :alexandria
   :cl-arrowz
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :cl-who)
  (:export
   :ast-equal-p
   :ast-cost
   :ast-can-recurse
   :ast-on-recurse
   :ast-un-recurse
   :ast-text
   ;; :costed
   ;; :cobj
   ;; :clength
   ;; :ccost
   ;; :to-costed
   ;; :from-costed
   ;; :ccons
   ;; :cconsp
   ;; :cnull
   ;; :ccar
   ;; :cdar
   :ast-diff
   :ast-diff-elide-same
   :ast-patch))
(in-package :software-evolution-library/ast-diff)
(in-readtable :curry-compose-reader-macros)


;;; Supporting structures.
(defstruct (costed (:conc-name c))
  "Objects with a cost and length."
  (obj nil)
  (orig nil)
  (length 1 :type fixnum)
  (cost 0 :type fixnum))

(defun to-costed (obj)
  (labels ((recurse (obj)
             (if (consp obj)
                 (ccons (recurse (car obj))
                        (recurse (cdr obj))
                        :orig obj)
                 (make-costed :obj obj :orig obj :cost (ast-cost obj)))))
    (recurse obj)))

(defmethod from-costed ((costed costed))
  (labels ((recurse (costed)
             (if (costed-p costed)
                 (or (corig costed)
                     (if (cconsp costed)
                         (cons (recurse (ccar costed))
                               (recurse (ccdr costed)))
                         (recurse (cobj costed))))
                 costed)))
    (values (recurse costed) (ccost costed))))

(defmethod ccons ((car costed) (cdr costed) &key orig)
  (make-costed :obj (cons car cdr)
               :orig orig
               :length (1+ (clength cdr))
               :cost (+ (ccost car) (ccost cdr))))

(defmethod cconsp ((costed costed))
  (consp (cobj costed)))

(defmethod cnull ((clist costed))
  (null (cobj clist)))

(defmethod ccar ((costed costed))
  (car (cobj costed)))

(defmethod ccdr ((costed costed))
  (cdr (cobj costed)))


;;; Interface functions.
(defgeneric ast-equal-p (ast-a ast-b)
  (:documentation "Return T AST-A and AST-B are equal for diffing."))

(defmethod ast-equal-p ((ast-a t) (ast-b t))
  (equalp ast-a ast-b))

(defmethod ast-equal-p ((ast-a cons) (ast-b cons))
  (and (ast-equal-p (car ast-a) (car ast-b))
       (ast-equal-p (cdr ast-a) (cdr ast-b))))

(defmethod ast-equal-p ((ast-a costed) (ast-b costed))
  (ast-equal-p (cobj ast-a) (cobj ast-b)))

(defgeneric ast-cost (ast)
  (:documentation "Return cost of AST."))

(defmethod ast-cost ((ast t))
  1)

(defmethod ast-cost ((ast cons))
  (if (consp ast)
      (+ (ast-cost (car ast)) (ast-cost (cdr ast)))
      1))

(defmethod ast-cost ((ast costed))
  (ast-cost (cobj ast)))

(defgeneric ast-can-recurse (ast-a ast-b)
  (:documentation "Check if recursion is possible on AST-A and AST-B."))

(defmethod ast-can-recurse ((ast-a costed) (ast-b costed))
  (ast-can-recurse (cobj ast-a) (cobj ast-b)))

(defmethod ast-can-recurse ((ast-a t) (ast-b t))
  (and (consp ast-a) (consp ast-b)))

(defgeneric ast-on-recurse (ast)
  (:documentation "Possibly AST on recursion."))

(defmethod ast-on-recurse ((ast t))
  ast)

(defmethod ast-on-recurse ((ast costed))
  (ast-on-recurse (cobj ast)))

(defgeneric ast-un-recurse (ast sub-ast)
  (:documentation
   "Reverse the effect of `ast-on-recurse' recombining SUB-AST into AST."))

(defmethod ast-un-recurse ((ast t) (sub-ast t))
  sub-ast)

(defgeneric ast-text (ast)
  (:documentation "Return textual representation of AST."))

(defmethod ast-text ((ast t))
  (write ast :stream nil :readably t))

(defmethod ast-text ((ast costed))
  (ast-text (cobj ast)))


;;; Main interface to calculating ast differences.
(defgeneric ast-diff (ast-a ast-b)
  (:documentation
   "Return a least-cost edit script which transforms AST-A into AST-B.
Also return a second value indicating the cost of the edit.

See `ast-patch' for more details on edit scripts.

The following generic functions may be specialized to configure
differencing of specialized AST structures.; `ast-equal-p',
`ast-cost', `ast-can-recurse', and `ast-on-recurse'."))

;;; TODO: Fix failing test, should recurse but doesn't.
(defmethod ast-diff ((ast-a t) (ast-b t))
  ;; Edit scripts are represented by a (length a) x (length b) grid.
  ;; Each position A,B on the grid stores the diff between (subseq
  ;; ast-a A) and (subseq ast-b B), along with its cost. The problem
  ;; then reduces to finding the least-cost path from the upper-left
  ;; to lower-right corners of the grid.

  ;; Along the way we can make the following moves:
  ;; Delete the head of AST-A (move right in the grid)
  ;; Insert the head of AST-B (move down in the grid)\
  ;; If the head of AST-A and AST-B are equal, move diagonally.
  ;; Recursively edit the head of AST-A to match the head of AST-B,
  ;; and move diagonally.

  ;; Intermediate results are cached a in two-dimensional array to
  ;; avoid redundant computation.
  (declare (optimize speed))
  (let* ((costed-a (to-costed (ast-on-recurse ast-a)))
         (costed-b (to-costed (ast-on-recurse ast-b))))
    (labels
        ((recursive-diff (costed-a costed-b)
           (let ((costs (make-array (list (clength costed-a)
                                          (clength costed-b))
                                    :initial-element nil)))

             ;; Compute diff from start (top,left) to the target (bottom,right).
             (compute-diff costed-a costed-b costs)
             ;; The following may be used to return or print the
             ;; memoized costs grid.
             #+(or )
             (multiple-value-bind (diff cost)
                 (compute-diff costed-a costed-b costs)
               ;; Print the grid of costs which can be informative.
               (let ((cost-ret (make-array (list (clength costed-a)
                                                 (clength costed-b))
                                           :initial-element nil)))
                 (dotimes (x (clength costed-a) cost-ret)
                   (dotimes (y (clength costed-b))
                     (format t "~4,d "
                             (setf (aref cost-ret x y)
                                   (if (aref costs x y)
                                       (ccost (aref costs x y))
                                       -1))))
                   (format t "~%")))
               (values diff cost))))

         (memoized-compute-diff (costed-a costed-b costs)
           ;; (format t "~a ~a~%" (corig costed-a) (corig costed-b))

           ;; Check memoized.
           (destructuring-bind (pos-a pos-b)
               (destructuring-bind (size-a size-b) (array-dimensions costs)
                 (declare (type fixnum size-a))
                 (declare (type fixnum size-b))
                 (list (- size-a (clength costed-a))
                       (- size-b (clength costed-b))))

             ;; Check for nulls.
             (when (and (cnull costed-a) (cnull costed-b)) ; Both are null.
               (return-from memoized-compute-diff
                 (make-costed)))
             (when (cnull costed-a)       ; Just original is null.
               (return-from memoized-compute-diff
                 (ccons (ccons (make-costed :obj :insert) (ccar costed-b))
                        (make-costed))))
             (when (cnull costed-b)       ; Just new is null.
               (return-from memoized-compute-diff
                 (ccons (ccons (make-costed :obj :delete) (ccar costed-a))
                        (make-costed))))

             ;; Both trees are down to a single atomic element.
             (when (and (= (clength costed-a) 1)
                        (= (clength costed-b) 1))
               (return-from memoized-compute-diff
                 (if (ast-equal-p (cobj costed-a) (cobj costed-b))
                     (ccons (make-costed :obj :same)
                            (make-costed :obj (cobj costed-a)))
                     (ccons (ccons (make-costed :obj :delete) costed-a)
                            (ccons (make-costed :obj :insert) costed-b)))))

             ;; Use memoized value if available.
             (when-let ((memoized (aref costs pos-a pos-b)))
               (return-from memoized-compute-diff memoized))

             (setf (aref costs pos-a pos-b)
                   (compute-diff costed-a costed-b costs))))

         (compute-diff (costed-a costed-b costs)
           (declare (type (SIMPLE-ARRAY T) costs))
           (let ((insert (if (cconsp costed-b)
                             (ccons (ccons (make-costed :obj :insert) (ccar costed-b))
                                    (memoized-compute-diff costed-a (ccdr costed-b) costs))
                             (ccons (make-costed :obj :insert) costed-b)))
                 (delete (if (cconsp costed-a)
                             (ccons (ccons (make-costed :obj :delete) (ccar costed-a))
                                    (memoized-compute-diff (ccdr costed-a) costed-b costs))
                             (ccons (make-costed :obj :delete) costed-a))))

             ;; Try diagonal is the cheapest when heads are equal.
             (when (and (cconsp costed-a) (cconsp costed-b))
               (if (ast-equal-p (ccar costed-a) (ccar costed-b))
                   (let ((same (ccons (make-costed :obj (ccons (make-costed :obj :same)
                                                               (ccar costed-a))
                                                   :cost 0)
                                      (memoized-compute-diff ; Diagonal.
                                       (ccdr costed-a) (ccdr costed-b)
                                       costs))))
                     (when (and (<= (ccost same) (ccost insert))
                                (<= (ccost same) (ccost delete)))
                       (return-from compute-diff same)))
                   ;; Try recursion if not same can recurse.
                   (when (ast-can-recurse (ccar costed-a) (ccar costed-b))
                     (let* ((subtree (recursive-diff (to-costed (ast-on-recurse (corig (ccar costed-a))))
                                                     (to-costed (ast-on-recurse (corig (ccar costed-b))))))
                            (recurse (ccons (ccons (make-costed :obj :recurse)
                                                   subtree)
                                            (memoized-compute-diff ; Diagonal.
                                             (ccdr costed-a) (ccdr costed-b)
                                             costs))))
                       (when (and (<= (ccost recurse) (ccost insert))
                                  (<= (ccost recurse) (ccost delete)))
                         (return-from compute-diff recurse))))))
             
             ;; Else return the cheapest of insert or delete.
             (if (< (ccost insert) (ccost delete))
                 insert
                 delete))))
      (multiple-value-bind (script cost)
          (from-costed (recursive-diff costed-a costed-b))
        (values script cost)))))

(defun ast-diff-elide-same (edit-script)
  "Return the non-same subset of EDIT-SCRIPT with path information.
Path's are represented as a sequence of car (:A) and cdr (:D) from the
root of the edit script (and implicitly also the program AST)."
  (labels
      ((follow (edit-script path)
         (when edit-script
           (append
            (case (caar edit-script)
              (:same nil)
              (:recurse (follow (cdar edit-script) (cons :a path)))
              (t (list (cons (reverse path) (car edit-script)))))
            (follow (cdr edit-script) (cons :d path))))))
    (follow edit-script nil)))

(defgeneric ast-patch (original diff)
  (:documentation "Create an edited AST by applying DIFF to ORIGINAL.

A diff is a sequence of actions as returned by `ast-diff' including:
:same A B  : keep the current AST
:insert B  : insert B at the current position
:remove A  : remove the current AST
:recurse S : recursively apply script S to the current AST"))

(defmethod ast-patch ((original t) (script list))
  (labels
      ((edit (asts script)
         (when script
           (destructuring-bind (action . args) (car script)
             (ecase action
               (:recurse (cons (ast-patch (car asts) args)
                               (edit (cdr asts) (cdr script))))
               (:same (cons (car asts)
                            (edit (cdr asts) (cdr script))))
               (:delete (assert (ast-equal-p (car asts) args))
                        (edit (cdr asts) (cdr script)))
               (:insert (cons args (edit asts (cdr script)))))))))
    (ast-un-recurse original (edit (ast-on-recurse original) script))))
