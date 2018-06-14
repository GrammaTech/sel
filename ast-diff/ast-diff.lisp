;;; ast-diff.lisp --- diffs between ASTs and other tree structures
;;
;; From http://thume.ca/2017/06/17/tree-diffing/#a-tree-diff-optimizer
;;
;;; TODO:
;; - Add cost of switch, two grids
;; - Lazy “switch” creation
;; - Profile
;; - A-star
;; - Sub-diff for text (customizable)
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
   :arrow-macros
   :named-readtables
   :curry-compose-reader-macros
   :metabang-bind
   :iterate
   :cl-heap)
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
   :ast-patch
   :print-diff))
(in-package :software-evolution-library/ast-diff)
(in-readtable :curry-compose-reader-macros)


;;; Supporting structures.
(defstruct (costed (:conc-name c))
  "Objects with a cost and length."
  (obj nil)
  (orig nil)
  (length 0 :type fixnum)
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

(defmethod ccons ((car costed) (cdr costed) &key orig cost)
  (make-costed :obj (cons car cdr)
               :orig orig
               :length (1+ (clength cdr))
               :cost (or cost (+ (ccost car) (ccost cdr)))))

(defmethod cconsp ((costed costed))
  (consp (cobj costed)))

(defmethod cnull ((clist costed))
  (null (cobj clist)))

(defmethod ccar ((costed costed))
  (car (cobj costed)))

(defmethod ccdr ((costed costed))
  (cdr (cobj costed)))

(defmethod creverse ((costed costed))
  (labels ((helper (it acc)
             (if (cconsp it)
                 (helper (ccdr it) (ccons (ccar it) acc))
                 (if (cnull it)
                     acc
                     (ccons it acc)))))
    (helper costed (make-costed))))

(defmethod clast ((costed costed))
  (if (cconsp costed) (clast (ccdr costed)) costed))


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

(defmethod ast-text ((ast string))
  ast)

(defmethod ast-text ((ast cons))
  (concatenate 'string (ast-text (car ast)) (ast-text (cdr ast))))


;;; Main interface to calculating ast differences.
(defgeneric ast-diff (ast-a ast-b)
  (:documentation
   "Return a least-cost edit script which transforms AST-A into AST-B.
Also return a second value indicating the cost of the edit.

See `ast-patch' for more details on edit scripts.

The following generic functions may be specialized to configure
differencing of specialized AST structures.; `ast-equal-p',
`ast-cost', `ast-can-recurse', and `ast-on-recurse'."))

(defun remove-common-prefix-and-suffix (list-a list-b)
  "Return unique portions of LIST-A and LIST-B less shared prefix and postfix.
Prefix and postfix returned as additional values."
  ;; Just return the input lists immediately if not proper lists.
  (unless (and (consp list-a) (consp list-b)
               (proper-list-p list-a) (proper-list-p list-b))
    (return-from remove-common-prefix-and-suffix
      (values list-a list-b nil nil)))
  (labels ((prefix (list-a list-b)
             (iter (for a in list-a)
                   (for b in list-b)
                   (if (equalp a b)
                       (collect a into common)
                       (return common))
                   (finally (return common)))))
    (let* ((prefix (prefix list-a list-b))
           (pre-length (length prefix))
           (a (drop pre-length list-a))
           (b (drop pre-length list-b)))
      ;; If either list is completely consumed by the prefix, return here.
      (if (or (null a) (null b))
          (values a b prefix nil)
          ;; Calculate the postfix (less the prefix) if necessary.
          (let* ((postfix (prefix (reverse a) (reverse b)))
                 (post-length (length postfix)))
            (values (butlast a post-length)
                    (butlast b post-length)
                    prefix
                    (reverse postfix)))))))

(defun make-cache (total-a total-b)
  (make-array (list (1+ (clength total-a)) (1+ (clength total-b)))
              :initial-element nil))

(defun recursive-diff (total-a total-b
                       &aux
                         (from (make-cache total-a total-b))
                         (fringe (make-instance 'priority-queue))
                         (open (make-cache total-a total-b))
                         (total-open 0)
                         (closed (make-cache total-a total-b))
                         (g (make-cache total-a total-b))
                         (f (make-cache total-a total-b)))
  (labels
      ((heuristic (a-pos b-pos)
         ;; Currently the heuristic is the minimum distance to
         ;; the diagonal.
         ;;
         ;; NOTE: Should probably change this to actually return the
         ;;       costs of the deletion/insertion of the elements to
         ;;       get to the diagonal.
         (min (abs (- a-pos (clength total-a)))
              (abs (- b-pos (clength total-b)))))
       (reconstruct-path- (a b)
         (if (and (zerop a) (zerop b))
             (make-costed)
             (destructuring-bind ((new-a . new-b) . edge) (aref from a b)
               (ccons edge (reconstruct-path- new-a new-b)))))
       (reconstruct-path (last-a last-b a b)
         (creverse
          ;; Handle cdr of final cons.  This must be special-cased
          ;; because when nil (a list) this is ignored by functions
          ;; expecting lists (not cons trees).
          (if (ast-equal-p last-a last-b)
              (ccons
               (ccons (make-costed :obj :same) last-a :cost 0)
               (reconstruct-path- a b))
              (ccons (ccons (make-costed :obj :insert) last-b)
                     (ccons (ccons (make-costed :obj :delete) last-a)
                            (reconstruct-path- a b)))))))

    (setf (aref g 0 0) 0
          (aref f 0 0) (heuristic 0 0)
          (aref open 0 0) t
          total-open (1+ total-open))

    (enqueue fringe (cons total-a total-b)
             (aref f 0 0))

    (do ((current (dequeue fringe) (dequeue fringe)))
        ((zerop total-open)
         (reconstruct-path (clast total-a) (clast total-b)
                           (clength total-a) (clength total-b)))

      (let* ((a (car current)) (b (cdr current))
             (pos-a (- (clength total-a) (clength a)))
             (pos-b (- (clength total-b) (clength b))))

        (when (and (zerop (clength a))
                   (zerop (clength b)))
          (reconstruct-path a b (clength a) (clength b)))

        (when (aref open pos-a pos-b) (decf total-open))
        (setf (aref open pos-a pos-b) nil
              (aref closed pos-a pos-b) t)

        (labels                         ; Handle all neighbors.
            ((add (neighbor edge)
               (destructuring-bind (next-a next-b)
                   (list (- (clength total-a) (clength (car neighbor)))
                         (- (clength total-b) (clength (cdr neighbor))))
                 (unless (aref closed next-a next-b)
                   (unless (aref open next-a next-b) (incf total-open))
                   (setf (aref open next-a next-b) t)
                   (let ((tentative
                          (+ (aref g pos-a pos-b)
                             (ccost edge)))
                         (value (aref g next-a next-b)))
                     ;; Neighbor is an improvement.
                     (when (or (null value) (< tentative value))
                       (setf (aref from next-a next-b)
                             (cons (cons pos-a pos-b) edge)
                             (aref g next-a next-b) tentative
                             (aref f next-a next-b)
                             (+ tentative (heuristic next-a next-b)))
                       (enqueue fringe neighbor
                                (aref f next-a next-b))))))))

          ;; Check neighbors: diagonal, recurse, insert, delete.
          (when (and (cconsp a) (cconsp b))
            (cond
              ((ast-equal-p (ccar a) (ccar b)) ; Diagonal.
               (add (cons (ccdr a) (ccdr b))
                    (ccons (make-costed :obj :same) (ccar a) :cost 0)))
              ((ast-can-recurse (ccar a) (ccar b)) ; Recurse.
               (add (cons (ccdr a) (ccdr b))
                    (ccons (make-costed :obj :recurse)
                           (recursive-diff
                            (to-costed (ast-on-recurse (corig (ccar a))))
                            (to-costed (ast-on-recurse (corig (ccar b))))))))))
          (if (cconsp b)                ; Insert.
              (add (cons a (ccdr b))
                   (ccons (make-costed :obj :insert) (ccar b)))
              (add (cons a (make-costed))
                   (ccons (make-costed :obj :insert) b)))
          (if (cconsp a)                ; Delete.
              (add (cons (ccdr a) b)
                   (ccons (make-costed :obj :delete) (ccar a)))
              (add (cons (make-costed) b)
                   (ccons (make-costed :obj :delete) a))))))))

(defmethod ast-diff ((ast-a t) (ast-b t))
  ;; Drop common prefix and postfix, just run the diff on different middle.
  (multiple-value-bind (unique-a unique-b prefix postfix)
      (remove-common-prefix-and-suffix (ast-on-recurse ast-a)
                                       (ast-on-recurse ast-b))
    ;; NOTE: We assume that the top level is a list (not a cons tree).
    ;; This is true for any ASTs parsed from a source file as a
    ;; sequence of READs.
    (labels ((add-common (diff cost)
               ;; Some special handling is required to interface
               ;; between the list model of the common pre-/post-fixes
               ;; and the cons-tree model of the calculated diff.
               (values
                (let ((diff (if (equal '(:same) (lastcar diff))
                                (butlast diff)
                                diff)))
                  (let ((diff (if prefix
                                  (append (mapcar (lambda (it) (cons :same it)) prefix)
                                          diff)
                                  diff)))
                    (if postfix
                        (append diff
                                (mapcar (lambda (it) (cons :same it)) postfix))
                        diff)))
                cost)))
      (unless (or unique-a unique-b)
        (return-from ast-diff
          (multiple-value-call #'add-common
            (values nil 0))))
      (when (null unique-a)
        (return-from ast-diff
          (multiple-value-call #'add-common
            (values (mapcar (lambda (el) (cons :insert el)) unique-b)
                    (1- (ccost (to-costed unique-b))))))) ; 1- for trailing nil.
      (when (null unique-b)
        (return-from ast-diff
          (multiple-value-call #'add-common
            (values (mapcar (lambda (el) (cons :delete el)) unique-a)
                    (1- (ccost (to-costed unique-a))))))) ; 1- for trailing nil.

      (multiple-value-call #'add-common
        (from-costed (recursive-diff (to-costed unique-a) (to-costed unique-b)))))))

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
#+(or )
(iter (for base in '(2 10 20 50 100 200 500 1000 2000 5000))
                (let ((orig (iota base))
                      (other (iota base)))
                  (setf (nth 1 other) 'x
                        (nth (1- base) other) 'z)
                  (format t "Base:~A~%" base)
                  (time (dotimes (n 10) (ast-diff orig other)))))
;; SBCL 1.4.6
;; ============================================================
;; size	runtime	bytes-consed
;; 2	0.000	32752
;; 10	0.001	687904
;; 20	0.005	813248
;; 50	0.029	973968
;; 100	0.127	326336
;; 200	0.561	251744
;; 500	2.296	680432
;; -------------------------[memory bottleneck]
;; 1000	36.370	382080
;; 2000	188.012	716048
;; ============================================================
;;
;; CCL Version 1.12-dev  LinuxX8664
;; ============================================================
;; size	runtime		bytes-consed
;; 2	0.000095	18560
;; 10	0.002498	677760
;; 20	0.015647	2815360
;; 50	0.096857	17964160
;; 100	0.531638	72332160
;; 200	2.022867	290268160
;; 500	13.442549	1817676147
;; 1000	73.954506	7275356147
;; ============================================================

(defun ast-diff-elide-same (edit-script)
  "Return the non-same subset of EDIT-SCRIPT with path information.
Path's are represented as a sequence of car (:A) and cdr (:D) from the
root of the edit script (and implicitly also the program AST)."
  ;; TODO: Run length compression of these paths.
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
         (when (and script
                    ;; NOTE: Again handle the difference between
                    ;; top-level lists and sub-element cons-trees.
                    (not (and (null asts) (equal '(:same) (car script)))))
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

(defun print-diff (diff &optional
                          (stream *standard-output*)
                          (delete-start "[-")
                          (delete-end "-]")
                          (insert-start "{+")
                          (insert-end "+}"))
  (let ((*print-escape* nil))
    (mapc (lambda-bind ((type . content))
            (ecase type
              (:same (write (ast-text content) :stream stream))
              (:delete (write delete-start :stream stream)
                       (write (ast-text content) :stream stream)
                       (write delete-end :stream stream))
              (:insert (write insert-start :stream stream)
                       (write (ast-text content) :stream stream)
                       (write insert-end :stream stream))
              (:recurse (print-diff content stream delete-start delete-end insert-start insert-end))))
          (if (equalp '(:same) (lastcar diff)) (butlast diff) diff))))
