;;; ast-diff.lisp --- diffs between ASTs and other tree structures
;;
;; From http://thume.ca/2017/06/17/tree-diffing/#a-tree-diff-optimizer
;;
;;; TODO:
;; Costed structure
;; - cache s expression length and cost
;; - struct outside algorithm
;;
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
   :ast-diff
   :apply-edit-script
   :diff-elide-same
   :diff-to-html
   :ast-interface
   :ast-equal-p
   :ast-cost
   :ast-can-recurse
   :ast-on-recurse
   :ast-text
   :costed-list
   :ccar
   :ccdr
   :clength
   :ccost))
(in-package :software-evolution-library/ast-diff)
(in-readtable :curry-compose-reader-macros)

(defclass ast-interface ()
  ((equal-p :initarg :equal-p
            :documentation "Function to check if two subtrees are equal.")
   (cost :initarg :cost
         :documentation "Function to calculate cost of an AST subtree.")
   (can-recurse :initarg :can-recurse
                :documentation "Function to check if a subtree is atomic.")
   (on-recurse :initarg :on-recurse :initform #'identity
               :documentation "Function to apply on recursion.")
   (text :initarg :text
         :documentation "Function to return the text of a subtree."))
  (:documentation "Collection of functions for interfacing with ast-diff."))

(defmethod ast-equal-p ((interface ast-interface) ast-a ast-b)
  (funcall (slot-value interface 'equal-p) ast-a ast-b))

(defmethod ast-cost ((interface ast-interface) ast)
  (funcall (slot-value interface 'cost) ast))

(defmethod ast-can-recurse ((interface ast-interface) ast-a ast-b)
  (funcall (slot-value interface 'can-recurse) ast-a ast-b))

(defmethod ast-on-recurse ((interface ast-interface) ast)
  (funcall (slot-value interface 'on-recurse) ast))

(defmethod ast-text ((interface ast-interface) ast)
  (funcall (slot-value interface 'text) ast))

(defstruct (costed-list (:conc-name c))
  "Lists with a cost associated with every cons."
  ;; Used below to both:
  ;; - hold edit scripts with accumulating costs.
  ;; - hold costed versions of the input ASTs with associated costs.
  (car nil)
  (cdr nil)
  (length 0 :type fixnum)
  (cost 0 :type fixnum))

(defmethod ccons ((car costed-list) (cdr costed-list))
  (make-costed-list :car car
                    :cdr cdr
                    :length (1+ (clength cdr))
                    :cost (+ (ccost car) (ccost cdr))))

(defun list-to-costed-list (interface ast)
  (reduce (lambda (acc el)
            (if (consp el)
                (ccons (list-to-costed-list interface el)
                       acc)
                (ccons (make-costed-list
                        :car el
                        :length 1
                        :cost (ast-cost interface el))
                       acc)))
          (reverse (tree-right-walk ast))
          :initial-value (make-costed-list)))

(defmethod costed-list-to-list ((clist costed-list))
  (labels ((to-list (clist)
             (cons (if (costed-list-p (ccar clist))
                       (costed-list-to-list (ccar clist))
                       (ccar clist))
                   (if (costed-list-p (ccdr clist))
                       (costed-list-to-list (ccdr clist))
                       (ccdr clist)))))
    (values (to-list clist) (ccost clist))))

(defmethod cnull ((clist costed-list))
  (zerop (clength clist)))

;;; TODO: Fix this nonsense:
;; SEL/TEST> (ast-diff *sexp-diff-interface* '(1 2 3 4) '(1 2 (2 4 5) 3 88))
;; ((:SAME . 1) (:SAME . 2) (:DELETE . 3) (:DELETE . 4) NIL)
;; 14
;; SEL/TEST> (ast-diff *sexp-diff-interface* '(1 2 3 4 88) '(1 2 (2 4 5) 3 88))
;; ((:SAME . 1) (:SAME . 2) (:INSERT 2 4 5) (:SAME . 3) (:DELETE . 4) (:SAME . 88)
;;  NIL)
;; 4
;; TODO: Also, the apply-append thing is gross.
(defun ast-diff (interface ast-a ast-b)
  "Return a least-cost edit script which transforms AST-A into AST-B.
Also return a second value indicating the cost of the edit.

See APPLY-EDIT-SCRIPT for more details on edit scripts.

INTERFACE is an AST-INTERFACE object containing the functions need to
determine equality, cost, etc for ASTs.

ASTs should be AST structs from above or cons cells where the CAR
stores the node data (not used directly by the diff algorithm but
may be examined by the interface functions) and the CDR is the children.
"
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
  ;; avoid redundant computation. Each dimension is padded by 1 to
  ;; simplify boundary conditions.
  (declare (optimize speed))
  (let* ((costed-a (list-to-costed-list interface (ast-on-recurse interface ast-a)))
         (costed-b (list-to-costed-list interface (ast-on-recurse interface ast-b)))
         (maximum-cost (+ (ccost costed-a) (ccost costed-b) 1)))
    (labels
        ((recursive-diff (costed-a costed-b)
           (let ((costs (make-array (list (clength costed-a)
                                          (clength costed-b))
                                    :initial-element nil)))
             ;; Compute diff from start (top,left) to the target (bottom,right).
             (let ((it (compute-diff costed-a costed-b costs)))
               ;; (format t "RECURSIVE:~S~%" it)
               it)))

         (memoized-compute-diff (costed-a costed-b costs)
           ;; (format t "COMPUTE-DIFF:~S~%" (list costed-a costed-b costs))
           ;; Finished when both trees are empty.
           (when (and (cnull costed-a)
                      (cnull costed-b))
             (return-from memoized-compute-diff (make-costed-list)))
           ;; Moving off the grid is illegal.
           (when (or (cnull costed-a)
                     (cnull costed-b))
             ;; Compute a maximum cost.
             ;; (format t "OUTSIDE:~S~%" (make-costed-list :cost maximum-cost))
             (return-from memoized-compute-diff
               (make-costed-list :cost maximum-cost)))

           (destructuring-bind (pos-a pos-b)
               (destructuring-bind (size-a size-b) (array-dimensions costs)
                 (declare (type fixnum size-a))
                 (declare (type fixnum size-b))
                 (list (- size-a (clength costed-a))
                       (- size-b (clength costed-b))))

             ;; Use memoized value if available.
             (when-let ((memoized (aref costs pos-a pos-b)))
               ;; (format t "MEMOIZED:~S~%" memoized)
               (return-from memoized-compute-diff memoized))

             (setf (aref costs pos-a pos-b)
                   (compute-diff costed-a costed-b costs))))

         (compute-diff (costed-a costed-b costs)
           (declare (type (SIMPLE-ARRAY T) costs))
           (let ((insert (ccons (multiple-value-bind (list cost)
                                    (costed-list-to-list (ccar costed-b))
                                  (make-costed-list :car :insert :cdr (apply #'append list) :cost cost))
                                (memoized-compute-diff costed-a (ccdr costed-b) costs)))
                 (delete (ccons (multiple-value-bind (list cost)
                                    (costed-list-to-list (ccar costed-a))
                                  (make-costed-list :car :delete :cdr (apply #'append list) :cost cost))
                                (memoized-compute-diff (ccdr costed-a) costed-b costs)))
                 diagonal)

             ;; Try diagonal is the cheapest when heads are equal.
             (if (ast-equal-p interface (ccar costed-a) (ccar costed-b))
                 (let ((same (ccons (make-costed-list
                                     :car :same
                                     :cdr (apply #'append (costed-list-to-list (ccar costed-a)))
                                     :cost 0)
                                    (setf diagonal
                                          (memoized-compute-diff
                                           (ccdr costed-a) (ccdr costed-b)
                                           costs)))))
                   (when (and (<= (ccost same) (ccost insert))
                              (<= (ccost same) (ccost delete)))
                     (return-from compute-diff same)))
                 ;; Try recursion if not same can recurse.
                 (when (ast-can-recurse interface (ccar costed-a) (ccar costed-b))
                   (let* ((subtree (recursive-diff (ccar costed-a) (ccar costed-b)))
                          (recurse (ccons (multiple-value-bind (list cost)
                                              (costed-list-to-list subtree)
                                            (make-costed-list
                                             :car :recurse
                                             :cdr (apply #'append list)
                                             :cost cost))
                                          diagonal)))
                     (when (and (<= (ccost recurse) (ccost insert))
                                (<= (ccost recurse) (ccost delete)))
                       (return-from compute-diff recurse)))))
             ;; Else return the cheapest of insert or delete.
             (if (< (ccost insert) (ccost delete))
                 insert
                 delete))))
      (costed-list-to-list (recursive-diff costed-a costed-b)))))

(defun diff-elide-same (edit-script)
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

(defgeneric apply-edit-script (interface original script)
  (:documentation "Create an edited AST by applying SCRIPT to ORIGINAL.

An edit script is a sequence of actions, of the following types:
:same A B  : keep the current AST
:insert B  : insert B at the current position
:remove A  : remove the current AST
:recurse S : recursively apply script S to the current AST"))

(defmethod apply-edit-script ((interface ast-interface)
                              (original list)
                              (script list))
  (labels
      ((edit (asts script)
         (when script
           (destructuring-bind (action . args) (car script)
             (ecase action
               (:recurse (cons (apply-edit-script interface (car asts)
                                                  args)
                               (edit (cdr asts) (cdr script))))
               (:same (assert (apply #'ast-equal-p interface args))
                      (cons (car asts)
                            (edit (cdr asts) (cdr script))))
               (:delete (assert (ast-equal-p interface (car asts) args))
                        (edit (cdr asts) (cdr script)))
               (:insert (cons args
                              (edit asts (cdr script)))))))))
    (cons (car original) (edit (cdr original) script))))
