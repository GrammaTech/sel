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
   :ast-hash
   :ast-combine-hash-values
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
   :print-diff
   ;; Merge functions
   :chunk
   :diff3
   :show-chunks))
(in-package :software-evolution-library/ast-diff)
(in-readtable :curry-compose-reader-macros)


;;; Supporting structures.
(defstruct (costed (:conc-name c))
  "Objects with a cost and length."
  (obj nil)
  (orig nil)
  (length 0 :type fixnum)
  (cost 0 :type fixnum)
  (hash nil :type (or null fixnum)))

#+sbcl (declaim (optimize sb-cover:store-coverage-data))

;; (declaim (inline reduce-on))
(defun reduce-on (combining-fn tail-fn x &key (recur-p #'consp) (next #'cdr))
  "Use COMBINING-FN to combine, last to first, the values obtained by calling NEXT
zero or more times on X, until RECUR-P is false, at which point TAIL-FN is applied
instead to that last value."
  (let ((stack nil))
    (iter (while (funcall recur-p x))
	  (push x stack)
	  (setf x (funcall next x)))
    (let ((result (funcall tail-fn x)))
      (iter (while stack)
	    (setf result (funcall combining-fn (pop stack) result)))
      result)))

(defun to-costed (obj)
  (labels ((recurse (obj)
	     (reduce-on
	      (lambda (obj cdr) (ccons (recurse (car obj)) cdr :orig obj))
	      (lambda (obj) (make-costed :obj obj :orig obj :cost (ast-cost obj)))
	      obj)))
    (recurse obj)))

(defmethod from-costed ((costed costed))
  (labels ((recurse (costed)
	     (reduce-on
	      (lambda (costed rest) (cons (recurse (ccar costed)) rest))
	      (lambda (x) (if (costed-p x) (or (corig x) (recurse (cobj x))) x))
	      costed
	      :recur-p (lambda (x) (and (costed-p x) (not (corig x)) (cconsp x)))
	      :next #'ccdr)))
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
  (iter (while (cconsp costed))
	(setf costed (ccdr costed)))
  costed)


;;; Interface functions.
(defgeneric ast-equal-p (ast-a ast-b)
  (:documentation "Return T AST-A and AST-B are equal for diffing."))

(defmethod ast-equal-p ((ast-a t) (ast-b t))
  (equalp ast-a ast-b))

(defmethod ast-equal-p ((ast-a cons) (ast-b cons))
  (and (iter (while (consp ast-a))
	     (while (consp ast-b))
	     (always (ast-equal-p (pop ast-a) (pop ast-b))))
       (ast-equal-p ast-a ast-b)))

(defmethod ast-equal-p ((ast-a costed) (ast-b costed))
  (ast-equal-p (cobj ast-a) (cobj ast-b)))

(defgeneric ast-cost (ast)
  (:documentation "Return cost of AST."))

(defmethod ast-cost ((ast t))
  1)

(defmethod ast-cost ((ast cons))
  (+ (iter (sum (ast-cost (pop ast)))
	   (while (consp ast)))
     (ast-cost ast)))

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
  (format nil "~A" ast))

(defmethod ast-text ((ast costed))
  (ast-text (cobj ast)))

(defmethod ast-text ((ast string))
  ast)

(defmethod ast-text ((ast cons))
  (concatenate 'string (ast-text (car ast)) (ast-text (cdr ast))))

(defgeneric ast-hash (ast)
  (:documentation "A hash value for the AST, which is a nonnegative
integer.  It should be the case that (ast-equal-p x y) implies
(eql (ast-hash x) (ast-hash y)), and that if (not (ast-equal-p x y))
then the equality of the hashes is unlikely."))

(defconstant +ast-hash-base+ (- (ash 1 56) 5)
  "A prime that is close to a power of 2")

;; All hash values are of typer HASH-TYPE.
;; This was chosen to be large enough that collisions
;; are unlikely.  However, a collision can be expected
;; if hashing more than about (ash 1 28) (~ 256 million)
;; ASTs.  The value was chosen so the base is a fixnum
;; in both SBCL and CCL (64 bit).
(deftype hash-type () '(integer 0 (#.(- (ash 1 56) 5))))

(let ((a-coeffs
       (make-array '(32)
		   :element-type 'hash-type
		   :initial-contents
		   '(44772186367934537 40884819141045381 18268751919527175
		     12224412045766723 44747874473306482 6291300198851882
		     38208267184329 70824722016654862 68884710530037769
		     29266014118849078 16305173046113233 25526167110167858
		     69548398139113011 11845686404586539 13141703249234454
		     58585138257101406 63771603587465066 51818145761636769
		     11215313718595996 967321057564179 35579009383009840
		     21233262920564958 27885154493097833 45638112046788574
		     71667767543649984 11593336377822139 39832262451031385
		     64366124578464487 48093511540653115 11187607290745617
		     1718667612180730 55488393644215208)))

      (b-coeffs
       (make-array '(32)
		   :element-type 'hash-type
		   :initial-contents
		   '(15306130497698622 6962715537831413 23627614633074126
		     35426347469777435 6253504779322026 2685667771697079
		     12213574155663012 62015044820424341 63393789689534801
		     69752150146675013 21434622207040062 43200883849464758
		     23422157842437395 36720647208217461 67805387065755295
		     66857677050011714 71090740635621717 70425600738754230
		     56933545028670640 59684532028279319 54864461040550518
		     69504815912533426 35116612914715710 41513442981972055
		     4229361750527463 40744199140651635 33853319307875640
		     16951454121230159 31253281007319553 32992004582179554
		     13913708511125320 47256219783059968)))
      (p 13211719))

  (declare (type (and simple-array (vector hash-type 32)) a-coeffs b-coeffs))
  
  ;; functions, methods defined here can use a-coeffs, b-coeffs
  ;; at lower cost than special variables

  (defun ast-combine-hash-values (&rest args)
    "Given a list of hash values, combine them using a polynomial in P,
modile +AST-HASH-BASE+"
    (let ((result 0)
	  (hb +ast-hash-base+))
      (declare (type hash-type result))
      (iter (for i from 0 below (ash 1 30))
	    (for hv in args)
	    (let* ((im (logand i 31))
		   (a (aref a-coeffs im))
		   (b (aref b-coeffs im)))
	      ;; RESULT is squared to avoid linearity
	      ;; Without this, trees that have certain permutations of leaf
	      ;; values can be likely to hash to the same integer.
	      (setf result (mod (+ i b (* a hv) (* result result p)) hb))))
      result))
  
  (defmethod ast-hash ((i integer))
    (let ((c1 34188292748050745)
	  (c2 38665981814718286))
      (mod (+ (* c1 i) c2) +ast-hash-base+)))

  ;; could have specialized methods on strings
  ;; to speed up that common case
  (defmethod ast-hash ((s vector))
    (apply #'ast-combine-hash-values
	   38468922606716016
	   (length s)
	   (map 'list #'ast-hash s)))

  (defmethod ast-hash ((l cons))
    ;; Assumes not a circular list
    (apply #'ast-combine-hash-values
	   16335929882652762
	   (iter
	    (collect (if (consp l)
			 (ast-hash (car l))
			 ;; add a constant to distinguish (X Y)
			 ;; from (X . Y)
			 (+ 41019876016299766
			    (ast-hash l))))
	    (while (consp l))
	    (pop l))))

  (defmethod ast-hash ((n null))
    46757794301535766)

  (defmethod ast-hash ((c character))
    (let ((c1 3310905730158464)
	  (c2 4019805890044232))
      (mod (+ (* c1 (char-int c)) c2) +ast-hash-base+)))

  (defmethod ast-hash ((s symbol))
    (or (get s 'hash)
	(setf (get s 'hash)
	      (ast-combine-hash-values
	       30932222477428348
	       (ast-hash (symbol-package s))
	       (ast-hash (symbol-name s))))))

  (defmethod ast-hash ((p package))
    (ast-hash (package-name p)))

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

  )


(defun ast-hash-with-check (ast table)
  "Calls AST-HASH, but checks that if two ASTs have the same hash value,
they are actually equal.  If not, the second one gets a new, fresh hash
value that is used instead."
  (let* ((hash (ast-hash ast))
	 (old-ast (gethash hash table)))
    (when (and old-ast (not (ast-equal-p ast old-ast)))
      (iter (incf hash) ; this may be >= +ast-hash-base+, but that's ok
	    (while (gethash hash table)))
      (setf (gethash hash table) ast))
    hash))

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

;;; Simple queue.  This must be implemented in a library somewhere
;;; in Quicklisp.
(defun make-simple-queue ()
  (cons nil nil))

(defun simple-queue-dequeue (sq)
  (cond
    ((car sq) (pop (car sq)))
    ((cdr sq)
     (let ((r (nreverse (cdr sq))))
       (setf (car sq) (cdr r)
	     (cdr sq) nil)
       (car r)))
    (t nil)))

(defun simple-queue-enqueue (sq val)
  (push val (cdr sq)))    

(defun recursive-diff (total-a total-b &key (upper-bound most-positive-fixnum)
                       &aux
                         (from (make-cache total-a total-b))
			 ;; FRINGE is a queue used to order
			 ;; visits of 'open' nodes.  An open node should only
			 ;; be put on the queue when all its
			 ;; predecessors are closed.
                         ;; (fringe (make-instance 'priority-queue))
			 (fringe (make-simple-queue))
			 ;; When T, the node is stored in the priority queue already
                         (open (make-cache total-a total-b))
                         (total-open 0)
			 ;; When CLOSED is T, the node has been processed
                         (closed (make-cache total-a total-b))
			 ;; For closed nodes, G is the actual minimum cost
			 ;; of reaching the node.
                         (g (make-cache total-a total-b))
			 (r-cache (make-cache total-a total-b))
			 (lta (clength total-a))
			 (ltb (clength total-b))
			 )
  ;; UPPER-BOUND is a limit beyond which we give up on
  ;; pursuing edges.  This is not currently exploited.
  (labels
      ((%enqueue (node)
	 ;; (enqueue fringe node cost)
	 (simple-queue-enqueue fringe node)
	 )
       (%dequeue ()
	 ;; (dequeue fringe)
	 (simple-queue-dequeue fringe))
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
                            (reconstruct-path- a b))))))
       (%pos-a (a) (- lta (clength a)))
       (%pos-b (b) (- ltb (clength b))))

    (setf (aref g 0 0) 0  ;; initial node reachable at zero cost
          (aref open 0 0) t
          total-open (1+ total-open))

    (%enqueue (cons total-a total-b))

    (do ((current (%dequeue) (%dequeue)))
        ((zerop total-open)
         (reconstruct-path (clast total-a) (clast total-b) lta ltb))

      (let* ((a (car current)) (b (cdr current))
             (pos-a (%pos-a a))
             (pos-b (%pos-b b)))

        (when (and (zerop (clength a))
                   (zerop (clength b)))
          (reconstruct-path a b (clength a) (clength b)))

        (when (aref open pos-a pos-b) (decf total-open))
        (setf (aref open pos-a pos-b) nil
              (aref closed pos-a pos-b) t)

        (labels                         ; Handle all neighbors.
            ((add (neighbor edge)
	       (let ((next-a (%pos-a (car neighbor)))
		     (next-b (%pos-b (cdr neighbor))))
                 (unless (aref closed next-a next-b) ; should never happen?
		   (unless (aref open next-a next-b)
		     (incf total-open)
		     (setf (aref open next-a next-b) t))
		   (let ((tentative
			  (+ (aref g pos-a pos-b)
			     (ccost edge)))
			 (value (aref g next-a next-b)))
		     ;; Neighbor is an improvement.
		     (when (and (or (null value) (< tentative value))
				(< tentative upper-bound))
		       (setf (aref from next-a next-b)
			     (cons (cons pos-a pos-b) edge)
			     value tentative
			     (aref g next-a next-b) tentative))
		     ;; Only enqueue if ALL predecessors are closed
		     (when (and value
				(if (= next-a 0)
				    (aref closed next-a (1- next-b))
				    (and (aref closed (1- next-a) next-b)
					 (or (= next-b 0)
					     (and (aref closed (1- next-a) (1- next-b))
						  (aref closed next-a (1- next-b)))))))
		       (%enqueue neighbor))))))
	     (%recursive (a b)
	       (let ((i (%pos-a a))
		     (j (%pos-b b)))
		 (or (aref r-cache i j)
		     (setf (aref r-cache i j)
			   (recursive-diff
			    (to-costed (ast-on-recurse (corig (ccar a))))
			    (to-costed (ast-on-recurse (corig (ccar b))))))))))

          ;; Check neighbors: diagonal, recurse, insert, delete.
          (when (and (cconsp a) (cconsp b))
            (cond
              ((ast-equal-p (ccar a) (ccar b)) ; Diagonal.
               (add (cons (ccdr a) (ccdr b))
                    (ccons (make-costed :obj :same) (ccar a) :cost 0)))
              ((ast-can-recurse (ccar a) (ccar b)) ; Recurse.
               (add (cons (ccdr a) (ccdr b))
                    (ccons (make-costed :obj :recurse)
			   (%recursive a b))))))
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

(defun ast-diff-on-lists (ast-a ast-b)
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
        (return-from ast-diff-on-lists
          (multiple-value-call #'add-common
            (values nil 0))))
      (when (null unique-a)
        (return-from ast-diff-on-lists
          (multiple-value-call #'add-common
            (values (mapcar (lambda (el) (cons :insert el)) unique-b)
                    (1- (ccost (to-costed unique-b))))))) ; 1- for trailing nil.
      (when (null unique-b)
        (return-from ast-diff-on-lists
          (multiple-value-call #'add-common
            (values (mapcar (lambda (el) (cons :delete el)) unique-a)
                    (1- (ccost (to-costed unique-a))))))) ; 1- for trailing nil.

      (multiple-value-call #'add-common
        (from-costed (recursive-diff (to-costed unique-a) (to-costed unique-b)))))))

(defmethod ast-diff ((ast-a t) (ast-b t))
  (let ((new-ast-a (ast-on-recurse ast-a))
	(new-ast-b (ast-on-recurse ast-b)))
    (unless (and (proper-list-p new-ast-a) (proper-list-p new-ast-b))
      (return-from ast-diff (ast-diff-on-lists ast-a ast-b)))
    (setf ast-a new-ast-a ast-b new-ast-b))
  (let* ((table (make-hash-table))
	 (hashes-a (mapcar (lambda (ast) (ast-hash-with-check ast table)) ast-a))
	 (hashes-b (mapcar (lambda (ast) (ast-hash-with-check ast table)) ast-b))
	 (subseq-triples (good-common-subsequences hashes-a hashes-b))
	 diff-a common-a diff-b common-b)
    ;; split ast-a and ast-b into subsequences
    ;; Get lists of subsequences on which they differ, and subsequences on
    ;; which they are equal.  Some of the former may be empty.
    (setf (values diff-a common-a)
	  (split-into-subsequences ast-a (mapcar (lambda (x) (list (car x) (caddr x))) subseq-triples)))
    (setf (values diff-b common-b)
	  (split-into-subsequences ast-b (mapcar (lambda (x) (list (cadr x) (caddr x))) subseq-triples)))
    (assert (= (length diff-a) (length diff-b)))
    (assert (= (length common-a) (length common-b)))
    ;; (assert (= (length diff-a) (1+ (length common-a))))
    (let ((overall-diff nil)
	  (overall-cost 0))
      (iter (for da in (reverse diff-a))
	    (for db in (reverse diff-b))
	    (for ca in (reverse (cons nil common-a)))
	    ;; (for cb in (reverse (cons nil common-b)))
	    ;; (assert (ast-equal-p ca cb))
	    (multiple-value-bind (diff cost)
		(ast-diff-on-lists da db)
	      (when (and overall-diff (equalp (lastcar diff) '(:same)))
		(assert (>= cost 1))
		(decf cost)
		(setf diff (butlast diff)))
	      (setf overall-diff (append diff overall-diff))
	      (incf overall-cost cost))
	    (setf overall-diff (append (mapcar (lambda (it) (cons :same it)) ca) overall-diff)))
      (values overall-diff overall-cost))))

(defun split-into-subsequences (seq subseq-indices &aux (n (length seq)))
  "Given list SEQ and a list of pairs SUBSEQ-INDICES, which are start/length indices
for disjoint nonempty subsequences of SEQ, return a list of the N+1 subsequences between these
subsequences (some possibly empty), as well as the N subsequences themselves."
  (assert (every (lambda (x) (and (<= 0 (car x)) (< (car x) n) (<= 1 (cadr x))))
		 subseq-indices))
  (assert (every (lambda (x y) (<= (+ (car x) (cadr x)) (car y)))
		 subseq-indices (cdr subseq-indices)))
  (let ((pos 0)
	(common nil)
	(diff nil))
    (iter (for (start len) in subseq-indices)
	  (push (subseq seq pos start) diff)
	  (push (subseq seq start (+ start len)) common)
	  (setf pos (+ start len)))
    (values (nreconc diff (list (subseq seq pos))) (nreverse common))))


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

;;; These numbers are obsolete; re-run them.

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


;;; Merge algorithms
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
          ;; TODO: Unstable Cases:
          ;; - changed only in A
          ;; - changed only in B
          ;; - falsely conflicting
          ;; - truly conflicting
          chunk)))               ; Already labeled :unstable.
     (chunk (ast-diff original branch-a)
            (ast-diff original branch-b)))))

;;; TODO: printing clang-ast-node should use a safer printer ~s.
(defun show-chunks (chunks &optional (stream t))
  (mapc (lambda (chunk)
          (if (keywordp (car chunk))
              (ecase (car chunk)
                (:stable (format stream "~a" (cdr chunk)))
                (:unstable (format stream "+{UNSTABLE}+")))
              (show-chunks chunk)))
        chunks))

;;; Find "good" common subsequences of two sequences
;;; This is intended to run in linear time

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
		  (let ((m1 (car (gethash (car p1) table2)))
			(m2 (car (gethash (car p2) table1))))
		    (if (<= (- m2 pos1) (- m1 pos2))
			(chop1)
			(chop2))))))
	(if (> len 0)
	    (nreconc result (list (list start1 start2 len)))
	    (nreverse result))))))

;;; Comments on further algorithm improvements
;;;
;;; The "good enough" algorithm could be made slightly better
;;; by allowing limited lookahead.  With k lookahead it could
;;; run in O(k max(m,n)) time, m and n the lengths of the sequences.
;;;
;;; The hash function has not been fully tuned for speed.
;;;
;;; RECURSIVE-DIFF has an UPPER-BOUND argument.  This is not used
;;; now, but could be used to speed up the slow part of the algorithm
;;; if we want an exact solution rather than the "good enough" solution.
;;; Use the cost of the "good enough" solution to provide an upper bound
;;; for the exact solution.   Also, recursive calls could provide their
;;; own upper bounds based on the optimum path to the node from which
;;; the call is being made.
;;;
;;; The dynamic programming algorithm uses O(mn) space.  It can be
;;; changed to use linear space.  There are various schemes in the
;;; literature for doing LCS and edit distance in linear space.
;;; A simple one is as follows: note that we can compute the length
;;; of the LCS (or the edit distance) in linear space by scanning
;;; the array keeping just two rows.  We cannot reconstruct the
;;; edit from this, but we can record which entry on the m/2 row
;;; was in the final optimal solution.  Once we have done that,
;;; the parts before and after that point can be recomputed
;;; recursively.
;;;
;;; The speed on C programs is now dominated by the time needed
;;; for Clang to parse the C and send the AST to Lisp.
;;;
;;; It may be useful to have a hash function on ASTs that produces
;;; smaller integers, and use ast-hash-with-check to handle collisions.
;;; This could be tied in with a general mechanism for hash consing
;;; of ASTs.

#+sbcl (declaim (optimize (sb-cover:store-coverage-data 0)))
