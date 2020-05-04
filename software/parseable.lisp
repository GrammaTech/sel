;;; parseable.lisp --- Software which may be parsed into ASTs
(defpackage :software-evolution-library/software/parseable
  (:nicknames :sel/software/parseable :sel/sw/parseable)
  (:use :gt/full
        :metabang-bind
        :cl-store
        :bordeaux-threads
        :software-evolution-library
        :software-evolution-library/software/file)
  (:export ;; ASTs
           :ast
           :functional-tree-ast
           :to-alist
           :from-alist
           :ast-path
           :ast-stored-hash
           :ast-class
           :ast-annotation
           :ast-annotations
           :ast-children
           :ast-to-list
           :ast-equal-p
           :ast-hash
           :ast-later-p
           :conflict-ast
           :conflict-ast-child-alist
           :conflict-ast-default-children
           :combine-conflict-asts
           :source-text
           :rebind-vars
           :convert-list-to-ast-helper
           ;; Parseable software object.
           :parseable
           :asts
           :*parseable-mutation-types*
           :parseable-mutation
           :parseable-insert
           :parseable-swap
           :parseable-move
           :parseable-replace
           :parseable-cut
           :parseable-nop
           ;; Generic functions.
           :roots
           :get-ast
           :get-parent-ast
           :get-parent-asts
           :get-ast-types
           :get-unbound-vals
           :get-unbound-funs
           :enclosing-scope
           :scopes
           :get-vars-in-scope
           :update-caches
           :parse-asts
           :clear-caches
           :update-caches-if-necessary
           :bad-asts
           ;; :good-stmts
           :good-mutation-targets
           :bad-mutation-targets
           ;; :mutation-targets
           :pick-general
           :recontextualize-mutation
           :recontextualize
           :select-crossover-points
           :traceable-stmt-p
           :can-be-made-traceable-p
           :enclosing-traceable-stmt
           :parent-ast-p
           :get-children
           :get-immediate-children
           :prepend-text-to-genome
           :append-text-to-genome-preamble
           :append-text-to-genome
           :index-of-ast
           :ast-at-index
           ;; Mutation wrappers
           :insert-ast
           :remove-ast
           :replace-ast
           ;; Restarts
           :expand-stmt-pool))
(in-package :software-evolution-library/software/parseable)
(in-readtable :curry-compose-reader-macros)

(define-software parseable (software file)
  ((genome   :initarg :genome :accessor genome :initform ""
             :documentation "Lazily parsed AST representation of the code.")
   (asts     :initarg :asts :reader asts
             :initform nil :copier :direct
             :type #-sbcl list #+sbcl (or null (cons (cons keyword *) *))
             :documentation "List of all ASTs."))
  (:documentation "Parsed AST tree software representation."))


;;; AST data structure definitions.
(defclass ast () ()
  (:documentation "Base class for all ASTs in SEL.  This class acts as a tag
for objects to allow method dispatch on generic AST objects regardless of
whether they inherit from the functional trees library."))

(defclass functional-tree-ast (node ast)
  ((class :initarg :class :initform nil :reader ast-class
          :documentation "Class of the AST." :type symbol)
   (annotations :initarg :annotations :initform nil :reader ast-annotations
                :documentation "A-list of annotations." :type list)
   (stored-hash :initarg :stored-hash :initform nil
                :documentation "A cached hash." :type (or null hash-type)))
  (:documentation "Base class for SEL functional tree ASTs.
An applicative tree structure is used to hold the ASTs."))

(defclass conflict-ast (functional-tree-ast)
  ((child-alist :initarg :child-alist :initform nil
                :reader conflict-ast-child-alist
                :documentation "Child-Alist of the AST." :type list)
   (default-children :initarg :default-children :initform nil
                     :reader conflict-ast-default-children
                     :documentation "Default-Children of the AST." :type list))
  (:documentation "Node representing several possibilities for an AST.
The mapping from a conflicted AST into a regular AST is as follows: for
a given conflict key, and for each conflict node, get the list of children
corresponding to that key (default if the key is not present), and splice
that list of children in place of the conflict node in its parent's children
list."))

(defparameter *ast-print-cutoff* 20
  "Maximum number of characters to print for TEXT in
PRINT-OBJECT method on AST structures.")

(defmethod print-object ((obj functional-tree-ast) stream
                         &aux (cutoff *ast-print-cutoff*))
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a :PATH ~s :TEXT ~s"
                (ft::serial-number obj)
                (ast-path obj)
                (let* ((text (source-text obj))
                       (truncated
                        (if (> (length text) cutoff)
                            (concatenate 'string (subseq text 0 cutoff) "...")
                            text)))
                  (if-let ((position (search (string #\Newline) truncated)))
                    (concatenate 'string (subseq truncated 0 position) "...")
                    truncated))))))

(defmethod print-object ((obj conflict-ast) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a :PATH ~s~:_ :CHILD-ALIST ~s"
                (ft::serial-number obj)
                (ast-path obj)
                (conflict-ast-child-alist obj)))))

(defgeneric ast-annotation (ast annotation)
  (:documentation "Return given AST ANNOTATION.")
  (:method ((ast ast) (annotation symbol))
    (aget annotation (ast-annotations ast))))

;; FIXME: When clang is converted to utilize functional trees,
;; this method specialization will no longer be required as we
;; can utilize `finger`'s in all locations.
(defgeneric ast-path (ast)
  (:documentation "Return the children of AST.")
  (:method ((ast ast))
    (when (finger ast)
      (path (finger ast)))))

;; FIXME: When clang is converted to utilize functional trees,
;; this method specialization will no longer be required as we
;; can utilize `children` in all locations.
(defgeneric ast-children (ast)
  (:documentation "Return the children of AST.")
  (:method ((ast ast))
    (children ast)))

;; FIXME: When clang is converted to utilize functional trees,
;; this method specialization will no longer be required as we
;; can utilize trees in all locations.
(defgeneric ast-to-list (ast)
  (:documentation "Return AST and its children as a list.")
  (:method ((ast ast))
    (nest (reverse)
          (reduce (lambda (accum a) (cons a accum)) ast)))
  (:method ((ast t)) nil))

(defgeneric combine-conflict-asts (ca1 ca2)
  (:documentation
   "Merge conflict ast nodes CA1 and CA2, their alists and default values."))

(defmethod combine-conflict-asts ((ca1 conflict-ast) (ca2 conflict-ast))
  (let ((al1 (copy-alist (conflict-ast-child-alist ca1)))
        (al2 (copy-alist (conflict-ast-child-alist ca2)))
        (def1 (conflict-ast-default-children ca1))
        (def2 (conflict-ast-default-children ca2)))
    ;; Previously we removed alist entries mapping to nil,
    ;; but this has been removed so even empty lists show up
    ;; in a merge
    ;; Build combined alist
    (iter (for p in al1)
          (let* ((k (car p))
                 (vals2 (aget k al2)))
            (if vals2
                (setf (cdr p) (append (cdr p) vals2))
                (setf (cdr p) (append (cdr p) def2)))))
    (let ((al (append al1
                      (iter (for p in al2)
                            (let* ((k (car p))
                                   (vals1 (aget k al1)))
                              (unless vals1
                                (collect (cons k (append def1 (cdr p))))))))))
      (make-instance 'conflict-ast
       :child-alist al
       :default-children (append def1 def2)))))


;;; AST equality and hashing
(defgeneric ast-equal-p (ast-a ast-b)
  (:documentation "Return T AST-A and AST-B are equal for differencing."))

(defmethod ast-equal-p ((ast-a ast) (ast-b ast))
  (and (eq (ast-class ast-a) (ast-class ast-b))
       (eql (length (ast-children ast-a))
            (length (ast-children ast-b)))
       (every #'ast-equal-p (ast-children ast-a) (ast-children ast-b))))

(defmethod ast-equal-p ((ast-a t) (ast-b t))
  (equal ast-a ast-b))

(defmethod ast-equal-p ((ast-a cons) (ast-b cons))
  (and (iter (while (consp ast-a))
             (while (consp ast-b))
             (always (ast-equal-p (pop ast-a) (pop ast-b))))
       (ast-equal-p ast-a ast-b)))

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

;;; FIXME: Add a comment describing how a-coeffs and b-coeffs were generated.
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

  (declare (type (or simple-array (vector hash-type 32)) a-coeffs b-coeffs))

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
              ;; Without this, trees that have certain permutations
              ;; of leaf values can be likely to hash to the same integer.
              (setf result (mod (+ i b (* a hv) (* result result p)) hb))))
      result))

  (defun ast-combine-simple-vector-hash-values (sv)
    (declare (type simple-vector sv))
    (let ((result 0)
          (hb +ast-hash-base+)
          (len (length sv)))
      (declare (type hash-type result))
      (iter (for i from 0 below len)
            (for hv in-vector sv)
            (let* ((im (logand i 31))
                   (a (aref a-coeffs im))
                   (b (aref b-coeffs im)))
              ;; RESULT is squared to avoid linearity
              ;; Without this, trees that have certain permutations of leaf
              ;; values can be likely to hash to the same integer.
              (setf result (mod (+ i b (* a hv) (* result result p)) hb))))
      result))

  (defmethod ast-hash ((x t)) 0)

  (defmethod ast-hash ((i integer))
    (let ((c1 34188292748050745)
          (c2 38665981814718286))
      (mod (+ (* c1 i) c2) +ast-hash-base+)))

  ;; could have specialized methods on strings
  ;; to speed up that common case
  (defmethod ast-hash ((s vector))
    (ast-combine-hash-values
     38468922606716016
     (length s)
     (ast-combine-simple-vector-hash-values (map 'simple-vector #'ast-hash s))))

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
    (ast-hash (package-name p))))

;;; We cache this for ast nodes otherwise the time
;;; for computing ast-hash on a large tree can become very large
(defmethod ast-hash ((ast ast))
  (or (slot-value ast 'stored-hash)
      (setf (slot-value ast 'stored-hash)
            (ast-hash (cons (ast-class ast) (ast-children ast))))))


;;; Generic functions on ASTs
(defgeneric to-alist (struct)
  (:documentation "Convert struct to alist representation."))

(defgeneric from-alist (symbol alist)
  (:documentation "Convert alist to struct representation."))

(defgeneric ast-later-p (ast-a ast-b)
  (:documentation "Is AST-A later in the genome than AST-B?
Use this to sort AST asts for mutations that perform multiple
operations.")
  (:method ((ast-a ast) (ast-b ast))
    (labels
        ((path-later-p (a b)
           (cond
             ;; Consider longer asts to be later, so in case of nested ASTs we
             ;; will sort inner one first. Mutating the outer AST could
             ;; invalidate the inner ast.
             ((null a) nil)
             ((null b) t)
             (t (bind (((head-a . tail-a) a)
                       ((head-b . tail-b) b))
                      (cond
                        ((> head-a head-b) t)
                        ((> head-b head-a) nil)
                        (t (path-later-p tail-a tail-b))))))))
      (path-later-p (ast-path ast-a) (ast-path ast-b)))))

(defgeneric source-text (ast)
  (:documentation "Source code corresponding to an AST.")
  (:method ((ast null)) "")
  (:method ((str string))
    str)
  (:method ((c character))
    (string c))
  (:method ((c conflict-ast))
    (with-output-to-string (s)
      (format s "<")
      (iter (for e on (conflict-ast-child-alist c))
            (format s "~a: " (caar e))
            (iter (for x in (cdar e)) (format s "~a" (source-text x)))
            (when (cdr e) (format s "|")))
      (format s ">")))
  (:method ((ast ast))
    ;; In performance comparison the combination of
    ;; `with-output-to-string' and `write-string' was faster than
    ;; alternatives using `format' (which was still pretty fast) and
    ;; using `concatenate' (which was slow).
    ;;
    ;; More importantly using (apply #'concatenate ...) runs into
    ;; problems as the number of ASTs is very large.
    (with-output-to-string (out)
      (mapc [{write-string _ out} #'source-text] (ast-children ast)))))

(defgeneric rebind-vars (ast var-replacements fun-replacements)
  (:documentation
   "Replace variable and function references, returning a new AST.")
  (:method ((ast string) var-replacements fun-replacements)
    (reduce (lambda (new-ast replacement)
              (replace-all new-ast (first replacement) (second replacement)))
            (append var-replacements
                    (mapcar (lambda (fun-replacement)
                              (list (car (first fun-replacement))
                                    (car (second fun-replacement))))
                            fun-replacements))
            :initial-value ast)))

(defun convert-list-to-ast-helper (spec fn)
  "Helper function for converting a list SPECification of an AST to an
AST using FN to create the AST.

SPEC: List specification of an AST.  A SPEC should have the form

  (ast-class <optional-keyword-args-to-`make-instance <AST-TYPE>'>
             CHILDREN)

where CHILDREN may themselves be specifications suitable for passing
to `to-ast`

FN: Function taking three arguments (class, keys, and children) and
returning a newly created AST."
  (destructuring-bind (class &rest options-and-children) spec
    (multiple-value-bind (keys children)
        (let ((previous nil))
          (iter (for item in options-and-children)
                (if (or (keywordp previous)
                        (keywordp item))
                    ;; Collect keyword arguments.
                    (collect item into keys)
                    ;; Process lists as new AST nodes.
                    (if (listp item)
                        (collect (convert-list-to-ast-helper item fn)
                                 into children)
                        (collect item into children)))
                (setf previous item)
                (finally (return (values keys children)))))
      (funcall fn class keys children))))


;;; parseable software objects
(defgeneric roots (obj)
  (:documentation "Return all top-level ASTs in OBJ."))

(defgeneric get-ast (obj path)
  (:documentation "Return the AST in OBJ at the given PATH."))

(defgeneric get-parent-ast (obj ast)
  (:documentation "Return the parent node of AST in OBJ"))

(defgeneric get-parent-asts (obj ast)
  (:documentation "Return the parent nodes of AST in OBJ"))

(defgeneric get-children (obj ast)
  (:documentation "Return all the children of AST in OBJ."))

(defgeneric get-immediate-children (obj ast)
  (:documentation "Return the immediate children of AST in OBJ."))

(defgeneric get-ast-types (software ast)
  (:documentation "Types directly referenced within AST."))

(defgeneric get-unbound-funs (software ast)
  (:documentation "Functions used (but not defined) within the AST."))

(defgeneric get-unbound-vals (software ast)
  (:documentation "Variables used (but not defined) within the AST."))

(defgeneric enclosing-scope (software ast)
  (:documentation "Returns enclosing scope of AST."))

(defgeneric scopes (software ast)
  (:documentation "Return lists of variables in each enclosing scope.
Each variable is represented by an alist containing :NAME, :DECL, :TYPE,
and :SCOPE.
"))

(defgeneric get-vars-in-scope (software ast &optional keep-globals)
  (:documentation "Return all variables in enclosing scopes."))

(defgeneric parse-asts (software)
  (:documentation "Parse genome of SOFTWARE into an AST representation.
There are some requirements for the ASTs constructed by this method:
* We require that *all* source text be stored as a raw string
  somewhere in the AST tree.  Source text tucked inside of a
  non-string AST-NODE will be ignored.
* We also require that if two ASTs have the same class and the same
  source text then they are equal.

Other methods in on parseable objects, specifically `ast-can-recurse'
and `ast-equal-p' depend on these invariants."))

(defgeneric clear-caches (software)
  (:documentation "Clear cached fields on SOFTWARE"))

(defgeneric update-caches-if-necessary (software)
  (:documentation "Update cached fields in SOFTWARE if these fields have
not been set."))

(defgeneric bad-asts (software)
  (:documentation "Return a list of all bad asts in SOFTWARE."))

(defgeneric good-asts (software)
  (:documentation "Return a list of all good asts in SOFTWARE."))

(defgeneric good-mutation-targets (software &key filter)
  (:documentation "Return a list of all good mutation targets in
SOFTWARE matching FILTER."))

(defgeneric bad-mutation-targets (software &key filter)
  (:documentation "Return a list of all bad mutation targets in
SOFTWARE matching FILTER."))

(defgeneric mutation-targets (software &key filter stmt-pool)
  (:documentation "Return a list of target ASTs in SOFTWARE from
STMT-POOL for mutation, filtering using FILTER, and throwing a
'no-mutation-targets exception if none are available."))

(defgeneric recontextualize-mutation (parseable mutation)
  (:documentation "Bind free variables and functions in the mutation to concrete
values.  Additionally perform any updates to the software object required
for successful mutation."))

(defgeneric recontextualize (parseable ast pt)
  (:documentation "Perform any modifications to AST (e.g. variable rebinding)
to allow for successful mutation of SOFTWARE at PT."))

(defgeneric select-crossover-points (a b)
  (:documentation "Select suitable crossover points in A and B.
If no suitable points are found the returned points may be nil."))

(defgeneric traceable-stmt-p (software ast)
  (:documentation
   "Return TRUE if AST is a traceable statement in SOFTWARE."))

(defgeneric can-be-made-traceable-p (software ast)
  (:documentation "Check if AST can be made a traceable statement in SOFTWARE."))

(defgeneric enclosing-traceable-stmt (software ast)
  (:documentation
   "Return the first ancestor of AST in SOFTWARE which may be a full stmt.
If a statement is reached which is not itself traceable, but which could be
made traceable by wrapping with curly braces, return that."))


;;; Core parseable methods
(defmethod initialize-instance :after ((obj parseable) &rest initargs)
  "If a GENOME of ASTs is given in the initialization,
ensure all ASTs have PATHs."
  (declare (ignorable initargs))
  (with-slots (genome) obj
    (when (typep genome 'ast)
      (setf genome (populate-fingers genome)))))

(defmethod size ((obj parseable))
  "Return the number of ASTs in OBJ."
  (length (asts obj)))

(defmethod genome-string ((obj parseable) &optional stream)
  "Return the source code of OBJ, optionally writing to STREAM"
  (let ((genome-string (if (stringp (slot-value obj 'genome))
                           (slot-value obj 'genome)
                           (source-text (slot-value obj 'genome)))))
    (if stream (write-string genome-string stream) genome-string)))

(defmethod genome :before ((obj parseable))
  "Lazily parse the genome upon first access."
  (when (stringp (slot-value obj 'genome))
    (setf (slot-value obj 'genome)
          (populate-fingers (parse-asts obj)))))

(defmethod (setf genome) :before (new (obj parseable))
  "Clear fitness and other caches prior to updating the NEW genome."
  (declare (ignorable new))
  (setf (slot-value obj 'fitness) nil)
  (clear-caches obj))

(defmethod (setf genome) :after ((new ast) (obj parseable))
  "Ensure fingers are populated after setting the NEW genome AST."
  (setf (slot-value obj 'genome) (populate-fingers new)))

(defmethod from-file ((obj parseable) path)
  "Initialize OBJ with the contents of PATH."
  (setf (genome obj) (file-to-string path))
  obj)

(defmethod from-string ((obj parseable) string)
  "Initialize OBJ with the contents of STRING."
  (setf (genome obj) string)
  obj)

(defparameter *show-update-asts-errors* nil
  "When true, update-asts reports the original source file on an error,
if the original file is known.")

(defmethod update-asts :around ((sw parseable))
  (handler-bind
      ((error (lambda (e)
                (declare (ignore e))
                (when *show-update-asts-errors*
                  (when-let ((ofile (original-path sw)))
                    (format t "Failure in update-asts: original-path = ~a~%"
                            ofile))))))
    (call-next-method)))

(defmethod asts :before ((obj parseable))
  "Ensure the `asts' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

;;; NOTE: The `update-caches' method assumes that the initial
;;;       top-level AST can be thrown away.
(defgeneric update-caches (software)
  (:documentation "Update cached fields in SOFTWARE.")
  (:method ((obj parseable))
    (labels ((collect-asts (tree)
               (cons tree
                     (iter (for c in (ast-children tree))
                           (appending (when (typep c 'ast)
                                        (collect-asts c)))))))
      (setf (slot-value obj 'asts)
            (cdr (collect-asts (genome obj)))))))

(defmethod update-caches-if-necessary ((obj parseable))
  "Update cached fields of OBJ if these fields have not been set."
  (with-slots (asts) obj (unless asts (update-caches obj))))

(defmethod clear-caches ((obj parseable))
  "Clear cached fields on OBJ such as `asts'.
* OBJ object to clear caches for.
"
  (with-slots (asts) obj
    (setf asts nil)))


;;; Retrieving ASTs
(defmethod roots ((obj parseable))
  "Return all top-level ASTs in OBJ.
* OBJ software object to search for roots
"
  (roots (asts obj)))

(defmethod roots ((asts list))
  "Return all top-level ASTs in ASTS.
* ASTS list of ASTs to search for roots
"
  #+sbcl (declare (optimize (speed 0))) ;; to avoid compiler note
  (remove-if-not [{= 1} #'length #'ast-path] asts))

(defgeneric ast-at-index (software index)
  (:documentation "Return the AST in OBJ at INDEX.
* OBJ object to retrieve ASTs for
* INDEX nth AST to retrieve
")
  (:method ((obj parseable) index)
    (nth index (asts obj))))

(defgeneric index-of-ast (software ast)
  (:documentation "Return the index of AST in OBJ.
* OBJ object to query for the index of AST
* AST node to find the index of
")
  (:method  ((obj parseable) (ast ast))
    (position ast (asts obj) :test #'equalp)))

(defmethod get-ast ((obj parseable) (path list))
  "Return the AST in OBJ at the given PATH.
* OBJ software object with ASTs
* PATH path to the AST to return"
  (let ((tree (genome obj))
        (pred nil))
    (iter (for i from 0)
          (for j in path)
          (unless (typep tree 'ast)
            (if pred
                (error "At path ~a, below~%~a, not an AST:~%~a"
                       (subseq path 0 i)
                       pred
                       tree)
                (error "Root of ~a is not an AST: ~a"
                       obj tree)))
          (unless (typep j '(integer 0))
            (error "Not a valid path index: ~a" j))
          (let ((children (ast-children tree)))
            (unless (< j (length children))
              (error "Not a valid child index for~%~a:~%~a" tree j))
            (setf pred tree tree (elt children j))))
    tree))

(defgeneric parent-ast-p (software possible-parent-ast ast)
  (:documentation "Return true if POSSIBLE-PARENT-AST is a parent of AST in OBJ, nil
otherwise.
* OBJ software object containing AST and its parents
* POSSIBLE-PARENT-AST node to find as a parent of AST
* AST node to start parent search from")
  (:method ((obj parseable) (possible-parent-ast ast) (ast ast))
    (member possible-parent-ast (get-parent-asts obj ast)
            :test #'equalp)))

(defmethod get-parent-ast ((obj parseable) (ast ast))
  "Return the parent node of AST in OBJ
* OBJ software object containing AST and its parent
* AST node to find the parent of
"
  (when-let ((path (butlast (ast-path ast))))
    (get-ast obj path)))

(defmethod get-parent-asts ((obj parseable) (ast ast))
  "Return the parent nodes of AST in OBJ
* OBJ software object containing AST and its parents
* AST node to find the parents of
"
  (labels ((get-parent-asts-helper (subtree path)
             (if (null path)
                 nil
                 (let ((new-subtree (nth (car path) (ast-children subtree))))
                   (cons new-subtree
                         (get-parent-asts-helper new-subtree
                                                 (cdr path)))))))
    (reverse (get-parent-asts-helper (genome obj) (ast-path ast)))))

(defmethod get-children ((obj parseable) (ast ast))
  "Return all the children of AST in OBJ.
* OBJ software object containing AST and its children
* AST node to find the children of
"
  (labels ((get-children-helper (obj ast)
             (when ast
               (mappend (lambda (child)
                          (cons child (get-children-helper obj child)))
                        (get-immediate-children obj ast)))))
    (get-children-helper obj ast)))

(defmethod get-immediate-children ((obj parseable) (ast ast))
  "Return the immediate children of AST in OBJ.
* OBJ software object containing AST and its children
* AST node to find the immediate children of
"
  (declare (ignorable obj)) ;; TODO: Remove obj as a parameter
  ;; Q: can we share structure with the list from AST-CHILDREN?
  (remove-if-not {typep _ 'ast} (ast-children ast)))

(defmethod get-vars-in-scope ((obj parseable) (ast ast)
                              &optional (keep-globals t))
  "Return all variables in enclosing scopes.
* OBJ software object containing AST and its enclosing scopes
* AST node to find variables in scope for"
  ;; Remove duplicate variable names from outer scopes. Only the inner variables
  ;; are accessible.
  (remove-duplicates (apply #'append (if keep-globals
                                         (scopes obj ast)
                                         (butlast (scopes obj ast))))
                     :from-end t
                     :key {aget :name}))


;;; Genome manipulations
(defgeneric prepend-text-to-genome (software text)
  (:documentation "Prepend non-AST TEXT to OBJ genome.

* OBJ object to modify with text
* TEXT text to prepend to the genome")
  (:method ((obj parseable) (text string)
            &aux (root (genome obj)))
    (labels ((ensure-newline (text)
               (if (not (equalp #\Newline (last-elt text)))
                   (concatenate 'string text '(#\Newline))
                   text)))
      (setf (slot-value obj 'genome)
            (copy root
                  :children (cons (format nil "~a~a"
                                          (ensure-newline text)
                                          (car (ast-children root)))
                                  (cdr (ast-children root))))))))

(defgeneric append-text-to-genome-preamble (software text)
  (:documentation "Append non-AST TEXT to OBJ's genome preamble.

* OBJ object to modify with text
* TEXT text to append to the genome preamble")
  (:method ((obj parseable) (text string)
            &aux (root (genome obj)))
    (labels ((ensure-newline (text)
               (if (not (equalp #\Newline (last-elt text)))
                   (concatenate 'string text '(#\Newline))
                   text)))
      (setf (slot-value obj 'genome)
            (copy root
                  :children (cons (format nil "~a~a"
                                              (car (ast-children root))
                                              (ensure-newline text))
                                  (cdr (ast-children root))))))))

(defgeneric append-text-to-genome (software text)
  (:documentation "Append non-AST TEXT to OBJ genome.

* OBJ object to modify with text
* TEXT text to append to the genome")
  (:method ((obj parseable) (text string)
            &aux (root (genome obj)))
    (setf (slot-value obj 'genome)
          (copy root
                :children (if (stringp (lastcar (ast-children root)))
                              (append (butlast (ast-children root))
                                      (list (format nil "~a~a"
                                                    (lastcar (ast-children root))
                                                    text)))
                              (append (ast-children root) (list text)))))))


;; Targeting functions
(defmethod pick-bad ((obj parseable))
  "Pick a 'bad' index into a software object.
Used to target mutation."
  (if (bad-asts obj)
      (random-elt (bad-asts obj))
      (error (make-condition 'no-mutation-targets
               :obj obj :text "No asts to pick from"))))

(defmethod pick-good ((obj parseable))
  "Pick a 'good' index into a software object.
Used to target mutation."
  (if (good-asts obj)
      (random-elt (good-asts obj))
      (error (make-condition 'no-mutation-targets
               :obj obj :text "No asts to pick from"))))

(defmethod bad-asts ((obj parseable))
  "Return a list of all bad asts in OBJ"
  (asts obj))

(defmethod good-asts ((obj parseable))
  "Return a list of all good asts in OBJ"
  (asts obj))

(defmethod good-mutation-targets ((obj parseable) &key filter)
  "Return a list of all good mutation targets in OBJ matching FILTER.
* OBJ software object to query for good mutation targets
* FILTER predicate taking an AST parameter to allow for filtering
"
  (mutation-targets obj :filter filter :stmt-pool #'good-asts))

(defmethod bad-mutation-targets ((obj parseable) &key filter)
  "Return a list of all bad mutation targets in OBJ matching FILTER.
* OBJ software object to query for bad mutation targets
* FILTER predicate taking an AST parameter to allow for filtering
"
  (mutation-targets obj :filter filter :stmt-pool #'bad-asts))

(defmethod mutation-targets ((obj parseable)
                             &key (filter nil)
                                  (stmt-pool #'asts stmt-pool-supplied-p))
  "Return a list of target ASTs from STMT-POOL for mutation, throwing
a 'no-mutation-targets exception if none are available.

* OBJ software object to query for mutation targets
* FILTER filter AST from consideration when this function returns nil
* STMT-POOL method on OBJ returning a list of ASTs"
  (labels ((do-mutation-targets ()
             (if-let ((target-stmts
                        (if filter
                            (remove-if-not filter (funcall stmt-pool obj))
                            (funcall stmt-pool obj))))
               target-stmts
               (error (make-condition 'no-mutation-targets
                        :obj obj :text "No stmts match the given filter")))))
    (if (not stmt-pool-supplied-p)
        (do-mutation-targets)
        (restart-case
            (do-mutation-targets)
          (expand-stmt-pool ()
            :report "Expand statement pool of potential mutation targets"
            (mutation-targets obj :filter filter))))))

(defun pick-general (software first-pool &key second-pool filter)
  "Pick ASTs from FIRST-POOL and optionally SECOND-POOL.
FIRST-POOL and SECOND-POOL are methods on SOFTWARE which return a list
of ASTs.  An optional filter function having the signature 'f ast
&optional first-pick', may be passed, returning true if the given AST
should be included as a possible pick or false (nil) otherwise."
  (flet ((safe-random-elt (pool)
           (when pool (random-elt pool))))
    (let* ((first-pick (nest (safe-random-elt)
                             (mutation-targets software :filter filter
                                               :stmt-pool first-pool))))
      (if (null second-pool)
          (list (cons :stmt1 first-pick))
          (list (cons :stmt1 first-pick)
                (cons :stmt2
                      (nest (safe-random-elt)
                            (mutation-targets
                             software
                             :filter (lambda (ast)
                                       (if filter
                                           (funcall filter ast first-pick)
                                           t))
                             :stmt-pool second-pool))))))))

(defmethod pick-bad-good ((software parseable) &key filter
                          (bad-pool #'bad-asts) (good-pool #'good-asts))
  "Pick two ASTs from SOFTWARE, first from `bad-pool' followed
by `good-pool', excluding those ASTs removed by FILTER.
* SOFTWARE object to perform picks for
* FILTER function taking two AST parameters and returning non-nil if the
second should be included as a possible pick
* BAD-POOL function returning a pool of 'bad' ASTs in SOFTWARE
* GOOD-POOL function returning a pool of 'good' ASTs in SOFTWARE
"
  (pick-general software bad-pool
                :second-pool good-pool
                :filter filter))

(defmethod pick-bad-bad ((software parseable) &key filter
                         (bad-pool #'bad-asts))
  "Pick two ASTs from SOFTWARE, both from the `bad-asts' pool,
excluding those ASTs removed by FILTER.
* SOFTWARE object to perform picks for
* FILTER function taking two AST parameters and returning non-nil if the
second should be included as a possible pick
* BAD-POOL function returning a pool of 'bad' ASTs in SOFTWARE
"
  (pick-general software bad-pool
                :second-pool bad-pool
                :filter filter))

(defmethod pick-bad-only ((software parseable) &key filter
                          (bad-pool #'bad-asts))
  "Pick a single AST from SOFTWARE from `bad-pool',
excluding those ASTs removed by FILTER.
* SOFTWARE object to perform picks for
* FILTER function taking two AST parameters and returning non-nil if the
second should be included as a possible pick
* BAD-POOL function returning a pool of 'bad' ASTs in SOFTWARE
"
  (pick-general software bad-pool :filter filter))


;;; Mutations
(defvar *parseable-mutation-types*
  (cumulative-distribution
   (normalize-probabilities
    '((parseable-insert . 1)
      (parseable-swap . 1)
      (parseable-move . 1)
      (parseable-replace . 1)
      (parseable-cut . 1)
      (parseable-nop . 1))))
  "Cumulative distribution of normalized probabilities of weighted mutations.")

(defmethod pick-mutation-type ((obj parseable))
  "Select type of mutation to apply to OBJ."
  (random-pick *parseable-mutation-types*))

(defmethod mutate ((obj parseable))
  "Select a random mutation and mutate OBJ."
  (unless (> (size obj) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj obj)))
  (let ((mutation (make-instance (pick-mutation-type obj) :object obj)))
    (apply-mutation obj mutation)
    (values obj mutation)))

(defclass parseable-mutation (mutation)
  ()
  (:documentation "Specialization of the mutation interface for parseable
software objects."))

(define-mutation parseable-insert (parseable-mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "Perform an insertion operation on a parseable software
object."))

(defmethod build-op ((mutation parseable-insert) software)
  "Return an association list with the operations to apply a `parseable-insert'
MUTATION to SOFTWARE.
* MUTATION defines targets of insertion operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:insert . ,(targets mutation))))

(define-mutation parseable-swap (parseable-mutation)
  ((targeter :initform #'pick-bad-bad))
  (:documentation "Perform a swap operation on a parseable software object."))

(defmethod build-op ((mutation parseable-swap) software)
  "Return an association list with the operations to apply a `parseable-swap'
MUTATION to SOFTWARE.
* MUTATION defines targets of the swap operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:set (:stmt1 . ,(aget :stmt1 (targets mutation)))
          (:stmt2 . ,(aget :stmt2 (targets mutation))))
    (:set (:stmt1 . ,(aget :stmt2 (targets mutation)))
          (:stmt2 . ,(aget :stmt1 (targets mutation))))))

;;; Move
(define-mutation parseable-move (parseable-mutation)
  ((targeter :initform #'pick-bad-bad))
  (:documentation "Perform a move operation on a parseable software object."))

(defmethod build-op ((mutation parseable-move) software)
  "Return an association list with the operations to apply a `parseable-move'
MUTATION to SOFTWARE.
* MUTATION defines targets of the move operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:insert (:stmt1 . ,(aget :stmt1 (targets mutation)))
             (:stmt2 . ,(aget :stmt2 (targets mutation))))
    (:cut (:stmt1 . ,(aget :stmt2 (targets mutation))))))

;;; Replace
(define-mutation parseable-replace (parseable-mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "Perform a replace operation on a parseable
software object."))

(defmethod build-op ((mutation parseable-replace) software)
  "Return an association list with the operations to apply an
`parseable-replace' MUTATION to SOFTWARE.
* MUTATION defines targets of the replace operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:set . ,(targets mutation))))

(define-mutation parseable-cut (parseable-mutation)
  ((targeter :initform #'pick-bad-only))
  (:documentation "Perform a cut operation on a parseable software object."))

(defmethod build-op ((mutation parseable-cut) software)
  "Return an association list with the operations to apply a `parseable-cut'
MUTATION to SOFTWARE.
* MUTATION defines the targets of the cut operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:cut . ,(targets mutation))))

;;; Nop
(define-mutation parseable-nop (parseable-mutation)
  ()
  (:documentation "Perform a nop on a parseable software object."))

(defmethod build-op ((mutation parseable-nop) software)
  "Return an association list with the operations to apply a `nop'
MUTATION to SOFTWARE.
* MUATION defines teh targets of the nop operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software mutation))
  nil)


;;; General mutation methods
(defmethod apply-mutation ((software parseable)
                           (mutation parseable-mutation))
  "Apply MUTATION to SOFTWARE, returning the resulting SOFTWARE.
* SOFTWARE object to be mutated
* MUTATION mutation to be performed
"
  (apply-mutation-ops software
                      ;; Sort operations latest-first so they
                      ;; won't step on each other.
                      (sort (recontextualize-mutation software mutation)
                            #'ast-later-p :key [{aget :stmt1} #'cdr])))

(defmethod apply-mutation ((obj parseable) (op list))
  "Apply OPS to SOFTWARE, returning the resulting SOFTWARE.
* OBJ object to be mutated
* OP mutation to be performed
"
  (apply-mutation obj (make-instance (car op) :targets (cdr op))))

(defmethod apply-mutation-ops ((software parseable) (ops list))
  "Apply a recontextualized list of OPS to SOFTWARE, returning the resulting
SOFTWARE.
* SOFTWARE object to be mutated
* OPS list of association lists with operations to be performed
"
  (setf (genome software)
        (iter (for (op . properties) in ops)
              (let ((stmt1 (if (listp (aget :stmt1 properties))
                               (aget :stmt1 properties)
                               (ast-path (aget :stmt1 properties))))
                    (value1 (if (functionp (aget :value1 properties))
                                (funcall (aget :value1 properties))
                                (aget :value1 properties))))
                ;; Set the genome slot directly here to avoid
                ;; triggering any :before/:around/:after methods
                ;; associated with setting the genome through
                ;; the writer method.  These auxillary methods
                ;; are triggered after all mutations ops have
                ;; been applied.
                (setf (slot-value software 'genome)
                      (ecase op
                        (:set (with (genome software) stmt1 value1))
                        (:cut (less (genome software) stmt1))
                        (:insert (insert (genome software) stmt1 value1))
                        (:splice (splice (genome software) stmt1 value1)))))
              (finally (return (slot-value software 'genome)))))

  software)

(defmethod recontextualize-mutation ((software parseable) (mutation mutation))
  "Bind free variables and functions in the mutation to concrete
values.  Additionally perform any updates to the software object required
for successful mutation (e.g. adding includes/types/macros), returning
the mutation operations to be performed as an association list.
* OBJ object to be mutated
* MUT mutation to be applied
"
  (recontextualize-mutation software (build-op mutation software)))

(defmethod recontextualize-mutation ((software parseable) (ops list))
  "Bind free variables and functions in the mutation to concrete
values.  Additionally perform any updates to the software object required
for successful mutation (e.g. adding includes/types/macros), returning
the mutation operations to be performed as an association list.
* OBJ object to be mutated
* MUT mutation to be applied
"
  (loop :for (op . properties) :in ops
     :collecting
     (let ((stmt1  (aget :stmt1  properties))
           (stmt2  (aget :stmt2  properties))
           (value1 (aget :value1 properties))
           (literal1 (aget :literal1 properties)))
       (case op
         ((:cut :set :insert)
          (cons op
            (cons (cons :stmt1 stmt1)
                  (if (or stmt2 value1 literal1)
                      `((:value1 .
                         ,(if literal1 literal1
                              (recontextualize
                                software
                                (or stmt2 value1)
                                stmt1))))))))
         ;; Other ops are passed through without changes
         (otherwise (cons op properties))))))

(defmethod recontextualize ((software parseable)
                            ast pt)
  "Perform any modifications to AST (e.g. variable rebinding)
to allow for successful mutation of SOFTWARE at PT."
  (declare (ignorable software pt))
  ast)


;;; Mutation wrappers for common tree manipulations.

;; FIXME: When clang is converted to utilize functional trees, these
;; should be removed and clients updated to utilize the functional
;; tree interace.
(defgeneric insert-ast (obj location ast &key literal)
  (:documentation "Return the modified OBJ with AST inserted at LOCATION.
* OBJ object to be modified
* LOCATION location where insertion is to occur
* AST AST to insert
* LITERAL keyword to control whether recontextualization is performed
          For modifications where the replacement is to be directly
          inserted, pass this keyword as true.")
  (:method ((obj parseable) (location ast) (ast ast) &rest args)
    (apply #'insert-ast obj (ast-path location) ast args))
  (:method ((obj parseable) (location list) (ast ast) &key literal)
    (apply-mutation obj (at-targets (make-instance 'parseable-insert)
                                    (list (cons :stmt1 location)
                                          (cons (if literal :literal1 :value1)
                                                ast))))))

(defgeneric replace-ast (obj location replacement &key literal)
  (:documentation "Modify and return OBJ with the AST at LOCATION replaced
with REPLACEMENT.
* OBJ object to be modified
* LOCATION location where replacement is to occur
* REPLACEMENT AST to insert as a replacement
* LITERAL keyword to control whether recontextualization is performed
          For modifications where the replacement is to be directly
          inserted, pass this keyword as true.")
  (:method ((obj parseable) (location ast) (replacement ast) &rest args)
    (apply #'replace-ast obj (ast-path location) replacement args))
  (:method ((obj parseable) (location list) (replacement ast) &key literal)
    (apply-mutation obj (at-targets (make-instance 'parseable-replace)
                                    (list (cons :stmt1 location)
                                          (cons (if literal :literal1 :value1)
                                                replacement)))))
  (:method ((obj parseable) (location ast) (replacement string) &rest args)
    (apply #'replace-ast obj (ast-path location) (list replacement) args))
  (:method ((obj parseable) (location list) (replacement string) &rest args)
    (apply #'replace-ast obj location (list replacement) args))
  (:method ((obj parseable) (location ast) (replacement list) &rest args)
    (apply #'replace-ast obj (ast-path location) replacement args))
  (:method ((obj parseable) (location list) (replacement list) &rest args)
    (let* ((old-ast (get-ast obj (butlast location)))
           (new-ast (nest (copy old-ast :children)
                          (append (subseq (ast-children old-ast) 0
                                          (lastcar location))
                                  replacement
                                  (subseq (ast-children old-ast)
                                          (1+ (lastcar location)))))))
      (apply #'replace-ast obj old-ast new-ast args))))

(defgeneric remove-ast (obj location)
  (:documentation "Return the modified OBJ with the AST at LOCATION removed.
* OBJ object to be modified
* LOCATION location to be removed in OBJ")
  (:method ((obj parseable) (location ast))
    (remove-ast obj (ast-path location)))
  (:method ((obj parseable) (location list))
    (apply-mutation obj (at-targets (make-instance 'parseable-cut)
                                    (list (cons :stmt1 location))))))
