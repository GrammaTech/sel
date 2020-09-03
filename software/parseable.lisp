;;; parseable.lisp --- Software which may be parsed into ASTs
(defpackage :software-evolution-library/software/parseable
  (:nicknames :sel/software/parseable :sel/sw/parseable)
  (:use :gt/full
        :cl-store
        :bordeaux-threads
        :software-evolution-library
        :software-evolution-library/components/file
        :software-evolution-library/utility/range)
  (:import-from :functional-trees :path-later-p)
  (:export ;; ASTs
           :ast
           :functional-tree-ast
           :to-alist
           :from-alist
           :child-asts
           :sorted-children
           :ast-path
           :ast-annotation
           :ast-annotations
           :ast-hash
           :ast-stored-hash
           :ast-combine-hash-values
           :annotations
           :stored-hash
           :conflict-ast
           :conflict-ast-child-alist
           :conflict-ast-default-children
           :combine-conflict-asts
           :source-text
           :rebind-vars
           :collect-if
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
           :get-parent-ast
           :get-parent-asts
           :get-parent-full-stmt
           :get-ast-types
           :get-unbound-vals
           :get-unbound-funs
           :enclosing-scope
           :find-if-in-scope
           :find-if-in-parents
           :scopes
           :get-vars-in-scope
           :parse-asts
           :ast-source-ranges
           :asts-containing-source-location
           :asts-contained-in-source-range
           :asts-intersecting-source-range
           :good-asts
           :bad-asts
           :good-mutation-targets
           :bad-mutation-targets
           :shares-path-of-p
           :ancestor-of-p
           ;; :mutation-targets
           :pick-general
           :recontextualize-mutation
           :recontextualize
           :select-crossover-points
           :parent-ast-p
           :prepend-text-to-genome
           :append-text-to-genome-preamble
           :append-text-to-genome
           :index-of-ast
           :ast-at-index
           ;; Restarts
           :expand-stmt-pool))
(in-package :software-evolution-library/software/parseable)
(in-readtable :curry-compose-reader-macros)

(define-software parseable (software file)
  ((genome   :initarg :genome :accessor genome :initform ""
             :documentation "Lazily parsed AST representation of the code."))
  (:documentation "Parsed AST tree software representation."))


;;; AST data structure definitions.
(defclass ast () ()
  (:documentation "Base class for all ASTs in SEL.  This class acts as a tag
for objects to allow method dispatch on generic AST objects regardless of
whether they inherit from the functional trees library."))

;; All hash values are of typer HASH-TYPE.
;; This was chosen to be large enough that collisions
;; are unlikely.  However, a collision can be expected
;; if hashing more than about (ash 1 28) (~ 256 million)
;; ASTs.  The value was chosen so the base is a fixnum
;; in both SBCL and CCL (64 bit).
(deftype hash-type () '(integer 0 (#.(- (ash 1 56) 5))))

(defclass stored-hash ()
  ((stored-hash :initarg :stored-hash :initform nil
                :documentation "A cached hash." :type (or null hash-type)))
  (:documentation "Mixin for stored-hash slot"))

(defclass functional-tree-ast (node ast stored-hash)
  ((annotations :initarg :annotations :initform nil :reader ast-annotations
                :documentation "A-list of annotations." :type list))
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
        (format stream "~a :TEXT ~s"
                (serial-number obj)
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
        (format stream "~a :CHILD-ALIST ~s"
                (serial-number obj)
                (conflict-ast-child-alist obj)))))

(defgeneric ast-annotation (ast annotation)
  (:documentation "Return given AST ANNOTATION.")
  (:method ((ast ast) (annotation symbol))
    (aget annotation (ast-annotations ast))))

(defgeneric ast-path (obj ast)
  (:documentation "Return the PATH to AST in OBJ.")
  (:method :before ((root functional-tree-ast) (ast functional-tree-ast))
    ;; lazily populate fingers when paths are requested
    (unless (finger ast) (populate-fingers root)))
  (:method ((obj parseable) (ast functional-tree-ast))
    (ast-path (genome obj) ast))
  (:method ((root functional-tree-ast) (ast functional-tree-ast))
    (ast-path root (finger ast)))
  (:method ((root functional-tree-ast) (finger finger))
    (path (functional-trees::transform-finger finger root))))

(defmethod path-later-p ((obj parseable) path-a path-b)
  (path-later-p (genome obj) path-a path-b))

(defmethod path-later-p ((obj parseable) (a ast) (b ast))
  (path-later-p obj (ast-path obj a) (ast-path obj b)))

(defgeneric child-asts (ast &key recursive)
  (:documentation "Return the AST children of AST.  If the keyword
RECURSIVE is passed, recursive AST children will also be returned.")
  (:method ((ast ast) &key recursive)
    (if recursive
        (cdr (reverse (reduce (flip #'cons) ast)))
        (remove-if-not {typep _ 'ast} (children ast)))))

(defgeneric sorted-children (ast)
  (:documentation "Return the children of AST sorted in textual order.")
  (:method :before ((ast ast)
                    &aux (children (remove nil (children ast))))
    (assert (or (null (ast-annotation ast :child-order))
                (= (length children)
                   (length (ast-annotation ast :child-order))))
            (ast)
            "The number of elements in the AST's :child-order annotation ~
            defining the order of the children does not match the number ~
            of children, ~d versus ~d."
            (length (ast-annotation ast :child-order)) (length children)))
  (:method ((ast ast))
    (if (ast-annotation ast :child-order)
        (mapcar {lookup ast} (ast-annotation ast :child-order))
        (remove nil (children ast)))))

(defmethod initialize-instance :after ((ast functional-tree-ast)
                                       &rest args &key)
  "Wrapper around MAKE-INSTANCE to transform all keyword arguments
which are not explicit slot initargs into annotations for functional
tree ASTs."
  (let ((initargs (nest (mappend #'slot-definition-initargs)
                        (remove-if [{eql :class} #'slot-definition-allocation])
                        (class-slots (class-of ast)))))
    (iter (for (key . value) in (plist-alist args))
          (unless (member key initargs)
            (setf (slot-value ast 'annotations)
                  (cons (cons key value) (slot-value ast 'annotations)))))))

(defmethod copy :around ((ast functional-tree-ast) &rest keys)
  "Wrapper around COPY to transform all keyword arguments which are
not explicit slot initargs into annotations for functional tree ASTs."
  (let ((initargs (nest (mappend #'slot-definition-initargs)
                        (remove-if [{eql :class} #'slot-definition-allocation])
                        (class-slots (class-of ast)))))
    (nest (apply #'call-next-method ast)
          (iter (for (key . value) in (plist-alist keys))
                (cond ((eq key :annotations)
                       (appending value into annotations))
                      ((member key initargs)
                       (appending (list key value) into args))
                      (t (collecting (cons key value) into annotations)))
                (finally (return (append (when annotations
                                           (list :annotations annotations))
                                         args)))))))

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
(defmethod equal? ((ast-a ast) (ast-b ast))
  (let ((hash1 (slot-value ast-a 'stored-hash))
        (hash2 (slot-value ast-b 'stored-hash)))
    (if (and hash1 hash2 (not (eql hash1 hash2)))
        nil
        (and (eq (type-of ast-a) (type-of ast-b))
             (length= (children ast-a)
                      (children ast-b))
             (every #'equal? (children ast-a) (children ast-b))))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-method-combination ast-combine-hash-values
    :identity-with-one-argument t))

(defgeneric ast-hash (ast)
  (:method-combination ast-combine-hash-values)
  (:documentation "A hash value for the AST, which is a nonnegative
integer.  It should be the case that (equal? x y) implies
(eql (ast-hash x) (ast-hash y)), and that if (not (equal? x y))
then the equality of the hashes is unlikely."))

(defconstant +ast-hash-base+ (- (ash 1 56) 5)
  "A prime that is close to a power of 2")

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
modile +AST-HASH-BASE+.  0 values in ARGS are skipped."
    (let ((result 0)
          (hb +ast-hash-base+)
          (i 0))
      (declare (type hash-type result)
               (type (integer 0 (#.(ash 1 30))) i))
      (iter (for hv in args)
            (unless (eql hv 0)
              (locally
                  (declare (type hash-type hv))
                (let* ((im (logand i 31))
                       (a (aref a-coeffs im))
                       (b (aref b-coeffs im)))
                  ;; RESULT is squared to avoid linearity
                  ;; Without this, trees that have certain permutations
                  ;; of leaf values can be likely to hash to the same integer.
                  (setf result (mod (+ i b (* a hv) (* result result p)) hb)))
                (incf i))))
      result))

  (defun ast-combine-simple-vector-hash-values (sv)
    (declare (type simple-vector sv))
    (let ((result 0)
          (hb +ast-hash-base+)
          (len (length sv))
          (i 0))
      (declare (type hash-type result))
      (iter (for hv in-vector sv)
            (unless (eql hv 0)
              (locally (declare (type hash-type hv))
                (let* ((im (logand i 31))
                       (a (aref a-coeffs im))
                       (b (aref b-coeffs im)))
                  ;; RESULT is squared to avoid linearity
                  ;; Without this, trees that have certain permutations of leaf
                  ;; values can be likely to hash to the same integer.
                  (setf result (mod (+ i b (* a hv) (* result result p)) hb))))
              (incf i)))
      result))

  (defmethod ast-hash ast-combine-hash-values ((x t)) 0)

  (defmethod ast-hash ast-combine-hash-values ((i integer))
    (let ((c1 34188292748050745)
          (c2 38665981814718286))
      (mod (+ (* c1 i) c2) +ast-hash-base+)))

  ;; could have specialized methods on strings
  ;; to speed up that common case
  (defmethod ast-hash ast-combine-hash-values ((s vector))
    (ast-combine-hash-values
     38468922606716016
     (length s)
     (ast-combine-simple-vector-hash-values (map 'simple-vector #'ast-hash s))))

  (defmethod ast-hash ast-combine-hash-values ((l cons))
    ;; Assumes not a circular list
    (apply #'ast-combine-hash-values
           16335929882652762
           (iter
            (collect (if (consp l)
                         (ast-hash (car l))
                         ;; add a constant to distinguish (X Y)
                         ;; from (X . Y)
                         (mod
                          (+ 41019876016299766
                             (ast-hash l))
                          +ast-hash-base+)))
            (while (consp l))
            (pop l))))

  (defmethod ast-hash ast-combine-hash-values ((n null))
    46757794301535766)

  (defmethod ast-hash ast-combine-hash-values  ((c character))
    (let ((c1 3310905730158464)
          (c2 4019805890044232))
      (mod (+ (* c1 (char-int c)) c2) +ast-hash-base+)))

  (defmethod ast-hash ast-combine-hash-values ((s symbol))
    (or (get s 'hash)
        (setf (get s 'hash)
              (ast-combine-hash-values
               30932222477428348
               (ast-hash (symbol-package s))
               (ast-hash (symbol-name s))))))

  (defmethod ast-hash ast-combine-hash-values ((p package))
    (ast-hash (package-name p))))

(defmethod ast-hash ast-combine-hash-values ((ast ast))
  (ast-hash (cons (type-of ast) (children ast))))

;;; We cache this for ast nodes otherwise the time
;;; for computing ast-hash on a large tree can become very large
(defmethod ast-hash :around ((ast stored-hash))
  (or (slot-value ast 'stored-hash)
      (setf (slot-value ast 'stored-hash)
            (call-next-method))))


;;; Generic functions on ASTs
(defgeneric to-alist (struct)
  (:documentation "Convert struct to alist representation."))

(defgeneric from-alist (symbol alist)
  (:documentation "Convert alist to struct representation."))

(defgeneric source-text (ast &optional stream)
  (:documentation "Return the source code corresponding to an AST,
optionally writing to STREAM.")
  (:method :around ((ast t) &optional stream)
    (let (*print-pretty*)
      (with-string (s stream) (call-next-method ast s))))
  (:method ((ast null) &optional stream)
    (write-string "" stream))
  (:method ((str string) &optional stream)
    (write-string str stream))
  (:method ((c character) &optional stream)
    (source-text (string c) stream))
  (:method ((c conflict-ast) &optional stream)
    (format stream "<")
    (iter (for e on (conflict-ast-child-alist c))
          (format stream "~a: " (caar e))
          (iter (for x in (cdar e)) (source-text x stream))
          (when (cdr e) (format stream "|")))
    (format stream ">"))
  (:method ((ast ast) &optional stream)
    ;; In performance comparison the combination of
    ;; `with-output-to-string' and `write-string' was faster than
    ;; alternatives using `format' (which was still pretty fast) and
    ;; using `concatenate' (which was slow).
    ;;
    ;; More importantly using (apply #'concatenate ...) runs into
    ;; problems as the number of ASTs is very large.
    (mapc {source-text _ stream} (children ast))))

(defgeneric rebind-vars (ast var-replacements fun-replacements)
  (:documentation
   "Replace variable and function references, returning a new AST.")
  (:method ((ast string) var-replacements fun-replacements)
    (reduce (lambda (ast replacement-pair)
              (if (equal ast (first replacement-pair))
                  (second replacement-pair)
                  ast))
            (append var-replacements
                    (mapcar (lambda (fun-replacement)
                              (list (car (first fun-replacement))
                                    (car (second fun-replacement))))
                            fun-replacements))
            :initial-value ast)))

(defgeneric collect-if (predicate tree)
  (:documentation
   "Traverse TREE collecting every node that satisfies PREDICATE.")
  (:method ((predicate function) (tree ast))
    ;; reverse it to maintain the order it was found in.
    (reverse
     (reduce (lambda (accum ast)
               (if (funcall predicate ast)
                   (cons ast accum)
                   accum))
             tree))))


;;; parseable software objects
(defgeneric roots (obj)
  (:documentation "Return all top-level ASTs in OBJ."))

(defgeneric asts (obj)
  (:documentation "Deprecated: Return a list of all non-root ASTs in OBJ."))

(defgeneric get-parent-ast (obj ast)
  (:documentation "Return the parent node of AST in OBJ"))

(defgeneric get-parent-asts (obj ast)
  (:documentation "Return the parent nodes of AST in OBJ"))

(defgeneric get-parent-full-stmt (software ast)
  (:documentation
   "Return the first ancestor of AST in SOFTWARE which is a full statement.
Returns nil if no full statement parent is found."))

(defgeneric get-ast-types (software ast)
  (:documentation "Types directly referenced within AST."))

(defgeneric get-unbound-funs (software ast)
  (:documentation "Functions used (but not defined) within the AST."))

(defgeneric get-unbound-vals (software ast)
  (:documentation "Variables used (but not defined) within the AST."))

(defgeneric enclosing-scope (software ast)
  (:documentation "Returns enclosing scope of AST."))

(defgeneric find-if-in-scope (predicate obj ast &key reference-ast)
  (:documentation "Walk up the genome in OBJ starting at AST, searching for
an AST that satisfies PREDICATE that occurs before REFERENCE-AST.")
  (:method (predicate (obj parseable) ast
            &key (reference-ast ast)
            &aux (parent (get-parent-ast obj ast)))
    (labels ((get-reversed-children (parent-ast)
               "Return a reversed list of PARENT-AST's immediate children."
               (reverse
                (remove-if-not (lambda (child)
                                 (and (typep child 'ast)
                                      (path-later-p obj reference-ast child)))
                               (children parent-ast)))))
      (cond-let result
        ((not parent)
         ;; If the parent is null, the genome is the last thing that
         ;; needs to be checked before stopping recursion.
         (find-if predicate (get-reversed-children (genome obj))))
        ((find-if predicate (get-reversed-children parent))
         result)
        (t
         (find-if-in-scope predicate obj parent
                            :reference-ast reference-ast))))))

(defgeneric find-if-in-parents (predicate obj ast)
  (:documentation "Search through the parents of AST for the first one that
satisfies PREDICATE.")
  (:method (predicate (obj parseable) ast)
    (find-if predicate (get-parent-asts obj ast))))

(defgeneric scopes (software ast)
  (:documentation "Return lists of variables in each enclosing scope.
Each variable is represented by an alist containing :NAME, :DECL, :TYPE,
and :SCOPE."))

(defgeneric get-vars-in-scope (software ast &optional keep-globals)
  (:documentation "Return all variables in enclosing scopes."))

(defgeneric parse-asts (software &optional source-text)
  (:documentation "Parse genome of SOFTWARE into an AST representation.
There are some requirements for the ASTs constructed by this method:
* We require that *all* source text be stored as a raw string
  somewhere in the AST tree.  Source text tucked inside of a
  non-string AST-NODE will be ignored.
* We also require that if two ASTs have the same class and the same
  source text then they are equal.

Other methods in on parseable objects, specifically `ast-can-recurse'
and `equal?' depend on these invariants.

Optional argument SOURCE-TEXT holds the source code string to parse
into ASTs.  If not supplied it is generally assumed to already be set
in the software's genome."))

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

(defgeneric shares-path-of-p (obj target-ast shared-path-ast)
  (:documentation "Returns T if TARGET-AST has the same path or a super-path
of SHARED-PATH-AST's path in OBJ.")
  (:method ((obj parseable) target-ast shared-path-ast)
    (starts-with-subseq (ast-path obj shared-path-ast)
                        (ast-path obj target-ast)
                        :test #'equal)))

(defgeneric ancestor-of-p (obj target-ast ancestor)
  (:documentation "Returns T if ANCESTOR is an ancestor of TARGET-AST in OBJ.")
  (:method ((obj parseable) target-ast ancestor)
    (unless (eq target-ast ancestor)
      (shares-path-of-p obj target-ast ancestor))))


;;; Core parseable methods
(defmethod lookup ((obj parseable) key)
  ;; Enables the use of the `@' macro directly against parseable objects.
  (lookup (genome obj) key))

;;; FSet overrides for common-lisp sequence functions pass through to genome.
(defun write-sequence-function-parseable-method (name)
  (let ((lambda-list (generic-function-lambda-list (ensure-function name))))
    `(defmethod ,name
         ,(substitute '(collection parseable) 'collection lambda-list)
       (,name ,@(subseq lambda-list 0 (1- (position '&key lambda-list)))
              (genome collection)
              ,@(mappend (lambda (key) (list (make-keyword key) key))
                         (cdr (member '&key lambda-list)))))))

;;; FSet tree manipulations pass through to genome.
(defun write-tree-manipulation-function-parseable-method (name)
  (let ((lambda-list (generic-function-lambda-list (ensure-function name))))
    `(defmethod ,name ((obj parseable) ,@(cdr lambda-list))
       (setf (genome obj)
             (,name (genome obj)
                    ,(second lambda-list)
                    ,@(nest (mapcar (lambda (param) `(tree-copy ,param)))
                            (remove '&optional)
                            (cddr lambda-list))))
       obj)))

(eval `(progn
         ,@(mapcar #'write-sequence-function-parseable-method
                   '(reduce
                     find-if
                     find-if-not
                     find
                     count-if
                     count-if-not
                     count
                     position-if
                     position-if-not
                     position
                     remove-if
                     remove-if-not
                     remove
                     substitute-if
                     substitute-if-not
                     substitute))))

(eval `(progn
         ,@(mapcar #'write-tree-manipulation-function-parseable-method
                   '(less
                     with
                     insert
                     splice))))

(defmethod size ((obj parseable))
  "Return the number of non-root ASTs in OBJ."
  (1- (count-if {typep _ 'ast} (genome obj))))

(defmethod genome-string ((obj parseable) &optional stream)
  "Return the source code of OBJ, optionally writing to STREAM"
  (with-string (s stream)
    (with-slots (genome) obj
      (if (stringp genome)
          (write-string genome s)
          (source-text genome s)))))

(defmethod (setf genome-string) ((new string) (obj parseable))
  ;; We will lazily parse the ASTs from the genome when it is next accessed.
  (setf (genome obj) new))

(defmethod genome :before ((obj parseable))
  "Lazily parse the genome upon first access."
  (when (stringp (slot-value obj 'genome))
    (setf (slot-value obj 'genome)
          (parse-asts obj))))

(defmethod (setf genome) :before ((new t) (obj parseable))
  "Clear fitness prior to updating to the NEW genome."
  (setf (slot-value obj 'fitness) nil))

(defmethod from-file ((obj parseable) path)
  "Initialize OBJ with the contents of PATH."
  (setf (genome obj) (file-to-string path))
  obj)

(defmethod from-string ((obj parseable) string)
  "Initialize OBJ with the contents of STRING."
  (setf (genome obj) string)
  obj)

(defmethod parse-asts :around ((sw parseable) &optional text)
  (declare (ignorable text))
  (handler-bind
      ((error (lambda (e)
                (declare (ignore e))
                (when-let ((ofile (original-path sw)))
                  (warn "Failure in parse-asts: original-path = ~a"
                        ofile)))))
    (call-next-method)))

(defgeneric ast-source-ranges (obj)
  (:documentation "Return (AST . SOURCE-RANGE) for each AST in OBJ.")
  (:method ((ast ast))
    (labels ((source-location (line column)
               "Thin wrapper for creating a source location."
               (make-instance 'source-location :line line :column column))
             (source-range (begin end)
               "Thin wrapper for creating a source range."
               (make-instance 'source-range :begin begin :end end))
             (next-child (child children)
               "Return the AST after CHILD in CHILDREN."
               (when-let ((pos (position child children)))
                 (nth (1+ pos) children)))
             (ast-source-ranges* (ast line column)
               "Return the source ranges of AST and AST's children, starting
               at LINE and COLUMN."
               (let* ((begin (source-location line column))
                      (source-text (source-text ast))
                      (child-asts (nest (remove-if-not {typep _ 'ast})
                                        (sorted-children ast)))
                      (child (car child-asts)))
                 ;; TODO: This style is too imperative.
                 (iter (for i below (length source-text))
                       (if (and child (string^= (source-text child)
                                                (subseq source-text i)))
                           ;; Create the child source ranges when we find
                           ;; a child in the source text.
                           (let* ((ranges (ast-source-ranges* child line column))
                                  (end (end (cdar ranges))))
                              (setf line (line end)
                                    column (column end)
                                    i (1- (+ (length (source-text child)) i))
                                    child (next-child child child-asts))
                              (appending ranges into child-ranges))
                           (progn
                            ;; Continue iterating thru the source text,
                            ;; updating line and column appropriately.
                            (incf column)
                            (when (eq (elt source-text i) #\newline)
                              (incf line)
                              (setf column 1))))
                       (finally
                        ;; Prepend this AST's source range to the child ranges.
                        (return (cons (nest (cons ast)
                                            (source-range begin)
                                            (source-location line column))
                                      child-ranges)))))))
      (ast-source-ranges* ast 1 1)))
  (:method ((obj parseable))
    (ast-source-ranges (genome obj))))

(defgeneric asts-containing-source-location (software location)
  (:documentation "Return a list of ASTs in SOFTWARE containing LOC.")
  (:method ((obj parseable) (loc source-location))
    (nest (mapcar #'car)
          (remove-if-not [{contains _ loc} #'cdr])
          (ast-source-ranges obj))))

(defgeneric asts-contained-in-source-range (software range)
  (:documentation "Return a list of ASTs in SOFTWARE contained in RANGE.")
  (:method ((obj parseable) (range source-range))
    (nest (mapcar #'car)
          (remove-if-not [{contains range} #'cdr])
          (ast-source-ranges obj))))

(defgeneric asts-intersecting-source-range (software range)
  (:documentation "Return a list of ASTs in OBJ intersecting RANGE.")
  (:method ((obj parseable) (range source-range))
    (nest (mapcar #'car)
          (remove-if-not [{intersects range} #'cdr])
          (ast-source-ranges obj))))


;;; Retrieving ASTs
(defmethod roots ((obj parseable))
  "Return all top-level ASTs in OBJ.
* OBJ software object to search for roots
"
  (remove-if-not {typep _ 'ast} (children (genome obj))))

(defmethod asts ((obj parseable))
  ;; Deprecated: This method exists for interoperability with
  ;; legacy clang code.  If possible, clients should use
  ;; fset/functional tree overrides of CL functions such
  ;; as `mapcar` to iterate over ASTs.
  (child-asts (genome obj) :recursive t))

(defgeneric ast-at-index (software index)
  (:documentation "Deprecated: Return the AST in OBJ at INDEX.
If possible, only use when dealing with legacy code.

* OBJ object to retrieve ASTs for
* INDEX nth AST to retrieve
")
  (:method ((obj parseable) index)
    (nth index (asts obj))))

(defgeneric index-of-ast (software ast)
  (:documentation "Deprecated: Return the index of AST in OBJ.
If possible, only use when dealing with legacy code.

* OBJ object to query for the index of AST
* AST node to find the index of
")
  (:method  ((obj parseable) (ast ast))
    (position ast (asts obj) :test #'equalp)))

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
  (when-let ((path (butlast (ast-path obj ast))))
    (@ obj path)))

(defmethod get-parent-asts ((obj parseable) (ast ast))
  "Return the parent nodes of AST in OBJ
* OBJ software object containing AST and its parents
* AST node to find the parents of
"
 (nest (remove-if-not {typep _ 'ast})  ; Remove non-ASTs.
       (mapcar {lookup obj})           ; Lookup each prefix.
       (maplist #'reverse) (reverse)   ; Prefixes of path.
       (ast-path obj ast)))

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
                                          (car (children root)))
                                  (cdr (children root))))))))

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
                                              (car (children root))
                                              (ensure-newline text))
                                  (cdr (children root))))))))

(defgeneric append-text-to-genome (software text)
  (:documentation "Append non-AST TEXT to OBJ genome.

* OBJ object to modify with text
* TEXT text to append to the genome")
  (:method ((obj parseable) (text string)
            &aux (root (genome obj)))
    (setf (slot-value obj 'genome)
          (copy root
                :children (if (stringp (lastcar (children root)))
                              (append (butlast (children root))
                                      (list (format nil "~a~a"
                                                    (lastcar (children root))
                                                    text)))
                              (append (children root) (list text)))))))


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
                            {path-later-p software}
                            :key [{aget :stmt1} #'cdr])))

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
                               (ast-path software (aget :stmt1 properties))))
                    (value1
                     (tree-copy
                      (if (functionp (aget :value1 properties))
                          (funcall (aget :value1 properties))
                          (aget :value1 properties)))))
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
