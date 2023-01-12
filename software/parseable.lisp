;;; parseable.lisp --- Software which may be parsed into ASTs
(defpackage :software-evolution-library/software/parseable
  (:nicknames :sel/software/parseable :sel/sw/parseable)
  (:use :gt/full
        :cl-store
        :bordeaux-threads
        :software-evolution-library
        :software-evolution-library/components/file
        :software-evolution-library/utility/range
        :software-evolution-library/utility/limit-stream)
  (:import-from :functional-trees
   :path-later-p :slot-specifier-slot :slot-specifier-class
   :slot-specifier)
  (:import-from :functional-trees/attrs :with-attr-table)
  (:local-nicknames (:tg :trivial-garbage)
                    (:attrs :functional-trees/attrs))
  (:export ;; ASTs
           :ast
           :functional-tree-ast
           :to-alist
           :from-alist
           :child-asts
           :sorted-children
           :no-ast-path
           :ast-path
           :ast-annotation
           :ast-annotations
           :ast-hash
           :ast-stored-hash
           :ast-combine-hash-values
           :annotations
           :stored-hash
           :alternative-ast
           :alternative-ast-child-alist
           :alternative-ast-default-children
           :conflict-ast
           :conflict-ast-child-alist
           :conflict-ast-default-children
           :combine-conflict-asts
           :combine-conflict-asts-in-list
           :source-text
           :source-text=
           :source-text-take
           :source-text-take-lines
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
           :interleaved-text
           :find-deepest
           :get-parent-ast
           :get-parent-asts
           :get-parent-asts*
           :get-parent-full-stmt
           :is-stmt-p
           :get-ast-types
           :get-unbound-vals
           :get-unbound-funs
           :enclosing-scope
           :find-if-in-scope
           :find-if-in-parents
           :find-if-in-scopes
           :built-ins
           :scopes
           :scope-tree
           :all-scopes
           :get-vars-in-scope
           :parse-asts
           :ast-source-ranges
           :asts-containing-source-location
           :asts-contained-in-source-range
           :asts-intersecting-source-range
           :ast-start+end :ast-start :ast-end
           :good-asts
           :bad-asts
           :good-mutation-targets
           :bad-mutation-targets
           :shares-path-of-p
           :ancestor-of-p
           :descendant-of-p
           :get-function-from-function-call
           :map-arguments-to-parameters
           :assign-to-var-p
           ;; :mutation-targets
           :pick-general
           :recontextualize-mutation
           :recontextualize
           :select-crossover-points
           :select-crossover-points-pool
           :parent-ast-p
           :prepend-text-to-genome
           :append-text-to-genome-preamble
           :append-text-to-genome
           :index-of-ast
           :ast-at-index
           ;; Restarts
           :expand-stmt-pool
           ;; Indentation
           :software-indentation
           :indentation
           :indent-adjustment
           :indent-children
           :get-default-indentation
           :get-indentation-at
           :*indent-with-tabs-p*
           :*spaces-per-tab*
           :indentablep
           :combine-all-conflict-asts
           :ast-source-range
           :with-ast-property
           :dump-ast-properties
           :clear-ast-properties
           ;; Language inference
           :language-alias->language-symbol
           :language-symbol->language-aliases
           :define-language-alias-mappings))
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

(defmethod genome ((ast ast))
  "An AST is its own genome."
  ast)

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

(defclass functional-tree-ast (node ast oid-object stored-hash)
  ((annotations :initarg :annotations :initform nil :reader ast-annotations
                :documentation "A-list of annotations." :type list))
  (:documentation "Base class for SEL functional tree ASTs.
An applicative tree structure is used to hold the ASTs."))

(defclass alternative-ast (functional-tree-ast)
  ((child-alist :initarg :child-alist :initform nil
                :reader alternative-ast-child-alist
                :documentation "Child-Alist of the AST." :type list)
   (child-slot-specifiers
    :allocation :class)
   (default-children :initarg :default-children :initform nil
                     :reader alternative-ast-default-children
                     :documentation "Default-Children of the AST." :type list))
  (:documentation "Node representing several different ASTs at a point.
This can be used to create ASTs which represent several different nodes in the
same language or several nodes which differ in language."))

(defclass conflict-ast (alternative-ast)
  ((child-alist :reader conflict-ast-child-alist)
   (child-slot-specifiers :allocation :class :initform nil)
   (default-children :reader conflict-ast-default-children))
  (:documentation "Node representing several possibilities for an AST.
The mapping from a conflicted AST into a regular AST is as follows: for
a given conflict key, and for each conflict node, get the list of children
corresponding to that key (default if the key is not present), and splice
that list of children in place of the conflict node in its parent's children
list."))

(defmethod slot-unbound ((class t) (node conflict-ast) (slot (eql 'ft:descendant-map)))
  (let ((sn (ft:serial-number node)))
    (setf (slot-value node 'ft:descendant-map)
          (convert 'ft/it:itree (list (list (cons sn sn) nil))))))

(defparameter *ast-print-cutoff* 20
  "Maximum number of characters to print for TEXT in
PRINT-OBJECT method on AST structures.")

(defparameter *ast-print-min* 5
  "Minimum number of characters to print for TEXT in
PRINT-OBJECT method on AST structures.")

(defparameter *inline-newline-escape* "⏎"
  "String to substitute for a newline when printing an AST.")

(def +print-object-source-text+ t
  "Flag to control whether to print source text for an object.

Provided to make it easier to debug problems with AST printing.")

(defmethod print-object ((obj functional-tree-ast) stream
                         &aux (cutoff *ast-print-cutoff*)
                           (min *ast-print-min*))
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a~@[ :TEXT ~s~]"
                (serial-number obj)
                (and +print-object-source-text+
                     (ellipsize
                      (if-let (first-line (first (source-text-take-lines 1 obj)))
                        (if (length> first-line min) first-line
                            (string-replace-all (string #\Newline)
                                                (source-text-take min obj)
                                                *inline-newline-escape*))
                        "<NIL>")
                      cutoff))))))

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

(define-condition no-ast-path (error)
  ((root :initarg :root)
   (ast :initarg :ast))
  (:report (lambda (c s)
             (with-slots (root ast) c
               (format s "There is no path from ~a to ~a" root ast)))))

(defgeneric ast-path (obj ast)
  (:documentation "Return the PATH to AST in OBJ.")
  (:method ((obj parseable) (ast t))
    (ast-path (genome obj) ast))
  (:method ((root functional-tree-ast) (ast functional-tree-ast))
    (path-of-node root ast))
  (:method ((root functional-tree-ast) (serial-number integer))
    (path-of-node root serial-number)))

(defmethod path-later-p ((obj parseable) path-a path-b)
  (path-later-p (genome obj) path-a path-b))

(defmethod path-later-p ((obj parseable) (a ast) (b ast))
  (path-later-p obj (ast-path obj a) (ast-path obj b)))

(defmethod path-later-p ((obj ast) (a ast) (b ast))
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
                (length= children
                         (ast-annotation ast :child-order)))
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
            (setf (aget key (slot-value ast 'annotations)) value)))))

(defmethod copy :around ((ast functional-tree-ast) &rest keys)
  "Wrapper around COPY to transform all keyword arguments which are
not explicit slot initargs into annotations for functional tree ASTs."
  (let ((initargs (cons :serial-number
                        (nest (mappend #'slot-definition-initargs)
                        (remove-if [{eql :class} #'slot-definition-allocation])
                        (class-slots (class-of ast))))))
    (nest (apply #'call-next-method ast)
          (iter (for (key . value) in (plist-alist keys))
                (cond ((eq key :annotations)
                       (appending value into annotations))
                      ((member key initargs)
                       (appending (list key value) into args))
                      (t (collecting (cons key value) into annotations)))
                (finally (return (append (when annotations
                                           (list :annotations annotations))
                                         (list* :stored-hash nil
                                                args))))))))

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

(defgeneric combine-all-conflict-asts (parent child-list)
  (:documentation "Replace PARENT with a conflict node obtained by
combining the conflict nodes of CHILD-LIST, and whose elements are
version of PARENT with those various combinations."))

(defmethod combine-all-conflict-asts ((parent ast) (child-list list))
  (multiple-value-bind (alist def)
      (combine-conflict-asts-in-list child-list)
    (make-instance
     'conflict-ast
     :child-alist (iter (for (k . children) in alist)
                        (collecting (list k (copy parent :children children))))
     :default-children (list (copy parent :children def)))))

(defun combine-conflict-asts-in-list (child-list)
  "Combine a list of conflict-asts and other things into the components
of a single conflict-ast"
  (assert (some (lambda (a) (typep a 'conflict-ast)) child-list))
  (let ((keys nil))
    ;; Collect the keys of all conflict nodes in CHILD-LIST
    (iter (for c in child-list)
          (when (typep c 'conflict-ast)
            (iter (for a in (conflict-ast-child-alist c))
                  (pushnew (car a) keys))))
    (let ((alist (iter (for k in keys) (collecting (list k))))
          (def nil))
      ;; Now build the child alists for each complete version
      (iter
        (for c in child-list)
        (typecase c
          (conflict-ast
           (let ((c-alist (conflict-ast-child-alist c))
                 (c-def (conflict-ast-default-children c)))
             (setf def (revappend c-def def))
             (iter
               (for p1 in alist)
               (let ((p2 (assoc (car p1) c-alist)))
                 (setf (cdr p1)
                       (revappend (if p2 (cdr p2) c-def)
                                  (cdr p1)))))))
          (t
           (push c def)
           (iter
             (for p in alist)
             (push c (cdr p))))))
      (setf def (reverse def))
      (iter
        (for p in alist)
        (setf (cdr p) (reverse (cdr p))))
      (values alist def))))


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

  ;; See <https://github.com/Clozure/ccl/issues/229>.
  #-ccl (declare (type (simple-array hash-type (32)) a-coeffs b-coeffs))
  #+ccl (declare (type simple-array a-coeffs b-coeffs))

  ;; functions, methods defined here can use a-coeffs, b-coeffs
  ;; at lower cost than special variables

  (defun ast-combine-list-hash-values (args)
    "Given a list of hash values, combine them using a polynomial in P,
modulo +AST-HASH-BASE+.  0 values in ARGS are skipped."
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

  (defun ast-combine-hash-values (&rest args)
    "Invoke `ast-combine-list-hash-values' on ARGS."
    (declare (dynamic-extent args))
    (ast-combine-list-hash-values args))

  (defun ast-combine-simple-vector-hash-values (sv)
    (declare (type simple-vector sv))
    (let ((result 0)
          (hb +ast-hash-base+)
          (i 0))
      (declare (type hash-type result)
               (array-index i))
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
    (ast-combine-list-hash-values
     (cons 16335929882652762
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
            (pop l)))))

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

(defmethod ast-hash ast-combine-hash-values ((slot-specifier slot-specifier))
  (ast-hash (list 'slot-specifier
                  (slot-specifier-class slot-specifier)
                  (slot-specifier-slot slot-specifier))))


;;; Generic functions on ASTs
(defgeneric to-alist (struct)
  (:documentation "Convert struct to alist representation."))

(defgeneric from-alist (symbol alist)
  (:documentation "Convert alist to struct representation."))

(defun source-text-take-lines (n ast)
  "Return (at most) N lines of the source text of AST in constant time."
  (let ((dest (make-string-output-stream)))
    (tagbody
       (source-text
        ast
        :stream (make-limit-stream dest
                                   (lambda ()
                                     (go :finish))
                                   :newline-limit n))
     :finish
       (return-from source-text-take-lines
         ;; The restriction is approximate so there may actually be
         ;; more than one line.
         (take n (lines (get-output-stream-string dest) :count n))))))

(defgeneric source-text (ast &key stream &allow-other-keys)
  (:documentation "Return the source code corresponding to an AST,
optionally writing to STREAM.")
  (:method :around ((ast t) &rest args &key stream)
    (let (*print-pretty*)
      (with-string (s stream)
        (apply #'call-next-method ast :stream s (plist-drop :stream args)))))
  (:method ((ast null) &key stream)
    (write-string "" stream))
  (:method ((str string) &key stream)
    (write-string str stream))
  (:method ((c character) &rest args &key)
    (apply #'source-text (string c) args))
  (:method ((s symbol) &rest args &key)
    (apply #'source-text (string s) args))
  (:method ((c conflict-ast) &rest args &key stream)
    (format stream "<")
    (iter (for e on (conflict-ast-child-alist c))
          (format stream "~a: " (caar e))
          (iter (for x in (cdar e))
                (apply #'source-text x :stream stream args))
          (when (cdr e) (format stream "|")))
    (format stream ">"))
  (:method ((ast ast) &rest args &key)
    ;; In performance comparison the combination of
    ;; `with-output-to-string' and `write-string' was faster than
    ;; alternatives using `format' (which was still pretty fast) and
    ;; using `concatenate' (which was slow).
    ;;
    ;; More importantly using (apply #'concatenate ...) runs into
    ;; problems as the number of ASTs is very large.
    (mapc (lambda (ast)
            (apply #'source-text ast args))
          (children ast)))
  (:method ((software parseable) &rest args &key)
    (apply #'source-text (genome software) args)))

(defun source-text-take (n ast)
  "Take (at most) N characters from the source text of AST in constant
time."
  (let ((dest (make-string-output-stream)))
    (tagbody
       (source-text
        ast
        :stream (make-limit-stream dest
                                   (lambda ()
                                     (go :finish))
                                   :char-limit n))
     :finish
       (return-from source-text-take
         ;; The restriction is approximate so there may actually be
         ;; more than N chars.
         (take n (get-output-stream-string dest))))))

(defgeneric source-text= (x y)
  (:documentation "Return T if X and Y have the same source text under `string='.")
  (:method-combination standard/context)
  (:method :context (x y)
    (or (eql x y)
        (call-next-method)))
  (:method ((x string) (y string))
    (string= x y))
  (:method ((x ast) (y string))
    (source-text= y x))
  (:method ((x string) (y ast))
    ;; Add 1 to the length in case X is a prefix of Y.
    (string= x (source-text-take (1+ (length x)) y)))
  (:method ((x string) (y t))
    (string= x (source-text y)))
  (:method ((x t) (y string))
    (string= (source-text x) y))
  (:method (x y)
    (string= (source-text x) (source-text y))))

;;; This avoids warnings from Trivia about not being able to infer the
;;; type of source-text= tests.
(type-i:define-inference-rule source-text= (test)
  (declare (ignore test))
  '((typep type-i:? '(or ast string))))

(defpattern source-text= (arg)
  "Compare the source text against ARG (evaluated)."
  (with-unique-names (it)
    `(guard1 (,it) (source-text= ,it ,arg))))

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

(defgeneric collect-if (predicate tree &key key)
  (:documentation
   "Traverse TREE collecting every node that satisfies PREDICATE.")
  (:method ((predicate function) (tree ast) &key (key #'identity))
    ;; reverse it to maintain the order it was found in.
    (fbind (predicate)
      (with-item-key-function (key)
        (nreverse
         (reduce (lambda (accum ast)
                   (if (predicate (key ast))
                       (cons ast accum)
                       accum))
                 tree)))))
  (:method ((predicate function) (tree parseable) &key (key #'identity))
    (collect-if predicate (genome tree) :key key)))


;;; parseable software objects
(defgeneric roots (obj)
  (:documentation "Return all top-level ASTs in OBJ."))

(defgeneric interleaved-text (obj)
  (:documentation "Get the interleaved text for OBJ.")
  (:method (obj) nil))

(defgeneric asts (obj)
  (:documentation "Deprecated: Return a list of all non-root ASTs in OBJ."))

(defgeneric get-parent-ast (obj ast)
  (:documentation "Return the parent node of AST in OBJ")
  (:method (obj (ast ast))
    (let ((path (ast-path obj ast)))
      (cond ((null path) nil)
            ((single path)
             ;; The parent is the top-level AST.
             (@ obj nil))
            (t (@ obj (butlast path)))))))

(defgeneric get-parent-asts (obj ast)
  (:documentation "Return the parent nodes of AST in OBJ including AST.
Parents are returned deepest-first.

If OBJ is an AST, it is also included. ")
  (:method (obj (ast ast))
    (nest (remove-if-not {typep _ 'ast})  ; Remove non-ASTs.
          (mapcar {lookup obj})           ; Lookup each prefix.
          (maplist #'reverse) (reverse)   ; Prefixes of path.
          (ast-path obj ast)))
  (:method ((obj ast) (ast ast))
    (append1 (call-next-method) obj)))

(defgeneric get-parent-asts* (obj ast)
  (:documentation "Return the parent nodes of AST in OBJ not including AST.
Parents are returned deepest-first.")
  (:method (obj (ast ast))
    (cdr (get-parent-asts obj ast))))

(defgeneric get-parent-full-stmt (software ast)
  (:documentation
   "Return the first ancestor of AST in SOFTWARE which is a full statement.
Returns nil if no full statement parent is found."))

(defgeneric is-stmt-p (ast)
  (:documentation "Returns T if the AST is a full statement, NIL otherwise.")
  (:method ((ast ast)) nil))

(defgeneric get-ast-types (software ast)
  (:documentation "Types directly referenced within AST."))

(defgeneric get-unbound-funs (software ast)
  (:documentation "Functions used (but not defined) within the AST."))

(defgeneric get-unbound-vals (software ast &key &allow-other-keys)
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
    (find-if predicate (get-parent-asts* obj ast))))

(defgeneric built-ins (software)
  (:documentation "Return a list of built-in identifiers available in SOFTWARE."))

;;; Note a generic analysis because it can depend on the environment.
(defgeneric scopes (software ast)
  (:documentation "Return lists of variables in each enclosing scope.
Each variable is represented by an alist containing :NAME, :DECL, :TYPE,
and :SCOPE.

Scopes are returned innermost-first."))

(defun scope-table (software)
  "Build a table from nodes to bindings in SOFTWARE."
  (let ((table (make-hash-table)))
    (iter (for node in-tree (genome software))
          (iter (for scope in (scopes software node))
                (iter (for bind in scope)
                      (let ((scope-ast (aget :scope bind)))
                        (pushnew bind
                                 (gethash scope-ast table)
                                 :test #'equal)))))
    (iter (for (k v) in-hashtable table)
          (setf (gethash k table) (nreverse v)))
    table))

(defgeneric all-scopes (software)
  (:documentation "Return a flat list of all scopes in SOFTWARE. For a
tree that preserves how scopes are nested, use `scope-tree'.")
  (:method ((software parseable))
    (apply #'append (hash-table-values (scope-table software)))))

(defgeneric scope-tree (software &key table)
  (:documentation "Return a tree of scopes in SOFTWARE.")
  (:method ((root ast) &key table)
    (let ((subtrees (filter-map
                     (lambda (child)
                       (scope-tree child :table table))
                     (children root))))
      (if-let (scope (gethash root table))
        (cons scope subtrees)
        subtrees)))
  (:method ((software parseable) &key (table (scope-table software)))
    (scope-tree (genome software) :table table)))

(defun find-if-in-scopes (predicate scopes &key (key #'identity))
  "Return the first binding in SCOPES that satisfies PREDICATE."
  (some (lambda (scope)
          (find-if predicate scope :key key))
        scopes))

(defgeneric get-vars-in-scope (software ast &optional keep-globals)
  (:documentation "Return all variables in enclosing scopes."))

(defgeneric parse-asts (software &optional source-text)
  (:documentation "Parse genome of SOFTWARE into an AST representation.

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
  (:documentation "Select suitable crossover point in A and B. The point is
 represented as a path which refers to a node in both software objects.
 If no suitable point is found the returned point may be nil.")
  (:method ((a parseable) (b parseable))
    (let* ((pt-a (random-elt (select-crossover-points-pool a b)))
           (path-a (ast-path (genome a) pt-a))
           (pt-b (ignore-errors (@ (genome b) path-a))))
      (and pt-a pt-b path-a))))

(defgeneric select-crossover-points-pool (a b)
  (:documentation "Return a list of ASTs in A suitable for use in crossover.")
  (:method ((a parseable) (b parseable))
    (declare (ignorable b))
    (convert 'list a)))

(defmethod crossover ((a parseable) (b parseable))
  "Perform single-point crossover between two parseable software
 objects, and returning a new software object as the result. If
 an error occurs, returns NIL.
 Algorithm:
 A random node of the tree 'a' is chosen, its path calculated,
 and if there is a node at the corresponding path in 'b', then that
 node will replace the corresponding node in the genome of 'a'.
 In order to update the genome of 'a' the genome is copied and the resulting
 modified genome is stored in 'a'."
  (when-let* ((point-path (select-crossover-points a b))
              (pt-a (@ (genome a) point-path))
              (pt-b (@ (genome b) point-path))
              (new-a (with a pt-a pt-b)))
          (return-from crossover
            (values new-a pt-a pt-b)))
  (values a nil nil))  ; it didn't complete, just return original

(defgeneric shares-path-of-p (obj target-ast shared-path-ast)
  (:documentation "Returns T if TARGET-AST has the same path or a super-path
of SHARED-PATH-AST's path in OBJ.")
  (:method ((obj parseable) target-ast shared-path-ast)
    (starts-with-subseq (ast-path obj shared-path-ast)
                        (ast-path obj target-ast)
                        :test #'equal))
  (:method ((root ast) target-ast shared-path-ast)
    (starts-with-subseq (ast-path root shared-path-ast)
                        (ast-path root target-ast)
                        :test #'equal)))

(defgeneric ancestor-of-p (obj target-ast ancestor)
  (:documentation "Returns T if ANCESTOR is an ancestor of TARGET-AST in OBJ.")
  (:method ((obj parseable) target-ast ancestor)
    (unless (eq target-ast ancestor)
      (shares-path-of-p obj target-ast ancestor)))
  (:method ((root ast) target-ast ancestor)
    (unless (eq target-ast ancestor)
      (shares-path-of-p root target-ast ancestor))))

(defun descendant-of-p (obj target-ast descendant)
  "Returns T if DESCENDANT is a descendant of TARGET-AST in OBJ."
  (ancestor-of-p obj descendant target-ast))


;;; Core parseable methods
(defmethod lookup ((obj parseable) key)
  ;; Enables the use of the `@' macro directly against parseable objects.
  (lookup (genome obj) key))

(defmethod convert (type (obj parseable) &key &allow-other-keys)
  (convert type (genome obj)))

(eval-always
  (defun sequence-passthrough-for (name)
    "Return a defmethod form to define a passthrough method for NAME.
When NAME is called on a software object it will then be invoked on
the `genome' of the software object."
    (let ((lambda-list (generic-function-lambda-list (ensure-function name))))
      `(defmethod ,name
           ,(substitute '(collection parseable) 'collection
                        (append (take-until {eql '&key} lambda-list)
                                (list '&key)
                                (mapcar (lambda (key)
                                          (list key nil
                                                (symbol-cat-in-package 'fset key 'p)))
                                        (cdr (member '&key lambda-list)))))
         (apply ',name ,@(subseq lambda-list 0 (1- (position '&key lambda-list)))
                (genome collection)
                (append
                 ,@(mapcar (lambda (key)
                             `(when ,(symbol-cat-in-package 'fset key 'p)
                                (list ,(make-keyword key) ,key)))
                           (cdr (member '&key lambda-list)))))))))

;;; FSet overrides for common-lisp sequence functions pass through to genome.
(defmacro write-sequence-function-parseable-methods (&rest names)
  "Write sequence passthrough methods for NAMES using `sequence-passthrough-for'."
  `(progn ,@(mapcar #'sequence-passthrough-for names)))

;;; FSet tree manipulations pass through to genome.
(eval-always
  (defun tree-manipulation-passthrough-for (name)
    "Return a defmethod form to define a passthrough method for NAME.
When NAME is called on a software object it will then be invoked on
the `genome' of the software object."
    (let ((lambda-list (generic-function-lambda-list (ensure-function name))))
      `(defmethod ,name ((obj parseable) ,@(cdr lambda-list))
         (copy obj
               :genome
               (,name (genome obj)
                      ,(second lambda-list)
                      ,@(remove '&optional (cddr lambda-list))))))))

(defmacro write-tree-manipulation-function-parseable-methods (&rest names)
  "Write tree-manipulation passthrough methods for NAMES using `tree-manipulation-passthrough-for'."
  `(progn ,@(mapcar #'tree-manipulation-passthrough-for names)))

(write-sequence-function-parseable-methods
 reduce
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
 substitute)

(write-tree-manipulation-function-parseable-methods
 with
 insert
 splice)

(defmethod less ((obj parseable) value1 &optional value2)
  (declare (ignorable value2))
  (copy obj :genome (less (genome obj) value1)))

(defmethod mapc (function (obj parseable) &rest more)
  (declare (ignorable more))
  (mapc function (genome obj))
  obj)

(defmethod mapcar (function (obj parseable) &rest more)
  (declare (ignorable more))
  (copy obj :genome (mapcar function (genome obj))))

(defmethod size ((obj parseable))
  "Return the number of non-root ASTs in OBJ."
  (1- (count-if {typep _ 'ast} (genome obj))))

(defgeneric find-deepest (function AST)
  (:documentation "Find the deepest node in AST satisfying FUNCTION.")
  (:method (function (ast ast) &aux (deepest 0) result)
    (do-tree (node ast :index rpath :value result)
      (when (and (funcall function node)
                 (length> rpath deepest))
        (setf result node
              deepest (length rpath)))
      nil)))

(defmethod genome-string ((obj parseable) &optional stream)
  "Return the source code of OBJ, optionally writing to STREAM"
  (with-string (s stream)
    (with-slots (genome) obj
      (if (stringp genome)
          (write-string genome s)
          (source-text genome :stream s)))))

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
  ;; TODO: remove this if it is not needed as it appears to
  ;;       have been used as a work-around for a clang issue.
  (handler-bind
      ((error (lambda (e)
                (declare (ignore e))
                (when-let ((ofile (original-path sw)))
                  (warn "Failure in parse-asts: original-path = ~a"
                        ofile)))))
    (call-next-method)))

(defgeneric ast-source-ranges (obj)
  (:documentation "Return (AST . SOURCE-RANGE) for each AST in OBJ.")
  (:method ((root ast))
    (labels ((source-range (begin end)
               "Thin wrapper for creating a source range."
               (make-instance 'source-range :begin begin :end end))
             (children-with-text-nodes (ast)
               (let* (;; NB Clang ASTs have strings as children.
                      (child-nodes (sorted-children ast))
                      (interleaved-text (interleaved-text ast)))
                 (append (iter (for node in child-nodes)
                               (when-let (text (pop interleaved-text))
                                 (collect text))
                               (collect node))
                         interleaved-text)))
             (ast-source-positions (ast start)
               (if (stringp ast)
                   (list (list ast start (+ start (length ast))))
                   (let ((text (source-text ast)))
                     (unless (emptyp text)
                       (let ((children (children-with-text-nodes ast)))
                         (cons (list ast start (+ start (length text)))
                               (iter (for child in children)
                                     (for text = (source-text child))
                                     (appending (ast-source-positions child start))
                                     (incf start (length text))))))))))
      (let* ((text (source-text root))
             (newline-offsets (precompute-newline-offsets text)))
        (iter (for (ast start end) in (ast-source-positions root 0))
              (unless (stringp ast)
                (let ((range
                       (source-range
                        (position->source-location text start newline-offsets)
                        (position->source-location text end newline-offsets))))
                  (collect (cons ast range))))))))
  (:method ((obj parseable))
    (ast-source-ranges (genome obj))))

(defgeneric asts-containing-source-location (software location)
  (:documentation "Return a list of ASTs in SOFTWARE containing LOC.")
  (:method ((obj parseable) (loc source-location))
    (nest (mapcar #'car)
          (remove-if-not [{contains _ loc} #'cdr])
          (ast-source-ranges obj)))
  (:method ((ast ast) (loc source-location))
    (nest (mapcar #'car)
          (remove-if-not [{contains _ loc} #'cdr])
          (ast-source-ranges ast)))
  (:method ((obj parseable) (location integer))
    (asts-containing-source-location
     obj
     (position->source-location (genome-string obj) location)))
  (:method ((ast ast) (location integer))
    (asts-containing-source-location
     ast
     (position->source-location (source-text ast) location))))

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

(defmethod source-range (software ast)
  "Collect the source-range for AST and convert from 1-indexed to 0-indexed."
  (flet ((0-index (location)
           (with-slots (line column) location
             (make-instance 'source-location
               :line (1- line) :column (1- column)))))
    (when-let ((range (aget ast (ast-source-ranges software))))
      (with-slots (begin end) range
        (make-instance 'source-range
          :begin (0-index begin)
          :end (0-index end))))))


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
                     :key {aget :name}
                     :test #'equal))

(defgeneric get-function-from-function-call (obj funcall-ast)
  (:documentation "Return the function ast associated with the
FUNCALL-AST if it exists in OBJ."))

(defgeneric map-arguments-to-parameters (obj funcall-ast)
  (:documentation "Return an alist mapping parameters of a function
in OBJ to its arguments in FUNCALL-AST."))

(defgeneric assign-to-var-p (ast var)
  (:documentation "Return TRUE if AST represents an assignment to VAR."))


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
(defmethod apply-mutation :around ((software parseable)
                           (mutation parseable-mutation))
  "Apply MUTATION to SOFTWARE, returning the resulting SOFTWARE.
* SOFTWARE object to be mutated
* MUTATION mutation to be performed
"
  (restart-case
      (call-next-method)
    (skip-mutation ()
      :report "Skip mutation and return nil"
      (values nil 1))
    (retry-mutation ()
      :report "Retry the mutation"
      (apply-mutation software mutation))
    (mutate ()
      :report "Apply another mutation before re-attempting mutations"
      (mutate software)
      (apply-mutation software mutation))))

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


;;; @subheading Structured text in ASTs
;;;
;;; (Rough notes from our preliminary meeting.)
;;;
;;; We will run an initial experiment on a selection (one or two) of
;;; AST classes to try to replace the use of interleaved-text with
;;; @it{structured text} -- meaning a class-specific collection of
;;; named text slots (or maybe a single slot with a format string)
;;; holding text which may be in certain specific places in the
;;; printed AST.  (A common working example driving our discussion was
;;; the text between the "if" of an if AST and the "(" starting the
;;; conditional of the if.)
;;;
;;; The anticipated restructuring is as follows:
;;;
;;; * We add define a set of fixed text slots for the ASTs.
;;;
;;; * We define delimiters on the AST class (e.g., semicolons to
;;;   separate statements in a block or commas to separate arguments
;;;   to a function).
;;;
;;; * The text around the delimiters is pushed down into the before
;;;   and after strings of the relevant AST children's interleaved
;;;   text.
;;;
;;; * Comments (and possibly indentation strings) are optionally
;;;   objects inside of these text slots.
;;;
;;; We want to ensure that the following use cases are easy:
;;;
;;; * Adding, removing, and moving ASTs without having to explicitly
;;;   fixup delimiters.
;;;
;;; * Matching, accessing and constructing ASTs.
;;;
;;; * Exactly reproducing the original text of a modified source file
;;;   (aside from the specifically modified portions).
;;;
;;; @texi{structured-text}
(defvar a-place-holder-so-documentation-continues-to-build nil
  "Turns out it is necessary to split the previous and subsequent comments.")


;;; The insertion and removal of ASTs in a software object often results
;;; in indentation that doesn't match the surrounding code. The indentation
;;; mix-in can be used to address these situations and help prevent
;;; undesirable indentation by maintaining the expected indentation when an AST
;;; is inserted into a new position. This is accomplished by storing the current
;;; line's indentation, and the amount of indentation that should be propagated
;;; to children and the difference between the indentation provided by all of
;;; the AST's parents. To simplify the implementation, all tabs are treated
;;; as spaces.
;;;
;;; @subsubheading Mix-ins
;;;
;;; The ability to reinsert tabs is provided through the software-indentation
;;; mix-in. The following slots are provided:
;;;
;;; * 'SPACES-PER-TAB
;;;     A number that indicates how many spaces each tab is worth.
;;;     The initial form is 4.
;;;
;;; * 'INDENT-WITH-TABS-P
;;;     A boolean value indicating whether tabs should be used over spaces. The
;;;     initial form is nil.
;;;
;;; The ast-indentation mix-in is used to add indentation information to an AST.
;;; It provided the following slots:
;;;
;;; * 'INDENT-CHILDREN
;;;     A number that indicates how many spaces should be added after newlines
;;;     the precede a child of the AST. The value 'T can be provided,
;;;     and the slot's value will be populated lazily with its parent's value or
;;;     a sane default based on the rest of the file. The initial form is NIL.
;;;
;;; * 'INDENT-ADJUSTMENT A number (possibly negative that indicates
;;;     the difference in indentation provided by an AST's parents and
;;;     the spaces on its current line. This value is also added to
;;;     the indentation given to children. The value NIL can be
;;;     provided, and a value will instead be used that will match the
;;;     indentation of the current line with that of its parent. The
;;;     initial form is NIL. Note that 0 and NIL are not the same: zero
;;;     indentation adjustment means the node will rigidly retain its
;;;     original indentation.
;;;
;;; @subsubheading Parsing
;;; When using the indentation mix-in for software, the parser for the
;;; language needs to be modified to not insert whitespace after newlines,
;;; instead opting for storing this information in the indentation slots of an
;;; AST. This is accomplished by tracking the indentation that each AST provides
;;; to its children. When a newline is reached for a child, the current
;;; indentation provided by all of its parents is compared to the indentation
;;; provided for the child. If the indentation is different, the difference is
;;; used to to set either the parent's indent-children slot when it hasn't been
;;; changed from the default value or the indent-adjustment slot of the
;;; child otherwise.
;;;
;;; @subsubheading Converting to Text
;;; The source-text method also needs to be modified when using the indentation
;;; mix-in. While converting an AST to text, the interleaved text needs to be
;;; examined for newlines. When a newline is found in a string and nothing
;;; follows it, the next AST reached determines the amount of indentation
;;; needed. This will be the indentation provided by the indentation slots of
;;; all of its parents plus the indentation provided by its indent-adjustment
;;; slot. If a newline is found in a string and other text follows it,
;;; indentation is added that matches the current indentation of its parent.
;;;
;;; @texi{indentation}
(defclass software-indentation ()
  ((spaces-per-tab :accessor spaces-per-tab
                   :initform 4
                   :initarg :spaces-per-tab)
   (indent-with-tabs-p :accessor indent-with-tabs-p
                       :initform nil
                       :initarg :indent-with-tabs-p)))

(defclass indentation ()
  ((indent-children :accessor indent-children
                    :initform nil
                    :initarg :indent-children)
   (indent-adjustment :accessor indent-adjustment
                      :initform nil
                      :initarg :indent-adjustment)))

(defparameter *spaces-per-tab* 4
  "The number of spaces per tab. This can be set to modify the
behavior of #'source-text and #'convert")

(defparameter *indent-with-tabs-p* nil
  "A boolean value that indicates whether tabs should be used instead of spaces.
This can be set to modify the behavior of #'source-text and #'convert")

(defgeneric get-default-indentation (ast parents)
  (:documentation "Get a sane indent-children default for AST.")
  (:method (ast parents)
    (declare (ignorable ast parents))
    *spaces-per-tab*))

(defgeneric get-indentation-at (ast parents)
  (:documentation "Get the indentation for AST given PARENTS.")
  (:method ((ast indentation) (parents list))
    (reduce (lambda (total parent)
              (+ total
                 (or (indent-adjustment parent) 0)
                 (or (indent-children parent) 0)))
            parents
            :initial-value (or (indent-adjustment ast) 0))))

(defmethod copy :around ((ast indentation) &key &allow-other-keys)
  ;; TODO: these are also being copied to the annotations slot
  ;;       in another specializer.
  (let ((copy (call-next-method)))
    (setf (indent-children copy) (indent-children ast)
          (indent-adjustment copy) (indent-adjustment ast))
    copy))

(defmethod tree-copy :around ((ast indentation))
  ;; TODO: this is for #'apply-mutation-ops. Not sure
  ;;       if this is the correct way to fix this problem
  ;;       though.
  (let ((copy (call-next-method)))
    (setf (indent-children copy) (indent-children ast)
          (indent-adjustment copy) (indent-adjustment ast))
    copy))

(defgeneric indentablep (ast)
  (:documentation "Return T if AST is indentable. This
is useful for ASTs that may have newline literals.")
  (:method (ast) t))

(defparameter *is-computing-ast-source-ranges* nil
  "Global indicating if we are AST computing source ranges.")

(define-condition node-location ()
  ((ast :initarg :ast :reader node-location-ast)))

(defmethod source-text :around ((ast indentation) &key stream)
  (declare (ignore stream))
  (cond
    (*is-computing-ast-source-ranges*
     (with-simple-restart (continue "") (signal 'node-location :ast ast))
     (multiple-value-prog1 (call-next-method)
       (with-simple-restart (continue "") (signal 'node-location :ast ast))))
    (t (call-next-method))))

(defmethod ast-source-ranges ((ast indentation))
  (flet ((source-range (begin end)
           (make 'source-range :begin begin :end end)))
    (let* ((*is-computing-ast-source-ranges* t)
           (string-stream (make-string-output-stream))
           (positions
            (serapeum:collecting
             (handler-bind ((node-location
                             (lambda (c)
                               (collect
                                (cons (node-location-ast c)
                                      (file-position string-stream)))
                               (invoke-restart 'continue))))
               (source-text ast :stream string-stream))))
           (text (get-output-stream-string string-stream))
           (assorted (assort positions :key [#'serial-number #'car] :hash t))
           (newline-offsets (precompute-newline-offsets text)))
      (iter (for ((ast . start) (nil . end)) in assorted)
            (collect
             (cons ast
                   (source-range
                    (position->source-location text start
                                               newline-offsets)
                    (position->source-location text end
                                               newline-offsets))))))))

(-> ast-source-range (t t) (or null source-range))
(defun ast-source-range (software ast)
  "Return the source range for AST in SOFTWARE."
  (values (assocdr ast (ast-source-ranges software))))

(-> ast-start+end (t t)
    (values (or null source-location) (or null source-location) &optional))
(defun ast-start+end (software ast)
  "Return the start and end of AST in SOFTWARE (if any) as source
locations."
  (if-let (range (assocdr ast (ast-source-ranges software)))
    (values (begin range)
            (end range))
    (values nil nil)))

(-> ast-start (t t) (or null source-location))
(defun ast-start (software ast)
  "Return the start of AST in software, as a source location."
  (values (ast-start+end software ast)))

(-> ast-end (t t) (or null source-location))
(defun ast-end (software ast)
  "Return the end of AST in software, as a source location."
  (nth-value 1 (ast-start+end software ast)))


;;; Language to symbol mapping

;;; TODO: figure out if this is the best place for language alias functionality.
;;;       Considering that all supported languages stem from parseable, it may at
;;;       least be a reasonable place.
(defgeneric language-alias->language-symbol (alias-string &key &allow-other-keys)
  (:documentation "Return the class symbol which corresponds to the language
represented by ALIAS-STRING.")
  (:method (alias-string &key &allow-other-keys)
    nil)
  (:method ((alias-string string) &rest rest &key &allow-other-keys)
    (apply #'language-alias->language-symbol
           (find-keyword (string-upcase alias-string))
           rest))
  (:method ((alias-string (eql 'text)) &key &allow-other-keys)
    'simple))

(defgeneric language-symbol->language-aliases (symbol)
  (:documentation "Return the list of aliases which corresponds to the language
represented by ALIAS-STRING.")
  (:method ((symbol symbol))
    nil))

(defmacro define-language-alias-mappings (return-symbol (&rest alias-strings))
  (let ((aliases (adjoin (string-downcase return-symbol)
                         alias-strings
                         :test #'equal)))
    `(progn
       (defmethod language-symbol->language-aliases ((symbol (eql ',return-symbol)))
         ',aliases)
       ,@(iter
          (for string in aliases)
          (collect
           `(defmethod language-alias->language-symbol
                ((alias-string (eql ,(make-keyword (string-upcase string))))
                 &key &allow-other-keys)
              ',return-symbol))))))


;;; Attributes
(defmethod apply-mutation :around ((obj parseable) mutation)
  ;; Ensure an attribute table is available.
  (with-attr-table (genome obj)
    (call-next-method)))

(defmethod mutate :around ((obj parseable))
  ;; Ensure an attribute table is available.
  (with-attr-table (genome obj)
    (call-next-method)))


;;; Logging

(defmethod log-message ((ast ast) &key root &allow-other-keys)
  (assert root)
  (format nil "AST-PATH: '~a'
POSITION RANGE: ~a~%"
          (ast-path root ast)
          (source-range root ast)))

(defmethod log-message ((software parseable) &key &allow-other-keys)
  (format nil "FILE-PATH: '~a'~%"
          (original-path software)))
