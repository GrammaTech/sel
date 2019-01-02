;;; ast.lisp --- Applicative ASTs structure
;;;
;;; The machinery to make applicative ASTs.  They're structures.
;;;
;;; * Define them with (define-ast foo slot-descriptions)
;;; * Create them with (make-foo ... )
;;; * Copy them with COPY.
;;;

(defpackage :software-evolution-library/software/ast
  (:nicknames :sel/software/ast :sel/sw/ast)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :software-evolution-library
        :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:export :ast
           :define-ast
           :define-immutable-node-struct
           :to-alist
           :from-alist
           :ast-path
           :ast-node
           :ast-class
           :ast-aux-data
           :source-text
           :rebind-vars
           :replace-in-vars
           :insert-ast
           :insert-ast-after
           :replace-nth-child
           :replace-ast
           :remove-ast
           :splice-asts
           :fixup-mutation
           :ast-children
           :ast-text
	   :ast-hash
           :to-ast
           :ast-later-p
	   :map-ast
	   :ast-meld?
	   :ast-class-meld?
           :replace-in-ast))
(in-package :software-evolution-library/software/ast)
(in-readtable :curry-compose-reader-macros)


;;; Data structure definitions
(defstruct ast-node "Base type of immutable portion of ast nodes.")

(defstruct (ast (:constructor make-raw-ast))
  "Base type of sub-tree of an applicative AST tree."
  (path nil :type list)                      ; Path to subtree from root of tree.
  (node nil :type (or ast-node string null)) ; Pointer to immutable AST data.
  (children nil :type list)                  ; Remainder of subtree.
  (stored-hash nil :type (or null fixnum)))

(defmethod print-object ((obj ast) stream)
  "Print a representation of the ast OBJ to STREAM, including
the ast path and source text.
* OBJ ast to print
* STREAM stream to print OBJ to
"
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream ":PATH ~s ~:_ :AST ~s ~:_ :TEXT ~a"
                (ast-path obj) (ast-node obj) (source-text obj)))))

(defgeneric ast-class (ast) (:documentation "Class of AST."))

(defmacro define-immutable-node-struct (name-and-options &rest slot-descriptions)
  (with-gensyms ((obj obj-))
    (let* ((name (get-struct-name name-and-options))
           (conc-name (get-struct-conc-name name-and-options))
           (slot-names (get-struct-slot-names slot-descriptions))
           (names-w-types (mapcar (lambda (slot)
                                    (if (listp slot)
                                        (list (car slot)
                                              (or (plist-get :type slot) t))
                                        (list slot t)))
                                  (remove-if #'stringp slot-descriptions))))
      `(prog1
           ;; Define the immutable structure.
           (defstruct (,name ,@(when (listp name-and-options)
                                 (remove-if
                                  (lambda (pair)
                                    (member (car pair) '(:conc-name)))
                                  (cdr name-and-options))))
             ,(format nil "Immutable structure holding ~a slots." name)
             ;; Define slots, add ':read-only t'
             ,@(mapcar (lambda (slot)
                         (if (listp slot)
                             (plist-merge slot '(:read-only t))
                             (list slot nil ':read-only t)))
                       (remove-if #'stringp slot-descriptions)))
         ;; Copy method
         (defmethod copy
             ((,obj ,name)
              &key
                ,@(mapcar (lambda (name)
                            `(,name nil ,(symbol-cat name 'supplied-p)))
                          slot-names))
           (if (or ,@(mapcar {symbol-cat _ 'supplied-p} slot-names))
               (funcall ',(symbol-cat 'make name)
                        ,@(mappend (lambda (name)
                                     (list (make-keyword name)
                                           `(if ,(symbol-cat name 'supplied-p)
                                                ,name
                                                (,(symbol-cat conc-name name)
                                                  ,obj))))
                                   slot-names))
               ,obj))
         ;; Define accessors for fields.
         ,@(mapcar
            (lambda (slot)
              `(defmethod ,(symbol-cat conc-name slot) ((,obj ,name))
                 (,(symbol-cat name slot) ,obj)))
            slot-names)
         ;; Define the alist conversion functions.
         (defmethod from-alist ((,obj (eql ',name)) alist)
           (declare (ignorable ,obj))
           (funcall ',(symbol-cat 'make name)
                    ,@(mappend (lambda-bind ((name type))
                                   (let ((name (make-keyword name)))
                                     (list name
                                           (if (or (eql type 'symbol)
                                                   (and (listp type)
                                                        (member 'symbol type)))
                                               `(make-keyword
                                                 (string-upcase
                                                  (aget ,name alist)))
                                               `(aget ,name alist)))))
                                 names-w-types)))
           (defmethod to-alist ((,obj ,name))
             (list ,@(mapcar
                       (lambda-bind ((name type))
                         `(cons ,(make-keyword name)
                                ,(if (or (eql type 'symbol)
                                         (and (listp type)
                                              (member 'symbol type)))
                                     `(symbol-name
                                       (,(symbol-cat conc-name name) ,obj))
                                     `(,(symbol-cat conc-name name) ,obj))))
                       names-w-types)))))))

(defmacro define-ast (name-and-options &rest slot-descriptions
                      &aux (default-ast-slot-descriptions
                             '((class nil :type symbol)
                               (aux-data nil :type list))))
  "Implicitly defines an AST wrapper for the defined AST-node."
  (with-gensyms ((obj obj-))
    (let* ((doc-strings (remove-if-not #'stringp slot-descriptions))
	   (slot-descriptions (remove-if #'stringp slot-descriptions))
	   (all-slot-descriptions (append default-ast-slot-descriptions
                                          slot-descriptions))
           (name (get-struct-name name-and-options))
           (conc-name (get-struct-conc-name name-and-options))
           (slot-names (get-struct-slot-names all-slot-descriptions)))
      `(prog2
           ;; Define the immutable node.
           (define-immutable-node-struct (,(symbol-cat name 'node)
                                          (:include ast-node)
					   (:conc-name ,conc-name))
	     ,@doc-strings
             ,@all-slot-descriptions)
           ;; Define and return the wrapper.
           (defstruct (,name (:include ast)
                             (:copier nil)
                             ,@(when (listp name-and-options)
                                 (remove-if
                                  (lambda (pair)
                                    (member (car pair)
                                            '(:conc-name :copier :include)))
                                  (cdr name-and-options))))
	     ,@doc-strings)
           ;; Copy method for light weight copies of wrapper only.
           (defmethod copy
               ((,obj ,name)
                &key path
                     (children nil children-supplied-p)
                     ,@(mapcar (lambda (name)
                                 `(,name nil ,(symbol-cat name 'supplied-p)))
                               slot-names))
             (,(symbol-cat 'make name)
               :path path
               :node (if (or ,@(mapcar {symbol-cat _ 'supplied-p} slot-names))
                         (funcall #'copy
                           (,(symbol-cat name 'node) ,obj)
                           ,@(mappend
                               (lambda (name)
                                 (list (make-keyword name)
                                       `(if ,(symbol-cat name 'supplied-p)
                                            ,name
                                            (,(symbol-cat conc-name name)
                                             ,obj))))
                               slot-names))
                        (,(symbol-cat name 'node) ,obj))
               :children (if children-supplied-p
                             children
                             (,(symbol-cat name 'children) ,obj))))
           ;; Define accessors for inner fields on outer structure.
           ,@(mapcar
               (lambda (slot)
                 `(defmethod ,(symbol-cat conc-name slot) ((,obj ,name))
                    (,(symbol-cat conc-name slot) (ast-node ,obj))))
               slot-names)))))

(defun to-ast (ast-type spec)
  "Walk a potentially recursive AST SPEC creating an AST-TYPE AST.
A SPEC should have the form

  (ast-class <optional-keyword-args-to-`make-<AST-TYPE>-node'>
             CHILDREN)

where CHILDREN may themselves be specifications suitable for passing
to `to-ast`.  E.g.

  (to-ast 'clang-ast (:callexpr (:implicitcastexpr
                                 :includes '(\"<string.h>\")
                                 \"\" \"(|strcpy|)\" \"\")
                                \"(\" \"arg-1\" \",\" \"arg-2\" \")\"))"
  (labels ((convert-to-node (spec)
             (destructuring-bind (class &rest options-and-children) spec
               (multiple-value-bind (keys children)
                   (iter (for item in options-and-children)
                         (with previous)
                         (if (or (keywordp previous)
                                 (keywordp item))
                             ;; Collect keyword arguments.
                             (collect item into keys)
                             ;; Process lists as new AST nodes.
                             (if (listp item)
                                 (collect (convert-to-node item) into children)
                                 (collect item into children)))
                         (setf previous item)
                         (finally (return (values keys children))))
                 (funcall (symbol-cat-in-package (symbol-package ast-type)
                                                 'make ast-type)
                   :node (apply (symbol-cat-in-package (symbol-package ast-type)
                                                       'make ast-type 'node)
                                :class (intern (symbol-name class))
                                keys)
                   :children children)))))
    (convert-to-node spec)))

(defun symbol-cat (&rest symbols)
  "Return a symbol concatenation of SYMBOLS."
  (intern (string-upcase (mapconcat #'symbol-name symbols "-"))))

(defun symbol-cat-in-package (package &rest symbols)
  "Return a symbol concatenation of SYMBOLS in PACKAGE."
  (intern (string-upcase (mapconcat #'symbol-name symbols "-"))
          package))

(defun get-struct-name (name-and-options)
  "Given NAME-AND-OPTIONS, return the struct name."
  (if (listp name-and-options)
      (car name-and-options)
      name-and-options))

(defun get-struct-conc-name (name-and-options)
  "Given NAME-AND-OPTIONS, return the struct conc name."
  (if (listp name-and-options)
      (or (second (assoc :conc-name
                         (cdr name-and-options)))
          (car name-and-options))
      name-and-options))

(defun get-struct-slot-names (slot-descriptions)
  "Given SLOT-DESCRIPTIONS, return the slot names."
  (mapcar (lambda (slot)
            (if (listp slot)
                (car slot)
                slot))
          (remove-if #'stringp slot-descriptions)))


;;; Generic functions on ASTs
(defgeneric to-alist (struct)
  (:documentation "Convert struct to alist representation."))

(defgeneric from-alist (symbol alist)
  (:documentation "Convert alist to struct representation."))

(defgeneric source-text (ast)
  (:documentation "Source code corresponding to an AST."))

(defgeneric rebind-vars (ast var-replacements fun-replacements)
  (:documentation
   "Replace variable and function references, returning a new AST."))

(defgeneric replace-in-ast (ast replacements &key test)
  (:documentation "Make arbitrary replacements within AST, returning a new
AST."))

(defgeneric insert-ast (tree location ast)
  (:documentation "Return the modified TREE with AST inserted at
LOCATION."))

(defgeneric insert-ast-after (tree location ast)
  (:documentation "Insert AST immediately after LOCATION in TREE, returning
new tree."))

(defgeneric replace-ast (tree location replacement)
  (:documentation "Return the modified TREE with the AST at LOCATION replaced
with REPLACEMENT."))

(defgeneric remove-ast (tree location)
  (:documentation "Return the modified TREE with the AST at LOCATION removed."))

(defgeneric splice-asts (tree location new-asts)
  (:documentation "Splice NEW-ASTS directly into the given LOCATION in TREE,
replacing the original AST."))

(defgeneric fixup-mutation (operation current before ast after)
  (:documentation "Adjust mutation result according to syntactic context."))


;;; Base implementation of generic functions
(defmethod source-text ((ast ast))
  "Return the source code corresponding to AST."
  ;; In performance comparison the combination of
  ;; `with-output-to-string' and `write-string' was faster than
  ;; alternatives using `format' (which was still pretty fast) and
  ;; using `concatenate' (which was slow).
  ;;
  ;; More importantly using (apply #'concatenate ...) runs into
  ;; problems as the number of ASTs is very large.
  (with-output-to-string (out)
    (mapc [{write-string _ out} #'source-text]
          (cons (ast-node ast) (ast-children ast)))))

(defmethod source-text ((node ast-node))
  "Return a source text representation of a single, immutable, AST node."
  "")

(defmethod source-text ((str string))
  "Return the source code corresponding to STR."
  str)

(defmethod rebind-vars ((ast string) var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (reduce (lambda (new-ast replacement)
            (replace-all new-ast (first replacement) (second replacement)))
          (append var-replacements
                  (mapcar (lambda (fun-replacement)
                            (list (car (first fun-replacement))
                                  (car (second fun-replacement))))
                          fun-replacements))
          :initial-value ast))

(defmethod replace-in-ast ((ast ast) replacements &key (test #'equalp))
  "Make arbritrary replacements within AST, returning a new AST.
* AST node to perform modifications to
* REPLACEMENTS association list of key, value pairs to replace in AST
* TEST function to test if a given replacement key can be found in AST"
  (or
   ;; If replacement found, return it
   (cdr (find ast replacements :key #'car :test test))
   ;; Otherwise recurse into children
   (copy ast :children (mapcar {replace-in-ast _ replacements :test test}
                               (ast-children ast)))))

(defmethod replace-in-ast ((ast string) replacements &key (test #'equalp))
  "Make arbritrary replacements within AST, returning a new AST.
* AST node to perform modifications to
* REPLACEMENTS association list of key, value pairs to replace in AST
* TEST function to test if a given replacement key can be found in AST"
  (or (cdr (find ast replacements :key #'car :test test))
      ast))

(defun ast-later-p (ast-a ast-b)
  "Is AST-A later in the genome than AST-B?

Use this to sort AST asts for mutations that perform multiple
operations."
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
    (path-later-p (ast-path ast-a) (ast-path ast-b))))


;;; Tree manipulations
(defun replace-nth-child (ast n replacement)
  "Return AST with the nth child of AST replaced with REPLACEMENT.
* AST tree to modify
* N child to modify
* REPLACEMENT replacement for the nth child"
  (copy ast
        :children (nconc (subseq (ast-children ast) 0 n)
                         (list replacement)
                         (subseq (ast-children ast) (+ 1 n)))))

(defmethod replace-ast ((tree ast) (location ast) (replacement ast))
  "Return the modified TREE with the AST at LOCATION replaced with
REPLACEMENT.
* TREE Applicative AST tree to be modified
* LOCATION AST to be replaced in TREE
* REPLACEMENT Replacement AST"
  (replace-ast tree (ast-path location) replacement))

(defmethod replace-ast ((tree ast) (location list) (replacement ast))
  "Return the modified TREE with the AST at LOCATION replaced with
REPLACEMENT.
* TREE Applicative AST tree to be modified
* LOCATION path to the AST to be replaced in TREE
* REPLACEMENT Replacement AST"
  (labels
    ((non-empty (str)
       "Return STR only if it's not empty.

asts->tree tends to leave dangling empty strings at the ends of child
list, and we want to treat them as NIL in most cases."
       (when (not (emptyp str)) str))
     (helper (tree path next)
         (bind (((head . tail) path)
                (children (ast-children tree)))
           (if tail
               ;; The insertion may need to modify text farther up the
               ;; tree. Pass down the next bit of non-empty text and
               ;; get back a new string.
               (multiple-value-bind (child new-next)
                   (helper (nth head children)
                           tail
                           (or (non-empty (nth (1+ head) children))
                               next))
                 (if (and new-next (non-empty (nth (1+ head) children)))
                     ;; The modified text belongs here. Insert it.
                     (values (copy tree
                                   :children (nconc (subseq children
                                                            0 head)
                                                    (list child new-next)
                                                    (subseq children
                                                            (+ 2 head))))
                             nil)

                     ;; Otherwise keep passing it up the tree.
                     (values (replace-nth-child tree head child)
                             new-next)))
               (let* ((after (nth (1+ head) children))
                      (fixed (fixup-mutation :instead
                                             (nth head children)
                                             (if (positive-integer-p head)
                                                 (nth (1- head) children)
                                                 "")
                                             replacement
                                             (or (non-empty after) next))))

                 (if (non-empty after)
                     ;; fixup-mutation can change the text after the
                     ;; insertion (e.g. to remove a semicolon). If
                     ;; that text is part of this AST, just include it
                     ;; in the list.
                     (values
                      (copy tree
                            :children (nconc (subseq children
                                                     0 (max 0 (1- head)))
                                             fixed
                                             (nthcdr (+ 2 head) children)))
                      nil)

                     ;; If the text we need to modify came from
                     ;; farther up the tree, return it instead of
                     ;; inserting it here.
                     (values
                      (copy tree
                            :children (nconc (subseq children
                                                     0 (max 0 (1- head)))
                                             (butlast fixed)
                                             (nthcdr (+ 2 head) children)))
                      (lastcar fixed))))))))
    (helper tree location nil)))

(defmethod remove-ast ((tree ast) (location ast))
  "Return the modified TREE with the AST at LOCATION removed.
* TREE Applicative AST tree to be modified
* LOCATION AST to be removed in TREE
"
  (remove-ast tree (ast-path location)))

(defmethod remove-ast ((tree ast) (location list))
  "Return the modified TREE with the AST at LOCATION removed.
* TREE Applicative AST tree to be modified
* LOCATION path to the AST to be removed in TREE
"
  (labels
      ((helper (tree path)
         (bind (((head . tail) path)
                (children (ast-children tree)))
           (if tail
               ;; Recurse into child
               (replace-nth-child tree head (helper (nth head children) tail))

               ;; Remove child
               (copy tree
                     :children (nconc (subseq children 0 (max 0 (1- head)))
                                      (fixup-mutation
                                        :remove
                                        (nth head children)
                                        (if (positive-integer-p head)
                                            (nth (1- head) children)
                                            "")
                                        nil
                                        (or (nth (1+ head) children) ""))
                                      (nthcdr (+ 2 head) children)))))))
    (helper tree location)))

(defmethod splice-asts ((tree ast) (location ast) (new-asts list))
  "Splice a list directly into the given location, replacing the original AST.

Can insert ASTs and text snippets. Does minimal syntactic fixups, so
use carefully.

* TREE Applicative AST tree to be modified
* LOCATION AST marking location where insertion is to occur
* NEW-ASTS ASTs to be inserted into TREE
"
  (splice-asts tree (ast-path location) new-asts))

(defmethod splice-asts ((tree ast) (location list) (new-asts list))
  "Splice a list directly into the given location, replacing the original AST.

Can insert ASTs and text snippets. Does minimal syntactic fixups, so
use carefully.

* TREE Applicative AST tree to be modified
* LOCATION path to the AST where insertion is to occur
* NEW-ASTS ASTs to be inserted into TREE
"
  (labels
    ((helper (tree path)
       (bind (((head . tail) path)
              (children (ast-children tree)))
         (if tail
             ;; Recurse into child
             (replace-nth-child tree head (helper (nth head children) tail))

             ;; Splice into children
             (copy tree
                   :children (nconc (subseq children 0 head)
                                    new-asts
                                    (nthcdr (1+ head) children)))))))
    (helper tree location)))

(defmethod insert-ast ((tree ast) (location ast) (replacement ast))
  "Return the modified TREE with the REPLACEMENT inserted at LOCATION.
* TREE Applicative AST tree to be modified
* LOCATION AST marking location where insertion is to occur
* REPLACEMENT AST to insert
"
  (insert-ast tree (ast-path location) replacement))

(defmethod insert-ast ((tree ast) (location list) (replacement ast))
  "Return the modified TREE with the REPLACEMENT inserted at LOCATION.
* TREE Applicative AST tree to be modified
* LOCATION path to the AST where insertion is to occur
* REPLACEMENT AST to insert
"
  (labels
    ((helper (tree path)
       (bind (((head . tail) path)
              (children (ast-children tree)))
         (if tail
             ;; Recurse into child
             (replace-nth-child tree head (helper (nth head children) tail))

             ;; Insert into children
             (copy tree
                   :children (nconc (subseq children 0 (max 0 (1- head)))
                                    (fixup-mutation
                                      :before
                                      (nth head children)
                                      (if (positive-integer-p head)
                                          (nth (1- head) children)
                                          "")
                                      replacement
                                      (or (nth head children) ""))
                                    (nthcdr (1+ head) children)))))))
    (helper tree location)))

(defmethod insert-ast-after ((tree ast) (location ast) (replacement ast))
  "Return the modified TREE with the REPLACEMENT inserted after LOCATION.
* TREE Applicative AST tree to be modified
* LOCATION path to the AST where insertion is to occur immediately after
* REPLACEMENT AST to insert
"
  (insert-ast-after tree (ast-path location) replacement))

(defmethod insert-ast-after ((tree ast) (location list) (replacement ast))
  "Return the modified TREE with the REPLACEMENT inserted after LOCATION.
* TREE Applicative AST tree to be modified
* LOCATION path to the AST where insertion is to occur immediately after
* REPLACEMENT AST to insert
"
  (labels
    ((helper (tree path)
       (bind (((head . tail) path)
              (children (ast-children tree)))
         (if tail
             ;; Recurse into child
             (replace-nth-child tree head (helper (nth head children) tail))

             ;; Insert into children
             (copy tree
                   :children (nconc (subseq children 0 (max 0 head))
                                    (fixup-mutation
                                      :after
                                      (nth head children)
                                      (nth head children)
                                      replacement
                                      (or (nth (1+ head) children) ""))
                                    (nthcdr (+ 2 head) children)))))))
    (helper tree location)))

(defgeneric map-ast-strings (tree fn)
  (:documentation "Build a new AST obtained by replacing each string with
(funcall FN string).  If the FN returns NIL do not replace."))

(defmethod map-ast-strings ((tree ast) (fn function))
  (let* ((children (ast-children tree))
	 (new-children (mapcar (lambda (child) (map-ast-strings child fn)) children)))
    (if (every #'eq children new-children)
	tree
	(copy tree :children new-children))))

(defmethod map-ast-strings ((str string) (fn function))
  (or (funcall fn str) str))

(defmethod map-ast-strings ((tree t) (fn function)) tree)
(defmethod map-ast-strings ((tree t) (sym symbol))
  (let ((fn (symbol-function sym)))
    (unless fn
      (error "No function found for ~A" sym))
    (map-ast-strings tree fn)))

;;; Map over the nodes of an AST

(defgeneric map-ast (tree fn)
  (:documentation "Apply fn to each node of an AST, in preorder"))

(defmethod map-ast ((tree ast) fn)
  (funcall fn tree)
  (dolist (c (ast-children tree))
    (when (typep c 'ast) (map-ast c fn)))
  tree)

(defmethod map-ast (tree fn) nil)


;;; AST diffs
(defgeneric ast-text (ast)
  (:documentation "Return textual representation of AST.")
  (:method ((ast ast)) (peel-bananas (source-text ast))))


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
              ;; Without this, trees that have certain permutations of leaf
              ;; values can be likely to hash to the same integer.
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
    (ast-hash (package-name p)))

  )

(defmethod ast-hash ((ast ast))
;;  (or (ast-stored-hash ast)
;;      (setf (ast-stored-hash ast)
  (ast-hash (cons (ast-class ast) (ast-children ast))))
;; ))

(defgeneric ast-clear-hash (ast)
  (:documentation "Clear the stored hash fields of an ast"))

(defmethod ast-clear-hash ((ast t))
  ast)

(defmethod ast-clear-hash ((ast ast))
  (setf (ast-stored-hash ast) nil)
  (mapc #'ast-clear-hash (ast-children ast))
  ast)

(defgeneric ast-meld? (ast)
  (:documentation "Returns true if the children of this AST are to be combined when a merge conflict is found during patching."))

(defmethod ast-meld? ((ast ast))
  (ast-class-meld? (ast-class ast) ast))

(defgeneric ast-class-meld? (ast-class ast)
  (:documentation "Dispatches on the ast-class of an ast to compute ast-meld?"))

(defmethod ast-class-meld? ((ast-class t) (ast t)) nil)

(defmethod ast-class-meld? ((ast-class (eql :TopLevel)) ast) t)
