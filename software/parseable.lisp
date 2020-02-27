;;; parseable.lisp --- Software which may be parsed into ASTs
(defpackage :software-evolution-library/software/parseable
  (:nicknames :sel/software/parseable :sel/sw/parseable)
  (:use :gt/full
        :metabang-bind
        :cl-store
        :bordeaux-threads
        :software-evolution-library
        :software-evolution-library/software/source
        :software-evolution-library/software/file
        :software-evolution-library/utility/range)
  (:export :ast
           :ast-stub
           :ast-p
           :define-ast
           :define-immutable-node-struct
           :to-alist
           :from-alist
           :ast-path
           :ast-node
           :ast-stored-hash
           :ast-class
           :ast-annotations
           :ast-aux-data
           :source-text
           :rebind-vars
           :replace-in-vars
           :replace-nth-child
           :fixup-mutation
           :ast-children
           :ast-nodes-in-subtree
           :ast-equal-p
           :ast-text
           :ast-hash
           :make-raw-ast
           :make-conflict-ast
           :conflict-ast
           :conflict-ast-p
           :conflict-ast-child-alist
           :conflict-ast-default-children
           :combine-conflict-asts
           :to-ast
           :to-ast*
           :ast-later-p
           :map-ast
           :map-ast-postorder
           :map-ast-with-ancestors
           :mapc-ast
           :mapc-ast-and-strings
           :ast-to-list
           :map-ast-strings
           :ast-meld-p
           :ast-class-meld?
           :replace-in-ast
           :move-prefixes-down
           :defgeneric-kind
           ;; Parseable software object.
           :parseable
           :ast-root
           :asts
           :copy-lock
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
           :update-asts
           :update-caches
           :parse-asts
           :clear-caches
           :update-asts-if-necessary
           :update-caches-if-necessary
           :bad-asts
           :stmt-range
           ;; :good-stmts
           :good-mutation-targets
           :bad-mutation-targets
           ;; :mutation-targets
           :pick-general
           :recontextualize-mutation
           :recontextualize
           :select-crossover-points
           :parse-source-snippet
           :traceable-stmt-p
           :can-be-made-traceable-p
           :enclosing-traceable-stmt
           :asts-containing-source-location
           :ast-to-source-range
           :parent-ast-p
           :get-children
           :get-immediate-children
           :prepend-to-genome
           :append-to-genome-preamble
           :append-to-genome
           :index-of-ast
           :ast-at-index
           ;; Mutation wrappers
           :insert-ast
           :replace-ast
           :remove-ast
           ;; Restarts
           :expand-stmt-pool))
(in-package :software-evolution-library/software/parseable)
(in-readtable :curry-compose-reader-macros)

(define-software parseable (source)
  ((ast-root :initarg :ast-root :initform nil :accessor ast-root
             :documentation "Root node of AST.")
   (asts     :initarg :asts :reader asts
             :initform nil :copier :direct
             :type #-sbcl list #+sbcl (or null (cons (cons keyword *) *))
             :documentation
             "List of all ASTs.
See the documentation of `update-asts' for required invariants.")
   (copy-lock :initform (make-lock "parseable-copy")
              :copier :none
              :documentation "Lock while copying parseable objects."))
  (:documentation "Parsed AST tree software representation."))


;;; AST data structure definitions.
(defstruct ast-node "Base type of immutable portion of ast nodes.")

(defstruct ast "Base structure of mutable portion of ast nodes.")

(defstruct (ast-stub (:include ast) (:constructor make-raw-ast) (:conc-name ast-internal-))
  "Base type of sub-tree of an applicative AST tree."
  (path nil :type list)                      ; Path to subtree from root of tree.
  (children nil :type list)                  ; Remainder of subtree.
  (annotations nil :type list)
  (aux-data nil :type list)
  (stored-hash nil :type (or null fixnum)))

(defgeneric ast-path (a)
  (:documentation "Genericized version of path reader for AST structs")
  (:method ((a ast-stub)) (ast-internal-path a)))
(defgeneric (setf ast-path) (v a)
  (:documentation "Genericized version of path writer for AST structs")
  (:method ((v list) (a ast-stub)) (setf (ast-internal-path a) v)))
(defgeneric ast-children (a)
  (:documentation "Genericized version of children reader for AST structs")
  (:method ((a ast-stub)) (ast-internal-children a)))
(defgeneric (setf ast-children) (v a)
  (:documentation "Genericized version of children writer for AST structs")
  (:method ((v list) (a ast-stub)) (setf (ast-internal-children a) v)))
(defgeneric ast-annotations (a)
  (:documentation "Genericized version of annotations reader for AST structs")
  (:method ((a ast-stub)) (ast-internal-annotations a)))
(defgeneric (setf ast-annotations) (v a)
  (:documentation "Genericized version of annotations writer for AST structs")
  (:method ((v list) (a ast-stub)) (setf (ast-internal-annotations a) v)))
(defgeneric ast-aux-data (a)
  (:documentation "Genericized version of aux-data reader for AST structs")
  (:method ((a ast-stub)) (ast-internal-aux-data a)))
(defgeneric (setf ast-aux-data) (v a)
  (:documentation "Genericized version of aux-data writer for AST structs")
  (:method ((v list) (a ast-stub)) (setf (ast-internal-aux-data a) v)))
(defgeneric ast-stored-hash (a)
  (:documentation "Genericized version of stored-hash reader for AST structs")
  (:method ((a ast-stub)) (ast-internal-stored-hash a)))
(defgeneric (setf ast-stored-hash) (v a)
  (:documentation "Genericized version of children writer for AST structs")
  (:method (v (a ast-stub)) (setf (ast-internal-stored-hash a) v)))

(defmethod copy ((a ast-stub)
                 &key (path nil path-provided-p)
                   (children nil children-provided-p)
                   (annotations nil annotations-provided-p)
                   (aux-data nil aux-data-provided-p)
                   (stored-hash nil stored-hash-provided-p))
  (make-raw-ast
   :path (if path-provided-p path (ast-path a))
   :children (if children-provided-p children (ast-children a))
   :annotations (if annotations-provided-p annotations (ast-annotations a))
   :aux-data (if aux-data-provided-p aux-data (ast-aux-data a))
   :stored-hash (if stored-hash-provided-p stored-hash (ast-stored-hash a))))

(defparameter *ast-print-cutoff* 20
  "Maximum number of characters to print for TEXT in
PRINT-OBJECT method on AST structures.")

(defmethod print-object ((obj ast) stream &aux (cutoff *ast-print-cutoff*))
  (if *print-readably*
      (call-next-method)
      (default-ast-printer obj stream cutoff)))

(defun default-ast-printer (obj stream cutoff)
  (print-unreadable-object (obj stream :type t)
    (format stream ":PATH ~s~:_ :NODE ~s~:_ :TEXT ~s"
            (ast-path obj) (ast-node obj)
            (let* ((text (source-text obj))
                   (truncated
                    (if (> (length text) cutoff)
                        (concatenate 'string (subseq text 0 cutoff) "...")
                        text)))
              (if-let ((position (search (string #\Newline) truncated)))
                (concatenate 'string (subseq truncated 0 position) "...")
                truncated)))))

(defmethod print-object ((obj ast-stub) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream ":PATH ~s~:_ :HASH ~s~:_"
                (ast-path obj) (ast-hash obj)))))

(defgeneric ast-class (ast) (:documentation "Class of AST."))

(defmethod ast-class ((ast ast-stub))
  (declare (ignorable ast))
  nil)

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

(defun get-struct-include (name-and-options)
  "Given NAME-AND-OPTIONS, return the struct :include name, or NIL if none."
  (and (listp name-and-options)
       (cadr (assoc :include (cdr name-and-options)))))

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
      `(progn
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
                    names-w-types)))
         ',name))))

(defmacro define-ast (name-and-options &rest slot-descriptions
                      &aux (default-ast-slot-descriptions
                               '((class nil :type symbol)
                                 (aux-data nil :type list))))
  "Implicitly defines an AST wrapper for the defined AST-node."
  (with-gensyms ((obj obj-))
    (let* ((include (get-struct-include name-and-options))
           (doc-strings (remove-if-not #'stringp slot-descriptions))
           (slot-descriptions (remove-if #'stringp slot-descriptions))
           (all-slot-descriptions (append default-ast-slot-descriptions
                                          slot-descriptions))
           (name (get-struct-name name-and-options))
           (conc-name (get-struct-conc-name name-and-options))
           (slot-names (get-struct-slot-names all-slot-descriptions)))
      `(progn
         ;; Define the immutable node.
         (define-immutable-node-struct (,(symbol-cat name 'node)
                                         (:include ast-node)
                                         (:conc-name ,conc-name))
             ,@doc-strings
           ,@all-slot-descriptions)
         ;; Define and return the wrapper.
         (defstruct (,name ;; (:include ast-stub)
                      (:include ,(or include 'ast))
                      (:copier nil)
                      ,@(when (listp name-and-options)
                          (remove-if
                           (lambda (pair)
                             (member (car pair)
                                     '(:conc-name :copier :include)))
                           (cdr name-and-options))))
           ,@doc-strings
           ;; Duplicated from ast-stub
           (path nil :type list)           ;; Path to subtree from root of tree.
           (node nil :type (or ast-node string null)) ;; Pointer to immutable AST data.
           (children nil :type list)                  ;; Remainder of subtree.
           (annotations nil :type list)
           (stored-hash nil :type (or null fixnum)))
         ;; Copy method for light weight copies of wrapper only.
         (defmethod copy
             ((,obj ,name)
              &key path
                (children nil children-supplied-p)
                annotations
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
                           (ast-children ,obj))
             :annotations annotations))

         ;; Define accessors for internal fields on outer structure
         (defmethod ast-path ((,obj ,name))
           (,(symbol-cat name 'path) ,obj))
         (defmethod (setf ast-path) ((v list) (,obj ,name))
           (setf (,(symbol-cat name 'path) ,obj) v))
         (defmethod ast-node ((,obj ,name))
           (,(symbol-cat name 'node) ,obj))
         (defmethod (setf ast-node) (v (,obj ,name))
           (setf (,(symbol-cat name 'node) ,obj) v))
         (defmethod ast-children ((,obj ,name))
           (,(symbol-cat name 'children) ,obj))
         (defmethod (setf ast-children) ((v list) (,obj ,name))
           (setf (,(symbol-cat name 'children) ,obj) v))
         (defmethod ast-stored-hash ((,obj ,name))
           (,(symbol-cat name 'stored-hash) ,obj))
         (defmethod (setf ast-stored-hash) (v (,obj ,name))
           (setf (,(symbol-cat name 'stored-hash) ,obj) v))

         ;; Define accessors for inner fields on outer structure.
         ,@(mapcar
            (lambda (slot)
              `(defmethod ,(symbol-cat conc-name slot) ((,obj ,name))
                 (,(symbol-cat conc-name slot) (,(symbol-cat conc-name 'node) ,obj))))
            slot-names)
         ',name))))

(defstruct (conflict-ast (:include ast-stub))
  "Node representing several possibilities for insertion into an AST.
The mapping from a conflicted AST into a regular AST is as follows: for
a given conflict key, and for each conflict node, get the list of children
corresponding to that key (default if the key is not present), and splice
that list of children in place of the conflict node in its parent's children
list."
  ;; Mapping from conflict options to lists of children.
  (child-alist nil)
  ;; Children to use for keys not present in CHILD-ALIST.
  (default-children nil))

(defmethod copy
    ((struct conflict-ast) &key
                             (path nil path-provided-p)
                             (children nil children-provided-p)
                             (annotations nil annotations-provided-p)
                             (aux-data nil aux-data-provided-p)
                             (stored-hash nil stored-hash-provided-p)
                             (child-alist nil child-alist-provided-p)
                             (default-children nil default-children-provided-p))
  (make-conflict-ast
   :path (if path-provided-p path (ast-path struct))
   :children (if children-provided-p children (ast-children struct))
   :annotations (if annotations-provided-p
                    annotations
                    (ast-annotations struct))
   :aux-data (if aux-data-provided-p
                 aux-data
                 (ast-aux-data struct))
   :stored-hash (if stored-hash-provided-p
                    stored-hash
                    (ast-stored-hash struct))
   :child-alist (if child-alist-provided-p
                    child-alist
                    (conflict-ast-child-alist struct))
   :default-children (if default-children-provided-p
                         default-children
                         (conflict-ast-default-children struct))))

(defmethod print-object ((obj conflict-ast) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream ":PATH ~s~:_ :CHILD-ALIST ~s"
                (ast-path obj) ;; (ast-node obj)
                (conflict-ast-child-alist obj)))))

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
      (make-conflict-ast
       :child-alist al
       :default-children (append def1 def2)))))

(defmethod ast-class ((c conflict-ast)) nil)

;;; There should be functions for stripping conflict nodes out of a tree,
;;; based on option keys.

(defgeneric to-ast (ast-type spec)
  (:documentation
   "Walk a potentially recursive AST SPEC creating an AST-TYPE AST.
A SPEC should have the form

  (ast-class <optional-keyword-args-to-`make-<AST-TYPE>-node'>
             CHILDREN)

where CHILDREN may themselves be specifications suitable for passing
to `to-ast`.  E.g.

  (to-ast 'clang-ast (:callexpr (:implicitcastexpr
                                 :includes '(\"<string.h>\")
                                 \"\" \"(|strcpy|)\" \"\")
                                \"(\" \"arg-1\" \",\" \"arg-2\" \")\"))"))

(defun to-ast* (spec fn)
  (labels ((convert-to-node (spec)
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
                                   (collect (convert-to-node item)
                                            into children)
                                   (collect item into children)))
                           (setf previous item)
                           (finally (return (values keys children)))))
                 (funcall fn class keys children)))))
    (convert-to-node spec)))

(defmethod to-ast (ast-type spec)
  (to-ast* spec
           (lambda (class keys children)
             (funcall (symbol-cat-in-package (symbol-package ast-type)
                                             'make ast-type)
                      :node (apply (symbol-cat-in-package (symbol-package ast-type)
                                                          'make ast-type 'node)
                                   :class (if (keywordp class)
                                              class
                                              (intern (symbol-name class) "KEYWORD"))
                                   keys)
                      :children children))))


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

(defgeneric fixup-mutation (operation current before ast after)
  (:documentation "Adjust mutation result according to syntactic context."))


;;; Base implementation of generic functions
(defmethod source-text ((ast null)) "")

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

(defmethod source-text ((ast ast-stub))
  (with-output-to-string (out)
    (mapc [{write-string _ out} #'source-text]
          (ast-children ast))))

(defmethod source-text ((c conflict-ast))
  (with-output-to-string (s)
    (format s "<")
    (iter (for e on (conflict-ast-child-alist c))
          (format s "~a: " (caar e))
          (iter (for x in (cdar e)) (format s "~a" (source-text x)))
          (when (cdr e) (format s "|")))
    (format s ">")))

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
;;;
;;; NOTE: These methods are helpers performing the AST manipulations for
;;; the corresponding mutation operations in parseable.lisp.  They should
;;; not be called directly unless you are confident in what you are doing
;;; and accept that behavior of the parent software object may be incorrect.

(defgeneric replace-nth-child (ast n replacement)
  (:documentation "Return AST with the nth child of AST replaced with
REPLACEMENT.

* AST tree to modify
* N child to modify
* REPLACEMENT replacement for the nth child")
  (:method ((ast ast) (n integer) replacement)
    (replace-nth-child ast n (list replacement)))
  (:method ((ast ast) (n integer) (replacement list))
    (copy ast
          :children (append (subseq (ast-children ast) 0 n)
                            replacement
                            (subseq (ast-children ast) (+ 1 n))))))

(defgeneric replace-ast
    (tree location replacement &key literal &allow-other-keys)
  (:documentation "Modify and return TREE with the AST at LOCATION replaced
with REPLACEMENT.
* OBJ object to be modified
* LOCATION location where replacement is to occur
* REPLACEMENT AST or ASTs to insert
* LITERAL keyword to control whether recontextualization is performed
          For modifications where the replacement is to be directly
          inserted, pass this keyword as true.")
  (:method ((tree t) (location ast) (replacement ast)
            &rest args &key &allow-other-keys)
    (apply #'replace-ast tree (ast-path location) replacement args))
  (:method ((tree ast) (location list) (replacement ast)
            &key &allow-other-keys)
    (labels        ; TODO: Remove or relocate all of this "fixup" logic.
        ((non-empty (str)
           "Return STR only if it's not empty.

          asts->tree tends to leave dangling empty strings at the ends of child
          list, and we want to treat them as NIL in most cases."
           (when (not (equalp str "")) str))
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
      (if location
          (helper tree location nil)
          replacement)))
  (:method ((obj parseable) (location list) (replacement ast)
            &key literal &allow-other-keys)
    (apply-mutation obj (at-targets (make-instance 'parseable-replace)
                                    (list (cons :stmt1 location)
                                          (cons (if literal :literal1 :value1)
                                                replacement)))))
  (:method ((obj parseable) (location ast) (replacement string)
            &rest args &key &allow-other-keys)
    (apply #'replace-ast obj (ast-path location) (list replacement) args))
  (:method ((obj parseable) (location list) (replacement string)
            &rest args &key &allow-other-keys)
    (apply #'replace-ast obj location (list replacement) args))
  (:method ((obj parseable) (location ast) (replacement list)
            &rest args &key &allow-other-keys)
    (apply #'replace-ast obj (ast-path location) replacement args))
  (:method ((obj parseable) (location list) (replacement list)
            &key literal &allow-other-keys)
    (apply-mutation obj
                    (at-targets (make-instance 'parseable-replace)
                                (list (cons :stmt1 (butlast location))
                                      (cons (if literal :literal1 :value1)
                                            (replace-nth-child
                                             (get-ast obj (butlast location))
                                             (lastcar location)
                                             replacement)))))))

(defgeneric remove-ast (tree location &key &allow-other-keys)
  (:documentation "Return the modified TREE with the AST at LOCATION removed.

* TREE Applicative AST tree to be modified
* LOCATION AST to be removed in TREE")
  (:method ((tree t) (location ast) &rest args &key &allow-other-keys)
    (apply #'remove-ast tree (ast-path location) args))
  (:method ((tree ast) (location list) &key &allow-other-keys)
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
  (:method ((obj parseable) (location list) &key &allow-other-keys)
    (apply-mutation obj (at-targets (make-instance 'parseable-cut)
                                    (list (cons :stmt1 location))))))

(defgeneric splice-asts (tree location new-asts)
  (:documentation "Splice NEW-ASTS directly into the given LOCATION in TREE,
replacing the original AST.

Can insert ASTs and text snippets. Does minimal syntactic fixups, so
use carefully.

* TREE Applicative AST tree to be modified
* LOCATION AST marking location where insertion is to occur
* NEW-ASTS ASTs to be inserted into TREE

Note: This method is a helper for performing an AST mutation operation
specified in parseable.lisp and should not be called directly without
knowledge of the underlying code architecture.  Please consider using
the methods in parseable.lisp instead.")
  (:method ((tree ast) (location ast) (new-asts list))
    (splice-asts tree (ast-path location) new-asts))

  (:method ((tree ast) (location list) (new-asts list))
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
      (helper tree location))))

(defgeneric insert-ast (tree location ast &key literal &allow-other-keys)
  (:documentation "Return the modified OBJ with AST inserted at LOCATION.
* OBJ object to be modified
* LOCATION location where insertion is to occur
* AST AST to insert
* LITERAL keyword to control whether recontextualization is performed
          For modifications where the replacement is to be directly
          inserted, pass this keyword as true.")

  (:method ((tree t) (location ast) (ast ast)
            &rest args &key &allow-other-keys)
    (apply #'insert-ast tree (ast-path location) ast args))
  (:method ((tree ast) (location list) (replacement ast)
            &key &allow-other-keys)
    (labels
        ((helper (tree path)
           (bind (((head . tail) path)
                  (children (ast-children tree)))
                 (assert (>= head 0))
                 (assert (< head (length children)))
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
  (:method ((obj parseable) (location list) (ast ast)
            &key literal &allow-other-keys)
    (apply-mutation obj (at-targets (make-instance 'parseable-insert)
                                    (list (cons :stmt1 location)
                                          (cons (if literal :literal1 :value1)
                                                ast))))))

(defgeneric insert-ast-after (tree location ast)
  (:documentation "Insert AST immediately after LOCATION in TREE, returning
new tree.

* TREE Applicative AST tree to be modified
* LOCATION path to the AST where insertion is to occur immediately after
* REPLACEMENT AST to insert

Note: This method is a helper for performing an AST mutation operation
specified in parseable.lisp and should not be called directly without
knowledge of the underlying code architecture.  Please consider using
the methods in parseable.lisp instead.")
  (:method ((tree ast) (location ast) (replacement ast))
    (insert-ast-after tree (ast-path location) replacement))

  (:method ((tree ast) (location list) (replacement ast))
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
      (helper tree location))))


;;; Map over the nodes of an AST
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
  (unless (fboundp sym)
    (error "No function found for ~A" sym))
  (map-ast-strings tree (symbol-function sym)))

(defgeneric map-ast (tree fn)
  (:documentation "Apply FN to each node of AST, in preorder."))

(defmethod map-ast ((tree ast) fn)
  (funcall fn tree)
  (dolist (c (ast-children tree))
    (when (ast-p c) (map-ast c fn)))
  tree)

(defmethod map-ast (tree fn)
  (declare (ignorable tree fn))
  nil)

(defgeneric map-ast-with-ancestors (tree fn &optional ancestors)
  (:documentation "Apply FN to each node of the AST, and its list of ancestors,
in preorder.  The ancestor list is in decreasing order of depth in the AST."))

(defmethod map-ast-with-ancestors ((tree ast) fn &optional ancestors)
  (funcall fn tree ancestors)
  (let ((ancestors (cons tree ancestors)))
    (dolist (c (ast-children tree))
      (when (ast-p c) (map-ast-with-ancestors c fn ancestors))))
  tree)

(defmethod map-ast-with-ancestors (tree fn &optional ancestors)
  (declare (ignore tree fn ancestors))
  nil)

(defun mapc-ast (ast fn)
  "Apply FN to AST collecting the results with `cons'."
  (cons (funcall fn ast)
        (mapcar {mapc-ast _ fn} (remove-if-not #'ast-p (ast-children ast)))))

(defun ast-nodes-in-subtree (ast)
  (let ((result nil))
    (map-ast ast (lambda (a) (push a result)))
    (nreverse result)))

(defgeneric map-ast-postorder (tree fn)
  (:documentation "Apply FN to each node of AST, in postorder."))

(defmethod map-ast-postorder ((tree ast) fn)
  (dolist (c (ast-children tree))
    (when (ast-p c) (map-ast-postorder c fn)))
  (funcall fn tree)
  tree)

(defun mapc-ast-and-strings (ast fn)
  "Apply FN to ASTs and strings in AST collecting the results with `cons'."
  (typecase ast
    (string (funcall fn ast))
    (conflict-ast (funcall fn ast))
    (ast (cons (funcall fn ast)
               (mapcar {mapc-ast-and-strings _ fn} (ast-children ast))))))

(defgeneric ast-to-list (obj)
  (:documentation "Return ASTs under OBJ as a list.")
  (:method (ast &aux result)
    (if (not (ast-p ast))
        (call-next-method)
        (progn
          (map-ast ast (lambda (ast) (push ast result)))
          (reverse result)))))

;;; AST equality
(defgeneric ast-equal-p (ast-a ast-b)
  (:documentation "Return T AST-A and AST-B are equal for differencing."))

(defmethod ast-equal-p ((ast-a ast) (ast-b ast))
  (and (eq (ast-class ast-a) (ast-class ast-b))
       (eql (length (ast-children ast-a))
            (length (ast-children ast-b)))
       (every #'ast-equal-p (ast-children ast-a) (ast-children ast-b))))

(defmethod ast-equal-p ((ast-a t) (ast-b t))
  (equalp ast-a ast-b))

(defmethod ast-equal-p ((ast-a string) (ast-b string))
  (string= ast-a ast-b))

(defmethod ast-equal-p ((ast-a cons) (ast-b cons))
  (and (iter (while (consp ast-a))
             (while (consp ast-b))
             (always (ast-equal-p (pop ast-a) (pop ast-b))))
       (ast-equal-p ast-a ast-b)))

(defgeneric ast-text (ast)
  (:documentation "Return textual representation of AST.")
  (:method (ast)
    (if (ast-p ast)
        (source-text ast)
        (format nil "~A" ast))))

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
  (or (ast-stored-hash ast)
      (setf (ast-stored-hash ast)
            (ast-hash (cons (ast-class ast) (ast-children ast))))))

(defgeneric ast-clear-hash (ast)
  (:documentation "Clear the stored hash fields of an ast"))

(defmethod ast-clear-hash ((ast t))
  ast)

(defmethod ast-clear-hash ((ast ast))
  (setf (ast-stored-hash ast) nil)
  (mapc #'ast-clear-hash (ast-children ast))
  ast)

(defgeneric ast-meld-p (ast)
  (:documentation
   "Returns true if the children of AST are to be combined on merge conflict."))

(defmethod ast-meld-p (ast)
  (ast-class-meld? (ast-class ast) ast))

(defgeneric ast-class-meld? (ast-class ast)
  (:documentation
   "Dispatches on the ast-class of an ast to compute `ast-meld-p'"))

(defmethod ast-class-meld? ((ast-class t) (ast t)) nil)

(defmethod ast-class-meld? ((ast-class (eql :TopLevel)) ast)
  (declare (ignorable ast))
  t)


;;;; Utility function for comment/terminator normalization

(defun move-prefixes-down (children allowed-fn prefix-fn)
  "Give a list CHILDREN of strings and AST nodes, find children
that satisfy ALLOWED-FN, are followed by a string, and for for which
PREFIX-FN returns a non-null value, which must be a position
in the string.   Move the [0..pos) prefix of that string
down into the list of children of the preceding node, concatenating
it onto the end of the last string in that node's children.
All list operations are destructive."
  (loop for p on children
     do (when-let* ((node (car p))
                    (pos (and (cdr p)
                              (stringp (cadr p))
                              (funcall allowed-fn node)
                              (funcall prefix-fn (cadr p))))
                    (l (lastcar (ast-children node))))
          (if (stringp l)
              (setf (ast-children node)
                    (append (butlast (ast-children node))
                            (list (format nil "~a~a"
                                          l (subseq (cadr p) 0 pos)))))
              (setf (ast-children node)
                    (append (ast-children node)
                            (list (subseq (cadr p) 0 pos)))))
          (setf (cadr p) (subseq (cadr p) pos)))))


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

(defgeneric update-asts (software)
  (:documentation "Update the store of asts associated with SOFTWARE.
There are some requirements for the ASTs constructed by this method:
* We require that *all* source text be stored as a raw string
  somewhere in the AST tree.  Source text tucked inside of a
  non-string AST-NODE will be ignored.
* We also require that if two ASTs have the same class and the same
  source text then they are equal.

Other methods in on parseable objects, specifically `ast-can-recurse'
and `ast-equal-p' depend on these invariants."))

(defgeneric parse-asts (software)
  (:documentation "Parse genome of SOFTWARE, returning a list of ASTs."))

(defgeneric clear-caches (software)
  (:documentation "Clear cached fields on SOFTWARE"))

(defgeneric update-asts-if-necessary (software)
  (:documentation "Parse ASTs in SOFTWARE if the `ast-root' field
has not been set."))

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

(defgeneric parse-source-snippet (type snippet &key)
  (:documentation "Parse a source SNIPPET of the given TYPE (e.g clang)
into a list of free-floating ASTs."))

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

(defgeneric stmt-range (software function)
  (:documentation
   "The indices of the first and last statements in a function.
Return as a list of (first-index last-index). Indices are positions in
the list returned by (asts software)."  ) )


;;; Core parseable methods
(defvar *parseable-obj-code* (register-code 45 'parseable)
  "Object code for serialization of parseable software objects.")

(defstore-cl-store (obj parseable stream)
  ;; NOTE: Does *not* support documentation.
  (let ((copy (copy obj)))
    (setf (slot-value copy 'copy-lock) nil)
    (output-type-code *parseable-obj-code* stream)
    (cl-store::store-type-object copy stream)))

(defrestore-cl-store (parseable stream)
  ;; NOTE: Does *not* support documentation.
  (let ((obj (cl-store::restore-type-object stream)))
    (setf (slot-value obj 'copy-lock) (make-lock "parseable-copy"))
    obj))

(defmethod initialize-instance :after ((obj parseable) &rest initargs)
  "If an AST-ROOT is given in the initialization, ensure all ASTs have PATHs."
  (declare (ignorable initargs))
  (with-slots (ast-root genome) obj
    (when ast-root
      (setf ast-root (update-paths ast-root)
            genome nil))))

(defmethod copy :before ((obj parseable) &key)
  "Update ASTs in OBJ prior to performing a copy.
* OBJ software object to copy
"
  ;; Update ASTs before copying to avoid duplicates. Lock to prevent
  ;; multiple threads from updating concurrently.
  (unless (slot-value obj 'ast-root)
    (bordeaux-threads:with-lock-held ((slot-value obj 'copy-lock))
      (update-asts obj))))

(defmethod size ((obj parseable))
  "Return the number of ASTs in OBJ."
  (length (asts obj)))

(defmethod genome ((obj parseable))
  "Return the source code in OBJ."
  ;; If genome string is stored directly, use that. Otherwise,
  ;; build the genome by walking the AST.
  (if-let ((val (slot-value obj 'genome)))
    (progn (assert (null (slot-value obj 'ast-root)) (obj)
                   "Software object ~a has both genome and ASTs saved" obj)
           val)
    (source-text (ast-root obj))))

(defmethod (setf genome) :before (new (obj parseable))
  "Clear ASTs, fitness, and other caches prior to updating the NEW genome."
  (declare (ignorable new))
  (with-slots (ast-root fitness) obj
    (setf ast-root nil
          fitness nil))
  (clear-caches obj))

(defmethod (setf ast-root) :before (new (obj parseable))
  "Clear fitness and other caches prior to updating
the NEW ast-root."
  (declare (ignorable new))
  (with-slots (fitness) obj
    (setf fitness nil))
  (clear-caches obj))

(defmethod (setf ast-root) :after (new (obj parseable))
  "Ensure the AST paths in NEW are correct after modifying the
applicative AST tree and clear the genome string."
  (setf (slot-value obj 'ast-root)
        (update-paths new)
        (slot-value obj 'genome)
        nil))

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

(defmethod update-paths
    ((tree ast) &optional path)
  "Return TREE with all paths updated to begin at PATH"
  (copy tree
        :path (reverse path)
        :children (iter (for c in (ast-children tree))
                        (for i upfrom 0)
                        (collect (if (typep c 'ast)
                                     (update-paths c (cons i path))
                                     c)))))

(defmethod ast-root :before ((obj parseable))
  "Ensure the `ast-root' field is set on OBJ prior to access."
  (update-asts-if-necessary obj))

(defmethod size :before ((obj parseable))
  "Ensure the `asts' field is set on OBJ prior to access."
  (update-asts-if-necessary obj))

(defmethod asts :before ((obj parseable))
  "Ensure the `asts' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod update-asts-if-necessary ((obj parseable))
  "Parse ASTs in obj if the `ast-root' field has not been set.
* OBJ object to potentially populate with ASTs"
  (with-slots (ast-root) obj (unless ast-root (update-asts obj))))

;;; NOTE: The `update-caches' method assumes that the initial
;;;       top-level AST can be thrown away.
(defgeneric update-caches (software)
  (:documentation "Update cached fields in SOFTWARE.")
  (:method ((obj parseable))
    (labels ((collect-asts (tree)
               (cons tree
                     (iter (for c in (ast-children tree))
                           (appending (when (ast-p c)
                                        (collect-asts c)))))))
      (setf (slot-value obj 'asts)
            (cdr (collect-asts (ast-root obj)))))))

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
  (let ((tree (ast-root obj))
        (pred nil))
    (iter (for i from 0)
          (for j in path)
          (unless (ast-p tree)
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

(defmethod (setf get-ast) (new (obj parseable) (path list))
  "Set the AST at location PATH in OBJ to NEW.
* OBJ software object with ASTs
* PATH path to the AST to replace
* NEW ast or asts to replace with"
  (replace-ast obj path new :literal t))

(defgeneric parent-ast-p (software possible-parent-ast ast)
  (:documentation "Return true if POSSIBLE-PARENT-AST is a parent of AST in OBJ, nil
otherwise.
* OBJ software object containing AST and its parents
* POSSIBLE-PARENT-AST node to find as a parent of AST
* AST node to start parent search from
")
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
    (reverse (get-parent-asts-helper (ast-root obj) (ast-path ast)))))

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
  (remove-if-not #'ast-p (ast-children ast)))

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

(defgeneric ast-to-source-range (software ast)
  (:documentation "Convert AST to pair of SOURCE-LOCATIONS.")
  (:method ((obj parseable) (ast ast))
    (labels
        ((scan-ast (ast line column)
           "Scan entire AST, updating line and column. Return the new values."
           (if (stringp ast)
               ;; String literal
               (iter (for char in-string ast)
                     (incf column)
                     (when (eq char #\newline)
                       (incf line)
                       (setf column 1)))
               ;; Subtree
               (iter (for child in (ast-children ast))
                     (multiple-value-setq (line column)
                       (scan-ast child line column))))
           (values line column))
         (ast-start (ast path line column)
           "Scan to the start of an AST, returning line and column."
           (bind (((head . tail) path))
             ;; Scan preceeding ASTs
             (iter (for child in (subseq (ast-children ast) 0 head))
                   (multiple-value-setq (line column)
                     (scan-ast child line column)))
             ;; Recurse into child
             (when tail
               (multiple-value-setq (line column)
                 (ast-start (nth head (ast-children ast)) tail line column)))
             (values line column))))
      (bind (((:values start-line start-col)
              (ast-start (ast-root obj) (ast-path ast) 1 1))
             ((:values end-line end-col)
              (scan-ast ast start-line start-col)))
        (make-instance 'source-range
          :begin (make-instance 'source-location
                   :line start-line
                   :column start-col)
          :end (make-instance 'source-location
                 :line end-line
                 :column end-col))))))

(defgeneric ast-source-ranges (software)
  (:documentation "Return (AST . SOURCE-RANGE) for each AST in OBJ.")
  (:method ((obj parseable))
    (labels
        ((source-location (line column)
           (make-instance 'source-location :line line :column column))
         (scan-ast (ast line column)
           "Scan entire AST, updating line and column. Return the new values."
           (let* ((begin (source-location line column))
                  (ranges
                   (if (stringp ast)
                       ;; String literal
                       (iter (for char in-string ast)
                             (incf column)
                             (when (eq char #\newline)
                               (incf line)
                               (setf column 1)))

                       ;; Subtree
                       (iter (for child in (ast-children ast))
                             (appending
                              (multiple-value-bind
                                    (ranges new-line new-column)
                                  (scan-ast child line column)
                                (setf line new-line
                                      column new-column)
                                ranges)
                              into child-ranges)
                             (finally
                              (return
                                (cons (cons ast
                                            (make-instance 'source-range
                                              :begin begin
                                              :end (source-location
                                                    line column)))
                                      child-ranges)))))))

             (values ranges line column))))
      (cdr (scan-ast (ast-root obj) 1 1)))))

(defgeneric asts-containing-source-location (software location)
  (:documentation "Return a list of ASTs in SOFTWARE containing LOC.")
  (:method ((obj parseable) (loc source-location))
    (when loc
      (mapcar #'car
              (remove-if-not [{contains _ loc} #'cdr]
                             (ast-source-ranges obj))))))

(defgeneric asts-contained-in-source-range (software range)
  (:documentation "Return a list of ASTs in SOFTWARE contained in RANGE.")
  (:method ((obj parseable) (range source-range))
    (when range
      (mapcar #'car
              (remove-if-not [{contains range} #'cdr]
                             (ast-source-ranges obj))))))

(defgeneric asts-intersecting-source-range (software range)
  (:documentation "Return a list of ASTs in OBJ intersecting RANGE.")
  (:method ((obj parseable) (range source-range))
    (when range
      (mapcar #'car
              (remove-if-not [{intersects range} #'cdr]
                             (ast-source-ranges obj))))))



;;; Genome manipulations
(defgeneric prepend-to-genome (software text)
  (:documentation "Prepend non-AST TEXT to OBJ genome.

* OBJ object to modify with text
* TEXT text to prepend to the genome
")
  (:method ((obj parseable) text)
    (labels ((ensure-newline (text)
               (if (not (equalp #\Newline (last-elt text)))
                   (concatenate 'string text '(#\Newline))
                   text)))
      (with-slots (ast-root) obj
        (setf ast-root
              (copy ast-root
                    :children
                    (append (list (concatenate 'string
                                    (ensure-newline text)
                                    (car (ast-children ast-root))))
                            (cdr (ast-children ast-root)))))))))

(defgeneric append-to-genome-preamble (software text)
  (:documentation "Append non-AST TEXT to OBJ's genome preamble.

* OBJ object to modify with text
* TEXT text to append to the genome preamble")
  (:method ((obj parseable) text)
    (labels ((ensure-newline (text)
               (if (not (equalp #\Newline (last-elt text)))
                   (concatenate 'string text '(#\Newline))
                   text)))
      (with-slots (ast-root) obj
        (setf ast-root
              (copy ast-root
                    :children
                    (append (list (concatenate 'string
                                    (car (ast-children ast-root))
                                    (ensure-newline text)))
                            (cdr (ast-children ast-root)))))))))

(defgeneric append-to-genome (software text)
  (:documentation "Append non-AST TEXT to OBJ genome.  The new text will not be parsed.

* OBJ object to modify with text
* TEXT text to append to the genome
")
  (:method ((obj parseable) text)
    (with-slots (ast-root) obj
      (setf ast-root
            (copy ast-root
                  :children
                  (if (stringp (lastcar (ast-children ast-root)))
                      (append (butlast (ast-children ast-root))
                              (list (concatenate 'string
                                      (lastcar (ast-children ast-root))
                                      text)))
                      (append (ast-children ast-root) (list text))))))))


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
  (setf (ast-root software)
        (with-slots (ast-root) software
          (iter (for (op . properties) in ops)
                (let ((stmt1 (aget :stmt1 properties))
                      (value1 (if (functionp (aget :value1 properties))
                                  (funcall (aget :value1 properties))
                                  (aget :value1 properties))))
                  (setf ast-root
                        (ecase op
                          (:set (replace-ast ast-root stmt1 value1))
                          (:cut (remove-ast ast-root stmt1))
                          (:insert (insert-ast ast-root stmt1 value1))
                          (:splice (splice-asts ast-root stmt1 value1))
                          (:insert-after
                           (insert-ast-after ast-root stmt1 value1)))))
                (finally (return ast-root)))))

  (clear-caches software)
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
