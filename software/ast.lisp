;;; ast.lisp --- Applicative ASTs structure
;;;
;;; The machinery to make applicative ASTs.  They're structures.
;;;
;;; * Define them with (define-ast foo slot-descriptions)
;;; * Create them with (make-foo ... )
;;; * Copy them with COPY.
;;;

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; Data structure definitions
(defstruct ast-node "Base type of immutable portion of ast nodes.")

(defstruct (ast (:constructor make-raw-ast))
  "Base type of sub-tree of an applicative AST tree."
  (path nil :type list)                      ; Path to subtree from root of tree.
  (node nil :type (or ast-node string null)) ; Pointer to immutable AST data.
  (children nil :type list))                 ; Remainder of subtree.

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

(defmacro define-ast (name-and-options &rest slot-descriptions)
  "Implicitly defines an AST wrapper for the defined AST-node."
  (with-gensyms ((obj obj-))
    (let* ((name (get-struct-name name-and-options))
           (conc-name (get-struct-conc-name name-and-options))
           (slot-names (get-struct-slot-names slot-descriptions)))
      `(prog2
           ;; Define the immutable node.
           (define-immutable-node-struct (,(symbol-cat name 'node)
                                          (:include ast-node)
                                          (:conc-name ,conc-name))
             ,@slot-descriptions)
           ;; Define and return the wrapper.
           (defstruct (,name (:include ast)
                             (:copier nil)
                             ,@(when (listp name-and-options)
                                 (remove-if
                                  (lambda (pair)
                                    (member (car pair)
                                            '(:conc-name :copier :include)))
                                  (cdr name-and-options))))
             ,@(when (and (car slot-descriptions)
                          (stringp (car slot-descriptions)))
                 (list (car slot-descriptions))))
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

(defun symbol-cat (&rest symbols)
  "Return a symbol concatenation of SYMBOLS."
  (intern (string-upcase (mapconcat #'symbol-name symbols "-"))))

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
  "Return the source code corresponding to AST.
* AST ast to retrieve source code for
"
  (format nil "~{~a~}"
          (iter (for a in (cons (ast-node ast) (ast-children ast)))
                (collecting (source-text a)))))

(defmethod source-text ((node ast-node))
  "Return a source text representation of a single, immutable,
AST node."
  "")

(defmethod source-text ((str string))
  "Return the source code corresponding to STR.
* STR string to retrieve source code for
"
  str)

(defmethod replace-in-ast ((ast ast) replacements &key (test #'equalp))
  "Make arbritrary replacements within AST, returning a new AST.
* AST node to perform modifications to
* REPLACEMENTS association list of key, value pairs to replace in AST
* TEST function to test if a given replacement key can be found in AST
"
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
* TEST function to test if a given replacement key can be found in AST
"
  (or (cdr (find ast replacements :key #'car :test test))
      ast))

(defun ast-later-p (ast-a ast-b)
  "Is AST-A later in the genome than AST-B?

Use this to sort AST asts for mutations that perform multiple
operations.
"
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
* REPLACEMENT replacement for the nth child
"
  (copy ast
        :children (nconc (subseq (ast-children ast) 0 n)
                         (list replacement)
                         (subseq (ast-children ast) (+ 1 n)))))

(defmethod replace-ast ((tree ast) (location ast) (replacement ast))
  "Return the modified TREE with the AST at LOCATION replaced with
REPLACEMENT.
* TREE Applicative AST tree to be modified
* LOCATION AST to be replaced in TREE
* REPLACEMENT Replacement AST
"
  (replace-ast tree (ast-path location) replacement))

(defmethod replace-ast ((tree ast) (location list) (replacement ast))
  "Return the modified TREE with the AST at LOCATION replaced with
REPLACEMENT.
* TREE Applicative AST tree to be modified
* LOCATION path to the AST to be replaced in TREE
* REPLACEMENT Replacement AST
"
  (labels
    ((non-empty (str)
       "Return STR only if it's not empty.

asts->tree tends to leave dangling empty strings at the ends of child
list, and we want to treat them as NIL in most cases.
"
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


;;; AST diffs 
(defmethod apply-edit-script ((interface ast-interface)
                              (original ast)
                              (script list))
  (copy original
        :children (cdr (apply-edit-script interface
                                          (cons (ast-node original)
                                                (ast-children original))
                                          script))))
