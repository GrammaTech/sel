;;; ast-tree.lisp --- ast-tree software representation

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

;;; ast-tree software objects
(define-software ast-tree (source)
  ((ast-root :initarg :ast-root :initform nil :accessor ast-root
             :documentation "Root node of AST.")
   (asts     :initarg :asts :initform nil
             :accessor asts :copier :direct
             :type #+sbcl (list (cons keyword *) *) #-sbcl list
             :documentation
             "List of all ASTs.")
   (asts-changed-p :accessor asts-changed-p
                   :initform t :type boolean
                   :documentation
                   "Have ASTs changed since the last parse?")
   (copy-lock :initform (make-lock "ast-tree-copy")
              :copier :none
              :documentation "Lock while copying ast-reeobjects."))
  (:documentation "Parsed AST tree software representation."))

(defgeneric roots (obj)
  (:documentation "Return all top-level ASTs in OBJ."))

(defgeneric asts (obj)
  (:documentation "Return a list of all asts in OBJ."))

(defgeneric get-ast (obj path)
  (:documentation "Return the AST in OBJ at PATH."))

(defgeneric get-parent-ast (obj ast)
  (:documentation "Return the parent node of AST in OBJ"))

(defgeneric get-parent-asts (obj ast)
  (:documentation "Return the parent nodes of AST in OBJ"))

(defgeneric get-immediate-children (obj ast)
  (:documentation "Return the immediate children of AST in OBJ."))

(defgeneric get-ast-types (software ast)
  (:documentation "Types directly referenced within AST."))

(defgeneric get-unbound-funs (software ast)
  (:documentation "Functions used (but not defined) within the AST."))

(defgeneric get-unbound-vals (software ast)
  (:documentation "Variables used (but not defined) within the AST."))

(defgeneric scopes (software ast)
  (:documentation "Return lists of variables in each enclosing scope.
Each variable is represented by an alist containing :NAME, :DECL, :TYPE,
and :SCOPE.
"))

(defgeneric get-vars-in-scope (software ast &optional keep-globals)
  (:documentation "Return all variables in enclosing scopes."))

(defgeneric update-asts (software)
  (:documentation "Update the store of asts associated with SOFTWARE."))

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

(defgeneric rebind-vars (ast var-replacements fun-replacements)
  (:documentation
   "Replace variable and function references, returning a new AST."))

(defgeneric replace-in-ast (ast replacements &key test)
  (:documentation
   "Make arbitrary replacements within AST, returning a new AST."))

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

(defgeneric build-op (software mutation)
  (:documentation "Build ast-tree operation on SOFTWARE from a MUTATION."))

(defgeneric recontextualize-mutation (ast-tree mutation)
  (:documentation "Bind free variables and functions in the mutation to concrete
values.  Additionally perform any updates to the software object required
for successful mutation."))

(defgeneric fixup-mutation (operation current before ast after)
  (:documentation "Adjust mutation result according to syntactic context."))

(defgeneric select-crossover-points (a b)
  (:documentation "Select suitable crossover points in A and B.
If no suitable points are found the returned points may be nil."))


;;; Core ast-tree methods
(defvar *ast-tree-obj-code* (register-code 45 'ast-tree)
  "Object code for serialization of ast-tree software objects.")

(defstore-cl-store (obj ast-tree stream)
  ;; NOTE: Does *not* support documentation.
  (let ((copy (copy obj)))
    (setf (slot-value copy 'copy-lock) nil)
    (output-type-code *ast-tree-obj-code* stream)
    (cl-store::store-type-object copy stream)))

(defrestore-cl-store (ast-tree stream)
  ;; NOTE: Does *not* support documentation.
  (let ((obj (cl-store::restore-type-object stream)))
    (setf (slot-value obj 'copy-lock) (make-lock "ast-tree-copy"))
    obj))

(defmethod copy :before ((obj ast-tree))
  "Update ASTs in OBJ prior to performing a copy.
* OBJ ast-tree software object to copy
"
  ;; Update ASTs before copying to avoid duplicates. Lock to prevent
  ;; multiple threads from updating concurrently.
  (unless (slot-value obj 'ast-root)
    (bordeaux-threads:with-lock-held ((slot-value obj 'copy-lock))
      (update-asts obj))))

(defmethod size ((obj ast-tree))
  "Return the number of ASTs in OBJ."
  (length (asts obj)))

(defmethod genome ((obj ast-tree))
  "Return the source code in OBJ."
  ;; If genome string is stored directly, use that. Otherwise,
  ;; build the genome by walking the AST.
  (if-let ((val (slot-value obj 'genome)))
    (progn (assert (null (slot-value obj 'ast-root)) (obj)
                   "Software object ~a has both genome and ASTs saved" obj)
           val)
    (peel-bananas (source-text (ast-root obj)))))

(defmethod (setf genome) :before (new (obj ast-tree))
  "Clear ASTs, fitness, and other caches prior to updating the NEW genome."
  (declare (ignorable new))
  (with-slots (ast-root fitness) obj
    (setf ast-root nil
          fitness nil))
  (clear-caches obj))

(defmethod (setf ast-root) :before (new (obj ast-tree))
  "Clear fitness and other caches prior to updating
the NEW ast-root."
  (declare (ignorable new))
  (with-slots (fitness) obj
    (setf fitness nil))
  (clear-caches obj))

(defmethod ast-root :before ((obj ast-tree))
  "Ensure the `ast-root' field is set on OBJ prior to access."
  (update-asts-if-necessary obj))

(defmethod size :before ((obj ast-tree))
  "Ensure the `asts' field is set on OBJ prior to access."
  (update-asts-if-necessary obj))

(defmethod asts :before ((obj ast-tree))
  "Ensure the `asts' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod update-asts :around ((obj ast-tree))
  "Wrap update-asts to only parse OBJ when the `asts-changed-p'
field indicates the object has changed since the last parse."
  (when (asts-changed-p obj)
    (clear-caches obj)
    (call-next-method))
  (setf (asts-changed-p obj) nil))

(defmethod update-asts-if-necessary ((obj ast-tree))
  "Parse ASTs in obj if the `ast-root' field has not been set.
* OBJ object to potentially populate with ASTs
"
  (with-slots (ast-root) obj (unless ast-root (update-asts obj))))

(defmethod update-caches ((obj ast-tree))
  (labels ((helper (tree path)
             ;; Collect all ast-refs
             (when (listp tree)
               (cons (make-ast-ref :ast tree :path (reverse path))
                     (iter (for c in (cdr tree))
                           (for i upfrom 0)
                           (unless (stringp c)
                             (appending (helper c (cons i path)))))))))
    (setf (slot-value obj 'asts)
          (cdr (helper (ast-root obj) nil)))))

(defmethod update-caches-if-necessary ((obj ast-tree))
  "Update cached fields if these fields have not been set.
* OBJ object to potentially populate with cached fields
"
  (with-slots (asts) obj (unless asts (update-caches obj))))

(defmethod clear-caches ((obj ast-tree))
  "Clear cached fields on OBJ, including `asts' and `asts-changed-p`.
* OBJ object to clear caches for.
"
  (with-slots (asts asts-changed-p) obj
    (setf asts nil
          asts-changed-p t)))


;;; Retrieving ASTs
(defmethod roots ((obj ast-tree))
  "Return all top-level ASTs in OBJ.
* OBJ ast-tree software object to search for roots
"
  (roots (asts obj)))

(defmethod roots ((asts list))
  "Return all top-level ASTs in ASTS.
* ASTS list of ASTs to search for roots
"
  (remove-if-not [{= 1} #'length #'ast-ref-path] asts))

(defmethod ast-at-index ((obj ast-tree) index)
  "Return the AST in OBJ at INDEX.
* OBJ object to retrieve ASTs for
* INDEX nth AST to retrieve
"
  (nth index (asts obj)))

(defmethod index-of-ast ((obj ast-tree) (ast ast-ref))
  "Return the index of AST in OBJ.
* OBJ object to query for the index of AST
* AST node to find the index of
"
  (position ast (asts obj) :test #'equalp))

(defmethod get-ast ((obj ast-tree) (path list))
  "Return the AST in OBJ at the given PATH.
* OBJ ast-tree software object with ASTs
* PATH path to the AST to return
"
  (get-ast (ast-root obj) path))

(defmethod get-ast ((tree list) (path list))
  "Return the AST in TREE at the given PATH.
* TREE tree data structure containing ASTs
* PATH path to the AST to return
"
    (if path
        (destructuring-bind (head . tail) path
          (get-ast (nth head (cdr tree))
                   tail))
        tree))

(defmethod parent-ast-p ((obj ast-tree) possible-parent-ast ast)
  "Return true if POSSIBLE-PARENT-AST is a parent of AST in OBJ, nil
otherwise.
* OBJ software object containing AST and its parents
* POSSIBLE-PARENT-AST node to find as a parent of AST
* AST node to start parent search from
"
  (member possible-parent-ast (get-parent-asts obj ast)
          :test #'equalp))

(defmethod get-parent-ast ((obj ast-tree) (ast ast-ref))
  "Return the parent node of AST in OBJ
* OBJ software object containing AST and its parent
* AST node to find the parent of
"
  (when-let ((path (butlast (ast-ref-path ast))))
    (make-ast-ref :ast (get-ast obj path)
                  :path path)))

(defmethod get-parent-asts ((obj ast-tree) (ast ast-ref))
  "Return the parent nodes of AST in OBJ
* OBJ software object containing AST and its parents
* AST node to find the parents of
"
  (labels ((get-parent-asts-helper (path tree)
             (if (null path)
                 nil
                 (let ((subtree (nth (car path) (cdr tree)))
                       (subtree-path (take (- (length (ast-ref-path ast))
                                              (length (cdr path)))
                                           (ast-ref-path ast))))
                   (cons (make-ast-ref :path subtree-path :ast subtree)
                         (get-parent-asts-helper (cdr path) subtree))))))
    (-> (get-parent-asts-helper (ast-ref-path ast) (ast-root obj))
        (reverse))))

(defmethod get-immediate-children ((obj ast-tree) (ast ast-ref))
  "Return the immediate children of AST in OBJ.
* OBJ software object containing AST and its children
* AST node to find the children of
"
  (declare (ignorable obj))
  (let ((path (ast-ref-path ast)))
    (iter (for child in (cdr (ast-ref-ast ast)))
          (for i upfrom 0)
          (when (listp child)
            (collect (make-ast-ref :ast child :path (append path (list i))))))))

(defmethod ast-to-source-range ((obj ast-tree) (ast ast-ref))
  "Convert AST to pair of SOURCE-LOCATIONS."
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
             (iter (for child in (cdr ast))
               (multiple-value-setq (line column)
                 (scan-ast child line column))))

         (values line column))
       (ast-start (ast path line column)
         "Scan to the start of an AST, returning line and column."
         (bind (((head . tail) path)
                ((_ . children) ast))
           ;; Scan preceeding ASTs
           (iter (for child in (subseq children 0 head))
                 (multiple-value-setq (line column)
                   (scan-ast child line column)))
           ;; Recurse into child
           (when tail
             (multiple-value-setq (line column)
               (ast-start (nth head children) tail line column)))
           (values line column))))

    (when ast
      (bind (((:values start-line start-col)
              (ast-start (ast-root obj) (ast-ref-path ast) 1 1))
             ((:values end-line end-col)
              (scan-ast (ast-ref-ast ast) start-line start-col)))
       (make-instance 'source-range
                      :begin (make-instance 'source-location
                                            :line start-line
                                            :column start-col)
                      :end (make-instance 'source-location
                                          :line end-line
                                          :column end-col))))))

(defmethod ast-source-ranges ((obj ast-tree))
  "Return (AST . SOURCE-RANGE) for each AST in OBJ."
  (labels
      ((source-location (line column)
         (make-instance 'source-location :line line :column column))
       (scan-ast (ast path line column)
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
                     (iter (for child in (cdr ast))
                           (for i upfrom 0)
                           (appending
                            (multiple-value-bind
                                  (ranges new-line new-column)
                                (scan-ast child (append path (list i))
                                          line column)
                              (setf line new-line
                                    column new-column)
                              ranges)
                            into child-ranges)
                           (finally
                            (return
                              (cons (cons (make-ast-ref :path path
                                                        :ast ast)
                                          (make-instance 'source-range
                                                         :begin begin
                                                         :end (source-location
                                                               line column)))
                                    child-ranges)))))))

           (values ranges line column))))

    (cdr (scan-ast (ast-root obj) nil 1 1))))

(defmethod asts-containing-source-location
    ((obj ast-tree) (loc source-location))
  "Return a list of ASTs in OBJ containing LOC."
  (when loc
    (mapcar #'car
            (remove-if-not [{contains _ loc} #'cdr] (ast-source-ranges obj)))))

(defmethod asts-contained-in-source-range
    ((obj ast-tree) (range source-range))
  "Return a list of ASTs in contained in RANGE."
  (when range
    (mapcar #'car
            (remove-if-not [{contains range} #'cdr] (ast-source-ranges obj)))))

(defmethod asts-intersecting-source-range
    ((obj ast-tree) (range source-range))
  "Return a list of ASTs in OBJ intersecting RANGE."
  (when range
    (mapcar #'car
            (remove-if-not [{intersects range} #'cdr]
                           (ast-source-ranges obj)))))


;;; Tree manipulations
(defun replace-nth-child (ast n replacement)
  "Return AST with the Nth element of AST replaced with REPLACEMENT.
* AST ast to modify
* N element to modify
* REPLACEMENT replacement for the Nth element
"
  (nconc (subseq ast 0 (+ 1 n))
         (list replacement)
         (subseq ast (+ 2 n))))

(defmethod replace-ast ((tree list) (location ast-ref)
                        (replacement ast-ref))
  "Return the modified TREE with the AST at LOCATION replaced with
REPLACEMENT.
* TREE Applicative AST tree to be modified
* LOCATION AST to be replaced in TREE
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
                ((node . children) tree))
           (if tail
               ;; The insertion may need to modify text farther up the
               ;; tree. Pass down the next bit of non-empty text and
               ;; get back a new string.
               (multiple-value-bind (child new-next)
                   (helper (nth head children) tail
                           (or (non-empty (nth (1+ head) children))
                               next))
                 (if (and new-next (non-empty (nth (1+ head) children)))
                     ;; The modified text belongs here. Insert it.
                     (values (nconc (subseq tree 0 (+ 1 head))
                                    (list child new-next)
                                    (subseq tree (+ 3 head)))
                             nil)

                     ;; Otherwise keep passing it up the tree.
                     (values (replace-nth-child tree head child)
                             new-next)))
               (let* ((after (nth (1+ head) children))
                      (fixed (fixup-mutation :instead
                                             (car (nth head children))
                                             (if (positive-integer-p head)
                                                 (nth (1- head) children)
                                                 "")
                                             (ast-ref-ast replacement)
                                             (or (non-empty after) next))))

                 (if (non-empty after)
                     ;; fixup-mutation can change the text after the
                     ;; insertion (e.g. to remove a semicolon). If
                     ;; that text is part of this AST, just include it
                     ;; in the list.
                     (values
                      (cons node (nconc (subseq children 0 (max 0 (1- head)))
                                        fixed
                                        (nthcdr (+ 2 head) children)))
                      nil)

                     ;; If the text we need to modify came from
                     ;; farther up the tree, return it instead of
                     ;; inserting it here.
                     (values
                      (cons node (nconc (subseq children 0 (max 0 (1- head)))
                                         (butlast fixed)
                                         (nthcdr (+ 2 head) children)))
                      (lastcar fixed))))))))
    (helper tree (ast-ref-path location) nil)))

(defmethod remove-ast ((tree list) (location ast-ref))
  "Return the modified TREE with the AST at LOCATION removed.
* TREE Applicative AST tree to be modified
* LOCATION AST to be removed in TREE
"
  (labels
      ((helper (tree path)
         (bind (((head . tail) path)
                ((node . children) tree))
           (if tail
               ;; Recurse into child
               (replace-nth-child tree head (helper (nth head children) tail))

               ;; Remove child
               (cons node
                     (nconc (subseq children 0 (max 0 (1- head)))
                            (fixup-mutation
                             :remove
                             (car (nth head children))
                             (if (positive-integer-p head)
                                 (nth (1- head) children)
                                 "")
                             nil
                             (or (nth (1+ head) children) ""))
                            (nthcdr (+ 2 head) children)))))))
    (helper tree (ast-ref-path location))))

(defmethod splice-asts ((tree list) (location ast-ref) (new-asts list))
  "Splice a list directly into the given location, replacing the original AST.

Can insert ASTs and text snippets. Does minimal syntactic fixups, so
use carefully.

* TREE Applicative AST tree to be modified
* LOCATION AST marking location where insertion is to occur
* NEW-ASTS ASTs to be inserted into TREE
"
  (labels
    ((helper (tree path)
       (bind (((head . tail) path)
              ((node . children) tree))
         (if tail
             ;; Recurse into child
             (replace-nth-child tree head (helper (nth head children) tail))

             ;; Splice into children
             (cons node
                   (nconc (subseq children 0 head)
                          new-asts
                          (nthcdr (1+ head) children)))))))
    (helper tree (ast-ref-path location))))

(defmethod insert-ast ((tree list) (location ast-ref)
                       (replacement ast-ref))
  "Return the modified TREE with the REPLACEMENT inserted at LOCATION.
* TREE Applicative AST tree to be modified
* LOCATION AST marking location where insertion is to occur
* REPLACEMENT AST to insert
"
  (labels
    ((helper (tree path)
       (bind (((head . tail) path)
              ((node . children) tree))
         (if tail
             ;; Recurse into child
             (replace-nth-child tree head (helper (nth head children) tail))

             ;; Insert into children
             (cons node
                   (nconc (subseq children 0 (max 0 (1- head)))
                          (fixup-mutation :before
                                          (car (nth head children))
                                          (if (positive-integer-p head)
                                              (nth (1- head) children)
                                              "")
                                          (ast-ref-ast replacement)
                                          (or (nth head children) ""))
                          (nthcdr (1+ head) children)))))))
    (helper tree (ast-ref-path location))))

(defmethod insert-ast-after ((tree list) (location ast-ref)
                             (ast ast-ref))
  "Insert AST immediately after LOCATION in TREE, returning new tree.

Does not modify the original TREE.
"
  (labels
    ((helper (tree path)
       (bind (((head . tail) path)
              ((node . children) tree))
         (if tail
             ;; Recurse into child
             (replace-nth-child tree head (helper (nth head children) tail))

             ;; Insert into children
             (cons node
                   (nconc (subseq children 0 (max 0 head))
                          (fixup-mutation :after
                                          (car (nth head children))
                                          (nth head children)
                                          (ast-ref-ast ast)
                                          (or (nth (1+ head) children) ""))
                          (nthcdr (+ 2 head) children)))))))
    (helper tree (ast-ref-path location))))


;;; Recontextualization
(defmethod rebind-vars ((ast ast-ref) var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding
"
  (make-ast-ref :path (ast-ref-path ast)
                :ast (rebind-vars (ast-ref-ast ast)
                                  var-replacements fun-replacements)))

(defmethod rebind-vars ((ast list)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding
"
  ;; var-replacements looks like:
  ;; ( (("(|old-name|)" "(|new-name|)") ... )
  ;; These name/depth pairs can come directly from ast-unbound-vals.

  ;; fun-replacements are similar, but the pairs are function info
  ;; lists taken from ast-unbound-funs.

  (destructuring-bind (node . children) ast
    (let ((new (copy-structure node)))
      (setf (ast-unbound-vals new)
            (remove-duplicates
             (mapcar (lambda (v)
                       (or (&>> var-replacements
                                (find-if [{equal v} #'peel-bananas #'car])
                                (second)
                                (peel-bananas))
                           v))
                     (ast-unbound-vals new))
             :test #'equal))

      (cons new
            (mapcar {rebind-vars _ var-replacements fun-replacements}
                    children)))))

(defmethod rebind-vars ((ast string) var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding
"
  (reduce (lambda (new-ast replacement)
            (replace-all new-ast (first replacement) (second replacement)))
          (append var-replacements
                  (mapcar (lambda (fun-replacement)
                            (list (car (first fun-replacement))
                                  (car (second fun-replacement))))
                          fun-replacements))
          :initial-value ast))

(defmethod replace-in-ast ((ast ast-ref) replacements &key (test #'eq))
  "Make arbitrary replacements within AST, returning a new AST.
* AST node to perform modifications to
* REPLACEMENTS association list of key, value pairs to replace in AST
* TEST function to test if a given replacement key can be found in AST
"
  (make-ast-ref :path (ast-ref-path ast)
                :ast (replace-in-ast (ast-ref-ast ast) replacements
                                     :test test)))

(defmethod replace-in-ast ((ast list) replacements &key (test #'eq))
  "Make arbritrary replacements within AST, returning a new AST.
* AST node to perform modifications to
* REPLACEMENTS association list of key, value pairs to replace in AST
* TEST function to test if a given replacement key can be found in AST
"
  (or
   ;; If replacement found, return it
   (cdr (find ast replacements :key #'car :test test))
   ;; Otherwise recurse into children
   (destructuring-bind (node . children) ast
     (cons node
           (mapcar {replace-in-ast _ replacements :test test}
                   children)))))

(defmethod replace-in-ast (ast replacements &key (test #'eq))
  "Make arbritrary replacements within AST, returning a new AST.
* AST node to perform modifications to
* REPLACEMENTS association list of key, value pairs to replace in AST
* TEST function to test if a given replacement key can be found in AST
"
  (or (cdr (find ast replacements :key #'car :test test))
      ast))


;;; Genome manipulations
(defmethod prepend-to-genome ((obj ast-tree) text)
  "Prepend non-AST TEXT to OBJ genome.

* OBJ object to modify with text
* TEXT text to prepend to the genome
"
  (labels ((ensure-newline (text)
             (if (not (equalp #\Newline (last-elt text)))
                 (concatenate 'string text '(#\Newline))
                 text)))
    (with-slots (ast-root) obj
      (setf ast-root
            (destructuring-bind (first second . rest) (ast-root obj)
              (list* first
                     (concatenate 'string (ensure-newline text) second)
                     rest))))))

(defmethod append-to-genome ((obj ast-tree) text)
  "Append non-AST TEXT to OBJ genome.  The new text will not be parsed.

* OBJ object to modify with text
* TEXT text to append to the genome
"
  (with-slots (ast-root) obj
    (setf ast-root
          (if (stringp (lastcar (ast-root obj)))
              (append (butlast (ast-root obj))
                      (list (concatenate 'string (lastcar (ast-root obj))
                                                 text)))
              (append (ast-root obj) (list text))))))


;; Targeting functions
(defmethod pick-bad ((obj ast-tree))
  "Pick a 'bad' index into a software object.
Used to target mutation."
  (if (bad-asts obj)
      (random-elt (bad-asts obj))
      (error (make-condition 'no-mutation-targets
               :obj obj :text "No asts to pick from"))))

(defmethod pick-good ((obj ast-tree))
  "Pick a 'good' index into a software object.
Used to target mutation."
  (if (good-asts obj)
      (random-elt (good-asts obj))
      (error (make-condition 'no-mutation-targets
               :obj obj :text "No asts to pick from"))))

(defmethod bad-asts ((obj ast-tree))
  "Return a list of all bad asts in OBJ"
  (asts obj))

(defmethod good-asts ((obj ast-tree))
  "Return a list of all good asts in OBJ"
  (asts obj))

(defmethod good-mutation-targets ((obj ast-tree) &key filter)
  "Return a list of all good mutation targets in OBJ matching FILTER.
* OBJ software object to query for good mutation targets
* FILTER predicate taking an AST parameter to allow for filtering
"
  (mutation-targets obj :filter filter :stmt-pool #'good-asts))

(defmethod bad-mutation-targets ((obj ast-tree) &key filter)
  "Return a list of all bad mutation targets in OBJ matching FILTER.
* OBJ software object to query for bad mutation targets
* FILTER predicate taking an AST parameter to allow for filtering
"
  (mutation-targets obj :filter filter :stmt-pool #'bad-asts))

(defmethod mutation-targets ((obj ast-tree)
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
  "Pick ASTs from FIRST-POOL and optionally SECOND-POOL, where FIRST-POOL and
SECOND-POOL are methods on SOFTWARE which return a list of ASTs.  An
optional filter function having the signature 'f ast &optional first-pick',
may be passed, returning true if the given AST should be included as a possible
pick or false (nil) otherwise."
  (let ((first-pick (&> (mutation-targets software :filter filter
                                                   :stmt-pool first-pool)
                        (random-elt))))
    (if (null second-pool)
        (list (cons :stmt1 first-pick))
        (list (cons :stmt1 first-pick)
              (cons :stmt2 (&> (mutation-targets software
                                 :filter (lambda (ast)
                                           (if filter
                                               (funcall filter ast first-pick)
                                               t))
                                 :stmt-pool second-pool)
                               (random-elt)))))))

(defmethod pick-bad-good ((software ast-tree) &key filter
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

(defmethod pick-bad-bad ((software ast-tree) &key filter
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

(defmethod pick-bad-only ((software ast-tree) &key filter
                          (bad-pool #'bad-asts))
  "Pick a single AST from SOFTWARE from `bad-pool',
excluding those ASTs removed by FILTER.
* SOFTWARE object to perform picks for
* FILTER function taking two AST parameters and returning non-nil if the
second should be included as a possible pick
* BAD-POOL function returning a pool of 'bad' ASTs in SOFTWARE
"
  (pick-general software #'bad-asts :filter filter))


;;; Mutations
(defclass ast-tree-mutation (mutation)
  ()
  (:documentation "Specialization of the mutation interface for ast-tree
software objects."))

(define-mutation ast-tree-insert (ast-tree-mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "Perform an insertion operation on an ast-tree software
object."))

(defmethod build-op ((mutation ast-tree-insert) software)
  "Return an association list with the operations to apply a `ast-tree-insert'
MUTATION to SOFTWARE.
* MUTATION defines targets of insertion operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:insert . ,(targets mutation))))

(define-mutation ast-tree-swap (ast-tree-mutation)
  ((targeter :initform #'pick-bad-bad))
  (:documentation "Perform a swap operation on an ast-tree software object."))

(defmethod build-op ((mutation ast-tree-swap) software)
  "Return an association list with the operations to apply a `ast-tree-swap'
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
(define-mutation ast-tree-move (ast-tree-mutation)
  ((targeter :initform #'pick-bad-bad))
  (:documentation "Perform a move operation on an ast-tree software object."))

(defmethod build-op ((mutation ast-tree-move) software)
  "Return an association list with the operations to apply a `ast-tree-move'
MUTATION to SOFTWARE.
* MUTATION defines targets of the move operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:insert (:stmt1 . ,(aget :stmt1 (targets mutation)))
             (:stmt2 . ,(aget :stmt2 (targets mutation))))
    (:cut (:stmt1 . ,(aget :stmt2 (targets mutation))))))

;;; Replace
(define-mutation ast-tree-replace (ast-tree-mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "Perform a replace operation on an ast-tree
software object."))

(defmethod build-op ((mutation ast-tree-replace) software)
  "Return an association list with the operations to apply an `ast-tree-replace'
MUTATION to SOFTWARE.
* MUTATION defines targets of the replace operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:set . ,(targets mutation))))

(define-mutation ast-tree-cut (ast-tree-mutation)
  ((targeter :initform #'pick-bad-only))
  (:documentation "Perform a cut operation on an ast-tree software object."))

(defmethod build-op ((mutation ast-tree-cut) software)
  "Return an association list with the operations to apply a `ast-tree-cut'
MUTATION to SOFTWARE.
* MUTATION defines the targets of the cut operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:cut . ,(targets mutation))))

;;; Nop
(define-mutation ast-tree-nop (ast-tree-mutation)
  ()
  (:documentation "Perform a nop on an ast-tree software object."))

(defmethod build-op ((mutation ast-tree-nop) software)
  "Return an association list with the operations to apply a `nop'
MUTATION to SOFTWARE.
* MUATION defines teh targets of the nop operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software mutation))
  nil)


;;; General mutation methods
(defmethod apply-mutation ((software ast-tree)
                           (mutation ast-tree-mutation))
  "Apply MUTATION to SOFTWARE, returning the resulting SOFTWARE.
* SOFTWARE object to be mutated
* MUTATION mutation to be performed
"
  (apply-mutation-ops software
                      ;; Sort operations latest-first so they
                      ;; won't step on each other.
                      (sort (recontextualize-mutation software mutation)
                            #'ast-later-p :key [{aget :stmt1} #'cdr])))

(defmethod apply-mutation ((obj ast-tree) (op list))
  "Apply OPS to SOFTWARE, returning the resulting SOFTWARE.
* OBJ object to be mutated
* OP mutation to be performed
"
  (apply-mutation obj (make-instance (car op) :targets (cdr op))))

(defmethod apply-mutation-ops ((software ast-tree) (ops list))
  "Apply a recontextualized list of OPS to SOFTWARE, returning the resulting
SOFTWARE.
* SOFTWARE object to be mutated
* OPS list of association lists with operations to be performed
"
  (setf (ast-root software)
        (with-slots (ast-root) software
          (iter (for (op . properties) in ops)
                (let ((stmt1 (aget :stmt1 properties))
                      (value1 (aget :value1 properties)))
                  (setf ast-root
                        (ecase op
                          (:set
                            (replace-ast ast-root stmt1 value1))
                          (:cut
                            (remove-ast ast-root stmt1))
                          (:insert
                            (insert-ast ast-root stmt1 value1))
                          (:insert-after
                            (insert-ast-after ast-root stmt1 value1))
                          (:splice
                            (splice-asts ast-root stmt1 value1)))))
                (finally (return ast-root)))))

  (clear-caches software)
  software)


;;; Interface for ast-diff
(defvar ast-tree-diff-interface
  (labels       ; Defined w/labels so they're defined at load time.
      ((ast-equal-p (ast-a ast-b)
         (or (eq ast-a ast-b)
             (and (stringp ast-a) (stringp ast-b) (string= ast-a ast-b))
             (and (consp ast-a) (consp ast-b)
                  (eq (sel:ast-class (car ast-a)) (sel:ast-class (car ast-b)))
                  (eq (length ast-a) (length ast-b))
                  (every #'ast-equal-p (cdr ast-a) (cdr ast-b)))))
       (ast-cost (ast)
         (if (listp ast)
             (apply #'+ (mapcar #'ast-cost (cdr ast)))
             1))
       (can-recurse (ast-a ast-b)
         (and (consp ast-a)
              (consp ast-b)
              (eq (ast-class (car ast-a))
                  (ast-class (car ast-b))))))
    (make-instance 'ast-interface
      :equal-p #'ast-equal-p
      :cost #'ast-cost
      :can-recurse #'can-recurse
      :text [#'peel-bananas #'source-text]))
  "AST-DIFF interface for AST-TREE objects.")

(defmethod diff-software ((ast-tree-a ast-tree) (ast-tree-b ast-tree))
  (ast-diff ast-tree-diff-interface
            (ast-root ast-tree-a)
            (ast-root ast-tree-b)))

(defmethod edit-software ((obj ast-tree) edit-script)
  (setf (ast-root obj)
        (apply-edit-script ast-tree-diff-interface
                           (ast-root obj)
                           edit-script))
  obj)
