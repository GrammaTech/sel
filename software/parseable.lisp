;;; parseable.lisp --- software which may be parsed into ASTs
(defpackage :software-evolution-library/software/parseable
  (:nicknames :sel/software/parseable :sel/sw/parseable)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :cl-store
        :bordeaux-threads
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/source)
  (:export :parseable
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
           :force-include
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
           :expand-stmt-pool
           :name=
           :name-emptyp
           :equal-with-name=))
(in-package :software-evolution-library/software/parseable)
(in-readtable :curry-compose-reader-macros)

;;; parseable software objects
(define-software parseable (source)
  ((ast-root :initarg :ast-root :initform nil :accessor ast-root
             :documentation "Root node of AST.")
   (asts     :initarg :asts :reader asts
             :initform nil :copier :direct
             :type #+sbcl (list (cons keyword *) *) #-sbcl list
             :documentation
             "List of all ASTs.
See the documentation of `update-asts' for required invariants.")
   (copy-lock :initform (make-lock "parseable-copy")
              :copier :none
              :documentation "Lock while copying parseable objects."))
  (:documentation "Parsed AST tree software representation."))

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

;; These name functions are used by CLANG and NEW-CLANG.  When old CLANG
;; goes away, move it down into the remaining package, as it is
;; not generic to all parseables.
(defgeneric name= (n1 n2)
  (:documentation "Generalized name equality for AST names")
  (:method ((n1 string) (n2 string))
    (string= n1 n2)))

(defgeneric name-emptyp (n)
  (:documentation "Generalized name emptiness check")
  (:method ((n sequence)) (emptyp n))
  (:method (n) (declare (ignorable n)) nil))

(defun equal-with-name= (n1 n2)
  (if (consp n1)
      (and (consp n2)
           (equal-with-name= (car n1) (car n2))
           (equal-with-name= (cdr n1) (cdr n2)))
      (name= n1 n2)))

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

(defgeneric force-include (software include)
  (:documentation "Add an #include directive for an INCLUDE to SOFTWARE
even if such an INCLUDE already exists in SOFTWARE."))

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
    (peel-bananas (source-text (ast-root obj)))))

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
                  (when-let ((ofile (original-file sw)))
                    (format t "Failure in update-asts: original-file = ~a~%"
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
  (labels ((helper (tree path)
             (if path
                 (destructuring-bind (head . tail) path
                   (helper (nth head (ast-children tree))
                           tail))
                 tree)))
    (helper (ast-root obj) path)))

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
    (-> (get-parent-asts-helper (ast-root obj) (ast-path ast))
        (reverse))))

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
                     :key {aget :name}
                     :test #'name=))

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
  (let* ((first-pick (some-> (mutation-targets software :filter filter
                                               :stmt-pool first-pool)
                             (random-elt))))
    (if (null second-pool)
        (list (cons :stmt1 first-pick))
        (list (cons :stmt1 first-pick)
              (cons :stmt2 (some->
                            (mutation-targets
                             software
                             :filter (lambda (ast)
                                       (if filter
                                           (funcall filter ast first-pick)
                                           t))
                             :stmt-pool second-pool)
                            (random-elt)))))))

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
  (restart-case
      (let ((mutation (make-instance (pick-mutation-type obj) :object obj)))
        (apply-mutation obj mutation)
        (values obj mutation))
    (try-another-mutation ()
      :report "Try another mutation"
      (mutate obj))))

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
                          (:set
                            (sel/sw/ast::replace-ast ast-root stmt1 value1))
                          (:cut
                            (sel/sw/ast::remove-ast ast-root stmt1))
                          (:insert
                            (sel/sw/ast::insert-ast ast-root stmt1 value1))
                          (:insert-after
                            (sel/sw/ast::insert-ast-after ast-root stmt1 value1))
                          (:splice
                            (sel/sw/ast::splice-asts ast-root stmt1 value1)))))
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


;;; Generic tree interface
(defgeneric insert-ast (obj location ast &key literal &allow-other-keys)
  (:documentation "Return the modified OBJ with AST inserted at LOCATION.
* OBJ object to be modified
* LOCATION location where insertion is to occur
* AST AST to insert
* LITERAL keyword to control whether recontextualization is performed
          For modifications where the replacement is to be directly
          inserted, pass this keyword as true.")
  (:method ((obj parseable) (location ast) (ast ast)
            &rest args &key &allow-other-keys)
    (apply #'insert-ast obj (ast-path location) ast args))
  (:method ((obj parseable) (location list) (ast ast)
            &key literal &allow-other-keys)
    (apply-mutation obj (at-targets (make-instance 'parseable-insert)
                                    (list (cons :stmt1 location)
                                          (cons (if literal :literal1 :value1)
                                                ast))))))

(defgeneric replace-ast (obj location replacement
                         &key literal &allow-other-keys)
  (:documentation "Modify and return OBJ with the AST at LOCATION replaced
with REPLACEMENT.
* OBJ object to be modified
* LOCATION location where replacement is to occur
* REPLACEMENT AST or ASTs to insert
* LITERAL keyword to control whether recontextualization is performed
          For modifications where the replacement is to be directly
          inserted, pass this keyword as true.")
  (:method ((obj parseable) (location ast) (replacement ast)
            &rest args &key &allow-other-keys)
    (apply #'replace-ast obj (ast-path location) replacement args))
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
                                            (sel/sw/ast::replace-nth-child
                                             (get-ast obj (butlast location))
                                             (lastcar location)
                                             replacement)))))))

(defgeneric remove-ast (obj location &key &allow-other-keys)
  (:documentation "Return the modified OBJ with the AST at LOCATION removed.
* OBJ object to be modified
* LOCATION location to be removed in OBJ")
  (:method ((obj parseable) (location ast) &rest args &key &allow-other-keys)
    (apply #'remove-ast obj (ast-path location) args))
  (:method ((obj parseable) (location list) &key &allow-other-keys)
    (apply-mutation obj (at-targets (make-instance 'parseable-cut)
                                    (list (cons :stmt1 location))))))
