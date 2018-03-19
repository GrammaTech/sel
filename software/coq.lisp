;;; coq.lisp --- Coq software representation
;;;
;;; @subsection Coq Module Organization
;;;
;;; The `coq' module is split into two layers. The lower layer, implemented in
;;; `serapi-io.lisp', is strictly responsible for serialization of Coq abstract
;;; syntax trees (ASTs) and is described in-depth in its own section.
;;;
;;; The `coq' and `coq-project' software objects, implemented in `coq.lisp' and
;;; `coq-project.lisp', respectively, are higher-level abstractions built on
;;; `serapi-io'. Ideally, clients should only have to construct software objects
;;;  and use API functions provided by `coq.lisp' or `coq-project.lisp' without
;;;  having to worry about the lower-level functions.
;;;
;;; @subsection Creating Coq Objects
;;;
;;; Coq software objects have the following fields:
;;;
;;; * project-file - path to _CoqProject file
;;; * file-source - path to the Coq source file this object represents
;;; * imports - list of ASTs representing load or require statements
;;; * genome - list of ASTs representing the Coq source, excluding imports
;;; * ast-ids - list of AST IDs assigned to ASTs in the genome
;;; * fitness - fitness of the object last time it was evaluated
;;;
;;; The recommended way to create a Coq software object is using `from-file':
;;;
;;; @code{
;;; (with-serapi ()
;;;   (from-file (make-instance 'coq :project-file "/path/to/_CoqProject")
;;;              "/path/to/Foo.v"))
;;; }
;;;
;;; If you don't have a _CoqProject file, you may omit the `:project-file'
;;; keyword.
;;;
;;; Since many API functions require interactions with SerAPI, a `with-serapi'
;;; macro is provided to automatically create a sertop process. This works by
;;; binding the dynamic variable `*serapi-process*' to an interactive sertop
;;; process. This is important to be aware of if you intend to use multiple
;;; threads: each thread _must_ have its own separate sertop process (in
;;; bordeaux-threads this may be accomplished by creating thread-local copies
;;; with `*default-special-bindings*').
;;;
;;; Each `-I' or `-R' line in _CoqProject will be automatically added to the Coq
;;; loadpath in the sertop process. If a new sertop process is created, you must
;;; ensure that these load paths are reset and any imports are reloaded. You can
;;; do this with either `set-load-paths' (which only sets the load paths) or
;;; `reset-and-load-imports' (which in addition to setting load paths also
;;; executes import statements).
;;;
;;; @subsection Usage of Coq Objects
;;;
;;; The ASTs of a Coq genome are lists whose elements are lists, symbols, and
;;; strings. A `type-safe-swap' mutation selects two subtrees that have the same
;;; ``tag'' (i.e., the first symbol in that list) and swaps them. Favoring this
;;; mutation helps to cut down on type errors that would result from swapping
;;; arbitrary subtrees.
;;;
;;; Additional mutations are forthcoming.
;;;
;;; In the AST representations provided by Coq, many statements include location
;;; information tying AST nodes to locations in the source file. To help reduce
;;; the size of the AST, `from-file' replaces the full location information with
;;; the list `(:loc nil)'. This ensures that it's clear where the location
;;; information was without overly cluttering the AST. The implementation of
;;; `lookup-source-strings' ensures that these are removed prior to sending the
;;; ASTs to sertop to look up source strings. The default implementations of
;;; `pick-bad' and `pick-good' for Coq objects exclude ``located'' statements.
;;;
;;; Since sertop maintains a stateful representation of definitions that have
;;; been loaded and Coq prevents redefinition, it is sometimes necessary to
;;; reset the state of sertop to an earlier state. The easiest way to do this is
;;; to use `insert-reset-point' to indicate that you may later want to restore
;;; sertop to the current state, and `reset-serapi-process' to reset to a
;;; previously saved state. Both `from-file' and `reset-and-load-imports' insert
;;; a reset point before returning.
;;;
;;; @texi{coq}
(in-package :software-evolution-library)
(in-readtable :serapi-readtable)

;; Coq object
(define-software coq (lisp)
  ((ast-ids
    :initarg :ast-ids :accessor ast-ids :initform nil :copier copy-tree
    :documentation "List of IDs for the ASTs when they were loaded.")
   (project-file
    :initarg :project-file :accessor project-file :initform nil :copier :direct
    :documentation "Path to _CoqProject file, if it exists.")
   (file-source
    :initarg :file-source :accessor file-source :initform nil :copier :direct
    :documentation "Name of source file.")
   (imports
    :initarg :imports :accessor imports :initform nil
    :copier copy-list
    :documentation "ASTs for imports, not part of genome."))
  (:documentation "Coq software object."))

(defun set-load-paths (project-file)
  "Add to the Coq load paths using contents of _CoqProject file PROJECT-FILE.
For each \"-R\" or \"-I\" line in PROJECT-FILE, add the directory to the Coq
load path, with a nickname if one was provided in the PROJECT-FILE."
  (let ((dir (cl-fad:pathname-directory-pathname project-file)))
    (when project-file
      (with-open-file (in project-file)
        (iter (for line = (read-line in nil nil))
              (while line)
              (when (or (starts-with-subseq "-R" line)
                        (starts-with-subseq "-I" line))
                (let* ((split-line (split-sequence #\Space line))
                       (rel-dir (second split-line))
                       (nickname (third split-line)))
                  (lib-add (namestring
                            (cl-fad:merge-pathnames-as-file dir rel-dir))
                           :lib-name nickname))))))))

(defgeneric reset-and-load-imports (coq &key imports)
  (:documentation
   "Reset the SerAPI process and load IMPORTS for COQ software object."))

(defmethod reset-and-load-imports ((obj coq) &key (imports (imports obj)))
  "Reset the SerAPI process and load IMPORTS for COQ software object.
IMPORTS defaults to the list of `imports' in COQ."
  (reset-serapi-process)
  (set-load-paths (project-file obj))
  (mapc (lambda (import)
          (add-coq-string (if (stringp import)
                              import
                              (lookup-coq-string import))))
        imports))

(defun tag-loc-info (sexpr)
  "Return SEXPR with Coq location info replaced by `(:loc NIL)'.
See also `is-loc-info' and `untag-loc-info'."
  (cond
    ((not (listp sexpr)) sexpr)
    ((is-loc-info sexpr) '(:loc nil))
    (t (mapcar #'tag-loc-info sexpr))))

(defun untag-loc-info (sexpr)
  "Return SEXPR with occurrences of `(:loc NIL)' replaced by NIL.
See also `tag-loc-info'."
  (cond
    ((not (listp sexpr)) sexpr)
    ((equal sexpr '(:loc nil)) nil)
    (t (mapcar #'untag-loc-info sexpr))))

(defgeneric unannotated-genome (software)
  (:documentation "Remove any annotations added to SOFTWARE."))

(defmethod unannotated-genome ((obj coq))
  "Remove :LOC tags from Coq OBJ."
  (untag-loc-info (copy-tree (genome obj))))


(defmethod from-file ((obj coq) file)
  "Load Coq OBJ from file FILE using."
  (when (project-file obj)
    (set-load-paths (project-file obj))
    (insert-reset-point))
  (bind ((ast-ids (load-coq-file file))
         ((import-asts import-strs asts new-ids)
          (iter (for id in ast-ids)
                (let ((ast (lookup-coq-ast id))
                      (str (lookup-coq-string id)))
                  (if (is-import-ast ast)
                      ;; separate out import asts
                      (progn
                        (collect ast into imports)
                        (collect str into import-strs))
                      ;; save non-import asts and strings
                      (progn
                        (collect ast into asts)
                        (collect id into new-ids))))
                (finally
                 (return (list imports import-strs asts new-ids))))))
    (setf (genome obj) (tag-loc-info asts))
    (setf (ast-ids obj) new-ids)
    (setf (file-source obj) file)
    (setf (imports obj) import-asts)

    ;; load imports and update reset-point
    (reset-and-load-imports obj :imports import-strs)
    (insert-reset-point))
  obj)

(defgeneric lookup-source-strings (software &key include-imports)
  (:documentation "Return a list of source strings for the ASTs in SOFTWARE.
Set INCLUDE-IMPORTS to T to include import statements in the result."))

(defmethod lookup-source-strings ((obj coq) &key include-imports)
  "Return a list of source strings for the ASTs in COQ.
Set INCLUDE-IMPORTS to T to include import statements in the result."
  (append
   (when include-imports
     (iter (for import in (imports obj))
           (collecting (lookup-coq-string import))))
   (iter (for ast in (unannotated-genome obj))
         (when (listp ast)
           (collecting (lookup-coq-string ast))))))

(defmethod coq-type-checks ((obj coq))
  "Return the fraction of ASTs in Coq software OBJ that typecheck."
  (reset-serapi-process)
  (insert-reset-point)
  (iter (for str in (lookup-source-strings obj :include-imports nil))
        (let ((new-ids (add-coq-string str)))
          (appending new-ids into new-ast-ids)
          (sum (if new-ids 1 0) into typecheck-sum))
        (finally
         (setf (ast-ids obj) new-ast-ids)
         (return (if (zerop (length (genome obj)))
                     0
                     (/ typecheck-sum (length (genome obj))))))))

(defmethod to-file ((obj coq) path)
  "Look up source strings for Coq OBJ ASTs and write to PATH."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "~{~a~%~^~}"
            (mapcar #'unescape-string
                    (lookup-source-strings obj :include-imports t)))))


(defgeneric filter-subtrees-indexed (predicate software)
  (:documentation
   "Return a list of subtrees in SOFTWARE that satisfy PREDICATE.
Unlike `filter-subtrees', PREDICATE accepts two parameters: the index of the
subtree in genome and the subtree itself."))

(defmethod filter-subtrees-indexed (predicate (lisp lisp))
  "Return a list of subtrees in SOFTWARE that satisfy PREDICATE.
Unlike `filter-subtrees', PREDICATE accepts two parameters: the index of the
subtree in genome and the subtree itself."
  (iter (for i below (size lisp))
        (when (funcall predicate i (sel::subtree (genome lisp) i))
          (collect i))))

(defun non-located-stmts (tree)
  "Return a list of the indices of subtrees which are not tagged location info.
See also `tag-loc-info'."
  (labels ((descend (tree index)
             (cond
               ((and (listp tree) (eql :loc (car tree)))
                (values nil
                        (1- (+ index (sel::tree-size tree)))))
               ((consp tree)
                (bind (((:values car-stmts car-index)
                        (descend (car tree) (1+ index)))
                       ((:values cdr-stmts cdr-index)
                        (descend (cdr tree) (1+ car-index))))
                  (values (cons index (append car-stmts cdr-stmts))
                          cdr-index)))
               (t (values nil
                          (1- index))))))
    (descend tree 0)))

(defmethod bad-stmts ((obj coq))
  "Return a list of the indices of bad statements in OBJ."
  (remove-if-not {member _ (non-located-stmts (genome obj))}
                 (iota (size obj))))

(defmethod good-stmts ((obj coq))
  "Return a list of the indices of good statements in OBJ."
  (remove-if-not {member _ (non-located-stmts (genome obj))}
                 (iota (size obj))))

(defmethod pick-bad ((obj coq))
  "Return a randomly selected index of a bad statement in OBJ.
Remove statements containing Coq location info from consideration.
If none exist, raise a `no-mutation-targets' error."
  (if-let ((stmts (bad-stmts obj)))
    (random-elt stmts)
    (error (make-condition
            'no-mutation-targets
            :obj obj
            :text "No non-location statements in Coq genome."))))

(defmethod pick-good ((obj coq))
  "Return a randomly selected index of a good statement in OBJ.
Remove statements containing Coq location info from consideration.
If none exist, raise a `no-mutation-targets' error."
  (if-let ((stmts (good-stmts obj)))
    (if stmts
        (random-elt stmts)
        (error (make-condition
                'no-mutation-targets
                :obj obj
                :text "No non-location statements in Coq genome.")))))

(defun find-nearest-type (coq index)
  "For object COQ, find the nearest tag for the subtree at INDEX in the genome.
Search by checking the first element of subtrees, moving forward from INDEX if
needed. If the end of the genome is reached, search backward instead."
  (let ((max-index (sel::tree-size (genome coq)))
        (start-ast (sel::subtree (genome coq) index)))
    (labels ((forward-type-and-index (tree idx)
               (cond
                 ((and (consp tree) (car tree) (symbolp (car tree)))
                  (values (car tree) idx))
                 ((consp tree) (forward-type-and-index (car tree) (1+ idx)))
                 ((>= idx max-index)
                  (values nil idx))
                 (t (forward-type-and-index
                     (sel::subtree (genome coq) (1+ idx))
                     (1+ idx)))))
             (backward-type-and-index (tree idx)
               (cond
                 ((and (consp tree) (car tree) (symbolp (car tree)))
                  (values (car tree) idx))
                 ((zerop idx)
                  (values nil idx))
                 (t (backward-type-and-index
                     (sel::subtree (genome coq) (1- idx))
                     (1- idx))))))
      (bind (((:values type idx) (forward-type-and-index start-ast index)))
        (if type
            (values type idx)
            (backward-type-and-index start-ast (1- index)))))))

(defun pick-subtree-matching-type (coq type ignore-index)
  "For object COQ, randomly select the index of a subtree tagged as TYPE.
Ensure that the selected index is not equal to IGNORE-INDEX.
* COQ - a Coq software object.
* TYPE - a symbol indicating the type tag to be matched.
* IGNORE-INDEX - the index of a subtree tagged as TYPE which is not to be
selected.
"
  (when-let ((indices (filter-subtrees-indexed
                       (lambda (i tr)
                         (declare (ignorable tr))
                         (and (eql type (find-nearest-type coq i))
                              (not (eql i ignore-index))
                              (member i (bad-stmts coq))))
                       coq)))
    (random-elt indices)))

(defmethod pick-typesafe-bad-good ((obj coq))
  "For object COQ, return a pair of bad and good statements of the same type.
Return a list whose first element is the index of the bad statement and whose
second element is the index of the good statement. If no good statement is found
with the same type as the bad statement, raise a `no-mutation-targets'
condition."
  (bind ((first (pick-bad obj))
         ((:values type type-index) (find-nearest-type obj first))
         (second (when type
                   (pick-subtree-matching-type obj type type-index))))
    (if second
        (list type-index second)
        (error (make-condition 'no-mutation-targets
                               :obj obj
                               :op 'pick-typesafe-bad-good
                               :text "Typesafe mutation targets not found.")))))

(define-mutation type-safe-swap (lisp-swap)
  ((targeter :initform #'pick-typesafe-bad-good))
  (:documentation "Swap two Coq ASTs tagged with the same type."))

(defmethod apply-mutation ((obj coq) (mutation type-safe-swap))
  "Apply a `type-safe-swap' MUTATION to Coq object OBJ."
  (bind (((s1 s2) (targets mutation)))
    (note 3 "Applying `type-safe-swap' mutation with targets: (~a ~a)" s1 s2)
    (let ((s1 (max s1 s2))
          (s2 (min s1 s2)))
      (with-slots (genome) obj
        (let ((left  (copy-tree (sel::subtree genome s1)))
              (right (copy-tree (sel::subtree genome s2))))
          ;; Need the 1- so that we change the whole subtree and not just
          ;; the car (because `setf' for subtrees uses rplaca)
          (setf (sel::subtree genome (if (or (listp (car left)) (zerop s1))
                                         s1
                                         (1- s1)))
                right)
          (setf (sel::subtree genome (if (or (listp (car right)) (zerop s2))
                                         s2
                                         (1- s2)))
                left)))))
  obj)

(defmethod pick-mutation-type ((obj coq))
  "Randomly select a mutation that may be performed on OBJ."
  (declare (ignorable obj))
  (random-pick (list (cons 'type-safe-swap 0.8) (cons 'lisp-swap 1.0))))
