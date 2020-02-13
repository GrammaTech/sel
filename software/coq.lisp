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
;;; * PROJECT-FILE path to _CoqProject file
;;; * FILE-SOURCE path to the Coq source file this object represents
;;; * IMPORTS list of ASTs representing load or require statements
;;; * GENOME list of ASTs representing the Coq source, excluding imports
;;; * AST-IDS list of AST IDs assigned to ASTs in the genome
;;; * FITNESS fitness of the object last time it was evaluated
;;;
;;; The recommended way to create a Coq software object is using `from-file':
;;;
;;;     (with-serapi ()
;;;       (from-file (make-instance 'coq :project-file "/path/to/_CoqProject")
;;;                  "/path/to/Foo.v"))
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
(defpackage :software-evolution-library/software/coq
  (:nicknames :sel/software/coq :sel/sw/coq)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :split-sequence
        :cl-ppcre
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/parseable
        :software-evolution-library/software/sexp
        :software-evolution-library/components/serapi-io)
  (:shadowing-import-from :uiop :pathname-directory-pathname)
  (:export :coq
           :ast-ids
           :project-file
           :file-source
           :imports
           :coq-modules
           :coq-sections
           :coq-assumptions
           :coq-definitions
           :*coq-mutation-types*
           :reset-and-load-imports
           :init-coq-project
           :find-nearest-type
           :pick-subtree-matching-type
           :pick-typesafe-bad-good
           :type-safe-swap
           :tag-loc-info
           :untag-loc-info
           :lookup-source-strings
           :coq-type-checks
           :load-coq-object
           :synthesize-typed-coq-expression))
(in-package :software-evolution-library/software/coq)
(in-readtable :serapi-readtable)

(defvar *coq-mutation-types*
  (cumulative-distribution
   (normalize-probabilities
    '((type-safe-swap .  5)
      (sexp-swap      .  5))))
  "Cumulative distribution fo normalized probabilities of weighted mutations.")

;; Coq object
(define-software coq (sexp)
  ((project-file
    :initarg :project-file :accessor project-file :initform nil :copier :direct
    :documentation "Path to _CoqProject file, if it exists.")
   (file-source
    :initarg :file-source :accessor file-source :initform nil :copier :direct
    :documentation "Name of source file.")
   (imports
    :initarg :imports :accessor imports :initform nil
    :copier copy-list
    :documentation "ASTs for imports, not part of genome.")
   (coq-modules
    :initarg :modules :accessor coq-modules :initform nil
    :copier copy-list
    :documentation "List of Modules defined by this object.")
   (coq-sections
    :initarg :sections :accessor coq-sections :initform nil
    :copier copy-list
    :documentation "List of Sections defined by this object.")
   (coq-assumptions
    :initarg :assumptions :accessor coq-assumptions :initform nil
    :copier copy-list
    :documentation
    "List of Parameters, Variables, etc. defined by this object.")
   (coq-definitions
    :initarg :definitions :accessor coq-definitions :initform nil
    :copier copy-list
    :documentation
    "List of values defined in this object.
Includes Definition, Inductive, Fixpoint, etc."))
  (:documentation "Coq software object."))

(defun set-load-paths (project-file)
  "Add to the Coq load paths using contents of _CoqProject file PROJECT-FILE.
For each \"-R\" or \"-I\" line in PROJECT-FILE, add the directory to the Coq
load path, with a nickname if one was provided in the PROJECT-FILE."
  (let ((dir (pathname-directory-pathname project-file)))
    (when project-file
      (with-open-file (in project-file)
        (iter (for line = (read-line in nil nil))
              (while line)
              (when (starts-with-subseq "-R" line)
                (let* ((split-line (split-sequence #\Space line))
                       (rel-dir (second split-line))
                       (nickname (third split-line)))
                  (add-coq-lib (namestring
                                (merge-pathnames-as-file dir rel-dir))
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

(defun coq-asts-from-file (file project-file)
  (when project-file
    (set-load-paths project-file)
    (insert-reset-point))
  (with-open-file (in file)
    (iter (for line = (read-line in nil nil))
          (while line)
          (with current-ast = "")
          ;; if line is the end of a Coq stmt, look up the Coq ast
          (if (ends-with #\. line)
              (let* ((ast-string (format nil "~a ~a~%" current-ast line))
                     (ast-ids (add-coq-string ast-string))
                     (asts
                      (iter (for ast-id in ast-ids)
                            (with found-error = nil)
                            (handler-case
                                (let ((ast (lookup-coq-ast ast-id)))
                                  (if (and ast (lookup-coq-string ast))
                                      (collect ast into coq-genome)
                                      (setf found-error t)))
                              ;; On error, ast isn't re-serializable, so save
                              ;; the raw string. Otherwise, save the ASTs.
                              (serapi-error ()
                                (setf found-error t))
                              (condition ()
                                (setf found-error t)))
                            (finally
                             (return (if found-error
                                         (list ast-string)
                                         coq-genome))))))
                (appending asts into genome-asts)
                (setf current-ast ""))
              ;; line doesn't end with ., append to current-ast statement
              (setf current-ast (format nil "~a ~a" current-ast line)))
          (finally
           (reset-serapi-process)
           (return genome-asts)))))

(defmethod from-file ((obj coq) file)
  "Load Coq OBJ from file FILE, initializing fields in OBJ.
Resets the SerAPI process so that imports are loaded but no definitions from the
file have been added."
  (when (project-file obj)
    (set-load-paths (project-file obj))
    (insert-reset-point))
  (bind ((asts (coq-asts-from-file file (project-file obj)))
         ((import-asts import-strs genome-asts modules sections
                       assumptions definitions)
          (iter (for ast in asts)
                (with initial-imports = t)
                ;; Collect all ASTs except imports into genome.
                (if (and initial-imports (coq-import-ast-p ast))
                    ;; separate out import asts at top of file
                    (progn
                      (collect ast into imports)
                      (collect (lookup-coq-string ast) into import-strs))
                    ;; all other asts
                    (progn
                      (setf initial-imports nil)
                      (collect ast into genome-asts)))
                (when-let ((module (coq-module-ast-p ast)))
                  (collect module into modules))
                (when-let ((section (coq-section-ast-p ast)))
                  (collect section into sections))
                (when-let ((assumption (coq-assumption-ast-p ast)))
                  (collect assumption into assumptions))
                (when-let ((definition (coq-definition-ast-p ast)))
                  (collect definition into definitions))
                (finally (return (list imports import-strs genome-asts modules
                                       sections assumptions definitions))))))
    (setf (genome obj) (tag-loc-info genome-asts))
    (setf (file-source obj) file)
    (setf (imports obj) import-asts)
    (setf (coq-modules obj) modules)
    (setf (coq-sections obj) sections)
    (setf (coq-assumptions obj) assumptions)
    (setf (coq-definitions obj) definitions)

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
           (when import
             (collecting (lookup-coq-string import)))))
   (iter (for ast in (unannotated-genome obj))
         (when ast
           (collecting (lookup-coq-string ast))))))

(defun lookup-coq-import-strings (coq)
  "Return a list of the string names of modules imported at top of file."
  (remove-if «or {equal "Require"} {equal "Import"}»
             (mappend [{split "\\s+"} #'lookup-coq-string]
                      (imports coq))))

(defun load-coq-object (coq &key (include-imports t) num-asts)
  "Load the first NUM-ASTS ASTs of the COQ genome.
If NUM-ASTS is NIL, load all ASTs. INCLUDE-IMPORTS (default T) specifies that
imports should be loaded first."
  (restart-case
      (let ((ast-ids nil))
        (when include-imports
          (iter (for import in (imports coq))
                (when import
                  (when-let ((str (lookup-coq-string import)))
                    (add-coq-string str)))))
        (let ((num-asts (or num-asts (length (unannotated-genome coq)))))
          (iter (for ast in (take num-asts (unannotated-genome coq)))
                (when ast
                  (when-let ((str (lookup-coq-string ast)))
                    (append ast-ids (add-coq-string str))))
                (finally (return ast-ids)))))
    (cancel-asts-and-retry-load (ast)
      :report "Cancel ASTs and retry loading object."
      (cancel-coq-asts ast)
      (load-coq-object coq :include-imports include-imports :num-asts num-asts))
    (recreate-serapi-process-and-retry ()
      :report "Create a new SerAPI process and retry loading object."
      (kill-serapi *serapi-process*)
      (setf *serapi-process* (make-serapi))
      (unless include-imports
        (reset-and-load-imports coq))
      (load-coq-object coq :include-imports include-imports
                       :num-asts num-asts))))

(defgeneric coq-type-checks (coq)
  (:documentation
   "Return the fraction of ASTs in COQ software object that typecheck."))

(defmethod coq-type-checks ((obj coq))
  "Return the fraction of ASTs in Coq software OBJ that typecheck.
Return NIL if source strings cannot be looked up."
  (reset-serapi-process)
  (insert-reset-point)
  (iter (for str in (lookup-source-strings obj :include-imports nil))
        (let ((new-ids (add-coq-string str)))
          (sum (if new-ids 1 0) into typecheck-sum))
        (finally
         (return (if (zerop (length (genome obj)))
                     0
                     (/ typecheck-sum (length (genome obj))))))))

(defmethod to-file ((obj coq) path)
  "Look up source strings for Coq OBJ ASTs and write to PATH."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "~{~a~%~^~}"
            (mapcar #'sel/cp/serapi-io::unescape-coq-string
                    (lookup-source-strings obj :include-imports t)))))


(defgeneric filter-subtrees-indexed (predicate software)
  (:documentation
   "Return a list of subtrees in SOFTWARE that satisfy PREDICATE.
Unlike `filter-subtrees', PREDICATE accepts two parameters: the index of the
subtree in genome and the subtree itself."))

(defmethod filter-subtrees-indexed (predicate (obj sexp))
  "Return a list of subtrees in SOFTWARE that satisfy PREDICATE.
Unlike `filter-subtrees', PREDICATE accepts two parameters: the index of the
subtree in genome and the subtree itself."
  (iter (for i below (size obj))
        (when (funcall predicate i (subtree (genome obj) i))
          (collect i))))

(defun non-located-stmts (tree)
  "Return a list of the indices of subtrees which are not tagged location info.
See also `tag-loc-info'."
  (labels ((descend (tree index)
             (cond
               ((and (listp tree) (eql :loc (car tree)))
                (values nil
                        (1- (+ index (tree-size tree)))))
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
    (random-elt stmts)
    (error (make-condition
               'no-mutation-targets
             :obj obj
             :text "No non-location statements in Coq genome."))))

(defun find-nearest-type (coq index)
  "For object COQ, find the nearest tag for the subtree at INDEX in the genome.
Search by checking the first element of subtrees, moving forward from INDEX if
needed. If the end of the genome is reached, search backward instead."
  (let ((max-index (tree-size (genome coq)))
        (start-ast (subtree (genome coq) index)))
    (labels ((forward-type-and-index (tree idx)
               (cond
                 ((and (consp tree) (car tree) (symbolp (car tree)))
                  (values (car tree) idx))
                 ((consp tree) (forward-type-and-index (car tree) (1+ idx)))
                 ((>= idx max-index)
                  (values nil idx))
                 (t (forward-type-and-index
                     (subtree (genome coq) (1+ idx))
                     (1+ idx)))))
             (backward-type-and-index (tree idx)
               (cond
                 ((and (consp tree) (car tree) (symbolp (car tree)))
                  (values (car tree) idx))
                 ((zerop idx)
                  (values nil idx))
                 (t (backward-type-and-index
                     (subtree (genome coq) (1- idx))
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
                               :operation 'pick-typesafe-bad-good
                               :text "Typesafe mutation targets not found.")))))

(define-mutation type-safe-swap (sexp-swap)
  ((targeter :initform #'pick-typesafe-bad-good))
  (:documentation "Swap two Coq ASTs tagged with the same type."))

(defmethod apply-mutation ((obj coq) (mutation type-safe-swap))
  "Apply a `type-safe-swap' MUTATION to Coq object OBJ."
  (bind (((s1 s2) (targets mutation)))
    (note 3 "Applying `type-safe-swap' mutation with targets: (~a ~a)" s1 s2)
    (let ((s1 (max s1 s2))
          (s2 (min s1 s2)))
      (with-slots (genome) obj
        (let ((left  (copy-tree (subtree genome s1)))
              (right (copy-tree (subtree genome s2))))
          ;; Need the 1- so that we change the whole subtree and not just
          ;; the car (because `setf' for subtrees uses rplaca)
          (setf (subtree genome (if (or (listp (car left)) (zerop s1))
                                         s1
                                         (1- s1)))
                right)
          (setf (subtree genome (if (or (listp (car right)) (zerop s2))
                                         s2
                                         (1- s2)))
                left)))))
  obj)

(defmethod pick-mutation-type ((obj coq))
  "Randomly select a mutation that may be performed on OBJ."
  (declare (ignorable obj))
  (random-pick *coq-mutation-types*))

(defmethod stmt-range ((obj coq) (function string))
  "Return a list of the indices of the first and last ASTs of FUNCTION in OBJ.
Assumes FUNCTION is defined in a top-level AST in OBJ."
  (when-let ((top-level-pos
              (iter (for defn in (genome obj))
                    (for i upfrom 0)
                    (when-let ((defn-name (coq-definition-ast-p defn)))
                      (when (eql defn-name (intern function :sel/cp/serapi-io))
                        (collecting i))))))
    (let ((end-prev (tree-size (take (first top-level-pos) (genome obj)))))
      (list (1+ end-prev)
            (+ end-prev (tree-size (nth (first top-level-pos)
                                        (genome obj))))))))

(defun parse-coq-assumption-type (coq ast-num assum-name)
  (reset-and-load-imports coq)
  (insert-reset-point)
  ;; load object up through AST we care about
  (load-coq-object coq :include-imports nil
                   :num-asts (1+ ast-num))
  (prog1
      (check-coq-type assum-name)
    (reset-serapi-process)))

(defun parse-coq-function-local-types (coq ast-num)
  (reset-and-load-imports coq)
  (insert-reset-point)
  ;; Load object up through AST we care about, handling errors if they occur.
  (load-coq-object coq :include-imports nil :num-asts (1+ ast-num))
  (bind ((coq-ast (nth ast-num (unannotated-genome coq)))
         (fn-name (coq-definition-ast-p coq-ast))
         (local-names (mapcar
                       #'first
                       (sel/cp/serapi-io::parse-coq-function-locals coq-ast)))
         (type (split-sequence :-> (cddr (check-coq-type fn-name)))))
    (reset-serapi-process)
    (mapcar #'append
            (mapcar #'list
                    (mapcar #'string local-names)
                    (mapcar #'make-coq-var-reference local-names)
                    (repeatedly (length local-names) :COLON))
            type)))

(defun unqualify-coq-modules (env modules)
  ;; env is ((term :COLON type ...) ...)
  ;; Need to unqualify any qualified terms or types. Type may contain nested
  ;; lists.
  (labels ((unqualify-term (term module)
             (cond
               ((null term) nil)
               ((listp term)
                (cons (unqualify-term (car term) module)
                      (unqualify-term (cdr term) module)))
               ((and (stringp term)
                     (starts-with-subseq (format nil "~a." module)
                                         term))
                (subseq term (1+ (length module))))
               (t term))))
    (iter (for module in (mapcar #'string modules))
          ;; update env with unqualified module references
          (setf env (unqualify-term env module))
          (finally (return env)))))

(defun build-environment (coq modules &key num-asts whitelist-defs
                                        blacklist-defs imported-modules)
  (reset-and-load-imports coq)
  (insert-reset-point)
  (load-coq-object coq :include-imports nil
                   :num-asts (or num-asts (length (genome coq))))
  (prog1
      ;; Do nothing if no modules are specified (it's infeasible to collect
      ;; all terms in the Coq environment)
      (let* ((terms
              (when modules
                (iter (for module in modules)
                      (appending (search-coq-type "_" :module module)))))
             ;; remove module qualifications for imported modules
             (unqualified (unqualify-coq-modules terms imported-modules)))
        ;; Keep only types that are on the whitelist and not on the blacklist
        (remove-if-not
         #'identity
         (mapcar (lambda (uq-type)
                   (when (and
                          ;; In whitelist (or no whitelist present)
                          (or (not whitelist-defs)
                              (some {ends-with-subseq _ (first uq-type)
                                                      :test #'equal}
                                    whitelist-defs))
                          ;; Not in blacklist (or no blacklist present)
                          (or (not blacklist-defs)
                              (not (some {ends-with-subseq _ (first uq-type)
                                                           :test #'equal}
                                         blacklist-defs)))
                          (listp uq-type))
                     ;; For accepted types, return a list:
                     ;; (type-name coq-var-reference :COLON type)
                     (cons (car uq-type)
                           (cons (make-coq-var-reference (car uq-type))
                                 (cdr uq-type)))))
                 unqualified)))
      ;; reset after loading coq software object
      (reset-serapi-process)))

;; NOTE: this function generates a list of ASTs which may be converted to
;; strings using `(lookup-coq-string expr :input-format #!'CoqExpr)'.
(defun synthesize-typed-coq-expression (type scopes depth)
  "Synthesize an expression of type TYPE using in-scope values SCOPES.
Return a list of strings representing Coq expressions of type TYPE.
TYPE is a string denoting a Coq type (e.g., \"nat -> bool\").
SCOPES is a list of variables with tokenized types (e.g., as generated by
`search-coq-type').
DEPTH is a number limiting the search depth. Functions will be applied to no
more than DEPTH parameters."
  (bind (((:labels type-matches (ty1 ty2))
          "Return T if ty1 and ty2 are strings representing the same type.
Must account for the possibility that ty1 and ty2 might be either qualified or
unqualified type names."
          (cond
            ((and (null ty1) (null ty2)) t)
            ;; For strings, check if one is a suffix of the other
            ;; (essentially ignoring qualification)
            ((and (stringp ty1) (stringp ty2))
             (or (ends-with-subseq ty1 ty2)
                 (ends-with-subseq ty2 ty1)))
            ;; For symbols, convert to strings and check for match
            ((and (symbolp ty1) (symbolp ty2))
             (type-matches (string ty1) (string ty2)))
            ;; Two type lists of the same length: check that every element
            ;; matches.
            ((and (listp ty1) (listp ty2) (= (length ty1) (length ty2)))
             (every #'identity (mapcar #'type-matches ty1 ty2)))
            (t nil)))
         ((:flet extend-env (type scopes))
          "Return a list of curried function calls applying function TYPE to
every variable in SCOPES whose type matches that of TYPE's first parameter.
E.g., for type \"bool\", the list includes \"(implb true) : bool -> bool\" and
\"(negb true) : bool\"."
          (let ((split-type (split-sequence-if {eql :->} (cdddr type)))
                (name (first type))
                (type-ast (second type)))
            ;; Ensure that TYPE is a function (has at least 1 :->).
            (when (< 1 (length split-type))
              ;; Iterate over SCOPES, finding items with the correct type.
              (iter (for (scope-name scope-ast colon . scope-ty) in scopes)
                    (declare (ignorable colon))
                    (when (type-matches (car split-type) scope-ty)
                      (collecting
                       ;; Format as a tokenized type whose name is the curried
                       ;; function call.
                       (append (list (format nil "(~a ~a)" name scope-name)
                                     (make-coq-application type-ast scope-ast)
                                     :COLON)
                               ;; Drop the first type, join the rest with :->.
                               (cdr (mappend {cons :->}
                                             (cdr split-type))))))))))
         #+(or )                       ; NOTE: Uncomment if used below
         ((:flet synthesize-typed-coq-if-expression (bool-exprs result-exprs))
          "Return a list of if expressions testing BOOL-EXPRS and resulting in
pairs of expressions from RESULT-EXPRS."
          (iter (for test-expr in bool-exprs)
                (appending
                 (iter (for then-expr in result-exprs)
                       (appending
                        (iter (for else-expr in result-exprs)
                              (collecting
                               (list (format nil "(if ~a then ~a else ~a)"
                                             test-expr then-expr else-expr)
                                     (make-coq-if test-expr
                                                  then-expr
                                                  else-expr)))))))))
         (search-type (if (listp type) type (tokenize-coq-type type))))
    (if (zerop depth)
        (iter (for (name ast colon . scope-type) in scopes)
              (declare (ignorable colon))
              (when (type-matches search-type scope-type)
                (collecting ast into exact-names))
              (when (equal (tokenize-coq-type "bool") scope-type)
                (collecting name into bool-names)
                (collecting ast into bool-asts))
              (finally (return (append exact-names nil
                                       #+(or ) ; NOTE: Uncomment above if used.
                                       (remove-if
                                        #'null
                                        (synthesize-typed-coq-if-expression
                                         bool-names
                                         exact-names))))))
        ;; extend environment up to depth limit
        (iter (for scope-type in scopes)
              (unioning (extend-env scope-type scopes) into envs test #'equal)
              (finally (return (synthesize-typed-coq-expression
                                type
                                (union scopes envs :test (lambda (x y)
                                                           (equal (car x)
                                                                  (car y))))
                                (1- depth))))))))
