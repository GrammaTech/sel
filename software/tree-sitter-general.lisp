;;;; tree-sitter-general.lisp --- general code for tree-sitter representations.
;;; Dummy Package
(defpackage :software-evolution-library/software/tree-sitter-general
  (:nicknames :sel/software/ts-general :sel/sw/ts-general)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/tree-sitter)
  (:import-from :software-evolution-library/software/tree-sitter-code-gen))
(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)


;;; General
(defmethod pruned-rule ((ast t)) nil)
(defmethod slot-usage ((ast t)) nil)

(defgeneric language-ast-class (language-name)
  (:documentation "Get the name of the superclass common to all AST
  classes for LANGUAGE."))

(defgeneric ast-language-class (ast)
  (:documentation "Get the name of the language for AST."))

(defgeneric extra-asts (language)
  (:method (language) nil)
  (:documentation "Return a list of extra ASTs for LANGUAGE. These are the ASTs
which can occur anywhere in a tree and are put in the surrounding text or
internal slots."))

(defun extra-asts-symbols (language)
  (mapcar (op (find-symbol (fmt "~a-~a" language (symbol-name _)) :sel/sw/ts))
          (extra-asts language)))

(defgeneric parse-order (ast &key &allow-other-keys)
  (:documentation "Return a list of children intermixed with keywords
that specify which CHOICE branches are taken and how many times a REPEAT rule
repeats.")
  (:method (ast &key) nil)
  (:method ((ast structured-text) &key)
    (if-let ((rule (or (and (slot-exists-p ast 'pruned-rule)
                            (pruned-rule ast))))
             (slots (and (slot-exists-p ast 'slot-usage)
                         (slot-usage ast))))
      (children-parser ast rule slots)
      (call-next-method))))

(defun matches-rule (ast &optional pruned-rule)
  "Given a tree-sitter ast, returns true iff the pruned rule is valid.
 This is useful when synthesizing new asts, at each step you can validate
 that the rule has not been broken."
  (if-let ((rule (or pruned-rule (pruned-rule ast)))
           (slots (slot-usage ast)))
    (handler-case
        (children-parser ast rule slots)
      (rule-matching-error () nil))))

(defun operation-matches-rule-p (operation root ast1 &optional ast2)
  "Check performing operation on root and test if mutated subtree(s) contain
any rule-matching-errors. If no rule-matching-errors are found, return non-NIL."
  (let ((ast-parent-path1 (butlast (ast-path root ast1)))
        (ast-parent-path2 (when ast2 (butlast (ast-path root ast2))))
        (new-root (apply operation root ast1 (when ast2 (list ast2)))))
    (and (matches-rule (lookup new-root ast-parent-path1))
         (if ast-parent-path2
             (matches-rule (lookup new-root ast-parent-path2))
             t))))

(defun check-ast-replacement (root ast new-ast)
  "Given a root, an ast (a member of the root tree), and new-ast,
 an ast which is not a member, see if the new-ast is a valid replacement
 for ast. If so, returns non-NIL."
  ;; optimization: See if the new ast can be parsed by the previous ast's
  ;; pruned-rule. If not we assume it will error during mutation attempt.
  (if (and (slot-exists-p ast 'pruned-rule) (pruned-rule ast))
      (unless (and (slot-exists-p new-ast 'pruned-rule) (pruned-rule new-ast))
        (matches-rule new-ast (pruned-rule ast))
        (return-from check-ast-replacement nil)))
  (operation-matches-rule-p #'with root ast new-ast))

(defun check-ast-swappable (root ast1 ast2)
  "Given a root, and 2 asts (members of the root tree), see if 2 asts can
 be swapped. If so, returns returns non-NIL."
  (and (not (find ast1 ast2))
       (not (find ast2 ast1))
       (operation-matches-rule-p #'swap root ast1 ast2)))

(defun check-ast-insertable (root ast new-ast)
  "Given a root, an ast (a member of the root tree), and new-ast,
 an ast which is not a member, see if the new-ast can be inserted
 at ast. If so, returns non-NIL."
  (operation-matches-rule-p #'insert root ast new-ast))

(defun check-ast-cut (root ast)
  "Given a root, an ast (a member of the root tree), see if the selected
 ast can be cut, without a rule error. If so, returns non-NIL."
  (operation-matches-rule-p #'less root ast))

(defun change-to-subclass (ast subclasses)
  "Dynamically change the class until a subclass matches on a rule.
This is only used when a superclass instance is manually created.
Note that this won't always pick the correct subclass."
  (flet ((count-trailing-nulls (result)
           (let* ((rev (reverse result))
                  (after-nulls (drop-while #'null rev))
                  (trailing-nulls (ldiff rev after-nulls)))
             (if (equal (first after-nulls) '(:end-repeat))
                 (length trailing-nulls)
                 0))))
    (iter
     (iter:with superclass = (type-of ast))
     (for subclass in subclasses)
     (change-class ast subclass)
     (for result = (handler-case (parse-order ast)
                     (rule-matching-error ()
                       (next-iteration))))
     ;; The idea here is *almost* to take the first rule that
     ;; matches. The special case: if multiple rules match, and all
     ;; the matching rules end with a run of nils following a repeat,
     ;; then we take the one with the least number of trailing nils.
     ;; (This may prevent adding dangling commas.)
     (let ((trailing-null-count (count-trailing-nulls result)))
       (if (zerop trailing-null-count)
           (leave result)
           (finding subclass minimizing trailing-null-count
                    into best)))
     (finally
      (cond (best
             (change-class ast best)
             (return result))
            (t
             (change-class ast superclass)
             (cerror "Return the invalid AST"
                     'rule-matching-error
                     :rule-matching-error-rule (pruned-rule ast)
                     :rule-matching-error-ast ast)
             ast))))))

(defgeneric output-transformation
    (ast &rest rest &key &allow-other-keys)
  (:documentation "Return a list of strings and AST objects that
are ordered for reproduction as source text.")
  (:method ((ast structured-text) &rest rest &key &allow-other-keys)
    (declare (ignorable rest))
    (computed-text-output-transformation ast))
  (:method ((ast text-fragment) &rest rest &key &allow-other-keys)
    (declare (ignorable rest))
    (list (before-text ast) (text ast) (after-text ast)))
  (:method ((ast variation-point) &rest rest &key &allow-other-keys)
    (declare (ignorable rest))
    `(,(before-text ast) ,@(children ast) ,(after-text ast)))
  (:method :around ((ast structured-text)
                    &rest rest &key finalized-type &allow-other-keys)
    (declare (ignorable rest))
    (labels ((append-before-and-after-asts (output-transformation)
               "Append before and after ASTs to OUTPUT-TRANSFORMATION."
               (mappend
                (lambda (output)
                  (cond
                    ((typep output 'structured-text)
                     ;; TODO Currently before-text and after-text can contain
                     ;; alternative ASTs.
                     (append (and (typep (before-text output) 'alternative-ast)
                                  (list (before-text output)))
                             (before-asts output)
                             (list output)
                             (after-asts output)
                             (and (typep (after-text output) 'alternative-ast)
                                  (list (after-text output)))))
                    (t (list output))))
                output-transformation)))
      (cond-let subclasses
        ((and (not finalized-type)
              (slot-exists-p ast 'choice-subclasses)
              (choice-subclasses ast))
         ;; This allows instances of ASTs to be created from their superclass.
         ;; A subclass with a matching rule is assigned here.
         (change-to-subclass ast subclasses)
         (output-transformation ast :finalized-type t))
        ((typep ast 'computed-text)
         (append-before-and-after-asts
          (computed-text-output-transformation ast)))
        (t
         (block nil
           ;; Use handler-bind so we don't unwind if we can't handle.
           (handler-bind
               ((rule-matching-error
                 (lambda (rule-error)
                   (declare (ignore rule-error))
                   (if (and (not finalized-type)
                            (slot-exists-p ast 'choice-superclass))
                       ;; Try to find a relevant subclass if the current one does
                       ;; not match.
                       (return
                         (output-transformation
                          (change-class ast (slot-value ast 'choice-superclass))))))))
             (append-before-and-after-asts (call-next-method)))))))))

(defmethod predecessor ((root structured-text) (node structured-text))
  (when-let (parent (parent root node))
    (let ((predecessor
           (nest (second)
                 (member node)
                 (reverse)
                 (remove-if (conjoin #'stringp #'emptyp))
                 (output-transformation parent))))
      (if (stringp predecessor)
          (make-keyword predecessor)
          predecessor))))

(defmethod successor ((root structured-text) (node structured-text))
  (when-let (parent (parent root node))
    (let ((successor
           (nest (second)
                 (member node)
                 (remove-if (conjoin #'stringp #'emptyp))
                 (output-transformation parent))))
      (if (stringp successor)
          (make-keyword successor)
          successor))))

(defgeneric root-rule-ast-p (ast)
  (:documentation "Return T if AST represents the root rule or entry point
of a grammar.")
  (:method (ast) nil))

(defgeneric get-choice-expansion-subclass (class spec)
  (:documentation "Get the subclass of CLASS associated with SPEC.")
  (:method (class spec)
    (declare (ignorable spec))
    class))

(defun label-as (field-name &rest types)
  "Return a function that wraps parse trees of TYPE as fields named
FIELD-NAME.

If there are no types, always wrap."
  (check-type field-name keyword)
  (assert (every #'keywordp types))
  (lambda (tree)
    (let ((type (parse-tree-type tree)))
      (when (or (null types) (member type types))
        (cons (list field-name type)
              (cdr tree))))))

(defun wrap-with/child (symbol &rest types)
  (check-type symbol keyword)
  (assert (every #'keywordp types))
  (lambda (tree)
    (let ((type (parse-tree-type tree)))
      (when (or (null types) (member type types))
        `(,symbol ,(cadr tree) (,tree))))))

(defun wrap-with/in-slot (symbol &rest types)
  (check-type symbol keyword)
  (assert (every #'keywordp types))
  (lambda (tree)
    (match (parse-tree-type tree)
      ((list slot type)
       (when (or (null types) (member type types))
         `((,slot ,symbol) ,(cadr tree) ((,type ,@(cdr tree)))))))))

(defun wrap-with (symbol &rest types)
  (check-type symbol keyword)
  (assert (every #'keywordp types))
  (lambda (tree)
    (let ((type (parse-tree-type tree)))
      (if (listp type)
          (funcall
           (apply #'wrap-with/in-slot symbol types)
           tree)
          (funcall
           (apply #'wrap-with/child symbol types)
           tree)))))


(defun ignore-types (&rest types)
  (assert types)
  (lambda (tree)
    (and (member (parse-tree-type tree) types)
         tree)))

(defun rename-type-to (to from)
  (lambda (tree)
    (and (eql (parse-tree-type tree) from)
         (copy-parse-tree tree :type to))))

(defun rename-type-to/in-slot (to from)
  "Similar to RENAME-TYPE-TO but only transforms FROM to TO if it is in a slot."
  (lambda (tree &aux (slot-info (parse-tree-type tree)))
    (when (and (listp slot-info)
               (eql (cadr slot-info) from))
      (copy-parse-tree tree :type (list (car slot-info) to)))))

(-> modify-parse-tree (list &rest function) (values list &optional))
(defun modify-parse-tree (parse-tree &rest fns)
  "For each child of PARSE-TREE, call each function in FNS with the child tree.

The first function to return non-nil is treated as the new child;
otherwise the original child is used."
  (copy-parse-tree
   parse-tree
   :children
   (mapcar
    (lambda (child-tree)
      (cond ((consp (parse-tree-type child-tree))
             child-tree)
            ((some (op (funcall _ child-tree)) fns))
            (t child-tree)))
    (parse-tree-children parse-tree))))

(-> modify-parse-tree/slots (list &rest function) (values list &optional))
(defun modify-parse-tree/slots (parse-tree &rest fns)
  "For each slot of PARSE-TREE, call each function in FNS with the child tree.

The first function to return non-nil is treated as the new child;
otherwise the original child is used."
  (copy-parse-tree
   parse-tree
   :children
   (mapcar
    (lambda (child-tree)
      (cond ((not (consp (parse-tree-type child-tree)))
             child-tree)
            ((some (op (funcall _ child-tree)) fns))
            (t child-tree)))
    (parse-tree-children parse-tree))))

(-> modify-parse-tree/full (list &rest function) (values list &optional))
(defun modify-parse-tree/full (parse-tree &rest fns)
  "For each child and slot of PARSE-TREE, call each function in FNS with the
child tree.

The first function to return non-nil is treated as the new child;
otherwise the original child is used."
  (copy-parse-tree
   parse-tree
   :children
   (mapcar
    (lambda (child-tree)
      (or (some (op (funcall _ child-tree)) fns)
          child-tree))
    (parse-tree-children parse-tree))))

(deftype modify-parse-tree-style ()
  "A style to modify parse trees.
- :children indicates the children in the children slot of the parse tree
            are modified.
- :slots indicates the children in slots of the parse tree are modified.
- :all indicates that every child in the parse tree is modified."
  '(member :children :slots :all))

(defmacro with-modify-parse-tree ((parse-tree
                                   &key (modification-style :children))
                                  &body body)
  "Like `modify-parse-tree', but with a case-like syntax.
Each clause consists of a key (or list of keys) and a function call.
The keys are simply appended to the function call.

If the key is `t', the call is left unchanged."
  `(,(ecase modification-style
       (:children 'modify-parse-tree)
       (:slots 'modify-parse-tree/slots)
       (:all 'modify-parse-tree/full))
    ,parse-tree
    ,@(iter (for (key/s call) in body)
            (collect (if (eql key/s t) call
                         (append call (ensure-list key/s)))))))

(defgeneric transform-parse-tree
    (language class parse-tree &rest rest &key &allow-other-keys)
  (:documentation "Transform PARSE-TREE based on LANGUAGE and CLASS.")
  (:method (language class parse-tree &rest rest &key &allow-other-keys
            &aux (descriptor (and (listp parse-tree)
                                  (car parse-tree))))
    (cond
      (class parse-tree)
      ((and descriptor (not (length= 3 parse-tree)))
       parse-tree)
      ;; :class
      ((keywordp descriptor)
       (apply
        {transform-parse-tree
         language
         (convert-to-lisp-type language descriptor)
         parse-tree}
        rest))
      ;; :slot, :class list
      ((and (consp descriptor)
            (keywordp (cadr descriptor)))
       (apply
        {transform-parse-tree
         language
         (convert-to-lisp-type language (cadr descriptor))
         parse-tree}
        rest))
      (t parse-tree)))
  (:method :around (language class parse-tree &rest rest &key)
    ;; Create source-text-fragment-variation-points where they're needed.
    (declare (ignore rest))
    (if class
        (transform-malformed-parse-tree (call-next-method) :recursive nil)
        (call-next-method))))

(defgeneric generated-transform-parse-tree (language class parse-tree)
  (:documentation "Transform PARSE-TREE based on LANGUAGE with SPEC.
This is generated while creating tree-sitter code while transform-parse-tree
is hand-written.")
  (:method (language class parse-tree
            &aux (descriptor (and (listp parse-tree)
                                  (car parse-tree))))
    (cond
      (class parse-tree)
      ((and descriptor (not (length= 3 parse-tree)))
       parse-tree)
      ;; :class
      ((keywordp descriptor)
       (generated-transform-parse-tree
        language
        (convert-to-lisp-type language descriptor)
        parse-tree))
      ;; :slot, :class list
      ((and (consp descriptor)
            (keywordp (cadr descriptor)))
       (generated-transform-parse-tree
        language
        (convert-to-lisp-type language (cadr descriptor))
        parse-tree))
      (t parse-tree))))

(defmethod children :around ((ast structured-text))
  (symbol-macrolet ((ordered-children (slot-value ast 'ordered-children)))
    (if (slot-boundp ast 'ordered-children)
        ordered-children
        (setf ordered-children (call-next-method)))))

(defmethod children ((ast structured-text))
  (remove-if
   (of-type 'inner-whitespace)
   (remove-if-not (of-type 'ast) (output-transformation ast))))

;;; TODO This handles conflict ASTs in {before-,after-,}text slots.
(defmethod child-slots :around ((ast structured-text))
  "When there are ASTs in before-text, after-text, or text, expose them as
children. (This can happen when they store alternative ASTs)."
  (macrolet ((wrap-slot (slot)
               `(when (typep (slot-value ast ',slot) 'alternative-ast)
                  '((,slot . 1)))))
    (append
     (wrap-slot before-text)
     (wrap-slot text)
     (call-next-method)
     (wrap-slot after-text))))

;;; TODO This also handles conflict ASTs in {before-,after-,}text slots.
(defmethod child-slot-specifiers :around ((ast structured-text))
  "If there are ASTs in before-text, after-text, or text, expose them as
children. (This can happen when they store alternative ASTs)."
  (macrolet ((wrap-slot (slot)
               `(when (typep (slot-value ast ',slot) 'alternative-ast)
                  (load-time-value
                   (list
                    (make 'ft::slot-specifier
                          :class t
                          :slot ',slot
                          :arity 1))))))
    (append (wrap-slot before-text)
            (wrap-slot text)
            (call-next-method)
            (wrap-slot after-text))))


;;; tree-sitter parsing
(defvar *use-variation-point-tree* nil
  "Controls whether variation points for errors and source text fragments use
the tree representation as opposed to the string representation.")

(defun position-after-leading-newline (str &aux (pos 0))
  "Returns 1+ the position of the first newline in STR,
assuming it can be reached only by skipping over whitespace
or comments.  NIL if no such newline exists."
  (loop
    (when (length>= pos str) (return nil))
    (let ((c (elt str pos)))
      (case c
        (#\Newline (return (1+ pos)))
        ((#\Space #\Tab)
         (incf pos))
        (t (return nil))))))

(defun get-language-from-superclass (superclass)
  "Get the tree-sitter language associated with SUPERCLASS."
  (or (gethash superclass *superclass->language*)
      (error "No tree-sitter language known for ~a." superclass)))

(defmethod convert :around ((to-type (eql 'tree-sitter-ast)) (string string)
                            &key deepest &allow-other-keys)
  (if deepest
      (let* ((ast (call-next-method))
             (ranges (ast-source-ranges ast))
             (runs (runs ranges :test #'equal? :key #'cdr :count 1)))
        (car (lastcar (first runs))))
      (call-next-method)))


(defmethod get-parent-full-stmt (obj (ast tree-sitter-ast))
  (if (typep ast 'statement-ast)
      ast
      (get-parent-full-stmt obj (get-parent-ast obj ast))))

(defgeneric ast-type-to-rebind-p (ast)
  (:documentation "Return T if AST is of a type where its variables/functions
should be rebound.")
  (:method (ast) nil)
  (:method ((ast identifier-ast)) t))

(defgeneric scope-ast-p (ast)
  (:method ((ast t)) nil)
  (:method ((ast root-ast)) t)
  (:method ((ast function-ast)) t)
  (:method ((ast loop-ast)) t)
  (:method ((ast compound-ast)) t))

(defgeneric ltr-eval-ast-p (ast)
  (:documentation "Does AST evaluate its subexpressions left-to-right?")
  (:method ((ast t)) nil)
  (:method ((ast ltr-eval-ast)) t))

(defmethod enclosing-scope ((obj tree-sitter) (ast ast))
  "Return the enclosing scope of AST in OBJ.
- OBJ tree-sitter software object
- AST ast to return the enclosing scope for"
  (or (find-if #'scope-ast-p
               (get-parent-asts* obj ast))
      (genome obj)))

(defun ensure-children (value)
  (etypecase value
    (ast (values (direct-children value)
                 value))
    (list value)))

(defgeneric statements-in-scope (software scope ast)
  (:documentation "Return all child statements of SCOPE prior to AST.")
  (:method (obj (scope ast) (ast ast))
    (iter (for c in (remove nil (children scope)))
          (while (path-later-p obj ast c))
          (collect c))))

(defgeneric identifiers (ast)
  (:documentation "Return all identifier nodes in AST and its children.")
  (:method ((ast ast))
    (collect-if {typep _ 'identifier-ast} ast)))

(defgeneric call-arguments (ast)
  (:documentation "Return the arguments of AST.")
  (:method-combination standard/context)
  (:method :context ((ast t))
    (ensure-children (call-next-method)))
  (:method ((ast t))
    (call-arguments-ast ast)))

(defgeneric call-arguments-ast (ast)
  (:documentation "Return the arguments of AST."))

(defgeneric function-parameters (ast)
  (:documentation "Return the parameters of AST.")
  (:method-combination standard/context)
  (:method :context ((ast ast))
    (ensure-children (call-next-method))))

(defgeneric return-type (ast)
  (:documentation "Get the return type of AST."))

(defgeneric parameter-type (parameter-ast)
  (:documentation "Return a representation of the TYPE of PARAMETER-AST."))

(defgeneric parameter-name (parameter-ast)
  (:documentation "Return the name of PARAMETER-AST.")
  (:method ((ast ast))
    (if-let (names (parameter-names ast))
      (source-text (only-elt names))
      nil)))

(defgeneric parameter-names (parameter-ast)
  (:documentation "Return the names of PARAMETER-AST.

A parameter AST may introduce multiple names in languages with
argument destructuring (e.g. ECMAScript).")
  (:method ((ast t))
    (identifiers ast)))

(defgeneric function-body (ast)
  (:documentation "Return the body of AST."))

(defgeneric call-function (call-ast)
  (:documentation "Return the function of CALL-AST."))

(defgeneric call-name (call-ast)
  (:documentation "Return the name of CALL-AST.")
  (:method ((call-ast call-ast)) (source-text (call-function call-ast))))

(defgeneric variable-name (variable-ast)
  (:documentation "Return the name of VARIABLE-AST."))

(defvar no-return-function-names '("exit" "abort"))

(defgeneric no-fallthrough (ast)
  (:documentation "Return t if AST will never fall through.")
  (:method ((otherwise t)) nil)
  (:method ((ast call-ast))
    (member (call-name ast) no-return-function-names :test #'string=))
  (:method ((ast jump-ast)) t)
  (:method ((ast compound-ast))
    (no-fallthrough (lastcar (children ast)))))

(defgeneric inner-declarations (ast)
  (:documentation "Return a list of variable declarations affecting inner scopes.")
  (:method-combination standard/context)
  (:method ((ast ast)) nil)
  (:method ((ast function-ast))
    (mappend #'parameter-names (function-parameters ast)))
  (:method :context (ast)
    (handler-bind ((no-applicable-method-error
                    (lambda (condition)
                      (declare (ignorable condition))
                      (when (find-if
                             (of-type '(or source-text-fragment-variation-point
                                        error-variation-point))
                             ast)
                        (return-from inner-declarations)))))
      (call-next-method))))

(defgeneric outer-declarations (ast)
  (:documentation
   "Return a list of variable declarations affecting outer scopes. This can
return more information as values depending on the language.

For languages that have multiple namespaces (variables, functions,
types, etc.) this should return the namespaces as a second value.")
  (:method-combination standard/context)
  (:method ((ast ast)) nil)
  (:method :context (ast)
    ;; NOTE: this is to work around ASTs which may not have the expected ASTs
    ;;       in respective slots.
    (handler-bind ((no-applicable-method-error
                    (lambda (condition)
                      (declare (ignorable condition))
                      (when (find-if
                             (of-type '(or source-text-fragment-variation-point
                                        error-variation-point))
                             ast)
                        (return-from outer-declarations)))))
      (call-next-method))))

(defgeneric lhs (ast)
  (:documentation "Return the left-hand side of an AST."))

(defgeneric assignee (ast)
  (:documentation "Return the (possibly structured) binding that AST assigns to.")
  (:method ((ast ast)) nil))

(defgeneric rhs (ast)
  (:documentation "Return the right-hand side of an AST."))

(defgeneric operator (ast)
  (:method-combination standard/context)
  (:method :context ((ast t))
    (make-keyword (trim-whitespace (source-text (call-next-method)))))
  (:documentation "Return the operator from an AST as a keyword."))

(defgeneric control-flow-condition (control-flow-ast)
  (:documentation "Return the condition from a CONTROL-FLOW-AST."))

(defclass normal-scope () ()
  (:documentation "Tree-sitter mixin for languages with \"normal\" scoping."))

(defgeneric get-parent-decl (obj identifier)
  (:documentation "For the given IDENTIFIER AST, return the parent declaration.")
  (:method (obj identifier)
    (find-if (of-type 'declaration-ast)
             (get-parent-asts obj identifier))))

(defgeneric ast-to-scope-alist (obj scope ast)
  (:documentation "Return a scope alist based on AST with SCOPE.
The alist should contain at least the following:
 - :name :: a string which contains the name of the identifier.
 - :decl :: an AST which represents the declaration.
 - :scope :: an AST which represents the scope of the identifier.")
  (:method (obj scope ast)
    `((:name . ,(source-text ast))
      (:decl . ,(or (get-parent-decl obj ast) ast))
      (:scope . ,scope))))

(defmethod scopes ((obj normal-scope) (ast ast))
  (unless (null (ast-path obj ast))
    (let ((scope (enclosing-scope obj ast)))
      (cons (reverse
             (mapcar {ast-to-scope-alist obj scope}
                     (append (inner-declarations scope)
                             (mappend #'outer-declarations
                                      (statements-in-scope obj scope ast)))))
            (scopes obj scope)))))

(defgeneric find-enclosing (test obj ast)
  (:documentation "Return the nearest enclosing AST passing TEST in OBJ.
If TEST is a function, it is used as a predicate. Otherwise it is assumed to be a type.")
  (:method ((type t) (obj t) (ast ast))
    (find-enclosing (of-type type) obj ast))
  (:method ((pred function) (obj parseable) (ast ast))
    (find-enclosing pred (genome obj) ast))
  (:method ((pred function) (root ast) (ast ast))
    (find-if pred (get-parent-asts root ast))))

(defgeneric find-all-enclosing (test obj ast)
  (:documentation "Return the enclosing ASTs passing TEST in OBJ.
If TEST is a function, it is used as a predicate. Otherwise it is assumed to be a type.")
  (:method ((type t) (obj t) (ast ast))
    (find-all-enclosing (of-type type) obj ast))
  (:method ((pred function) (obj parseable) (ast ast))
    (find-all-enclosing pred (genome obj) ast))
  (:method ((pred function) (root ast) (ast ast))
    (filter pred (get-parent-asts* root ast))))

(defgeneric find-outermost (test obj ast)
  (:documentation "Return the outermost enclosing AST passing TEST in OBJ.
If TEST is a function, it is used as a predicate. Otherwise it is assumed to be a type.")
  (:method ((type t) (obj t) (ast ast))
    (find-outermost (of-type type) obj ast))
  (:method ((pred function) (obj parseable) (ast ast))
    (find-outermost pred (genome obj) ast))
  (:method ((pred function) (root ast) (ast ast))
    (find-if pred (get-parent-asts root ast) :from-end t)))

(defgeneric find-preceding (test obj ast)
  (:documentation "Return any siblings passing TEST preceding AST in OBJ.
If TEST is a function, it is used as a predicate. Otherwise it is assumed to be a type.")
  (:method ((type t) (root t) (ast tree-sitter-ast))
    ;; (assert (typep type '(or symbol (cons symbol t) class)))
    (find-preceding (of-type type) root ast))
  (:method ((pred function) (obj parseable) (ast ast))
    (find-preceding pred (genome obj) ast))
  (:method ((pred function) (root ast) (ast ast))
    ;; (assert (typep type '(or symbol (cons symbol t) class)))
    (when-let ((parent (get-parent-ast root ast)))
      (iter (for child in (children parent))
            (until (eql child ast))
            (when (funcall pred child)
              (collect child)))))
  (:method ((pred function) (root ast) (ast conflict-ast))
    (when-let (default (first (conflict-ast-default-children ast)))
      (find-preceding pred root default))))

(defgeneric find-previous-sibling (test obj ast)
  (:documentation "Return nearest sibling passing TEST preceding AST in OBJ.
If TEST is a function, it is used as a predicate. Otherwise it is assumed to be a type.")
  (:method ((type t) (root t) (ast tree-sitter-ast))
    (find-previous-sibling (of-type type) root ast))
  (:method ((pred function) (obj parseable) (ast ast))
    (find-previous-sibling pred (genome obj) ast))
  (:method ((pred function) (root ast) (ast ast))
    (when-let ((parent (get-parent-ast root ast)))
      (let (previous)
        (iter (for child in (children parent))
              (until (eql child ast))
              (when (funcall pred child)
                (setf previous child)))
        previous)))
  (:method ((pred function) (root ast) (ast conflict-ast))
    (when-let (default (first (conflict-ast-default-children ast)))
      (find-previous-sibling pred root default))))

(defgeneric find-following (test obj ast)
  (:documentation "Return any siblings passing TEST following AST in OBJ.
If TEST is a function, it is used as a predicate. Otherwise it is assumed to be a type.")
  (:method ((type t) (obj t) (ast ast))
    ;; (assert (typep type '(or symbol (cons symbol t) class)))
    (find-following (of-type type) obj ast))
  (:method ((pred function) (obj parseable) (ast ast))
    (find-following pred (genome obj) ast))
  (:method ((pred function) (root ast) (ast ast))
    ;; (assert (typep type '(or symbol (cons symbol t) class)))
    (when-let ((parent (get-parent-ast root ast)))
      (nest
       (filter pred)
       (rest)
       (drop-until (eqls ast))
       (children parent)))))

(defgeneric find-next-sibling (test obj ast)
  (:documentation "Return next sibling passing TEST following AST in OBJ.
If TEST is a function, it is used as a predicate. Otherwise it is assumed to be a type.")
  (:method ((type t) (obj t) (ast ast))
    (find-next-sibling (of-type type) obj ast))
  (:method ((pred function) (obj parseable) (ast ast))
    (find-next-sibling pred (genome obj) ast))
  (:method ((pred function) (root ast) (ast ast))
    (when-let (parent (get-parent-ast root ast))
      (find-if pred (rest (member ast (children parent)))))))

(defun sort-descendants (root asts &key (key #'identity))
  "Sort ASTs according to their position in ROOT."
  (let ((sorted
          (stable-sort-new
           asts
           (op (path-later-p root
                             (ast-path root _2)
                             (ast-path root _1)))
           :key key)))
    (coerce sorted 'list)))

(defgeneric comments-for (obj ast)
  (:documentation "Return the comments for AST in OBJ.")
  (:method ((software tree-sitter) (ast tree-sitter-ast))
    (comments-for (genome software) ast))
  (:method ((root ast) (ast tree-sitter-ast))
    (or (find-preceding 'comment-ast root ast)
        ;; In this case walk up to the enclosing function, if there is
        ;; one.
        (when-let (fn (find-enclosing 'function-ast root ast))
          (find-preceding 'comment-ast root fn)))))

(defgeneric definition-name (ast)
  (:documentation "Return a string that is the name of the things defined by a
definition. Return NIL if AST is not a definition.  If more than one name is
defined, return the first one.")
  (:method ((ast ast))
    (let ((name (definition-name-ast ast)))
      (etypecase name
        (null nil)
        (cons (source-text (car name)))
        (t (source-text name))))))

(defgeneric definition-name-ast (ast)
  (:documentation "Return an AST that is the name of the things
defined by a definition.  Return NIL if AST is not a definition.")
  (:method ((ast t)) nil))

(defgeneric declarator-name (ast)
  (:documentation "Returns a string that is the name of a things
declared in a declarator, or in the first element of a list of declarators.
Return NIL on the empty list.")
  (:method ((ast ast))
    (when-let (name (declarator-name-ast ast))
      (source-text name))))

(defgeneric declarator-name-ast (ast)
  (:documentation "Returns an AST that is the name of a things
declared in a declarator, or in the first element of a list of declarators.
Return NIL on the empty list.")
  (:method ((ast null)) nil)
  (:method ((ast ast)) nil)
  (:method ((ast identifier-ast)) ast)
  (:method ((ast list))
    (declarator-name-ast (car ast))))

(defgeneric enclosing-definition (sw ast)
  (:documentation "Find the enclosing definition AST in which AST resides,
or NIL if none."))

(defgeneric ast-imports (ast)
  (:documentation "Return a list of imports provided by AST.
Every element in the list has the following form:
(full-name alias/nickname named-imports)")
  (:method ((ast t)) nil))

(defun imports (software node)
  "Return a list of the imports available in SOFTWARE at AST.
Every element in the list has the following form:
    (full-name alias/nickname named-imports)"
  (with-attr-table software
    (imports-attr node)))

(def-attr-fun imports-attr (in)
  "Compute the imports available from a node."
  (:method ((software parseable) &optional in)
    (imports-attr (genome software) in))
  (:method ((node root-ast) &optional in)
    (propagate-imports node in))
  (:method ((node functional-tree-ast) &optional in)
    (cond
      ((scope-ast-p node)
       (propagate-imports node in)
       in)
      (t (mapc (op (imports-attr _ (append in (ast-imports node))))
               (children node))
         in))))

(defun propagate-imports (node in)
  (reduce (lambda (in2 child)
            (append
             (imports-attr child in2)
             (ast-imports child)))
          (children node)
          :initial-value in))

(defmethod attr-missing ((name (eql 'imports-attr)) node)
  (imports-attr (attrs-root *attrs*) nil))

(defun provided-by (software ast)
  "Return the library, package, or system in SOFTWARE providing AST."
  (with-attr-table software
    (provided-by-attr ast)))

(def-attr-fun provided-by-attr ())

(defgeneric comparisonp (ast)
  (:documentation "Is AST a comparison?")
  (:method ((ast t)) nil))

(defgeneric evaluation-order-children (ast)
  (:documentation
   "Return the children of AST in the order they are evaluated.")
  (:method (ast) (children ast)))

(defmethod get-unbound-vals ((obj normal-scope) (ast ast)
                             &key local-declarations)
  (labels ((function-call-identifier-p
               (identifier &aux (parent (parent (genome obj) identifier)))
             "Return T if identifier is the function identifier in a
              function call."
             (and (typep parent 'call-ast)
                  (eq identifier (call-function parent))))
           (unbound-val-p (identifier)
             "Return T if IDENTIFIER is unbound in LOCAL-SCOPES and
              isn't the function identifier in a function call."
             (and
              (not (or (member (source-text identifier) local-declarations
                               :key #'source-text
                               :test #'equal)
                       (function-call-identifier-p identifier)))
              (variable-use-p obj identifier)))
           (remove-declaration-identifiers
               (unbound-identifiers local-declarations)
             "Return UNBOUND-IDENTIFIERS with all ASTs present in
              LOCAL-DECLARATIONS removed from it."
             (remove-if
              (lambda (identifier)
                ;; unbound-identifiers and local-declarations are lists ASTs of
                ;; type identifier-ast. Any unbound identifier which is used
                ;; in a declaration should not be considered unbound.
                (member identifier local-declarations :test #'eq))
              unbound-identifiers))
           (get-unbound-children ()
             "Return all unbound vals in the children of AST."
             (iter
               (for locals first
                    (append local-declarations (inner-declarations ast))
                    then (append locals (outer-declarations child)))
               ;; NOTE: use evaluation order here for cases where
               ;;       variable shadowing may become an issue.
               (for child in (evaluation-order-children ast))
               (appending
                   (get-unbound-vals obj child :local-declarations locals)
                 into unbound-children)
               (finally
                (return (remove-declaration-identifiers
                         unbound-children locals))))))
    (if (and (typep ast 'identifier-ast) (unbound-val-p ast))
        (list ast)
        (get-unbound-children))))

(defgeneric variable-use-p (software ast &key &allow-other-keys)
  (:documentation "Return T if IDENTIFIER occurs in SOFTWARE as a variable."))

(define-condition unresolved-overloads-error (error)
  ((ast :initarg :ast :type ast
        :reader unresolved-overloads-error.ast)
   (overloads :initarg :overloads :type list
              :reader unresolved-overloads-error.overloads))
  (:documentation "Error when overloads cannot be resolved.")
  (:report (lambda (c s)
             (with-slots (ast overloads) c
               (format s "Multiple overloads for ~a:~%~{~a~%~^~}"
                       ast overloads)))))

(-> unresolved-overloads (ast list)
  (values null &optional))
(defun unresolved-overloads (ast overloads)
  (cerror "Return nothing"
          'unresolved-overloads-error
         :ast ast
         :overloads overloads)
  nil)

(defgeneric resolve-overloads (type ast &optional overloads)
  (:method-combination standard/context)
  (:documentation "Resolve the overloads in OVERLOADS.

This function should only be called when there are two or more
overloads to resolve.")
  (:method :context ((type t) (ast t) &optional overloads)
    (let ((overloads (nub overloads :test #'equal?)))
      (if (not (rest overloads))
          (first overloads)
          (let ((overloads (remove ast overloads)))
            (if (not (rest overloads))
                (first overloads)
                (or (find-if (op (ancestor-of-p (attrs-root*) ast _))
                             overloads)
                    (call-next-method)))))))
  (:method :before ((type t) (ast t) &optional overloads)
    (assert (and (listp overloads) overloads)))
  (:method (type (ast call-ast) &optional overloads)
    (resolve-overloads type (call-function ast) overloads))
  (:method ((type t) ast &optional overloads)
    (unresolved-overloads ast overloads)))

(defun get-declaration-ast (type ast)
  "Like `get-declaration-asts', but selects the
correct overload for AST if there is more than one."
  (let ((decls (remove-duplicates (get-declaration-asts type ast))))
    (match decls
      (() decls)
      ((list decl) decl)
      (otherwise (resolve-overloads type ast decls)))))

(defun get-declaration-id (type ast)
  "Like `get-declaration-ids', but selects the correct overload if
there is more than one."
  (let ((ids (get-declaration-ids type ast)))
    (match ids
      (() ids)
      ((list id) id)
      (otherwise
       (when-let* ((decls (get-declaration-asts type ast))
                   (decl (resolve-overloads type ast decls)))
         (nth (position decl decls) ids))))))

(-> get-declaration-asts (symbol ast)
    (values (soft-list-of ast) &optional))
(defun get-declaration-asts (type ast)
  "For an identifier, get a list of declaration ASTs.
\(There may be more than one if the language allows overloading.) For
a declaration AST, return AST unchanged."
  (flet ((collect-decls (ast)
           (let ((root (attrs-root*)))
             (iter (for id in (get-declaration-ids type ast))
                   (restart-case
                       (collect (find-enclosing-declaration type root id))
                     (continue ()
                       :report "Ignore this ID"))))))
    (ematch* (type ast)
      ((_ (call-ast))
       (get-declaration-asts type (call-function ast)))
      (((type keyword) (type degenerate-declaration-ast))
       (collect-decls ast))
      (((type keyword) (not (type declaration-ast)))
       (collect-decls ast))
      ((:variable (variable-declaration-ast))
       (adjoin ast (collect-decls ast)))
      ((:function (function-declaration-ast))
       (adjoin ast (collect-decls ast)))
      ((:type (type-declaration-ast))
       (adjoin ast (collect-decls ast)))
      ((:tag (type-declaration-ast))
       (adjoin ast (collect-decls ast)))
      ((:macro (macro-declaration-ast))
       (adjoin ast (collect-decls ast)))
      ((:namespace (namespace-declaration-ast))
       (adjoin ast (collect-decls ast)))
      ((:method (function-declaration-ast))
       (adjoin ast (collect-decls ast)))
      (((type keyword) (declaration-ast))
       (list ast))
      ((nil _)
       (error "Not a namespace: ~a" type))
      (((type symbol) _)
       (get-declaration-asts
        (assure keyword (decl-type-namespace type)) ast))
      ((_ _) nil))))

(defgeneric get-declaration-ids (type ast)
  (:documentation "Using the symbol table, find where AST is defined.")
  (:method-combination standard/context)
  (:method ((type (eql :variable)) ast)
    (find-in-symbol-table ast type ast))
  (:method ((type (eql :function)) ast)
    (find-in-symbol-table ast type ast))
  (:method ((type (eql :type)) ast)
    (find-in-symbol-table ast type ast))
  (:method ((type (eql :tag)) ast)
    (find-in-symbol-table ast type ast))
  (:method ((type (eql :macro)) ast)
    (find-in-symbol-table ast type ast))
  (:method ((type (eql :method)) ast)
    (find-in-symbol-table ast type ast))
  (:method ((type (eql :namespace)) ast)
    (find-in-symbol-table ast type ast))
  (:method :around ((type t) (ast call-ast))
    (get-declaration-ids type (call-function ast)))
  (:method ((type symbol) ast)
    (get-declaration-ids (assure keyword (decl-type-namespace type)) ast))
  (:method :around ((type t) (identifier identifier-ast)
                    &aux (decl-type (when (keywordp type)
                                      (namespace-decl-type type))))
    (or
     ;; Check if this identifier is part of a declaration before
     ;; checking scopes to avoid returning a shadowed variable.
     (ensure-list
      (iter
       (for parent in (filter (of-type decl-type)
                              (get-parent-asts* (attrs-root*) identifier)))
       (thereis (and (typep parent decl-type)
                     (not (typep parent 'degenerate-declaration-ast))
                     (find-in-defs parent type
                                   (qualify-declared-ast-name identifier))))))
     (call-next-method))))

(def-attr-fun relevant-declaration-type ()
  "Return the type of declaration we should look for.

That is, based on AST's context, figure out what kinds of declaration
AST we should be looking for \(such as a `variable-declaration-ast',
`function-declaration-ast', or `type-declaration-ast').

If the value is `nil' it is because the AST is not one for which a
declaration makes sense."
  ;; TODO Not every language has a separate class for type
  ;; identifiers. E.g. Python just has Python identifiers inside
  ;; Python types.
  (:method ((ast ast))
    nil)
  (:method ((ast identifier-ast))
    'variable-declaration-ast)
  (:method ((ast type-identifier-ast))
    'type-declaration-ast)
  (:method ((ast type-ast))
    'type-declaration-ast)
  (:method ((ast declaration-ast))
    (or (iter (for (ns . decl-type) in (namespace-decl-type-table))
              (thereis (and (typep ast decl-type)
                            ns
                            decl-type)))
        (error "Unclassified declaration: ~a" ast)))
  (:method :around ((ast ast) &aux (obj (attrs-root*)))
    (or
     ;; The relevant declaration type for a call function is a
     ;; function.
     (and-let* ((call (find-enclosing 'call-ast obj ast))
                ((eql ast (call-function call))))
       'function-declaration-ast)
     ;; The relevant declaration for a function name is a function.
     (and-let* ((fn (find-enclosing 'function-declaration-ast obj ast))
                ((eql ast (definition-name-ast fn))))
       'function-declaration-ast)
     (call-next-method))))

(def-attr-fun get-initialization-ast ()
  "Find where AST is initialized.

This is useful when languages allow a single declaration to initialize
more than one variable, and for languages that allow declaration and
initialization to be separate."
  (:method ((ast t) &aux (obj (attrs-root*)))
    (when-let (id (get-declaration-id :variable ast))
      (find-enclosing 'variable-initialization-ast
                      obj
                      id))))

(def-attr-fun variable-declaration-ids ()
  "Collect the variable declarations IDs for variables used in AST."
  (:method (ast)
    (iter (for id in (identifiers ast))
          (when (subtypep (relevant-declaration-type id)
                          'variable-declaration-ast)
            (let ((decls (get-declaration-asts :variable id)))
              (dolist (decl decls)
                (when (typep decl 'variable-declaration-ast)
                  (dolist (id (get-declaration-ids :variable id))
                    (set-collect (assure ast id)
                                 into ids))))))
          (finally (return (convert 'list ids))))))

(defgeneric same-variable-p (ast1 ast2)
  (:documentation "T if AST1 and AST2 resolve to the same declaration.
Returns a second value to represent certainty; returning NIL, T means
they are definitely not the same; returning NIL, NIL means
uncertainty.")
  (:method ((id1 identifier-ast) (id2 identifier-ast))
    (let ((decl1 (get-declaration-id :variable id1))
          (decl2 (get-declaration-id :variable id2)))
      (cond ((not (and decl1 decl2))
             (values nil nil))
            ((eql decl1 decl2)
             (values t t))
            (t
             (values nil t))))))

(defgeneric same-place-p (ast1 ast2)
  (:documentation "T if AST1 and AST2 share the same storage.

Differs from `same-variable-p' in that it takes references and
pointers into account in languages that support them.")
  (:method ((id1 identifier-ast) (id2 identifier-ast))
    (let ((aliasee1 (or (aliasee id1) id1))
          (aliasee2 (or (aliasee id2) id2)))
      (same-variable-p aliasee1 aliasee2))))

(defgeneric collect-var-uses (software ast)
  (:Documentation "Collect uses of IDENTIFIER in SOFTWARE.")
  (:method ((obj normal-scope) (identifier identifier-ast))
    (collect-var-uses (genome obj) identifier))
  (:method ((root normal-scope-ast) (identifier identifier-ast))
    (when-let ((identifier (get-declaration-id :variable identifier)))
      (collect-if (lambda (ast)
                    (and (not (eql ast identifier))
                         (typep ast 'identifier-ast)
                         (source-text= ast identifier)
                         (eql identifier
                              (get-declaration-id :variable ast))))
                  root))))

(defgeneric collect-fun-uses (root fun)
  (:Documentation "Collect uses of FUNCTION in ROOT.")
  (:method ((root software) (ast ast))
    (collect-fun-uses (genome root) ast))
  (:method ((root ast) (function function-declaration-ast))
    (collect-fun-uses root (definition-name-ast function)))
  (:method ((root ast) (function identifier-ast))
    (collect-if (lambda (ast)
                  (and (typep ast 'call-ast)
                       (eql (get-declaration-id :function (call-function ast))
                            function)))
                root)))

(defgeneric assignees (ast)
  (:documentation "Get the ASTs that AST assigns to.

This is like `assignee', but handles the case of an AST assigning to
more than one thing (destructuring).")
  (:method ((ast t)) nil)
  (:method ((ast ast))
    (when-let (assignee (assignee ast))
      (list assignee))))

(def-attr-fun assignees-table ()
  "Build a lookup table from assignees to their assigners."
  ;; This is a "synthesized attribute", built from the bottom-up.
  (:method ((software parseable))
    (assignees-table (genome software)))
  (:method ((assigner assignment-ast))
    (reduce (lambda (map assignee)
              (with map assignee (cons assigner (lookup map assignee))))
            (mappend (lambda (assignee)
                       (filter-map (op (get-declaration-id :variable _))
                                   (identifiers assignee)))
                     (assignees assigner))
            :initial-value (call-next-method)))
  (:method ((ast ast))
    (reduce (op (map-union _ _ #'append))
            (children ast)
            :key #'assignees-table
            :initial-value (empty-map))))

(defmethod attr-missing ((attr (eql 'assignees-table)) node)
  (assignees-table (attrs-root*)))

(def-attr-fun assignments ()
  "Return a list of ASTs that assign to TARGET.
TARGET should be the actual declaration ID (from `get-declaration-id'.)

Note that in languages with pointers, the assignments include both
assignments through the pointer and assignment of the pointer. To
get just the pointer assignments, use `pointer-assignments'."
  (:method ((target identifier-ast))
    (values
     (lookup (assignees-table (attrs-root*))
             (get-declaration-id :variable target))))
  (:method ((target t))
    nil))

(def-attr-fun pointer-assignments ()
  "Return a list of ASTs that assign a new address to TARGET, a pointer.
Assignments include pointer arithmetic.

TARGET should be the actual declaration ID (from `get-declaration-id'.)

Returns a subset of `assignments'.

For languages without pointers, this will always return nil."
  (:method ((target t))
    nil))

(defgeneric collect-arg-uses (software ast &optional alias)
  (:documentation "Collect function calls in SOFTWARE with TARGET as an argument.

If ALIAS is non-nil, resolve aliases during the search.")
  (:method (obj (target identifier-ast) &optional alias)
    (flet ((get-decl (var)
             (get-declaration-id :variable
                                 (or (and alias (aliasee var))
                                     var))))
      (let ((target (get-decl target)))
        (iter (for ast in-tree (genome obj))
              ;; The outer loop will recurse, so we don't
              ;; need to recurse here.
              (match ast
                ((call-ast (call-arguments (and args (type list))))
                 (when (member target
                               (filter (of-type 'identifier-ast) args)
                               :key (op (get-decl _)))
                   (collect ast)))))))))

(defmethod convert ((to-type (eql 'integer))
                    (ast integer-ast) &key)
  (parse-integer (text ast)))

(defmethod convert ((to-type (eql 'number))
                    (ast integer-ast) &key)
  (convert 'integer ast))

(defmethod convert ((to-type (eql 'float))
                    (ast float-ast)
                    &key (type 'double-float))
  (parse-float (text ast) :type type))

(defmethod convert ((to-type (eql 'number))
                    (ast float-ast)
                    &rest args &key)
  (apply #'convert 'float ast args))


;;;; Cross-language generics and methods
(defgeneric end-of-parameter-list (software node)
  (:documentation "Find the end position of the parameters of FUNCTION-NODE.")
  (:method (software (node function-ast))
    (error "END-OF-PARAMETER-LIST undefined for ~a" (type-of node))))

(defgeneric function-name (node)
  (:documentation "Extract the name of the function from NODE.
If NODE is not a function node, return nil.")
  (:method ((node t)) nil)
  (:method ((node function-ast) &aux (type (type-of node)))
    (unless (subtypep type 'lambda-ast)
      (warn "FUNCTION-NAME undefined for ~a" type))
    nil))

(defgeneric field-names (node)
  (:documentation "Extract the names (as strings) of a field from
NODE. If NODE is not a thing that has fields, return nil.")
  (:method ((node t))
    (field-name-asts node))
  (:method :around ((node t))
    (iter (for result in (call-next-method))
          (collecting
           (if (typep result 'tree-sitter-ast)
               (source-text result)
               result)))))

(defgeneric field-name-asts (node)
  (:documentation "Extract the names (as ASTs) of a field from
NODE. If NODE is not a thing that has fields, return nil.")
  (:method ((node t)) nil))

(defgeneric placeholder-type-p (ast)
  (:documentation "Does AST designate a placeholder type?

A placeholder type is a type like C++ `auto' or Java `var', a request
for the compiler to infer the type.")
  (:method ((ast t)) nil))

(defgeneric boolean-type-p (ast)
  (:documentation "Is AST a boolean type?")
  (:method ((ast t)) nil))

(defgeneric deref-type (type)
  (:documentation "Return the type that comes from dereferencing TYPE.")
  (:method (type) type))

(defun infer-type-from-declaration (ast)
  (when-let* ((decl-type (relevant-declaration-type ast))
              (decl (get-declaration-ast decl-type ast)))
    (resolve-declaration-type decl ast)))

(def-attr-fun infer-type ()
  (:documentation "Return the type of AST as an a AST, or nil if it
  could not be determined.

By default this first tries `expression-type', then invokes
`resolve-declaration-type' on the result of
`get-declaration-ast'.")
  (:method :context ((ast ast))
    (restart-case
        (call-next-method)
      (infer-type-as-nil ()
        :report "Return nil from infer-type"
        nil)))
  (:method (ast)
    (let ((expression-type (infer-expression-type ast)))
      (cond ((null expression-type)
             (infer-type-from-declaration ast))
            ((placeholder-type-p expression-type)
             (or (infer-type-from-declaration ast)
                 expression-type))
            (t expression-type)))))

(def-attr-fun infer-expression-type ()
  "Infer the type of AST in SOFTWARE as an expression.
Calls `expression-type' by default."
  (:method ((ast t))
    (expression-type ast))
  (:method ((ast call-ast))
    "Infer the type of a call from its declaration."
    (infer-type (call-function ast))))

(defgeneric expression-type (ast)
  (:documentation "Extract the type from AST, an expression.")
  (:method ((ast t)) nil))

(defgeneric resolve-declaration-type (decl-ast ast)
  (:documentation
   "Return the type that DECL-AST in SOFTWARE specifies
  for AST, as an AST, or nil if it could not be determined.

This differs from `declaration-type' only in cases when the type
declared differs between declarands (e.g. `auto' declarations in C++;
`auto x = 1, y = 2.0' effectively declares `x' as an integer but `y'
as a float; or `int y, *x' declares `y' as an integer but `x' as a
pointer to integer.)

By default simply calls `declaration-type' on DECL-AST.")
  (:method ((decl-ast ast) ast)
    (declaration-type decl-ast)))

(defgeneric declaration-type (declaration-ast)
  (:documentation "Return the type specified by DECLARATION-AST, as an AST, if no context is required to do so.")
  (:method ((ast ast)) nil))

;;; TODO Replace with canonicalize-type.
(defgeneric type-descriptor (type-ast)
  (:documentation "Return the type denoted by AST in some canonical form.
Equivalent type descriptors should be equal under `equal?'.")
  (:method ((type-ast null)) nil)
  (:method ((type-ast ast))
    ;; TODO A canonical that doesn't destroy the internal structure.
    (make-keyword (source-text type-ast))))

(defmethod is-stmt-p ((ast statement-ast)) t)

(defgeneric maybe-side-effect-p (ast)
  (:documentation "True if AST is something that may have a side effect")
  (:method ((ast comment-ast)) nil)
  (:method ((ast definition-ast)) nil)
  (:method ((ast type-declaration-ast)) nil)
  (:method ((ast composite-type-ast)) nil)
  (:method ((ast macro-declaration-ast)) nil)
  (:method ((ast function-declaration-ast)) nil)
  (:method ((ast identifier-ast)) nil)
  (:method ((ast field-ast)) nil)
  (:method ((ast literal-ast)) nil)
  (:method ((ast t)) t))

(defun contains-error-ast-p (ast)
  "Return non-NIL if AST is an error AST or contains an error AST child."
  (fbind ((test (of-type '(or source-text-fragment-variation-point
                           error-variation-point))))
    (find-if (lambda (ast)
               (or (test ast)
                   (some #'test (before-asts ast))
                   (some #'test (after-asts ast))))
             ast)))

(defclass canonical-type ()
  ()
  (:documentation "A representation of canonical types."))

(defgeneric canonicalize-type (type &key)
  (:documentation "Get a canonicalized form of TYPE. This form can be
used to determine whether two types are identical and should be an instance
of CANONICAL-TYPE. This form should also be a normalized such that two types
which are functionally equivalent are equivalent."))

(defgeneric canonical-type= (type1 type2 &key)
  (:documentation "Compare TYPE 1 and TYPE2 which are canonical types, and
return whether they are equal.")
  (:method ((type1 canonical-type) (type2 canonical-type) &key)
    (eql type1 type2)))

;;; TODO Consider rewriting aliasee to propagate down from the top
;;; (like symbol-table does) and alias-set to propagate backwards?

(def-attr-fun aliasee ()
  "If AST is a pointer variable, the plain variable it points to.")

(def-attr-fun alias-set ()
  "Get the declarations \(as identifiers, as in `get-declaration-id')
   of variables in SOFTWARE that are aliases \(pointers or references)
   for PLAIN-VAR.")

(defgeneric constant-fold (ast)
  (:documentation "Constant-fold AST into a Lisp value.")
  (:method ((ast t))
    nil)
  (:method ((ast number-ast))
    (convert 'number ast))
  (:method ((ast parenthesized-expression-ast))
    (match (children ast)
      ((list child) (constant-fold child))))
  (:method ((ast binary-ast))
    (and-let* ((lhs (lhs ast))
               (rhs (rhs ast))
               (lhs-value (constant-fold lhs))
               (rhs-value (constant-fold rhs))
               ((numberp lhs-value))
               ((numberp rhs-value))
               (fn (case (operator ast)
                     (:+ #'+)
                     (:- #'-)
                     (:* #'*)
                     (:/ #'/))))
      (funcall fn lhs-value rhs-value)))
  (:method ((ast unary-ast))
    (when (eql :- (operator ast))
      (when-let (value (constant-fold (argument ast)))
        (- value)))))

(def-attr-fun exit-control-flow ()
  (:documentation "Nodes AST might transfer control to on exit.
Note that the result will be coerced to a list with `ensure-list', so
each method does not need to return a list. Also, `nil' is
automatically removed.")
  (:method :context ((root ast) &aux (attrs-root (attrs-root*)))
    "Follow the CFG from ROOT to handle nonlocal exits."
    (labels ((walk-cfg (entry-points final-exits visited)
               "Follow CFG edges in the control flow subgraph from ENTRY-POINTS,
                appending exits from the subgraph to FINAL-EXITS."
               (assert (notany (op (ancestor-of-p attrs-root root _))
                               entry-points))
               ;; NB While attributes are implicitly cached, we still
               ;; need to track which ones have been seen to avoid
               ;; infinite loops between siblings in languages without
               ;; left-to-right evaluation order.
               (mvlet* ((entry-points
                         (remove-if (op (contains? visited _))
                                    (ensure-list entry-points)))
                        (exits (mappend #'exit-control-flow entry-points))
                        (visited (union visited (convert 'fset:set entry-points)))
                        ;; "Outer" exits leave the execution context
                        ;; (either returning to AST or jumping out of
                        ;; it). Inner exits leave AST on the "stack".
                        (outer-exits inner-exits
                         (partition (op (member _1 (get-parent-asts attrs-root root)))
                                    exits)))
                 (dbg:note 3 "Exits for ~a: ~a" entry-points exits)
                 (qappend final-exits outer-exits)
                 (mapc (op (walk-cfg _ final-exits visited))
                       inner-exits)))
             (final-exits (entry-points)
               (let ((final-exits (queue))
                     (visited (empty-set)))
                 (walk-cfg entry-points final-exits visited)
                 (nub (qlist final-exits)))))
      (let ((defaults (remove nil (ensure-list (call-next-method))))
            (entry-points (entry-control-flow root)))
        (dbg:note 3 "Default exits for ~a: ~a" root defaults)
        (if (no entry-points) defaults
            (let ((final-exits (final-exits entry-points)))

              (dbg:note 3 "Final exits for ~a: ~a" entry-points final-exits)
              ;; If control flow returns to the root of the subgraph,
              ;; add its exits.
              (or (flatten (substitute defaults root final-exits))
                  defaults))))))
  (:method ((ast statement-ast))
    (let ((parent (get-parent-ast (attrs-root*) ast)))
      (if (typep parent 'control-flow-fork-ast)
          parent
          (or (find-next-sibling '(or statement-ast return-ast)
                                 parent
                                 ast)
              parent))))
  (:method ((ast subexpression-ast))
    (when-let (parent (get-parent-ast (attrs-root*) ast))
      (subexpression-exit-control-flow parent ast)))
  (:method ((ast compound-ast))
    (append
     (if-let (parent (get-parent-ast (attrs-root*) ast))
       (subexpression-exit-control-flow parent ast)
       (call-next-method))))
  (:method ((ast terminal-symbol))
    (find-enclosing 'subexpression-ast (attrs-root*) ast))
  (:method ((ast loop-ast))
    (list ast (call-next-method)))
  (:method ((ast continue-ast))
    (list (find-enclosing (of-type 'continuable-ast) (attrs-root*) ast)))
  (:method ((ast break-ast))
    (list (find-enclosing (of-type 'breakable-ast) (attrs-root*) ast)))
  (:method ((ast return-ast))
    (list (find-enclosing (of-type 'returnable-ast) (attrs-root*) ast)))
  (:method ((ast variation-point) &aux (root (attrs-root*)))
    (append (remove-if (op (or (eql _1 ast)
                               (ancestor-of-p root _1 ast)))
                       (mappend #'exit-control-flow (children ast)))
            (list (or (find-next-sibling t root ast)
                      (get-parent-ast root ast))))))

(defgeneric subexpression-exit-control-flow (parent subexpression)
  (:documentation "Get the exit control flow of SUBEXPRESSION according to PARENT.
This expresses control flow that is either different based on the
parent, as within a conditional, or for a \"subexpression\" that is
not actually an expression, e.g. an initializer in a C/C++
declaration.")
  (:method ((parent ltr-eval-ast) (sub ast))
    (or (find-next-sibling 'subexpression-ast parent sub)
        parent))
  (:method ((parent ast) (child ast))
    (or (if (ltr-eval-ast-p parent)
            (find-next-sibling 'subexpression-ast parent child)
            (remove child
                    (filter (of-type 'subexpression-ast) (children parent))))
        parent))
  (:method ((parent if-ast) (child ast))
    (if (eql child (condition parent))
        (list (consequence parent)
              (or (alternative parent) parent))
        parent)))

(defun first-subexpression (ast)
  "Return the first subexpression of AST."
  (find-if
   (lambda (ast2)
     (and (typep ast2 'subexpression-ast)
          (not (eql ast2 ast))))
   ast))

(def-attr-fun entry-control-flow ()
  (:documentation "Nodes AST might transfer control to on entry.

That is, if AST is a basic block, then its control flow target is the first
node in the block.")
  (:method :context ((ast ast))
    (remove nil (ensure-list (call-next-method))))
  (:method :around ((ast compound-ast))
    (first (children ast)))
  (:method ((ast statement-ast))
    (if (ltr-eval-ast-p ast)
        (first-subexpression ast)
        (filter (of-type 'subexpression-ast) (children ast))))
  (:method ((ast identifier-expression-ast))
    nil)
  (:method ((ast literal-ast))
    nil)
  (:method ((ast subexpression-ast))
    (if (ltr-eval-ast-p ast)
        (first-subexpression ast)
        (filter (of-type 'subexpression-ast) (children ast))))
  (:method ((ast call-ast))
    (if (ltr-eval-ast-p ast)
        (call-function ast)
        (list (call-function ast)
              (call-arguments-ast ast))))
  (:method ((ast arguments-ast))
    (if (ltr-eval-ast-p ast)
        (first-subexpression ast)
        (filter (of-type 'subexpression-ast) (children ast))))
  (:method ((ast conditional-ast))
    (list (condition ast)))
  (:method :around ((ast assignment-ast))
    (rhs ast))
  (:method ((ast variation-point))
    (mappend #'entry-control-flow (children ast)))
  (:method ((ast source-text-fragment))
    nil)
  (:method ((ast if-ast))
    (condition ast)))

(deftype exception-set ()
  '(or (eql t) (cons (eql or) t)))

(defconst +exception-top-type+ t
  "The set of all possible exception sets.")

(defconst +exception-bottom-type+ '(or)
  "The empty exception set.")

(-> exception-set-union (exception-set exception-set) exception-set)
(defun exception-set-union (x y)
  (econd
   ((eql x +exception-top-type+) +exception-top-type+)
   ((eql y +exception-top-type+) +exception-top-type+)
   ((and (eql (car x) 'or)
         (eql (car y) 'or))
    (cons 'or (append (cdr x) (cdr y))))))

(-> exception-set-intersection (exception-set exception-set) exception-set)
(defun exception-set-intersection (x y)
  (econd
   ((and (eql x +exception-top-type+) (eql y +exception-top-type+)) +exception-top-type+)
   ((and (eql x +exception-top-type+) (listp y))
    y)
   ((and (listp x) (eql y +exception-top-type+))
    y)
   ((and (listp x) (listp y))
    (cons 'or (intersection (cdr x) (cdr y) :test #'source-text=)))))

(-> exception-set-difference (exception-set exception-set) exception-set)
(defun exception-set-difference (x y)
  (econd
   ((and (eql x +exception-top-type+) (eql y +exception-top-type+))
    +exception-bottom-type+)
   ((and (eql x +exception-top-type+) (listp y))
    ;; TODO
    +exception-top-type+)
   ((and (listp x) (eql y +exception-top-type+))
    +exception-bottom-type+)
   ((and (listp x) (listp y))
    (cons 'or
          (set-difference
           (append (rest x) (rest y))
           (rest (exception-set-intersection x y))
           :test #'source-text=)))))

(def-attr-fun exception-set ()
  (:documentation "Return the exception set of an AST.
The set should be t (if it could throw anything) or a list starting
with `or' for a specific list of exceptions.")
  (:method ((ast literal-ast))
    +exception-bottom-type+)
  (:method ((ast type-ast))
    +exception-bottom-type+)
  (:method ((ast ast))
    (if (no (children ast))
        +exception-bottom-type+
        (reduce #'exception-set-union
                (children ast)
                :key #'exception-set)))
  (:method ((ast compound-ast))
    (if (no (children ast))
        +exception-bottom-type+
        (reduce #'exception-set-union
                (children ast)
                :key #'exception-set)))
  (:method ((ast throw-ast))
    ;; TODO Add an infer-exception-type function we can use for C++ to
    ;; assume functions are constructors?
    (list 'or (infer-type (first (children ast)))))
  (:method ((ast call-ast))
    (exception-set-union
     (reduce #'exception-set-union
             (mapcar #'exception-set (call-arguments ast))
             :initial-value +exception-bottom-type+)
     (function-exception-set (call-function ast))))
  (:method ((ast if-ast))
    (reduce #'exception-set-union
            (remove nil
                    (list (condition ast)
                          (consequence ast)
                          (alternative ast)))
            :key #'exception-set
            :initial-value +exception-bottom-type+)))

(defgeneric function-exception-set (function)
  (:documentation "Get the exception set of FUNCTION.

We use `function-exception-set' to distinguish the exception set of
the function itself from the exception set of a function call (which
is the union of the function exception set and the argument exception
sets.")
  (:method ((ast ast))
    (if-let ((fn-defs (get-declaration-asts :function ast)))
      (reduce #'exception-set-union
              ;; Don't recurse on the enclosing function.
              (remove (find-enclosing 'function-declaration-ast
                                      (attrs-root*)
                                      ast)
                      fn-defs)
              :key #'exception-set
              :initial-value +exception-bottom-type+)
      ;; If we have no definition, assume it could throw anything.
      +exception-top-type+)))


;;;; Structured text
;;; TODO: remove this; it's for debugging.
(defmacro labels+ ((&rest forms) &body body)
  `(progn
     ,@(iter
         (for form in forms)
         (collect `(declaim (notinline ,(car form))))
         (collect (cons 'defun form)))
     ,@body))

(deftype stack-directive ()
  "A directive for translating stacks to inner asts..

- A symbol indicates the next inner AST grouping is to potentially be
  stored in a slot specified by the symbol.
- A cons has the number of terminal children to skip over in its car
  and the cdr is the number of groupings to be dropped in inner ASTS
  due to immediate tokens.
- A number is the same as the car of a cons."

  '(or symbol number (cons number number)))

(deftype child-category ()
  "A child category.
- :terminal indicates a terminal symbol.
- :ast indicates an AST which is stored in a slot.
- :extra-ast indicates an AST which is stored in a before/after
  slot or an inner-asts slot."
  '(member :terminal :ast :extra-ast))

(defgeneric get-representative-ast (child-ast parent-ast)
  (:documentation "Get an AST in CHILD-AST which can be used when
structured-text methods require a structured-text AST.

By default just returns CHILD-AST.")
  (:method ((child t) (parent t)) child)
  (:method ((ast conflict-ast) parent-ast)
    (some #'cadr (conflict-ast-child-alist ast))))

(defun children-parser (ast pruned-rule slots &aux (child-stack-key '#.(gensym)))
  "Return the children of AST in order based on PRUNED-RULE. SLOTS specifies
which slots are expected to be used."
  ;; NOTE: this does not back track; not sure if this will be a problem,
  ;;       but there aren't any rules that obviously require it--this may
  ;;       be the case if a rule is a subtype of another.
  ;; NOTE: esrap or smug may be useful if needed in the future.

  (labels ((get* (key box)
             (lookup (unbox box) (assure symbol key)))
           (ensure-get* (key table value)
             (multiple-value-bind (old old?) (get* key table)
               (if old? (values old old?)
                   (values (setf (get* key table) value)
                           nil))))
           ((setf get*) (value key box)
             (withf (unbox box) (assure symbol key) value)
             value)
           (copy-table (table)
             (box (unbox table)))
           (get-matchable-value (value)
             "Get a value that can be matched on by the tree-sitter rules."
             (get-representative-ast value ast))
           (populate-slot->stack ()
             "Create a table that maps a slot name to its
              corresponding stack."
             (lret ((table (box (empty-map))))
               (iter (for slot in slots)
                     (let ((slot-value (slot-value ast slot)))
                       (ensure-get* slot table
                                    (and slot-value (ensure-cons slot-value)))))))
           (identical-slot-stacks-p (slot->stack1 slot->stack2)
             "Return T if the slot stacks in slot->stack are identical
              except the child stack."
             (iter
              (for (slot stack) in-map (unbox slot->stack1))
               (always (or (eql slot child-stack-key)
                           (eq stack (get* slot slot->stack2))))))
           (push-child-stack (value slot->stack)
             "Push VALUE onto the child stack in SLOT->STACK."
             (push value (get* child-stack-key slot->stack)))
           (trim-slot-stack (slot slot->stack)
             "Removes one item from SLOT's stack in SLOT->STACK and returns
              a copy of SLOT->STACK with this change."
             (lret ((copy (copy-table slot->stack)))
               (symbol-macrolet ((slot-hash (get* slot copy)))
                 (push-child-stack (pop slot-hash) copy))))
           (match? (ast types)
             (let ((matchable (get-matchable-value ast)))
               ;; Treat source-text-fragment-variation-point as a wild
               ;; card.
               (or (typep matchable 'source-text-fragment-variation-point)
                   (some (op (typep matchable _)) types))))
           (handle-child (rule slot->stack)
             (when (match? (car (get* 'children slot->stack))
                           (cdr rule))
               (trim-slot-stack 'children slot->stack)))
           (handle-field (rule slot->stack &aux (slot (cadr rule)))
             (when (match? (car (get* slot slot->stack))
                           (cddr rule))
               (trim-slot-stack slot slot->stack)))
           (handle-slot (rule slot->stack &aux (slot (cadr rule)))
             (ensure-get* slot slot->stack (slot-value ast slot))
             (trim-slot-stack slot slot->stack))
           (handle-choice (rule slot->stack)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.

             ;; NOTE: this marks which branch was taken by adding a
             ;;       list of (:choice branch-num) into the children list.
             (if (every #'null (cdr rule))
                 ;; If every branch is nil, assume a match on the first one.
                 (lret ((copy (copy-table slot->stack)))
                   (push-child-stack `(:choice 0) copy))
                 (iter
                   (iter:with empty-match?)
                   (for branch in (cdr rule))
                   (for i upfrom 0)
                   (cond
                     (branch
                      (for copy = (copy-table slot->stack))
                      (push-child-stack `(:choice ,i) copy)
                      (when-let ((matched? (rule-handler branch copy)))
                        (leave matched?)))
                     (t (setf empty-match? i)))
                   (finally
                    (return
                      ;; Handles the case where it matched on an empty branch.
                      (when-let ((copy (and empty-match?
                                            (copy-table slot->stack))))
                        (push-child-stack `(:choice ,empty-match?)
                                          copy)
                        copy))))))
           (handle-repeat (rule slot->stack
                           &optional continuation?
                           &aux (copy (copy-table slot->stack)))
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.

             ;; NOTE: this marks the beginning of a repeat with (:repeat),
             ;;       every subsequent iteration of that repeat with a
             ;;       (:continue), and then end of the repeat with (:end-repeat)
             (push-child-stack
              (if continuation? '(:continue) '(:repeat))
              copy)
             (let ((repeat-slot->stack (rule-handler (cadr rule) copy)))
               (cond
                 ((and repeat-slot->stack
                       ;; Prevent infinite recursion on a nested, empty repeat.
                       (not (eq copy repeat-slot->stack))
                       ;; Check if they are the same except the child-stack.
                       (not (identical-slot-stacks-p
                             slot->stack repeat-slot->stack)))
                  (handle-repeat rule repeat-slot->stack t))
                 (t
                  ;; NOTE: this should only triggered once when the
                  ;;       repeat can't procede.
                  (push-child-stack '(:end-repeat) slot->stack)
                  slot->stack))))
           (handle-seq (rule slot->stack &aux (seq (cdr rule)))
             (iter
               (for subrule in seq)
               (unless subrule (next-iteration))
               (for sub-slot->stack first (rule-handler subrule slot->stack)
                    then (rule-handler subrule sub-slot->stack))
               (always sub-slot->stack)
               (finally
                (return
                  (cond
                    ((eql t sub-slot->stack) slot->stack)
                    (sub-slot->stack)
                    (t slot->stack))))))
           (rule-handler (rule slot->stack)
             "Handles dispatching RULE to its relevant rule handler."
             (ecase (car rule)
               (:CHOICE (handle-choice rule slot->stack))
               (:REPEAT (handle-repeat rule slot->stack))
               (:FIELD (handle-field rule slot->stack))
               (:CHILD (handle-child rule slot->stack))
               (:SEQ (handle-seq rule slot->stack))
               (:SLOT (handle-slot rule slot->stack))))
           (slot-stacks-empty-p (slot->stack)
             "Return T if SLOT->STACK doesn't have any slots that
              map to a non-empty stack."
             (iter (for (slot stack) in-map (unbox slot->stack))
                   (never
                    (unless (eq slot child-stack-key)
                      stack)))))
    (let ((slot->stack (rule-handler pruned-rule (populate-slot->stack))))
      (if (and slot->stack (slot-stacks-empty-p slot->stack))
          (reverse (get* child-stack-key slot->stack))
          (error 'rule-matching-error
                 :rule-matching-error-rule pruned-rule
                 :rule-matching-error-ast ast)))))

(defun computed-text-output-transformation (ast)
  "Gives the variable text output transformation for AST. This
representation is interleaved text though it's unlikely to
be more than one string outside of string literals."
  (flatten
   (list
    (before-text ast)
    (or (slot-value ast 'children)
        (text ast))
    (after-text ast))))

(defun match-parsed-children-json
    (language-prefix json-rule parse-tree
     &aux (token-count 0)
       (immediate-token-count 0))
  "Match a cl-tree-sitter PARSE-TREE as a JSON-RULE if possible.
Returns as values the updated parse tree, whether it matched, and a cons of the
number of terminals and the number of immediate tokens encountered. The number of
immediate tokens is used to discard inner-asts that were generated between two
tokens but don't actually belong in between them. These inner-asts are generated
in #'convert and inlined into the parse tree received from cl-tree-sitter."
  ;; NOTE: this could be expanded to match on the string too
  ;;       though the current representation of transformed json
  ;;       rules and pruned rules likely wouldn't benefit from it.
  (labels ((handle-alias (rule tree &aux (alias (car (car tree))))
             (cond
               ((aget :named rule)
                ;; Named aliases are unhandled by #'match-parsed-children-json.
                (error "Named alias in JSON subtree"))
               ((string-equal (if (consp alias)
                                  (cadr alias)
                                  alias)
                              (aget :value rule))
                (incf token-count)
                (values (cdr tree) t))))
           (handle-blank (tree) (values tree t))
           (handle-choice (rule tree)
             (mvlet* ((branches (aget :members rule))
                      (blank not-blank
                       (partition (lambda (rule)
                                    (equal "BLANK" (aget :type rule)))
                                  branches))
                      ;; Always try rules other than blanks first.
                      (branches
                       (append not-blank blank)))
               (iter
                (for branch in branches)
                (for (values result matched?) = (rule-handler branch tree))
                (when matched?
                  (leave (values result t))))))
           (handle-repeat (rule tree)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.
             (mvlet ((result matched? (rule-handler (aget :content rule) tree)))
               (cond
                 ((and matched?
                       ;; Prevent infinite recursion. This will probably
                       ;; never be an issue for JSON matching.
                       (not (eq tree result)))
                  (handle-repeat rule result))
                 (t (values tree t)))))
           (handle-seq (rule tree)
             (iter
               (for child in (aget :members rule))
               (for (values result matched?)
                    first (rule-handler child tree)
                    then (rule-handler child result))
               (unless matched?
                 (leave))
               (finally (return (values result t)))))
           (handle-string (rule tree &aux (token (car tree)))
             (incf token-count)
             (when (and (consp token)
                        (atom (car token))
                        (string-equal (car token)
                                      (convert-name language-prefix
                                                    (aget :value rule))))
               (values (cdr tree) t)))
           (handle-token (rule tree &aux (content (aget :content rule)))
             (when (equal (aget :type rule) "IMMEDIATE_TOKEN")
               (incf immediate-token-count))
             (cond
               ((equal "STRING" (aget :type content))
                (rule-handler content tree))
               (t
                (incf token-count)
                (values tree t))))
           (rule-handler (rule tree)
             "Handles dispatching RULE to its relevant rule handler."
             ;; NOTE: this will throw an error if an unexpected rule is
             ;;       encountered.
             (string-ecase (aget :type rule)
               ("ALIAS" (handle-alias rule tree))
               ("BLANK" (handle-blank tree))
               ("CHOICE" (handle-choice rule tree))
               ;; TODO: token rules are handled differently if they only
               ;;       produce one token.
               (("IMMEDIATE_TOKEN" "TOKEN") (handle-token rule tree))
               ("REPEAT" (handle-repeat rule tree))
               ("SEQ" (handle-seq rule tree))
               ("SLOT" (values tree t))
               ("STRING" (handle-string rule tree)))))
    (mvlet ((tree matched? (rule-handler json-rule parse-tree)))
      (values tree matched? (cons token-count immediate-token-count)))))

(defun match-parsed-children
    (language-prefix json-rule pruned-rule child-types parse-tree
     &aux (extra-asts (extra-asts (make-keyword language-prefix))))
  "Match a cl-tree-sitter PARSE-TREE as a PRUNED-RULE in LANGUAGE-PREFIX.
CHILD-TYPES is a list of lisp types that the children slot can contain.
Returns as values whether the match succeeded and if so, returns a list
specifying how to populate the inner-asts slots--'symbols' indicate a slot
to store the next grouping of ASTs on the inner-asts stack; a 'number' indicates
the number of terminals to drop from the children list; and a 'cons' where the
car does the same as the aformentioned 'number' and the cdr indicates the number
of groupings to drop from the stack. See convert-parse-tree for advanced usage."
  (labels ((remove-ignorable-items (tree)
             "Remove ignorable items from PARSE-TREE. This includes comments,
              errors, and internal ast slots."
             (declare (inline memq)
                      (optimize (speed 3)))
             (when tree
               `(,(car tree)
                 ,(cadr tree)
                 ,(iter (for child in (caddr tree))
                        (unless (memq (car child) extra-asts)
                          (collect (remove-ignorable-items child)))))))
           (get-children ()
             "Get the children slots and their types from parse-tree."
             (iter
               (for child in (caddr parse-tree))
               (for slot-pair = (car child))
               (for child-type = (unless (listp slot-pair)
                                   (convert-to-lisp-type
                                    language-prefix slot-pair)))
               (cond
                 ((not child-type)
                  (collect
                      (list
                       (convert-to-lisp-type
                        language-prefix (car slot-pair))
                       (convert-to-lisp-type
                        language-prefix (cadr slot-pair)))))
                 ((subtypep child-type 'comment-ast))
                 ((member child-type child-types :test #'subtypep)
                  (collect (convert-to-lisp-type language-prefix slot-pair))))))
           (handle-child (rule parse-stack inner-asts-order
                          &aux (child (car (car parse-stack))))
             (cond
               ((and (atom child)
                     (not (null child))
                     ;; Confirm tree is the relevant thing on the stack.
                     (member (convert-to-lisp-type language-prefix child)
                             ;; Treat source-text-fragment-variation-point as a
                             ;; wild card.
                             (cons 'source-text-fragment-variation-point
                                   (cdr rule))
                             :test #'subtypep))
                (values (cdr parse-stack) t (cons 1 inner-asts-order)))
               ;; This is an edge case for rules that allow null children.
               ((member 'null (cdr rule))
                (values parse-stack t inner-asts-order))))
           (handle-field (rule parse-stack inner-asts-order
                          &aux (parsed-field (car parse-stack))
                            (field-pair (and (consp parsed-field)
                                             (car parsed-field))))
             ;; Must handle field that isn't provided but has null.
             (cond
               ((and (consp field-pair)
                     (eql (cadr rule)
                          (convert-to-lisp-type
                           language-prefix (car field-pair)))
                     (member
                      (convert-to-lisp-type
                       language-prefix (cadr field-pair))
                      ;; Treat source-text-fragment-variation-point as a wild
                      ;; card.
                      (cons 'source-text-fragment-variation-point
                            (cddr rule))
                      :test #'subtypep))
                (values (cdr parse-stack) t (cons 1 inner-asts-order)))
               ;; This is an edge case for a field that allows nil.
               ((member 'null (cddr rule))
                (values parse-stack t inner-asts-order))))
           (handle-slot (rule parse-stack inner-asts-order)
             (values parse-stack t (cons (cadr rule) inner-asts-order)))
           (handle-choice (rule json parse-stack inner-asts-order)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.
             (iter
               (for branch in (cdr rule))
               (for json-branch in (aget :members json))
               (for (values stack matched? inner-asts) =
                    (rule-handler
                     branch json-branch parse-stack inner-asts-order))
               (when matched?
                 (return (values stack t inner-asts)))))
           (handle-repeat (rule json parse-stack inner-asts-order)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.
             (mvlet ((stack
                      matched?
                      inner-asts
                      (rule-handler
                       (cadr rule) (aget :content json) parse-stack
                       inner-asts-order)))
               ;; Prevent infinite recursion when the parse-stack is never
               ;; used.
               (if (and matched? (not (equal parse-stack stack)))
                   (handle-repeat rule json stack inner-asts)
                   (values parse-stack t inner-asts-order))))
           (handle-seq (rule json parse-stack inner-asts-order)
             (iter
               (for subrule in (cdr rule))
               (for json-subrule in (aget :members json))
               (for (values stack matched? inner-asts)
                    first (rule-handler
                           subrule json-subrule parse-stack inner-asts-order)
                    then (rule-handler
                          subrule json-subrule stack inner-asts))
               (always matched?)
               (finally (return (values stack t inner-asts)))))
           (rule-handler (rule json parse-stack inner-asts-order)
             "Handles dispatching RULE to its relevant rule handler."
             (ecase (car rule)
               (:CHOICE (handle-choice rule json parse-stack inner-asts-order))
               (:REPEAT (handle-repeat rule json parse-stack inner-asts-order))
               (:FIELD (handle-field rule parse-stack inner-asts-order))
               (:CHILD (handle-child rule parse-stack inner-asts-order))
               (:SEQ (handle-seq rule json parse-stack inner-asts-order))
               (:SLOT (handle-slot rule parse-stack inner-asts-order))
               ((nil)
                (mvlet ((stack
                         matched?
                         immediate-token-count
                         (match-parsed-children-json
                          language-prefix json parse-stack)))
                  (when matched?
                    (values
                     stack t (cons immediate-token-count inner-asts-order))))))))
    (cond
      ;; Prevent matching on an empty rule when there are children.
      ((or (not pruned-rule)
           (length= 1 pruned-rule))
       (not (get-children)))
      (pruned-rule
       (mvlet ((parse-stack
                success?
                inner-asts-order
                (rule-handler
                 pruned-rule
                 json-rule
                 (caddr (remove-ignorable-items
                         parse-tree))
                 nil)))
         ;; Avoid matching a rule if parse tree tokens still exist.
         (unless parse-stack
           (values success? (reverse inner-asts-order))))))))

(defmethod slot-unbound ((class t)
                         (obj structured-text)
                         (slot-name (eql 'ordered-children)))
  (setf (slot-value obj slot-name) (children obj)))

(defmethod copy :after ((ast structured-text) &key &allow-other-keys)
  "Invalidate the ordered-children cache after a copy."
  (slot-makunbound ast 'ordered-children))

(defmethod update-instance-for-different-class
    :after
    ((previous structured-text)
     (current structured-text) &key)
  "Invalidate the ordered-children cache if the instance changes class."
  (slot-makunbound current 'ordered-children))

(defmethod update-instance-for-redefined-class
    :after
    ((instance structured-text)
     added discarded plist &key)
  "Invalidate the ordered-children cache if the class is redefined."
  (when (slot-exists-p instance 'ordered-children)
    (slot-makunbound instance 'ordered-children)))


;;;
;;; Primitive mutation types
;;;
(defclass tree-sitter-mutation (mutation)
  ()
  (:documentation "Specialization of the mutation interface for tree-sitter ast
 software objects."))

;;; Mutations
(defparameter *tree-sitter-mutation-types*
  (cumulative-distribution
   (normalize-probabilities
    '((tree-sitter-insert . 1)
      (tree-sitter-swap . 1)
      (tree-sitter-move . 1)
      (tree-sitter-replace . 1)
      (tree-sitter-cut . 1)
      (tree-sitter-nop . 1))))
  "Cumulative distribution of normalized probabilities of weighted mutations.")

(defparameter *max-targeter-old-tries* 1000
  "Max number of attempts to find a compatible ast.")

(defparameter *max-targeter-new-tries* 100
  "Max number of attempts to find a compatible ast for reuse.")

(defparameter *max-targeter-moveable-tries* 100
  "Max number of attempts to find a compatible moveable target ast pair.")

(defun evolution-candidate-ast-p (ast)
  "Returns true iff the ast is one which we will select for mutations."
  (typep ast '(not (or inner-whitespace comment-ast root-ast))))

(defun evolution-candidate-asts (software &key (filter (constantly t)))
  "Returns list of asts in software genome which are valid mutation candidates."
  (iter (for a in-tree (genome software))
    (when (and (evolution-candidate-ast-p a)
               (funcall filter a))
      (collect a))))

(defmethod mutation-targets ((software tree-sitter)
                             &key (filter nil) (stmt-pool nil))
  "Return a list of target ASTs from STMT-POOL for mutation.

* SOFTWARE software object to query for mutation targets
* FILTER filter AST from consideration when this function returns nil
* STMT-POOL (non-empty) list of ASTs, or a method on SOFTWARE returning a
   non-emptylist of ASTs, or NIL (to indicate default pool)"
  (cond ((consp stmt-pool)
         (if filter (remove-if-not filter stmt-pool) stmt-pool))
        ((null stmt-pool)
         (apply 'evolution-candidate-asts
                software (if filter `(:filter ,filter))))
        (t (if filter
               (remove-if-not filter (funcall stmt-pool software))
               (funcall stmt-pool software)))))

(defun pick-2-replaceable (software)
  (when-let ((asts (evolution-candidate-asts software)))
    ;; need deep copy (tree-copy) to get new serial numbers on new ast
    ;; TODO: remove this tree-copy when functional-trees get copy-on-collision
    ;; support added.
    (do* ((old (random-elt asts) (random-elt asts))
          (new (tree-copy (random-elt asts)))
          (old-tries 0 (+ old-tries 1))
          (new-tries 0)
          (valid #1=(check-ast-replacement (genome software) old new) #1#))
         ((or valid (> new-tries *max-targeter-new-tries*))
          (if valid (list old new)))
      (if (> old-tries *max-targeter-old-tries*)
          (setf new (tree-copy (random-elt asts))
                new-tries (+ new-tries 1)
                old-tries 0)))))

(defun pick-2-insertable (software)
  (when-let ((asts (evolution-candidate-asts software)))
    ;; need deep copy (tree-copy) inserted ast to get new serial numbers
    ;; TODO: remove this tree-copy when functional-trees get copy-on-collision
    ;; support added.
    (do* ((old (random-elt asts) (random-elt asts))
          (new (tree-copy (random-elt asts)))
          (old-tries 0 (+ old-tries 1))
          (new-tries 0)
          (valid #1=(check-ast-insertable (genome software) old new) #1#))
         ((or valid (> new-tries *max-targeter-new-tries*))
          (if valid (list old new)))
      (if (> old-tries *max-targeter-old-tries*)
          (setf new (tree-copy (random-elt asts))
                new-tries (+ new-tries 1)
                old-tries 0)))))

(defun pick-2-swappable (software)
  "Return a random target of two asts which can be swapped in the software."
  (when-let ((asts (evolution-candidate-asts software)))
    ;; since we are doing a swap we don't need a deep copy of either ast
    (do* ((ast1 (random-elt asts) (random-elt asts))
          (ast2 (random-elt asts))
          (ast1-tries 0 (+ ast1-tries 1))
          (ast2-tries 0)
          (valid #1=(check-ast-swappable (genome software) ast1 ast2) #1#))
         ((or valid (> ast2-tries *max-targeter-new-tries*))
          (if valid (list ast1 ast2)))
      (if (> ast1-tries *max-targeter-old-tries*)
          (setf ast2 (random-elt asts)
                ast2-tries (+ ast2-tries 1)
                ast1-tries 0)))))

(defun pick-2-moveable (software)
  (iter (for i from 1 to *max-targeter-moveable-tries*)
    (when-let ((pick (pick-2-swappable software)))
      ;; make sure the old can be cut
      (if (check-ast-cut (genome software) (second pick))
          (leave pick)))))

(defun pick-1-cuttable (software)
  (when-let ((asts (evolution-candidate-asts software)))
    (do* ((old (random-elt asts) (random-elt asts))
          (old-tries 0 (+ old-tries 1))
          (valid #1=(check-ast-cut (genome software) old) #1#))
         ((or valid (> old-tries *max-targeter-old-tries*))
          (if valid (list old))))))

(define-mutation tree-sitter-replace (tree-sitter-mutation)
  ((targeter :initform #'pick-2-replaceable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-swap (tree-sitter-mutation)
  ((targeter :initform #'pick-2-swappable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-cut (tree-sitter-mutation)
  ((targeter :initform #'pick-1-cuttable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-insert (tree-sitter-mutation)
  ((targeter :initform #'pick-2-insertable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-move (tree-sitter-mutation)
  ((targeter :initform #'pick-2-moveable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-nop (tree-sitter-mutation) ())

(defmethod apply-mutation ((root root-ast)
                           (mutation mutation))
  "Wrapper to allow mutation of AST objects by wrapping and unwrapping them in
temporary software objects."
  (genome (apply-mutation (make (ast-language-class root) :genome root)
                          mutation)))

(defmethod apply-mutation :around ((software tree-sitter)
                                   (mutation tree-sitter-mutation))
  (unless (targets mutation)
    (error 'no-mutation-targets
           :obj software
           :text "No suitable ASTs for mutation."))
  (call-next-method))

(defmethod apply-mutation ((software tree-sitter)
                           (mutation tree-sitter-replace))
  (let ((target (targets mutation)))
    (setf (genome software)
          (with (genome software)
                (first target)
                (second target)))
    software))

(defmethod apply-mutation ((software tree-sitter)
                           (mutation tree-sitter-insert))
  (let ((target (targets mutation)))
    (setf (genome software)
          (insert (genome software)
                  (first target)
                  (second target)))
    software))

(defmethod apply-mutation ((software tree-sitter) (mutation tree-sitter-swap))
  (let ((target (targets mutation)))
    (setf (genome software)
          (swap (genome software)
                (first target)
                (second target)))
    software))

(defmethod apply-mutation ((software tree-sitter) (mutation tree-sitter-move))
  (let ((target (targets mutation)))
    (setf (genome software)
          ;; first, remove the ast from its current location, then add it back
          ;; in at new location
          (with (less (genome software)
                      (second target))
                (first target)
                (second target)))
    software))

(defmethod apply-mutation ((software tree-sitter) (mutation tree-sitter-cut))
  (let ((target (targets mutation)))
    (setf (genome software)
          (less (genome software)
                (first target)))
    software))

(defmethod apply-mutation ((software tree-sitter) (mutation tree-sitter-nop))
  (declare (ignorable software mutation))
  software)

(defmethod pick-mutation-type ((obj tree-sitter))
  "Select type of mutation to apply to OBJ."
  (random-pick *tree-sitter-mutation-types*))


;;;
;;; Crossover
;;;
(defmethod select-crossover-points-pool ((a tree-sitter) (b tree-sitter))
  (declare (ignorable b))
  (evolution-candidate-asts a))


;;; AST Construction

;;; BLOTTING:
;;; TODO:
;;;  The current implementation only supports 1:1 blots. This means that the
;;;  blotted subsequence of the source must be of the same length as the original
;;;  subsequence. It will likely be desirable to change this in the future.
;;;  To do this the following should be done:
;;;    - Have blot-out-ranges return as values the ranges to blot out and the
;;;      sequence to replace it with. A value of NIL will default to replacing
;;;      with spaces.
;;;    - insert-blots-into-parse-tree should keep track of the offset of adding
;;;      the blot in both the row and column indices. The row offset should be
;;;      reset on newlines. All ranges in the tree will need to be updated if
;;;      either offset exists.

(defvar *use-blotting* t
  "Controls whether blotting is used to hide problematic sections of source
from the tree-sitter parser.")

(defgeneric blot-out-ranges (superclass source &key)
  (:documentation "Return a list of ranges to blot out sections of SOURCE which
cause problems when parsing it as SUPERCLASS.")
  (:method (superclass source &key &allow-other-keys)
    nil))

(defgeneric blot-out (superclass ranges source &key)
  (:documentation "Return a version of SOURCE with RANGES blotted out.")
  (:method (superclass ranges (source string) &key &allow-other-keys)
    (iter
      (iter:with source-array =
                 (make-array (length source)
                             :initial-contents source
                             :element-type (array-element-type source)))
      (for (start . end) in ranges)
      (iter
        (for i from start to end)
        (symbol-macrolet ((array-i (aref source-array i)))
          ;; NOTE: keep newlines for tree-sitter parse tree ranges.
          (unless (eql array-i #\newline)
            (setf array-i #\space))))
      (finally
       (return
         (coerce source-array (typecase source
                                (base-string 'base-string)
                                (t 'string))))))))

(defun blot-ranges->parse-tree-ranges (ranges source
                                       &aux (row 0) (column 0)
                                         (blot-ranges (flatten ranges))
                                         (blot-ranges-i 0) source-ranges)
  "Convert RANGES from blot ranges to parse tree ranges using SOURCE to
determine the translation."
  ;; NOTE: assumes no overlap of ranges. Overlaps should be merged if this
  ;;       becomes a problem.
  (labels ((incf-row ()
             (incf row)
             (setf column 0))
           (increment (char)
             "Increment row and column based on CHAR."
             (if (eql char #\newline)
                 (incf-row)
                 (incf column)))
           (handle-conversion (i column row)
             "Convert COLUMN, ROW to a parse tree range if I
              is a blot index."
             (when (eql i (car blot-ranges))
               (incf blot-ranges-i)
               (pop blot-ranges)
               (push (list column row) source-ranges)))
           (group-by-two (ranges)
             "Create pairs of two ranges from a flat list of ranges."
             (iter
               (for (start end) on ranges by #'cddr)
               (collect (list start end)))))
    (iter
      (for char in-string source)
      (for i upfrom 0)
      (cond
        ((evenp blot-ranges-i)
         ;; NOTE: inclusive start
         (handle-conversion i column row)
         (increment char))
        (t
         ;; NOTE: exclusive end
         (increment char)
         (handle-conversion i column row)))
      (finally
       (return (group-by-two (reverse source-ranges)))))))

(defun insert-blots-into-parse-tree (parse-tree-ranges parse-tree)
  "Insert blot nodes into PARSE-TREE at all the ranges in PARSE-TREE-RANGES."
  (labels ((point<= (point1 point2)
             "Return T if POINT1 occurs before or at POINT2."
             (let ((line1 (cadr point1))
                   (line2 (cadr point2))
                   (column1 (car point1))
                   (column2 (car point2)))
               (cond
                 ((< line1 line2))
                 ((= line1 line2)
                  (<= column1 column2)))))
           (range-includes (parent-range includes-range)
             "Return T if PARENT-RANGE includes INCLUDES-RANGE."
             (let ((parent-start (car parent-range))
                   (parent-end (cadr parent-range))
                   (includes-start (car includes-range))
                   (includes-end (cadr includes-range)))
               (and (point<= parent-start includes-start)
                    (point<= includes-end parent-end))))
           (range-before-subtree-p (subtree range)
             "Return T if RANGE occurs before SUBTREE."
             (let ((subtree-start (car (parse-tree-range subtree)))
                   (range-end (cadr range)))
               (point<= range-end subtree-start)))
           (subtree-contains-range-p (subtree range)
             "Return T if SUBTREE contains RANGE."
             (range-includes (parse-tree-range subtree) range))
           (get-ranges (predicate)
             "Collect ranges that occur before CHILD. These ranges are dropped
              from the stack."
             (lret ((before-ranges (take-while predicate parse-tree-ranges)))
               (setf parse-tree-ranges (drop (length before-ranges)
                                             parse-tree-ranges))))
           (create-blot-nodes (predicate)
             "Create blot nodes for each blot range that satisfies PREDICATE.
              These ranges are removed from the parse-tree-ranges stack."
             (mapcar
              (op `(:blot ,_ nil))
              (get-ranges predicate)))
           (insert-parse-tree-ranges (parse-tree)
             "Recursively insert the blotted ranges back into PARSE-TREE."
             (if (not (and parse-tree-ranges
                           (subtree-contains-range-p parse-tree
                                                     (car parse-tree-ranges))))
                 parse-tree
                 (iter
                   (for child in (parse-tree-children parse-tree))
                   ;; NOTE: if there are performance issues, we can push them
                   ;;       and reverse at the end.
                   (appending (create-blot-nodes {range-before-subtree-p child})
                              into new-children)
                   (collect (insert-parse-tree-ranges child) into new-children)
                   (finally
                    (return
                      (list (car parse-tree)
                            (cadr parse-tree)
                            (append new-children
                                    (create-blot-nodes
                                     {subtree-contains-range-p
                                      parse-tree})))))))))
    (insert-parse-tree-ranges parse-tree)))

(defgeneric default-whitespace-style (ast)
  (:documentation "Get the default whitespace style for AST.")
  (:method ((ast t)) nil)
  (:method ((software parseable))
    (default-whitespace-style (genome software))))

(defgeneric whitespace-between/parent (parent style ast1 ast2)
  (:method-combination standard/context)
  (:method :context (parent s ast1 ast)
    (let ((result (call-next-method)))
      (etypecase result
        (string result)
        (character (string result)))))
  (:method :around (parent style (ast1 alternative-ast) ast2
                    &aux (representative-ast
                          (get-representative-ast ast1 parent)))
    (if (eq representative-ast ast1)
        (call-next-method)
        (whitespace-between/parent parent style representative-ast ast2)))
  (:method :around (parent style ast1 (ast2 alternative-ast)
                    &aux (representative-ast
                          (get-representative-ast ast2 parent)))
    (if (eq representative-ast ast2)
        (call-next-method)
        (whitespace-between/parent parent style ast1 representative-ast)))
  (:method ((parent t) style ast1 ast2)
    (whitespace-between style ast1 ast2))
  ;; No whitespace inside a terminal symbol.
  (:method ((parent terminal-symbol) s ast1 ast2)
    "")
  ;; No whitespace after a terminal symbol in a unary AST.
  (:method ((parent unary-ast) s (ast1 terminal-symbol) ast2)
    "")
  (:method (p s (ast1 ast) (ast2 string))
    (if (notevery #'whitespacep ast2)
        (whitespace-between/parent p s ast1 (make-keyword ast2))
        ""))
  (:method (p s (ast1 string) (ast2 ast))
    (if (notevery #'whitespacep ast1)
        (whitespace-between/parent p s (make-keyword ast1) ast2)
        ""))
  (:method (p s (ast1 inner-whitespace) ast2)
    "")
  (:method (p s ast1 (ast2 inner-whitespace))
    "")
  (:method ((parent computed-text) s ast1 ast2) ""))

(defgeneric whitespace-between (style ast1 ast2)
  (:method (s ast1 ast2) "")
  (:method (s (ast1 null) ast2) "")
  (:method (s (ast1 (eql :||)) ast2) "")
  (:method (s ast1 (ast2 null)) "")
  (:method (s ast1 (ast2 (eql :||))) "")
  ;; Sensible defaults for most (all?) programming languages.
  (:method (s (ast1 symbol) (ast2 ast))
    "No whitespace after an opening delimiter or blank."
    (if (some (op (string$= _ ast1))
              #.(string+ "([{" whitespace))
        "" " "))
  (:method (s (ast1 null) (ast2 ast)) "")
  (:method (s (ast1 ast) (ast2 symbol))
    "No whitespace before a closing delimiter or a comma."
    (if (some (op (string^= _ ast2)) ")]},") "" " "))
  (:method (s (ast1 ast) (ast2 null)) "")
  (:method (s (ast1 ast) (ast2 ast)) " ")
  (:method (s (ast1 inner-whitespace) ast2) "")
  (:method (s ast1 (ast2 inner-whitespace)) "")
  (:documentation "Return a string of whitespace that should occur between
AST1 and AST2.

STYLE can be used to control whitespace based on a standard format or
on the calculated format of a particular file."))

(defgeneric get-style-indentation (style software ast &key)
  (:method (style software (ast indentation) &key) (indent-children ast))
  (:documentation "Return a the indent-children value for AST in SOFTWARE for
STYLE."))

(defgeneric get-style-indentation-adjustment
    (style software ast parent &key parents)
  (:method (style software (ast indentation) parent &key parents)
    (declare (ignorable parents))
    (indent-adjustment ast))
  (:documentation "Return a value to set the indent-adjustment slot of AST."))

(defun empty-sequence-p (x)
  "Return T if X is an empty sequence."
  (and (sequencep x)
       (emptyp x)))

(defun copy-with-surrounding-text (copy-node reference-node)
  "Copy COPY-NODE with the surrounding text of REFERENCE-NODE. This is done
on a per slot basis, and the copy of text only happens if the relevant slot
doesn't already have a non-empty value and the value in REFERENCE-NODE isn't
also empty. This will prevent unnecessary copying."
  (let* ((before-copy (before-text copy-node))
         (after-copy (after-text copy-node))
         (before-reference (before-text reference-node))
         (after-reference (after-text reference-node))
         (empty-before-copy (empty-sequence-p before-copy))
         (empty-after-copy (empty-sequence-p after-copy))
         (not-empty-before-reference
          (not (empty-sequence-p before-reference)))
         (not-empty-after-reference
          (not (empty-sequence-p after-reference))))
    (cond
      ((and empty-before-copy empty-after-copy
            not-empty-before-reference not-empty-after-reference)
       (copy copy-node :before-text before-reference
                       :after-text after-reference))
      ((and empty-before-copy not-empty-before-reference)
       (copy copy-node :before-text before-reference))
      ((and empty-after-copy not-empty-after-reference)
       (copy copy-node :after-text after-reference))
      (t copy-node))))

(defmethod with :around ((ast structured-text) (value1 structured-text)
                         &optional value2)
  (if  (typep value2 'structured-text)
       (call-next-method ast value1 (copy-with-surrounding-text value2 value1))
       (call-next-method)))

(defmethod with :around ((ast structured-text) (value1 list) &optional value2)
  (if-let* ((structuredp (typep value2 'structured-text))
            (old (@ ast value1))
            (structuredp (typep old 'structured-text)))
    (call-next-method ast value1 (copy-with-surrounding-text value2 old))
    (call-next-method)))

(defmethod mapcar :around (fn (node structured-text) &rest more)
  "Give `mapcar' the same behavior as WITH on structured-text.
That is, inherit the before and after text of the node being replaced."
  (declare (ignore more))
  (let ((fn (ensure-function fn)))
    (call-next-method
     (lambda (ast)
       (when-let (result (funcall fn ast))
         (if (and (typep result 'structured-text)
                  (typep ast 'structured-text))
             (copy-with-surrounding-text result ast)
             result)))
     node)))

(defgeneric patch-whitespace (ast &key)
  (:documentation "Destructively patch whitespace on AST by adding a
  space to the before-text and after-text slots that need it.")
  (:method ((ast t) &key) ast)
  (:method ((ast terminal-symbol) &key) ast)
  (:method ((ast structured-text) &key (style (default-whitespace-style ast))
                                    software
                                    prettify
                                    (recursive t))
    "Destructively patch whitespace on AST by adding a
space to the before-text and after-text slots that need it.

:software can be provided for to help determine indentation that
depends on parent ASTS.

:prettify will always set the relevant before/after text as opposed to only
setting it if it isn't already set."
    ;; TODO: add functionality to create inner-whitespace objects and store them
    ;;       in the relevant inner-asts slot. This will be useful for some ASTs
    ;;       which have two terminals in a row that require whitespace between
    ;;       them, such as python-yield with 'yield' and 'from'.
    (labels ((update-indentation (ast parents)
               "Update the indentation of AST."
               (let ((indentation (get-style-indentation style software ast))
                     (indentation-adjustment
                       (get-style-indentation-adjustment
                        style software ast (car parents) :parents parents)))
                 (symbol-macrolet ((indent-children (indent-children ast))
                                   (indent-adjustment (indent-adjustment ast)))
                   (cond-every
                     ((or prettify
                          (and indentation (not indent-children)))
                      (setf indent-children indentation))
                     ((or prettify
                          (and indentation-adjustment (not indent-adjustment)))
                      (setf indent-adjustment indentation-adjustment))))))
             (patch-whitespace (ast parents)
               (update-indentation ast parents)
               (iter
                 (for item in (remove-if
                               (conjoin #'stringp #'emptyp)
                               (cdr (butlast (output-transformation ast)))))
                 (for previous-item previous item)
                 (for white-space =
                      (whitespace-between/parent
                       ast
                       style
                       previous-item
                       item))
                 (when (and recursive
                            (typep item '(and tree-sitter-ast
                                          (not terminal-symbol)))
                            (not (typep ast 'computed-text)))
                   (patch-whitespace item (cons ast parents)))
                 (cond
                   ((and (not prettify) (emptyp white-space)))
                   ((typep item 'structured-text)
                    (when (or prettify (emptyp (before-text item)))
                      (if (typep previous-item 'structured-text)
                          (when (emptyp (after-text previous-item))
                            (setf (before-text item) white-space))
                          (setf (before-text item) white-space))))
                   ((typep previous-item 'structured-text)
                    (when (or prettify (emptyp (after-text previous-item)))
                      (setf (after-text previous-item) white-space))))
                 (finally (return ast)))))
      (declare (dynamic-extent #'patch-whitespace))
      (patch-whitespace ast (and software (get-parent-ast software ast))))))

(defgeneric prettify-software (style software &key ast)
  (:documentation "Return a copy of SOFTWARE with its whitespace inserted based
on STYLE.

:AST can be provided to prettify an AST locally. It will still patch its
parent in the cases where the parent indentation changes.

If STYLE is T, it means to use the value of `default-whitespace-style'
on SOFTWARE.")
  (:method ((style (eql t)) software &key (ast (genome software)))
    (prettify-software (default-whitespace-style software)
                       software
                       :ast ast))
  (:method (style software &key (ast (genome software)))
      (lret ((ast-path (ast-path software ast))
             (new-software (copy software :genome (tree-copy (genome software)))))
        (when-let ((parent (lookup new-software (butlast ast-path))))
          (setf (indent-children parent)
                (get-style-indentation style software parent)))
        (patch-whitespace
         (lookup new-software ast-path)
         :allow-other-keys t
         :recursive t :prettify t :software new-software :style style))))

(defmacro define-empty-whitespace-methods ((&key (style t))
                                           &body pairs)
  `(progn
     ,@(iter (for (x y) in (batches pairs 2 :even t))
             (let ((x (if (keywordp x) `(eql ,x) x))
                   (y (if (keywordp y) `(eql ,y) y)))
               (collect `(defmethod whitespace-between ((style ,style)
                                                        (x ,x)
                                                        (y ,y))
                           ""))))))

(define-empty-whitespace-methods (:style t)
  identifier-ast parameters-ast
  identifier-ast arguments-ast)

(defgeneric prefer-child-indentation-p (ast)
  (:method (ast) nil)
  (:documentation " Return t if AST is a type that prefers its indentation be
attached to its indent-children slot as opposed to its parent's. This is
primarily for process-indentation and working around python-block which is a
unique edge case."))

(defun process-indentation (root &aux indentation-carryover indentation-ast)
  "Process the indentation of ROOT such that indentation information is stored in
the indentation slots."
  (labels ((adjusted-spaces-from-tabs
               (subseq &aux (tab-count (count #\tab subseq)))
             "Return the number of spaces that are used for tabs minus
              the number of tabs."
             (- (* tab-count *spaces-per-tab*)
                tab-count))
           (starts-with-indentation-p (string)
             "If STRING starts with indentation, return
              the first position without indentation."
             (when indentation-carryover
               (mvlet ((start end (scan "^[ \\t]+" string)))
                 (declare (ignorable start))
                 end)))
           (ends-with-newline-p (string)
             "If STRING ends with a newline and optionally indentation,
              return the position of the newline."
             (iter
               (for i from (1- (length string)) downto 0)
               (for character = (aref string i))
               (when (eql character #\newline)
                 (return i))
               (while (member character '(#\space #\tab)))))
           (backpatch-indent-adjustment-slots (asts indentation current-ast)
             "Backpatch items in ASTS such that any item before CURRENT-AST
              has its indent-adjustment slot set to INDENTATION less than its
              current value."
             ;; NOTE: this function is necessary for erratic indentation that
             ;;       is heavily nested in an AST.
             (mapc
              (lambda (ast)
                ;; The idea here is that, after we have transferred
                ;; indentation *i* from the before-text of child *n*
                ;; to the parent, we need to go back and remove *i*
                ;; spaces of indentation from the children 0..n-1.
                ;; If there is no explicit indentation adjustment on a
                ;; child, assume the current value is 0.
                (symbol-macrolet ((indent-adjustment (indent-adjustment ast)))
                  (if indent-adjustment
                      (decf indent-adjustment indentation)
                      (setf indent-adjustment (- indentation)))))
              (ldiff asts (member current-ast asts))))
           (backpatch-indent-children-slots (ast indentation)
             "Backpatch any child of AST that already has a value in the
              indent-children slot such that its value is INDENTATION less
              than its current."
             ;; NOTE: this function is necessary for erratic indentation that
             ;;       is heavily nested in an AST.
             (mapc
              (lambda (child)
                (symbol-macrolet ((indent-children (indent-children child)))
                  (when indent-children
                    (setf indent-children (- indent-children indentation)))))
              (children ast)))
           (update-indentation-slots
               (ast parents indentation text
                &aux (parent (car parents))
                  (adjusted-indentation
                   ;; total - inherited
                   (- (+ indentation indentation-carryover)
                      (get-indentation-at ast parents)))
                  (only-indentation? (not (scan "[^ \\t\\n]" text))))
             "Patch either AST or PARENT to have INDENTATION for the
              relevant line or lines."
             (symbol-macrolet ((indent-children-parent (indent-children parent))
                               (indent-adjustment (indent-adjustment ast))
                               (indent-children-current (indent-children ast)))
               (cond
                 ;; Avoid wasting the newline on empty text or indentation
                 ;; before reaching a child.
                 ((and only-indentation?
                       indentation-ast
                       (ancestor-of-p root indentation-ast ast)))
                 ;; Don't indent if the current AST already has an
                 ;; indentation slot assigned as this will result in
                 ;; back-propagation of indentation.
                 (indent-children-current
                  ;; In this case, we need to reset the indentation if
                  ;; any non-indentation is in this string since the reset
                  ;; will not happen anywhere after this.
                  ;;
                  ;; NOTE: because of this, certain forms, such as compound
                  ;;       statements which represent pairs of curly braces, will
                  ;;       not be reproduced exactly if they don't line up
                  ;;       with each other when on their own lines. There is no
                  ;;       way around this besides having ASTs that represent the
                  ;;       individual tokens or adding another list to keep track
                  ;;       of indentation at the interleaved-text level.
                  (unless only-indentation?
                    (setf indentation-carryover nil
                          indentation-ast nil)))
                 ((prefer-child-indentation-p ast)
                  (backpatch-indent-children-slots ast adjusted-indentation)
                  (setf indent-children-current adjusted-indentation
                        indentation-carryover nil
                        indentation-ast nil))
                 ((and parent (not indent-children-parent))
                  (backpatch-indent-adjustment-slots
                   (children parent) adjusted-indentation ast)
                  (setf indent-children-parent adjusted-indentation
                        indentation-carryover nil
                        indentation-ast nil))
                 ;; Backpatching shouldn't be needed for the following.
                 (t (setf indent-adjustment adjusted-indentation
                          indentation-carryover nil
                          indentation-ast nil)))))
           (patch-leading-indentation
               (text ast parents
                &key before-text
                &aux (indentation (starts-with-indentation-p text))
                  (not-empty-string-p (not (emptyp text))))
             "Return TEXT with the leading indentation removed and
              the relevant indentation slot updated."
             (cond-let leading-indentation
               ((and indentation
                     (length= indentation text)
                     not-empty-string-p)
                (setf indentation-carryover
                      (+ indentation-carryover
                         indentation
                         (adjusted-spaces-from-tabs
                          (subseq text 0 indentation))))
                "")
               ((and not-empty-string-p
                     (ends-with-newline-p text))
                (prog1
                    ;; Treat it like trailing indentation.
                    (patch-trailing-indentation text ast)
                  ;; But update it if it's before-text.
                  (when before-text
                    ;; NOTE: pass 0 in becase indentation-carryover should be
                    ;;       set if needed at this point.
                    (update-indentation-slots ast parents 0 text))))
               ((or indentation
                    ;; NOTE: check if text exists here so that
                    ;;       the inherited indentation can be
                    ;;       set to 0. This prevents back-propagation
                    ;;       of indentation to previous siblings.
                    (and indentation-carryover
                         ;; TODO: add carriage return here?
                         (scan "[^ \\t\\n]" text)))
                (update-indentation-slots
                 ast parents (+ leading-indentation
                                (adjusted-spaces-from-tabs
                                 (subseq text 0 leading-indentation)))
                 text)
                (subseq text leading-indentation))
               (t text)))
           (patch-trailing-indentation (text ast)
             "Return TEXT with the trailing indentation removed and
              indentation-carryover updated."
             (cond-let trailing-indentation
               ((ends-with-newline-p text)
                (setf indentation-carryover
                      (+ (- (length text) (1+ trailing-indentation))
                         (adjusted-spaces-from-tabs
                          (subseq text trailing-indentation)))
                      indentation-ast ast)
                (subseq text 0 (1+ trailing-indentation)))
               (t text)))
           (patch-internal-indentation (text)
             "Return TEXT where all newlines proceeded by indentation
              are replaced with a newline."
             (cl-ppcre:regex-replace-all "\\n[ \\t]+" text #.(format nil "~%")))
           (patch-text (text ast parents)
             "Patch TEXT such that it useable for inherited indentation.
              Updates AST and PARENTS slots if necessary."
             (patch-trailing-indentation
              (patch-leading-indentation text ast parents)
              ast))
           (process-indentation*
               (ast &optional parents
                &aux (output-transformation (output-transformation ast)))
             "Process the text of AST such that its indentation
              is in the indentation slots."
             (setf (before-text ast)
                   (patch-internal-indentation
                    (patch-leading-indentation (car output-transformation)
                                               ast parents :before-text t)))
             (mapc (lambda (output)
                     (cond
                       ((stringp output)
                        (patch-text output ast parents))
                       ((indentablep output)
                        (process-indentation* output (cons ast parents)))
                       (t
                        ;; NOTE: this assumes that if it isn't indentable,
                        ;;       it will have some non-whitespace characters.
                        ;;       It also assumes that there won't be any trailing
                        ;;       whitespace except possibly a newline.
                        (setf indentation-carryover
                              (when-let* ((text (source-text output))
                                          (newline (ends-with-newline-p text)))
                                ;; NOTE: only set to 0 if the very last thing
                                ;;       is a newline.
                                (when (length= (1+ newline) text)
                                  0))))))
                   (cdr (butlast output-transformation)))
             (setf (after-text ast)
                   (patch-internal-indentation
                    (patch-trailing-indentation (lastcar output-transformation) ast)))))
    (process-indentation* root)
    root))

(defun find-terminal-symbol-class (prefix class-name)
  ;; Check for a '-terminal first in case there's a name overlap.
  (when (symbolp class-name)
    (let ((ps (symbol-name prefix))
          (cns (symbol-name class-name)))
      (let ((terminal-with (intern (concatenate 'string ps "-" cns "-TERMINAL")
                                   'sel/sw/ts)))
        (if (find-class terminal-with nil)
            terminal-with
            (let ((terminal-without (intern (concatenate 'string ps "-" cns)
                                            'sel/sw/ts)))
              (when (find-class terminal-without nil)
                terminal-without)))))))

(defun terminal-symbol-class-p (prefix class-name)
  "Return true if CLASS-NAME inherits from the terminal symbol mix-in."
  (when-let ((class (find-terminal-symbol-class prefix class-name)))
    (subtypep class 'terminal-symbol)))

(defun convert-parse-tree
    (spec prefix superclass string
     &key computed-text-parent-p line-octets-cache variation-point-tree-parent-p
     &allow-other-keys
     &aux (*package* (symbol-package superclass))
       (class (symbolicate prefix '-
                           (let ((type (car spec)))
                             ;; The form can either be
                             ;; - :type
                             ;; - (:slot-name :type)
                             (if (listp type)
                                 (cadr type)
                                 type))))
       (instance (make-instance
                  (get-choice-expansion-subclass class spec)
                  :annotations
                  (when computed-text-parent-p
                    `((:range-start ,(caadr spec))
                      (:range-end . ,(cdadr spec))))))
       (error-p (eql class (symbolicate prefix '-error)))
       (line-octets
        (or line-octets-cache
            (map
             'vector
             #'string-to-octets
             (lines string :keep-eols t))))
       (computed-text-p (typep instance 'computed-text))
       (extra-asts-type (cons 'or (extra-asts-symbols prefix))))
  "Convert SPEC from a parse tree into an instance of SUPERCLASS."
  (labels ((safe-subseq
               (start end
                &aux (start-loc
                      (make-instance
                       'source-location
                       :line (cadr start) :column (car start)))
                  (end-loc
                   (make-instance
                    'source-location
                    :line (cadr end) :column (car end))))
             "Return STRING in the range [START, END) or an empty string if
              the offsets are invalid."
             (if (and
                  start
                  end
                  (source-< start-loc end-loc))
                 ;; Canon-string will convert strings to base-strings
                 ;; if possible, and canonicalize them with a weak
                 ;; value hash table.  Base-strings themselves
                 ;; cannot be used in templates for matching in
                 ;; trivia, so there we convert them back to
                 ;; (array character (*)) to avoid the bug.
                 (canon-string
                  (octets-to-string
                   (source-range-subseq
                    line-octets
                    (make-instance 'source-range :begin start-loc :end end-loc))))
                 ""))
           (get-start (ast)
             "Return the start offset into STRING from the AST representation."
             (car (ast-annotation ast :range-start)))
           (get-end (ast)
             "Return the end offset into STRING from the AST representation."
             (car (ast-annotation ast :range-end)))
           ;; NOTE: this may be useful for computed-text ASTs.
           (ranges (children from to)
             "Return the offsets of the source text ranges between CHILDREN.
              And drop their annotations."
             ;; TODO: at some point, refactor this.
             ;;       It's a bit of a hack to reuse existing code.
             (iter
               (for child in children)
               (for previous-child previous child)
               (collect (cons (if previous-child (get-end previous-child) from)
                              (get-start child))
                 into ranges)
               (when previous-child
                 (setf (slot-value previous-child 'annotations)
                       (adrop '(:range-start :range-end)
                              (slot-value previous-child 'annotations))))
               (finally
                (return
                  (prog1
                      (append ranges (list (cons (get-end child) to)))
                    (setf (slot-value child 'annotations)
                          (adrop '(:range-start :range-end)
                                 (slot-value child 'annotations))))))))
           ;; TODO: refactor as this function is used under a different
           ;;       name below.
           (skip-terminal-field-p (field-spec slot-info
                                   &aux (range (cadr field-spec)))
             "Return T if FIELD-SPEC represents a terminal symbol that shouldn't
              appear in the resulting AST."
             (cond
               ;; all terminals should be stored under a variation point to give
               ;; maximum control to users that want to modify the variation
               ;; point trees.
               (variation-point-tree-parent-p
                ;; NOTE: we actually do want to skip zero-width tokens.
                (when (equal (car range) (cadr range))
                  t))
               ;; Has an associated slot or has children
               ((or (listp slot-info) (caddr field-spec)) nil)
               (t (terminal-symbol-class-p prefix slot-info))))
           (get-converted-fields ()
             "Get the value of each field after it's been converted
              into an AST."
             (iter
               ;; Keep track of all groupings of internal ASTs that are dropped
               ;; between two terminal tokens. NIL should be inserted when
               ;; nothing is dropped between two terminals.
               (iter:with internal-asts-stack)
               (iter:with comment-error-and-whitespace-stack)
               ;; Keep track of the last non-comment/error AST seen. It is used
               ;; for populating 'after' comments.
               (iter:with previous-field)
               (for field in (caddr spec))
               (for slot-info = (car field))
               (for i upfrom 0)
               (when (skip-terminal-field-p field slot-info)
                 (cond
                   (previous-field
                    (setf (slot-value previous-field 'after-asts)
                          (reverse comment-error-and-whitespace-stack)))
                   (comment-error-and-whitespace-stack
                    (push comment-error-and-whitespace-stack
                          internal-asts-stack))
                   ;; Don't consider the initial AST as the whitespace before it
                   ;; should be part of the before text of its parent.
                   ((not (= i 0))
                    (push nil internal-asts-stack)))
                 ;; Reset the converted-field so that comments aren't pushed back
                 ;; to the last AST that wasn't a terminal symbol which could
                 ;; have been proceded by terminal symbols.
                 (setf previous-field nil
                       comment-error-and-whitespace-stack nil)
                 (next-iteration))
               (for converted-field =
                    (convert superclass field
                             :string-pass-through string
                             :computed-text-parent-p computed-text-p
                             :line-octets-cache line-octets
                             :variation-point-tree-parent-p
                             (member
                              (if (consp slot-info)
                                  (cadr slot-info)
                                  slot-info)
                              '(:error-tree
                                :source-text-fragment-tree))))
               ;; cl-tree-sitter appears to put the
               ;; slot name first unless the list goes
               ;; into the children slot.
               (cond
                 ((and (listp slot-info) (not error-p))
                  (setf (slot-value converted-field 'before-asts)
                        (reverse comment-error-and-whitespace-stack)
                        comment-error-and-whitespace-stack nil
                        previous-field converted-field)
                  (collect (list (car slot-info)
                                 converted-field)
                    into fields))
                 ;; Ignore inner whitespace in computed-text nodes as it
                 ;; will be handled regardless.
                 ((and computed-text-p
                       (typep converted-field 'inner-whitespace)))
                 ((and (not computed-text-p)
                       (typep converted-field extra-asts-type))
                  ;; NOTE: this won't put the comment in the children slot
                  ;;       when it's allowed. There may need to be a function
                  ;;       that signals their ability to be stored there if
                  ;;       that functionality is ever desired.
                  (push converted-field comment-error-and-whitespace-stack))
                 (t
                  (setf (slot-value converted-field 'before-asts)
                        (reverse comment-error-and-whitespace-stack)
                        comment-error-and-whitespace-stack nil
                        previous-field converted-field)
                  (collect converted-field into children)))
               (finally
                (cond
                  (previous-field
                   (setf (slot-value previous-field 'after-asts)
                         (reverse comment-error-and-whitespace-stack)))
                  (t
                   (push comment-error-and-whitespace-stack
                         internal-asts-stack)))
                (return
                  (values
                   (if children
                       (push `(:children ,children) fields)
                       fields)
                   (reverse internal-asts-stack))))))
           (merge-same-fields (field-list)
             "Merge all fields that belong to the same slot.
              This is used for setting slots with an arity of 0."
             (mapcar
              (lambda (grouping)
                (apply #'append
                       (list (caar grouping))
                       (mapcar #'cdr grouping)))
              (assort field-list :key #'car)))
           (categorize-children (children)
             "Categorize CHILDREN according to `child-category'."
             (mapcar
              (lambda (child &aux (type (car child)))
                (the child-category
                     (cond
                       ((terminal-symbol-class-p prefix type) :terminal)
                       ((member type (extra-asts prefix))
                        :extra-ast)
                       (t :ast))))
              children))
           (drop-before/after-asts (children)
             "Drop ASTs in CHILDREN which are stored in the before or
              after slots."
             (let ((category->child-runs
                     (runs (mapcar #'cons
                                   (categorize-children children)
                                   children)
                           :key #'car)))
               ;; NOTE: category->child-runs is a list of cons which map a
               ;;       category to a specification for a child.
               ;;
               ;;       Any :extra-ast run which has an :ast on either side is
               ;;       dropped as they will be stored in before or after slots.
               (iter
                 (for terminal-boundary-flag initially nil
                      then (eql previous-key :terminal))
                 (for current in category->child-runs)
                 (for previous previous current)
                 (for current-key = (caar current))
                 (for previous-key = (caar previous))
                 (ecase-of (or null child-category) previous-key
                   ((:terminal :ast) (appending (mapcar #'cdr previous)
                                                into result))
                   (:extra-ast
                    (unless (or (not terminal-boundary-flag)
                                (eql current-key :ast))
                      (appending (mapcar #'cdr previous)
                                 into result)))
                   ((nil)))
                 (finally
                  (unless (and (eql current-key :extra-ast)
                               (eql previous-key :ast))
                    (return (append result (mapcar #'cdr current))))))))
           (set-inner-ast-slots (inner-asts stack-directives children
                                 &aux (stack-directive (car stack-directives))
                                   (grouping (car inner-asts))
                                   (child (car (car children))))
             "Set the inner-asts slots in instance that correspond to
              INNER-ASTS and STACK-DIRECTIVES.
              STACK-DIRECTIVES is a list with elements of type
              `stack-directive'."
             ;; NOTE: this is slightly complicated due to how the information
             ;;       has been passed around.
             ;;       INNER-ASTS is a list of groupings of inner-asts. This
             ;;       has no information attached to it in regards to which slot
             ;;       it should be stored in.
             ;;       STACK-DIRECTIVES contains all of the directives needed to
             ;;       traverse CHILDREN and determine which internal-asts slot
             ;;       to place the next inner-asts grouping in. It's, more or
             ;;       less, a road map from match-parsed-children which gives
             ;;       enough information to determine if an internal-asts slot
             ;;       should be matched with NULL instead of an inner-ast
             ;;       grouping. This matching wasn't done in
             ;;       match-parsed-children since it strips inner-asts before
             ;;       matching. Thus, it is done here.
             (symbol-macrolet ((slot-value
                                 (slot-value instance stack-directive)))
               (etypecase-of stack-directive stack-directive
                 (null)
                 ((cons number number)
                  (set-inner-ast-slots
                   (nthcdr (cdr stack-directive) inner-asts)
                   (cdr stack-directives)
                   (nthcdr (car stack-directive) children)))
                 (number
                  (set-inner-ast-slots inner-asts
                                       (cdr stack-directives)
                                       (nthcdr stack-directive children)))
                 (symbol
                  (if (member child (extra-asts prefix))
                      (let ((value
                              (if (cdr grouping)
                                  ;; Store multiple ASTs in a wrapper AST so
                                  ;; that they continue to work with functional
                                  ;; trees. This notably occurs when multiple
                                  ;; comments are between two terminal tokens.
                                  (list
                                   (make-instance 'inner-parent
                                                  :children (reverse grouping)))
                                  grouping)))
                        (setf slot-value (append slot-value value))
                        (set-inner-ast-slots
                         (cdr inner-asts) (cdr stack-directives) (cdr children)))
                      (set-inner-ast-slots
                       inner-asts (cdr stack-directives) children))))))
           (set-slot-values (slot-values inner-asts)
             "Set the slots in instance to correspond to SLOT-VALUES."
             (when (and (slot-exists-p instance 'json-rule)
                        (not (typep instance 'computed-text)))
               (set-inner-ast-slots
                inner-asts
                (nth-value
                 1
                 (match-parsed-children
                  prefix (json-rule instance) (pruned-rule instance)
                  (slot-usage instance) spec))
                (drop-before/after-asts (caddr spec))))
             (mapc
              (lambda (list)
                (setf (slot-value
                       instance (translate-to-slot-name (car list) prefix))
                      (if (null (cddr list))
                          (cadr list)
                          (cdr list))))
              slot-values))
           (set-surrounding-text ()
             "Set the before and after slots of instance."
             (unless computed-text-parent-p
               (let* ((before (car (cadddr spec)))
                      (after (cadr (cadddr spec)))
                      (start (car (cadr spec)))
                      (end (cadr (cadr spec)))
                      (before-text
                        (and before start (safe-subseq before start)))
                      (after-text (and end after (safe-subseq end after))))
                 (unless (emptyp before-text)
                   (setf (before-text instance) before-text))
                 (unless (emptyp after-text)
                   (setf (after-text instance) after-text)))))
           (set-text (&aux (from (car (cadr spec)))
                        (to (cadr (cadr spec))))
             "Set the text slot in instance if it needs set."
             (when computed-text-p
               (if-let* ((children (children instance))
                         (text-fragments
                          (mapcar (lambda (range)
                                    (destructuring-bind (from . to) range
                                      (make-instance
                                       'text-fragment
                                       :text (safe-subseq from to))))
                                  (ranges children from to))))
                 (setf (slot-value instance 'children)
                       (iter
                         (while (and children text-fragments))
                         (collect (pop text-fragments) into result)
                         (collect (pop children) into result)
                         (finally (return (append result text-fragments)))))
                 ;; Else set it to everything in the range.
                 (setf (text instance) (safe-subseq from to)))))
           (update-slots-based-on-arity ()
             "Update any slot in instance that needs to be converted to a list
              to match its arity."
             (mapc
              (lambda (slot-arity
                       &aux (slot (car slot-arity)))
                (symbol-macrolet ((slot-value (slot-value instance slot)))
                  (unless (listp slot-value)
                    (setf slot-value (list slot-value)))))
              (remove-if-not {eql 0} (slot-value instance 'child-slots)
                             :key #'cdr))))
    ;; Don't store the ASTs inside errors since they're likely not useful.
    (mvlet ((converted-fields inner-asts (and (not error-p)
                                              (get-converted-fields))))
      (set-slot-values (merge-same-fields converted-fields) inner-asts))
    (set-surrounding-text)
    (set-text)
    (update-slots-based-on-arity)
    instance))

;; Make inline to save stack space.
(declaim (inline convert-spec))
(defun convert-spec (spec prefix superclass
                     &aux (package (symbol-package superclass))
                       (class (aget :class spec)))
  "Convert SPEC into an ast of type SUPERCLASS. PREFIX is used to find the
correct class name for subclasses of SUPERCLASS."
  (declare (optimize (speed 3)))
  (lret ((instance
          (if (eq :text-fragment class)
              (make-instance 'text-fragment)
              (make-instance
               (symbol-cat-in-package
                package
                prefix
                (if (stringp class)
                    (nest (make-keyword)
                          (string-upcase)
                          (translate-camelcase-name)
                          class)
                    class))))))
    (iter
      (iter:with child-types = (child-slots instance))
      (iter:with annotations = nil)
      (for (slot . value) in spec)
      (declare (symbol slot))
      (when (eq slot :class)
        (next-iteration))
      (for key = (intern (string slot) package))
      (for translated-key = (translate-to-slot-name key prefix))
      (cond
        ((slot-exists-p instance translated-key)
         ;; TODO: look into a better way to do this for structured text?
         (setf (slot-value instance translated-key)
               (if-let ((spec (find translated-key child-types :key #'car)))
                 (ematch spec
                   ;; (cons key arity)
                   ((cons _ 1) (convert superclass value))
                   ((cons _ 0) (iter (for item in value)
                                     (collect (convert superclass item)))))
                 value)))
        ;; Account for slots in superclasses.
        ((slot-exists-p instance key) (setf (slot-value instance key) value))
        (t (push (cons slot value) annotations)))
      (finally
       (with-slots ((annotations-slot annotations)) instance
         (when annotations
           (setf annotations-slot (append annotations annotations-slot))))))))

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (spec tree-sitter-ast)
                     &key &allow-other-keys)
  "Pass thru an existing tree-sitter AST. This useful in manual AST creation."
  spec)

(defmethod convert :around
    ((to-type (eql 'tree-sitter-ast)) (spec list)
     &key patch-whitespace &allow-other-keys)
  (lret ((ast (call-next-method)))
    (when patch-whitespace
      (patch-whitespace ast))))

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (spec list)
                    &rest args
                    &key superclass string-pass-through
                    &allow-other-keys)
  "Create a TO-TYPE AST from the SPEC (specification) list.
See CONVERT-TO-LIST-SPECIFICATION in tree-sitter-dev for easy generation of
list specifications."
  (with-serial-number-block ()
    (if string-pass-through
        (apply
         #'convert-parse-tree
         spec (get-language-from-superclass superclass) superclass
         string-pass-through
         args)
        (convert-spec
         spec (get-language-from-superclass superclass) superclass))))

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (string string)
                    &key superclass
                      (line-octets-cache
                       (map
                        'vector
                        #'string-to-octets
                        (lines string :keep-eols t)))
                    &allow-other-keys)
  (process-indentation
   (convert
    to-type
    (convert 'parse-tree
             string
             :superclass superclass
             :line-octets-cache line-octets-cache)
    :superclass superclass
    :string-pass-through string
    :line-octets-cache line-octets-cache)))

(defmethod convert ((to-type (eql 'parse-tree)) (string string)
                    &key superclass
                      (line-octets-cache
                       (map
                        'vector
                        #'string-to-octets
                        (lines string :keep-eols t)))
                    &allow-other-keys
                    &aux (prefix (get-language-from-superclass superclass)))
  (labels
      ((ensure-beginning-bound (parse-tree)
         "Desctructively ensures that the beginning bound of PARSE-TREE is the
          beginning of the string."
         (setf (car (cadr parse-tree)) '(0 0))
         parse-tree)
       (transform-tree (parse-tree)
         "Map transform-parse-tree over PARSE-TREE."
         ;; TODO: at some point, don't cons if nothing has changed.
         (generated-transform-parse-tree
          prefix nil
          (transform-parse-tree
           prefix nil
           `(,(car parse-tree)
             ,(cadr parse-tree)
             ,(mapcar #'transform-tree (caddr parse-tree)))
           :lines line-octets-cache)))
       (get-start (ast)
         "Return the start offset into STRING from the AST representation."
         (car (cadr ast)))
       (get-end (ast)
         "Return the end offset into STRING from the AST representation."
         (cadr (cadr ast)))
       (terminal-symbol-p (field-spec &aux (slot-info (car field-spec)))
         "Return T if FIELD-SPEC represents a terminal symbol that shouldn't
          appear in the resulting AST."
         ;; TODO: copied from above function
         (cond
           ;; Has an associated slot or has children
           ((or (listp slot-info) (caddr field-spec)) nil)
           (t (terminal-symbol-class-p prefix slot-info))))
       ;; NOTE: any consecutive terminals in a loop are going to be a problem
       ;;       if a parse tree transformation + json substitution isn't used.
       (annotate-surrounding-text (subtree-spec &key parent-from parent-to)
         "Annotate SUBTREE-SPEC by adding a fourth item to each subtree which
          contains a potential starting position for before text and another
          for after text."
         (iter
           (iter:with from)
           ;; NOTE: we shouldn't need to store the 'to' as it will
           ;;       be recursively called immediately with it.
           (for child in (caddr subtree-spec))
           (for previous-child previous child)
           (for terminal-child-p = (terminal-symbol-p child))
           (for terminal-previous-child-p =
                (when previous-child
                  (terminal-symbol-p previous-child)))
           (cond
             ((not (or previous-child terminal-child-p))
              ;; Set to the start of the current node passed in as an argument.
              (setf from (get-start subtree-spec)))
             ;; First child is a terminal.
             ((and (not previous-child) terminal-child-p))
             ;; NOTE: (not previous-child) should be covered by the
             ;;       previous two conditions.
             ((and (not terminal-previous-child-p) terminal-child-p)
              ;; Send the before text to the after of the previous child.
              (collect
                  (annotate-surrounding-text
                   previous-child :parent-from from :parent-to (get-start child))
                into annotated-children at beginning)
              (setf from nil))
             ((and (not terminal-previous-child-p) (not terminal-child-p))
              (collect
                  (annotate-surrounding-text previous-child :parent-from from)
                into annotated-children at beginning)
              ;; Prefer storing text in the before-text slot.
              (setf from (get-end previous-child)))
             ((and terminal-previous-child-p (not terminal-child-p))
              (collect previous-child into annotated-children at beginning)
              ;; Store the text in the before-text of child.
              (setf from (get-end previous-child)))
             ((and terminal-previous-child-p terminal-child-p)
              ;; This whitespace doesn't have an AST to be attached to.
              (collect previous-child into annotated-children at beginning)
              (collect `(:inner-whitespace ,(list (get-end previous-child)
                                                     (get-start child)))
                into annotated-children at beginning)
              (setf from nil)))
           (finally
            ;; Patch the final child if needed and then
            ;; reassemble the subtree spec with updated information.
            (return
              (list
               (car subtree-spec)
               (cadr subtree-spec)
               (reverse
                (append
                 (cond
                   ((not child) nil)
                   ((not terminal-child-p)
                    (list
                     (annotate-surrounding-text
                      child :parent-from from
                            :parent-to (get-end subtree-spec))))
                   ((or (root-rule-ast-p (car subtree-spec))
                        (not (equal (get-end child) (get-end subtree-spec))))
                    ;; This is an edge case where the after text won't be
                    ;; handled without an inner whitespace AST.
                    (list `(:inner-whitespace ,(list (get-end child)
                                                     (get-end subtree-spec)))
                          child))
                   (child (list child)))
                 annotated-children))
               ;; If either is nil, it means that there isn't text for that slot.
               (list parent-from parent-to)))))))
    (let ((blotted-ranges (when *use-blotting*
                            (blot-out-ranges superclass string))))
      (annotate-surrounding-text
       (insert-blots-into-parse-tree
        (blot-ranges->parse-tree-ranges blotted-ranges string)
        (transform-tree
         (ensure-beginning-bound
          (parse-language
           superclass
           (if *use-blotting*
               (blot-out superclass blotted-ranges string)
               string)))))))))

(defgeneric parse-language (superclass string &key)
  (:documentation "Get a parse tree for STRING using the language associated
with SUPERCLASS.")
  (:method (superclass string &key)
    (parse-string (get-language-from-superclass superclass) string
                  :produce-cst t)))

;;; By default, don't indent comments, text fragments or parsing errors.
(defmethod indentablep ((ast comment-ast)) nil)
(defmethod indentablep ((ast parse-error-ast)) nil)
(defmethod indentablep ((ast error-variation-point)) nil)
(defmethod indentablep ((ast text-fragment)) nil)
(defmethod indentablep ((ast source-text-fragment)) nil)
(defmethod indentablep ((ast source-text-fragment-variation-point)) nil)
(defmethod indentablep ((ast inner-whitespace)) nil)

(defmethod get-indentation-at ((ast inner-whitespace) (parents list)
                               &aux (parent (car parents)))
  "Get the indentation at AST when it is an inner-whitespace AST. This
is handled differently than other ASTs because it should be considered part
of the parent."
  (reduce (lambda (total parent)
            (+ total
               (or (indent-adjustment parent) 0)
               (or (indent-children parent) 0)))
          (cdr parents)
          :initial-value (or (and parent (indent-adjustment parent))
                             0)))

(defgeneric surrounding-text-transform (text)
  (:documentation "Transform VALUE into a string representation. This is useful
for ASTs which need to appear in the surrounding text slots.")
  (:method (text) text)
  (:method ((ast null)) "")
  (:method ((ast alternative-ast)) (source-text ast))
  (:method ((ast source-text-fragment)) (text ast)))

;;; Cache space and tab strings used in indentation, so we
;;; don't have to generate the same string more than once.

(defvar *space-strings-table* (make-array '(1000) :initial-element nil))
(defvar *tab-strings-table* (make-array '(100) :initial-element nil))

(defun space-string (n)
  "Create a string of N spaces, or use an existing one in the cache"
  (declare (type (integer 0) n))
  (cached-rep-string #\Space n '*space-strings-table*))

(defun tab-string (n)
  "Create a string of N tabs, or use an existing one in the cache"
  (declare (type (integer 0) n))
  (cached-rep-string #\Tab n '*tab-strings-table*))

(defun cached-rep-string (c n tabvar)
  (declare (type (integer 0) n))
  (declare (type character c))
  (let* ((tab (symbol-value tabvar))
         (len (length tab)))
    (declare (type simple-vector tab))
    (if (< n len)
        (or (svref tab n)
            (setf (svref tab n)
                  (make-string n :element-type (type-of c) :initial-element c)))
        (let ((newtab (make-array (list (* 2 n)) :initial-element nil)))
          (setf (cl:subseq newtab 0 len) tab
                (symbol-value tabvar) newtab)
          (cached-rep-string c n tabvar)))))

(defvar *string-canon-table* (make-weak-hash-table :test #'equal :weakness :value)
  "Table of canonical strings.  It must be the case that (string= key value)
   and (typep value 'simple-string).  Use weak value because if all values
   are gone we can never detect that new calls with this string (up to string=)
   reused that previous object.")

(defun canon-string (string &key (synchronize task:*task-runner*))
  "Canonize STRING using `*string-canon-table*'.
STRING is also converted to a `base-string', if possible.

Note STRING is canonized regardless of length. Duplication of long
strings is actually common in large projects (due for example to
copyright notices reproduced across many files)."
  (labels ((simplify-string (string)
             (etypecase string
               (simple-base-string string)
               (base-string (coerce string 'simple-base-string))
               (string
                (if (every (of-type 'base-char) string)
                    (coerce string 'simple-base-string)
                    (coerce string 'simple-string)))))
           (canon-string (string)
             (ensure-gethash string *string-canon-table* (simplify-string string))))
    (if synchronize
        (synchronized ('*string-canon-table*)
          (canon-string string))
        (canon-string string))))

;;; TODO: with unindentable ASTs, we still want to know if the last thing seen
;;;       was a newline or not.
(defmethod source-text ((ast indentation)
                        &key stream parents
                          ;; These are "boxed" values since they have
                          ;; to be propagated between adjacent
                          ;; siblings (not just down the stack).
                          (indent-p (box nil))
                          (indentation-ast (box nil))
                          (trim t)
                          (root ast))
  ;; Trim removes the before and after text from the output and the comments.
  ;; Note that it will always trim with the first AST it sees since
  ;; the top most AST shouldn't have any before or after text this
  ;; should maintain previous functionality while still being able to
  ;; reproduce the source-text at the top-level.
  (declare (special trim))
  (labels ((ends-with-newline-p (string)
             "Return T if STRING ends with a newline."
             (unless (emptyp string)
               (eql #\newline (last-elt string))))
           (make-indentation-string (indentation)
             "Create the string representation of INDENTATION.
            This handles converting spaces to tabs."
             ;; Protect from negative numbers.
             (let ((protected-indentation (if (< indentation 0) 0 indentation)))
               (if *indent-with-tabs-p*
                   (mvlet ((tabs spaces (floor protected-indentation
                                               *spaces-per-tab*)))
                     (concatenate
                      'base-string
                      (tab-string tabs)
                      (space-string spaces)
                      ))
                   (space-string protected-indentation)
                   )))
           (indentation-length (ast parent-list)
             "Get the indentation at AST with its parents provided
            in PARENT-LIST."
             ;; Patch the indent-children slots of AST if
             ;; the value is T. The value T could be provided
             ;; from a #'convert invocation.
             (cond
               ((typep ast 'indentation)
                (when (eq t (indent-children ast))
                  (setf (indent-children ast)
                        (get-default-indentation ast parent-list)))
                (get-indentation-at ast parent-list))
               (t (get-indentation-at (car parent-list) (cdr parent-list)))))
           (patch-inner-indentation (text ast parents
                                     &aux (indentation
                                           (indentation-length ast parents))
                                       (split-text (split "\\r?\\n" text)))
             "Patch the newlines that occur inside interleaved text.
            This assumes that the indentation should be the same
            as the parent."
             (cond
               ((not (indentablep ast))
                ;; NOTE: this won't correctly handle source-text fragments that
                ;;       end with newlines. This should only be a problem with
                ;;       ASTs that have an implicit newline.
                (setf (unbox indent-p) nil
                      (unbox indentation-ast) nil)
                text)
               ((and (length< 1 split-text)
                     (not (typep ast 'computed-text)))
                (with-output-to-string (s nil :element-type (array-element-type text))
                  (write-string (car split-text) s)
                  (dolist (subseq (cdr split-text))
                    (format s "~%~a~a"
                            (make-indentation-string
                             (if (emptyp subseq)
                                 0
                                 indentation))
                            subseq))
                  ;; Add the newline back in if there was one at the end
                  ;; of the string since it gets removed by #'split.
                  (format s "~a"
                          (if (ends-with-newline-p text) #\newline ""))))
               (t text)))
           (handle-leading-newline (text)
             "If the first character in TEXT is a newline, reset the
            indentation variables so no indentation is printed."
             (when (scan "^\\r?\\n" text)
               (setf (unbox indent-p) nil
                     (unbox indentation-ast) nil)))
           (handle-trailing-newline (text ast indentablep parents)
             "If the last character in TEXT is a newline, set the
            indentation variables."
             (when (and
                    ;; NOTE: don't keep track of indentation in computed-text.
                    ;;       This is only an issue when text-fragments are
                    ;;       present.
                    (not (find-if (of-type 'computed-text) parents))
                    (ends-with-newline-p text)
                    indentablep)
               (setf (unbox indent-p) t
                     (unbox indentation-ast) ast)))
           (handle-indentation (text ast indentablep parents
                                &key ancestor-check first-child
                                &aux
                                  (parent (car parents))
                                  (indentable-parent? (indentablep parent))
                                  (indent-computed-text?
                                   (and first-child
                                        (typep parent 'computed-text)))
                                  (empty? (emptyp text))
                                  (ignore-empty?
                                   (and empty?
                                        (not indent-computed-text?)))
                                  (indentablep (or indentablep
                                                   (and indent-computed-text?
                                                        indentable-parent?))))
             "If indentation to be written to stream, handle writing it."
             ;; NOTE: computed text nodes with children are a bit of an edge case
             ;;       where the computed text node turns into more of a wrapper
             ;;       class, and the first child gets the intended indentation.
             ;;       Since the first child is generally a text fragment, the
             ;;       indentation is lost since it isn't indentablep. The
             ;;       variables indentable-parent?, indent-computed-text?, and
             ;;       ignore-empty? are used to work around this, indenting the
             ;;       first child of a computed text node if it is needed.
             (when (and (unbox indent-p)
                        ;; Prevent indentation from being
                        ;; wasted on empty strings before it
                        ;; reaches a child. This is checking if
                        ;; it should not be skipped as opposed
                        ;; to the logic in process-indentation
                        ;; which is checking if it should be
                        ;; skipped.
                        (not (and ancestor-check
                                  ignore-empty?
                                  (ancestor-of-p
                                   root (unbox indentation-ast) ast))))
               (unless ignore-empty?
                 (setf (unbox indent-p) nil
                       (unbox indentation-ast) nil))
               (unless (or ignore-empty?
                           (not indentablep)
                            trim)
                 (write-string
                  (make-indentation-string (indentation-length ast parents))
                  stream))))
           (handle-text (text ast indentablep parents
                         &key ancestor-check surrounding-text first-child)
             "Handle writing TEXT to stream, updating any indentation
            variables that need updated."
             ;; Suppress indentation if TEXT begins with a newline.
             (let ((text (if surrounding-text
                             (surrounding-text-transform text)
                             text)))
               (handle-leading-newline text)
               (handle-indentation text ast indentablep parents
                                   :ancestor-check ancestor-check
                                   :first-child first-child)
               ;; Set indentation flag  when TEXT ends with a newline.
               (handle-trailing-newline text ast indentablep parents)
               (unless trim
                 (write-string
                  (patch-inner-indentation text ast parents)
                  stream))))
           (handle-ast (output &key (ast-parents (cons ast parents)))
             "Handle the source text of AST."
             (source-text output
                          :stream stream
                          :parents ast-parents
                          :indent-p indent-p
                          :indentation-ast indentation-ast
                          :trim nil
                          :root root)))
    (let ((indentablep (indentablep ast))
          ;; NOTE: keep track of whether its the first child of a node.
          ;;       This still allows computed text nodes to have the correct
          ;;       indentation when they have children.
          (first-child t))
      ;; before and after text is always considered indentable.
      (handle-text (before-text ast) ast t parents :surrounding-text t
                                                   :first-child t)
      (mapc (lambda (output &aux trim)
              (declare (special trim))
              (cond
                ((stringp output)
                 (handle-text output ast indentablep parents
                              :ancestor-check t
                              :first-child first-child))
                (t (handle-ast output)
                   (setf first-child nil))))
            (cdr (butlast (output-transformation ast))))
      (handle-text (after-text ast) ast t parents :surrounding-text t))))


;;; Common Methods
(defmethod rebind-vars ((ast tree-sitter-ast)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (if (ast-type-to-rebind-p ast)
      (copy ast :text (rebind-vars (text ast) var-replacements fun-replacements))
      (apply #'copy ast
             (mappend (lambda (child-slot)
                        (destructuring-bind (name . arity) child-slot
                          (list (make-keyword name)
                                (cond ((= arity 0)
                                       (mapcar {rebind-vars _ var-replacements
                                                            fun-replacements}
                                               (slot-value ast name)))
                                      ((slot-value ast name)
                                       (rebind-vars (slot-value ast name)
                                                    var-replacements
                                                    fun-replacements))))))
                      (child-slots ast)))))

;;; TODO: the ast-hash specialization for AST only considers #'children.
;;;       Does it make sense to create a specialization for structured-text
;;;       which considers the output-transformation?
(defmethod ast-hash ast-combine-hash-values ((ast computed-text))
  (ast-hash (text ast)))

(defmethod ast-hash ast-combine-hash-values ((ast structured-text))
  ;; TODO: need to add support for internal ASTs too.
  (ast-hash (flatten
             (list (before-text ast) (after-text ast)
                   (before-asts ast) (after-asts ast)))))

(defmethod equal? ((ast-a computed-text) (ast-b computed-text))
  (equal (source-text ast-a) (source-text ast-b)))

(defmethod equal? :around ((ast-a structured-text) (ast-b structured-text))
  (and (call-next-method)
       (equal (before-text ast-a) (before-text ast-b))
       (equal (after-text ast-a) (after-text ast-b))))


;;;; Whitespace styles
(defclass c-style-indentation ()
  ;; NOTE: it might make sense to store indentation size on a style object
  ;;       instead of the software object.
  ()
  (:documentation "A class used to represent indentation in c-style languages."))

(defmethod default-whitespace-style ((ast c-like-syntax-ast))
  (make 'c-style-indentation))

(defmethod get-style-indentation ((style c-style-indentation) software ast &key)
  nil)

(defmethod get-style-indentation
    ((style c-style-indentation) software (ast compound-ast) &key)
  t)

(defmethod get-style-indentation
    ((style c-style-indentation) software (ast if-ast) &key)
  (not (or (typep (consequence ast) 'compound-ast)
           (typep (alternative ast) 'compound-ast))))

(defmethod get-style-indentation
    ((style c-style-indentation) software (ast loop-ast) &key)
  (not (typep (body ast) 'compound-ast)))

(defmethod get-style-indentation-adjustment
    ((style c-style-indentation) software ast (parent if-ast)
     &key parents)
  (let ((target-child-list (list (consequence parent) (alternative parent))))
    (when (and (not (typep ast 'compound-ast))
               (member ast target-child-list :test #'eq)
               (some {typep _ 'compound-ast} target-child-list))
      (get-default-indentation ast parents))))

(defmethod whitespace-between/parent ((parent root-ast)
                                      (style c-style-indentation)
                                      (ast1 ast)
                                      (ast2 ast))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent compound-ast)
                                      (style c-style-indentation)
                                      (ast1 statement-ast)
                                      (ast2 statement-ast))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent compound-ast)
                                      (style c-style-indentation)
                                      (ast1 (eql ':|{|))
                                      (ast2 statement-ast))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent compound-ast)
                                      (style c-style-indentation)
                                      (ast1 statement-ast)
                                      (ast2 (eql ':|}|)))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent if-ast)
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 statement-ast))
  (if (and (not (typep ast2 'compound-ast))
           (or (eq (consequence parent) ast2)
               (eq (alternative parent) ast2)))
      #.(fmt "~%")
      (call-next-method)))

(defmethod whitespace-between/parent ((parent if-ast)
                                      (style c-style-indentation)
                                      (ast1 statement-ast)
                                      ast2)
  (if (and (not (typep ast1 'compound-ast))
           (or (eq (consequence parent) ast1)
               (eq (alternative parent) ast1)))
      #.(fmt "~%")
      (call-next-method)))

(defmethod whitespace-between/parent ((parent loop-ast)
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 statement-ast))
  (if (and (not (typep ast2 'compound-ast))
           (eq (body parent) ast2))
      #.(fmt "~%")
      (call-next-method)))

(defmethod whitespace-between/parent (parent
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 comment-ast))
  #.(fmt "~%"))

(defmethod whitespace-between/parent (parent
                                      (style c-style-indentation)
                                      (ast1 comment-ast)
                                      ast2)
  #.(fmt "~%"))

(defmethod whitespace-between ((style c-style-indentation)
                               (ast1 t)
                               (ast2 definition-ast))
  #\Newline)

(defmethod whitespace-between ((style c-style-indentation)
                               (ast1 null)
                               (ast2 definition-ast))
  "")

(defmethod whitespace-between ((style c-style-indentation)
                               (ast1 definition-ast)
                               (ast2 t))
  #\Newline)

(defmethod whitespace-between ((style c-style-indentation)
                               (ast1 definition-ast)
                               (ast2 null))
  "")


;;;; Parse Tree Util
(defun add-operator-to-binary-operation (parse-tree)
  "Adds the operator in a binary operation to the :operator slot."
  (with-modify-parse-tree (parse-tree)
    ((:error :comment) (ignore-types))
    (t (label-as :operator))))

(defun transform-malformed-parse-tree (parse-tree &key (recursive t))
  "Return a modified version of PARSE-TREE if it is malformed.
This occurs when the source text is not accurately represented by the parse tree
which is caused by dropped tokens or added zero-width tokens.
When a slot contains an error node, the issue is instead promoted to the parent,
making it a source-text-fragment.
This function also modifies error nodes, though they're not strictly malformed
per se. Otherwise, returns PARSE-TREE."
  (labels ((walk-parse-tree (function parse-tree)
             (funcall function parse-tree)
             (map nil {walk-parse-tree function} (caddr parse-tree)))
           (zero-width-p (subtree &aux (range (cadr subtree)))
             (when (listp range)
               (equal (car range) (cadr range))))
           (error-in-named-slot-p (subtree)
             ;; NOTE: errors in named slots are caused by parse tree transforms
             ;;       on the lisp side and do not occur in trees retrieved from
             ;;       tree-sitter. They are patched out here because most of the
             ;;       code assumes that they will not happen.
             ;; NOTE: error-variation-points are created by this function, and
             ;;       are not problematic even though they have an error in a
             ;;       slot.
             (unless (member (car subtree) '(:error-variation-point
                                             :error-variation-point-tree))
               (iter
                 (for child in (caddr subtree))
                 (for slot/type = (car child))
                 (thereis (and (listp slot/type)
                               (eql (cadr slot/type) :error))))))
           (problematicp (subtree)
             (and (listp subtree)
                  (or (zero-width-p subtree)
                      (error-in-named-slot-p subtree))))
           (problematic-subtree-p (tree)
             (if recursive
                 (walk-parse-tree
                  (lambda (subtree)
                    (when (problematicp subtree)
                      (return-from problematic-subtree-p t)))
                  tree)
                 (find-if
                  (lambda (subtree)
                    (problematicp subtree))
                  (caddr tree))))
           (strip-slots-from-children (tree)
             "Strip slots from the children of TREE."
             (let ((children (caddr tree)))
               (mapcar (lambda (child-tree)
                         (if (listp (car child-tree))
                             `(,(cadar child-tree) ,@(cdr child-tree))
                             child-tree))
                       children))))
    (let ((range (cadr parse-tree))
          (stored-in-slot? (consp (car parse-tree)))
          (error-class (if *use-variation-point-tree*
                           :error-variation-point-tree
                           :error-variation-point))
          (source-text-fragment-class
            (if *use-variation-point-tree*
                :source-text-fragment-variation-point-tree
                :source-text-fragment-variation-point))
          (error? (eql :error (car parse-tree))))
      (cond
        ;; Edge case where the input is completely erroneous and the lexer can't
        ;; even do anything with it.
        ((and error? (not (parse-tree-children parse-tree)))
         parse-tree)
        (error?
         `(,error-class
           ,range
           (((:parse-error-ast :error) ,range nil)
            ((:error-tree :error-tree) ,range
             ,(strip-slots-from-children parse-tree)))))
        ((problematic-subtree-p parse-tree)
         `(,(if stored-in-slot?
                (list (caar parse-tree) source-text-fragment-class)
                source-text-fragment-class)
           ,range
           (((:source-text-fragment :source-text-fragment) ,range nil)
            ((:source-text-fragment-tree :source-text-fragment-tree)
             ,range
             ,(strip-slots-from-children parse-tree)))))
        (t parse-tree)))))

(defun transform-c-style-variadic-parameter (parse-tree)
  "If PARSE-TREE represents a variadic function, return a modified version of it.
Otherwise, return PARSE-TREE."
  (with-modify-parse-tree (parse-tree)
    (:|...| (rename-type-to :variadic-declaration))))


;;; Symbol Table

(defconst +symbol-table-namespaces+
  '(nil :variable :function :type :tag :macro :namespace :method))

(deftype symbol-table-namespace ()
  "Possible namespaces in a symbol table."
  ;; The nil "namespace" is for languages that don't use namespaces.
  ;; The :tag namespace is for C (which has separate tag and type
  ;; namespaces).
  ;; The :method namespace is for Go which treats methods differently
  ;; than functions.
  (cons 'member +symbol-table-namespaces+))

(def +namespace-decl-type-table+
  '((:variable . variable-declaration-ast)
    (:function . function-declaration-ast)
    (:method . function-declaration-ast)
    (:type . type-declaration-ast)
    (:tag . type-declaration-ast)
    (:macro . macro-declaration-ast)
    (:namespace . namespace-declaration-ast)
    (nil . declaration-ast)))

(defun namespace-decl-type-table ()
  "Reader for `+namespace-decl-type-table+'."
  +namespace-decl-type-table+)

(defun namespace-decl-type (ns)
  (assure (and symbol (not null))
    (assocdr ns +namespace-decl-type-table+)))

(defun decl-type-namespace (type)
  (assure symbol-table-namespace
    (rassocar type +namespace-decl-type-table+)))

(defgeneric multi-declaration-keys (root)
  (:documentation "MULTI-DECLARATION-KEYS returns a list of keys which should not
 choose one value over another when there is a conflict between two keys in a
symbol table while merging; instead, it merges the two values together. For
example,'(:function) may be passed for C++ tables to indicate that functions
can be overloaded and all declarations should be kept for resolution at a later
time. It will produce something similar to the following:
'((:function ((x ast-1 ast-4) (y ast-2 ast-3)))).")
  (:method (root) nil))

(defun multi-map-symbol-table-union
    (shadowable-table shadowing-table &key allow-multiple
     &aux (keys (fset:union (domain shadowable-table)
                            (domain shadowing-table)))
       map-list)
  "Merge the maps in SHADOWABLE-TABLE and SHADOWING-TABLE. Each map is an fset
map which maps a key to another fset map. For example,
'((:function (('x ast-1) ('y ast-2)))).
For each key, the associated map for each table is merged together. Note that
SHADOWING-TABLE's values will shadow SHADOWABLE-TABLE's values when necessary.

ALLOW-MULTIPLE is a list keys that allow multiple definitions and is documented
by MULTI-DECLARATION-KEYS."
  (labels ((merge-function (key allow-multiple)
             (lambda (shadowable-value shadowing-value)
               (if (member key allow-multiple)
                   (remove-duplicates
                    (append shadowing-value shadowable-value))
                   shadowing-value))))
    (do-set (key keys (convert 'fset:map map-list))
      (let* ((shadowable-subtable (@ shadowable-table key))
             (shadowing-subtable (@ shadowing-table key))
             (merged-table (if (and shadowable-subtable shadowing-subtable)
                               (map-union shadowable-subtable
                                          shadowing-subtable
                                          (merge-function
                                           key
                                           allow-multiple))
                               (or shadowable-subtable
                                   shadowing-subtable))))
        (push (cons key merged-table) map-list)))))

(defgeneric symbol-table-union (root symbol-table-1 symbol-table-2 &key)
  (:documentation "Return the union of SYMBOL-TABLE-1 and SYMBOL-TABLE-2.")
  (:method (root symbol-table-1 symbol-table-2 &key &allow-other-keys)
    (map-union symbol-table-1 symbol-table-2))
  (:method ((root c-like-syntax-ast) table-1 table-2 &key &allow-other-keys)
    (multi-map-symbol-table-union
     table-1 table-2
     :allow-multiple (multi-declaration-keys root))))

(defun propagate-declarations-down (ast in)
  "Propagate the symbol table declarations down through AST's children."
  (reduce (lambda (in2 child)
            (symbol-table-union
             ast
             (symbol-table child in2)
             (outer-defs child)))
          (children ast)
          :initial-value (symbol-table-union ast in (inner-defs ast))))

(def-attr-fun symbol-table (in)
  "Compute the symbol table at this node."
  (:method ((software parseable) &optional in)
    (symbol-table (genome software) in))
  (:method ((node root-ast) &optional in)
    (propagate-declarations-down node in))
  (:method ((node functional-tree-ast) &optional in)
    (cond
      ((scope-ast-p node)
       (propagate-declarations-down node in)
       in)
      (t (mapc (op (symbol-table _ (symbol-table-union node
                                                       in
                                                       (inner-defs node))))
               (children node))
         in))))

(defmethod attr-missing ((fn-name (eql 'symbol-table)) node)
  (symbol-table (attrs-root *attrs*) (empty-map)))

(defmethod attr-missing ((fn-name (eql 'symbol-table)) (node tree-sitter-ast))
  (labels ((extra-ast-types (language)
             (mapcar (op (format-symbol 'sel/sw/ts "~a-~a" language _))
                     (extra-asts language))))
    (if (member node (extra-ast-types (make-keyword (ast-language-class node)))
                :test #'typep)
        (symbol-table node (empty-map))
        (symbol-table (attrs-root *attrs*) (empty-map)))))

(-> group-by-namespace
    ((soft-list-of ast)
     (soft-list-of symbol-table-namespace))
    (values (soft-alist-of symbol-table-namespace
                           (soft-list-of ast))
            &optional))
(defun group-by-namespace (declarations namespaces)
  (let ((table (make-hash-table)))
    (mapc (op (push _ (gethash _ table))) declarations namespaces)
    (hash-table-alist table)))

(-> convert-grouped-namespaces
    ((soft-alist-of symbol-table-namespace
                    (soft-list-of ast))
     &key (:source-text-fun function))
    (values (soft-alist-of symbol-table-namespace fset:map)
            &optional))
(defun convert-grouped-namespaces (grouped-namespaces
                                   &key (source-text-fun
                                         (lambda (ast)
                                           (or (declarator-name ast)
                                               (source-text ast)))))
  "Convert grouped-namespaces into a grouping of
(namespace . source-text/declaration-map)."
  (fbindrec (source-text-fun
             (create-source-text-map
              (lambda (type declarations)
                (reduce (lambda (map ast)
                          (let ((key (source-text-fun ast)))
                            (with map
                                  key
                                  ;; Preserve overloads within a
                                  ;; single AST.
                                  (if (member type (multi-declaration-keys ast))
                                      (cons ast (@ map key))
                                      (list ast)))))
                        declarations
                        :initial-value (empty-map)))))
    (mapcar
     (lambda (grouping &aux (type (car grouping)))
       (cons type
             (create-source-text-map type (cdr grouping))))
     grouped-namespaces)))

(def-attr-fun outer-defs ()
  "Map of outer definitions from a node"
  (:method ((node node))
    (convert 'fset:map
             (mapcar (op (list (source-text _1) _1))
                     (outer-declarations node))))
  (:method ((node c-like-syntax-ast))
    (mvlet ((declarations namespaces (outer-declarations node)))
      (convert 'fset:map
               (convert-grouped-namespaces
                (group-by-namespace declarations namespaces))))))

(def-attr-fun inner-defs ()
  "Map of inner definitions from a node"
  (:method ((node node))
    (convert 'fset:map
             (mapcar (op (list (source-text _1) _1))
                     (inner-declarations node))))
  (:method ((node c-like-syntax-ast))
    (mvlet ((declarations namespaces (inner-declarations node)))
      (convert 'fset:map
               (convert-grouped-namespaces
                (group-by-namespace declarations namespaces))))))

(defgeneric qualify-declared-ast-name (ast)
  (:documentation "Qualify the name of AST.
This is used both when adding ASTs to the symbol table and when
looking them up.")
  (:method ((ast identifier-ast))
    (or (declarator-name ast)
        (source-text ast)))
  (:method ((ast alternative-ast))
    (source-text ast)))

(defgeneric qualify-declared-ast-names-for-lookup (ast)
  (:documentation "Generate qualified names for AST.
This is used when looking up symbols in the symbol table.")
  (:method ((ast ast))
    (list (qualify-declared-ast-name ast)))
  (:method ((ast alternative-ast))
    (mappend #'qualify-declared-ast-names-for-lookup (children ast))))

(defsubst symbol-table-lookup (symbol-table query)
  "Lookup QUERY in SYMBOL-TABLE.
This is an inlined function, but if `find-in-symbol-table' is compiled
with debug settings it can be traced."
  (lookup symbol-table query))

(defgeneric find-in-symbol-table (ast namespace query)
  (:documentation "Lookup QUERY in the symbol table for AST using NAMESPACE.")
  (:method ((ast ast) (ns null) (query string))
    (let* ((symbol-table (symbol-table ast)))
      (symbol-table-lookup symbol-table query)))
  (:method ((ast ast) (ns null) (query ast))
    (find-in-symbol-table ast ns (qualify-declared-ast-names-for-lookup query)))
  (:method ((ast ast) (ns symbol) (query ast))
    (find-in-symbol-table ast ns (qualify-declared-ast-names-for-lookup query)))
  (:method ((ast ast) (namespace symbol) (query string))
    (when-let* ((symbol-table (symbol-table ast))
                (ns-table (lookup symbol-table namespace)))
      (values (symbol-table-lookup ns-table query))))
  (:method ((ast ast) (namespace symbol) (query list))
    (some (op (find-in-symbol-table ast namespace _))
          query)))

(defun outer-and-inner-defs (ast)
  (symbol-table-union
   ast
   (outer-defs ast)
   (inner-defs ast)))

(defgeneric find-in-defs (ast namespace query)
  (:documentation "Lookup QUERY in the inner-decls and outer-decls for AST
using NAMESPACE.")
  (:method ((ast ast) (ns null) (query string))
    (let* ((defs (outer-and-inner-defs ast)))
      (lookup defs query)))
  (:method ((ast ast) (ns null) (query ast))
    (find-in-defs ast ns (qualify-declared-ast-name query)))
  (:method ((ast ast) (ns symbol) (query ast))
    (find-in-defs ast ns (qualify-declared-ast-name query)))
  (:method ((ast ast) (namespace symbol) (query string))
    (assert (keywordp namespace))
    (when-let* ((defs (outer-and-inner-defs ast))
                (ns-table (lookup defs namespace)))
      (values (lookup ns-table query)))))

(define-condition no-enclosing-declaration-error (error)
  ((type :initarg :type :type symbol :reader no-enclosing-declaration-error.type)
   (root :initarg :root :reader no-enclosing-declaration-error.root)
   (id :initarg :id :type ast :reader no-enclosing-declaration-error.id))
  (:report (lambda (c s)
             (with-slots (type root id) c
               (format s "No ~a for ~a in ~a~%Path: ~a"
                       type id root
                       (ast-path root id))))))

(defgeneric find-enclosing-declaration (type root id)
  (:documentation "Like `find-enclosing', but implement special
  handling when languages don't clearly distinguish different kinds of
  declarations at the AST level.")
  (:method-combination standard/context)
  (:method :context (type root id)
    (or (call-next-method)
        (let ((root (attrs-root*)))
          (error 'no-enclosing-declaration-error
                 :type type
                 :root root
                 :id id))))
  (:method :context (type (root software) id)
    (find-enclosing-declaration type (genome root) id))
  (:method :context ((type symbol) root id)
    (if (keywordp type)
        (find-enclosing-declaration (namespace-decl-type type) root id)
        (call-next-method)))
  (:method :context ((type symbol) root (id identifier-ast))
    (assert (not (keywordp type)))
    (let ((ns (decl-type-namespace type))
          (id-string (qualify-declared-ast-name id)))
      (or
       ;; Check if this identifier is part of a declaration before
       ;; checking the symbol table to avoid returning a shadowed variable.
       (iter
        (for parent in (get-parent-asts* (attrs-root*) id))
        (finding parent such-that
                 (and (typep parent type)
                      (find-in-defs parent ns id-string))))
       (call-next-method))))
  (:method ((type symbol) root id)
    (find-enclosing type (attrs-root*) id)))


;;; Classes.

(fset:define-tuple-key +access+)
(fset:define-tuple-key +id+)
(fset:define-tuple-key +ns+)

(-> add-namespaced-field (fset:map symbol-table-namespace ast)
    (values fset:map &optional))
(defun add-namespaced-field (map ns id)
  "Add ID to MAP in NS, a namespace such as `:type' or `:variable'."
  (let* ((key (source-text id))
         (data (fset:tuple (+ns+ ns) (+id+ id))))
    ;; Conserve previous entry because of overloads.
    (with map key (cons data (@ map key)))))

(defun lookup-in-field-table (class namespace key)
  "Look up KEY in NAMESPACE of the field table of CLASS."
  (declare (type symbol-table-namespace namespace))
  (when-let* ((map (field-table class))
              (key (source-text key))
              (fields (@ map key)))
    (iter (for field in fields)
          (when (eql namespace (@ field +ns+))
            (collect (@ field +id+))))))

(defgeneric field-adjoin (field map)
  (:documentation
   "Adjoin FIELD to MAP, a field table, according to the type of FIELD.
Note that adding FIELD may introduce multiple identifiers into FIELD.")
  (:method ((field ast) map)
    (assure fset:map
      (multiple-value-bind (ids namespaces)
          (outer-declarations field)
        (iter (for id in ids)
              (for ns in namespaces)
              (let ((ns (or ns :variable)))
                (setf map (add-namespaced-field map ns id))))
        map))))

(defgeneric class-fields (class)
  (:documentation "Get the fields of CLASS."))

(-> adjoin-fields (fset:map list) fset:map)
(defun adjoin-fields (map fields)
  "Adjoins fields in FIELDS to MAP, a field table (see `field-table' for
the format)."
  (assure fset:map
    (reduce (flip #'field-adjoin)
            fields
            :initial-value map)))

(def-attr-fun field-table ()
  "Attribute storing a map from names to fields of a class.
\"Field\" is the language-agnostic term for what different languages
call members, methods, attributes, or slots.

The keys of the map are strings; the values are FSet tuples where
`+ns+` is the namespace of the definition (function, variable, etc.)
and `+id+` is the AST of the identifier (a single definition may
define multiple identifiers).

There may be additional keys in the tuple to record language-specific
information such as visibility."
  (:method ((class class-ast))
    (direct-field-table class)))

(def-attr-fun direct-field-table ()
  (:method ((class class-ast))
    (adjoin-fields (empty-map)
                   (class-fields class))))

(-> field-table-ids (fset:map &key
                     (:ns symbol-table-namespace)
                     (:sort-root t))
    (values list &optional))
(defun field-table-ids (field-table &key (ns nil ns-supplied?) sort-root)
  "Extract IDs of fields from FIELD-TABLE.
If NS is supplied, only return IDs for fields in the specified
namespace.

If SORT-ROOT is non-nil, sort the IDs using `sort-descendants', with
SORT-ROOT as the ancestor."
  (let* ((range (range field-table))
         (fields
           (apply #'append (convert 'list range)))
         (fields
           (if ns-supplied?
               (keep ns fields :key (op (@ _ +ns+)))
               fields))
         (ids
           (mapcar (op (@ _ +id+)) fields)))
    (if sort-root
        (sort-descendants sort-root ids)
        ids)))

(-> field-table-lookup (fset:map string &key (:ns symbol-table-namespace))
    (values (soft-list-of ast) &optional))
(defun field-table-lookup (field-table key &key (ns nil ns-supplied?))
  (let ((result (@ field-table key)))
    (mapcar (op (@ _ +id+))
            (if (not ns-supplied?)
                result
                (filter (op (eql ns (@ _ +ns+))) result)))))


;;; Namespace
(def-attr-fun namespace (in)
  "Compute the namespace at this node."
  (:method ((ast functional-tree-ast) &optional in)
    (mapc (op (namespace _ in))
          (children ast))
    in)
  (:method ((software software) &optional in)
    (namespace (genome software) in)
    in)
  (:method :context ((ast tree-sitter-ast) &optional in)
    "Ensure a table is available for interning namespaces."
    (declare (ignore in))
    (if (boundp 'namespaces*)
        (call-next-method)
        (let ((namespaces* (dict)))
          (declare (special namespaces*))
          (call-next-method))))
  (:method :around ((ast tree-sitter-ast) &optional in)
    "Intern computed namespaces."
    (declare (ignore in)
             (special namespaces*))
    (let ((result (call-next-method)))
      (ensure-gethash result namespaces* result))))

(defmethod attr-missing ((fn-name (eql 'namespace)) node
                         &aux (attrs-root (attrs-root*)))
  (namespace
   (or (find-if (of-type 'root-ast)
                (get-parent-asts attrs-root node))
       attrs-root)
   ""))


;;; Contextualization
(defgeneric contextualize-ast (software ast
                               &key ast-type parents
                               &allow-other-keys)
  (:method (software ast &key &allow-other-keys) ast)
  (:method :around (software ast &key &allow-other-keys)
    (or (call-next-method) ast))
  (:documentation "Return a version of AST which has been patched, if needed,
to take CONTEXT into account."))

;;; TODO: find a better name for this. Disambiguate?
(defgeneric add-context (software &key)
  (:documentation
   "Add context to SOFTWARE that wasn't available at parse time.")
  (:method ((software software) &key)
    ;; TODO: figure out how to pass the parent ASTs in. Right now,
    ;;       the relevant methods will look them up if unavailable.
    (copy software
          :genome
          (mapcar (op (contextualize-ast software _))
                  (genome software)))))


;;; Utility
(eval-always
  (define-method-combination append-groupings))

(defun append-groupings (&rest args)
  (labels ((groupingp (list)
             "Return T if the caar of LIST is a cons. It would otherwise be
              a keyword if it weren't grouped."
             (consp (caar list)))
           (merge-grouping (merged-groupings grouping &aux (key (car grouping)))
             "Merge GROUPING into MERGED-GROUPINGS."
             (areplace key
                       (append (aget key merged-groupings) (cdr grouping))
                       merged-groupings))
           (merge-groupings (merged-groupings groupings)
             "Merge GROUPINGS into MERGED-GROUPINGS."
             (reduce #'merge-grouping groupings :initial-value merged-groupings))
           (merge-results (results)
             "Merge RESULTS into a single groupings list."
             (reduce #'merge-groupings (cdr results)
                     :initial-value (car results))))
    (if (groupingp args)
        (merge-results args)
        args)))

(defgeneric preserve-properties (ast &key group-by-position)
  (:method-combination append-groupings)
  (:documentation
   "Return properties that should be preserved when creating a new AST.

:GROUP-BY-POSITION groups the properties based on whether they occur before
  or after AST. This is useful when properties need to be split across
  two ASTs.")
  (:method append-groupings ((ast structured-text) &key group-by-position)
    (if group-by-position
        `((:before (:before-text . ,(before-text ast))
                   (:before-asts . ,(before-asts ast)))
          (:after (:after-text . ,(after-text ast))
                  (:after-asts . ,(after-asts ast))))
        `((:before-text . ,(before-text ast))
          (:before-asts . ,(before-asts ast))
          (:after-text . ,(after-text ast))
          (:after-asts . ,(after-asts ast)))))
  (:method append-groupings ((ast indentation) &key group-by-position)
    (if group-by-position
        `((:before (:indent-adjustment . ,(indent-adjustment ast))
                   (:indent-children . ,(indent-children ast))))
        `((:indent-adjustment . ,(indent-adjustment ast))
          (:indent-children . ,(indent-children ast))))))

(defun merge-preserved-properties (before-alist after-alist)
  "Merge the return values of preserve-properties when called with BEFORE-LIST
and AFTER-LIST."
  (iter
    (for (key . before-value) in before-alist)
    (for after-value = (aget key after-alist))
    (collect
        (cons key
              (cond
                ((stringp before-value) (string+ before-value after-value))
                ((every (of-type 'number) (list before-value after-value))
                 ;; NOTE: this is almost certainly going to be incorrect.
                 ;;       If this function gains usage, it may make sense to have
                 ;;       a specialization which specifies how a keyword slot
                 ;;       should be handled.
                 (+ before-value after-value))
                ((typep before-value 'number) before-value)
                ((typep after-value 'number) after-value)
                (t (append before-value after-value)))))))

(defun copy/structure (ast property-ast &rest args &key &allow-other-keys)
  "Copy AST, giving it the structured text and indentation properties of
PROPERTY-AST."
  (multiple-value-call #'copy ast
    (values-list args)
    (values-list
     (mapcan (juxt #'car #'cdr)
             (preserve-properties property-ast)))))

(defsubst convert-terminal (to term)
  "Convert the text of a terminal AST to class TO."
  (if (typep term to) term
      (copy/structure (make to :text (source-text term))
                      term)))

(defun tree-sitter-class-name (class &key ignore-named)
  "If CLASS is a choice subclass, or the name of a choice subclass,
then get the tree-sitter class that that is its supertype, unless the
choice subclass has been given a name."
  (econd ((classp class)
          (tree-sitter-class-name (class-name class)))
         ((and (symbolp class)
               (or ignore-named
                   (scan "-\\d+$" (symbol-name class))))
          (if-let* ((class (find-class-safe class))
                    (superclass-slot
                     (find-if [{eql 'choice-superclass}
                               #'slot-definition-name]
                              (progn (ensure-finalized class)
                                     (class-slots class))))
                    (quoted-value
                     (slot-definition-initform superclass-slot)))
            (ematch quoted-value
              ((list 'quote (and symbol (type symbol)))
               symbol))
            class))
         ((symbolp class) class)))
