;;; template.lisp --- Templates for de/constructing ASTs
(defpackage :software-evolution-library/software/template
  (:nicknames :sel/software/template :sel/sw/template)
  (:use :gt/full
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/string-clauses
        :software-evolution-library/software/parseable))
(in-package :software-evolution-library/software/tree-sitter)

(defgeneric template-placeholder (ast name)
  (:documentation "Generate a placeholder for NAME.

The user should never see the placeholder; it is simply a string that
AST's language can parse as a valid identifier.

Generic so a different syntax can be used per-language.")
  (:method ((ast t) name)
    ;; A reasonable default for most programming languages. Add a
    ;; leading underscore to avoid having to worry about what
    ;; characters a symbol can begin with. (E.g. Python variables
    ;; cannot begin with a number.)
    (string-replace "@"
                    (string+ #\_ (substitute #\_ #\- (string name)))
                    "LIST_")))

(defun template-placeholder* (ast name)
  "Append a random number to the end of the placeholder.

Just in case of a conflict with another binding in the template."
  (let ((placeholder (template-placeholder ast name)))
    (fmt "~a~x" placeholder (random 1000000))))

(defgeneric template-metavariable (ast symbol)
  (:documentation "Generate the corresponding valid metavariable for SYMBOL.

By default this substitutes underscores for dashes and prepends a
sigil.

Generic so a different syntax can be used per-language.")
  (:method (ast name)
    (template-metavariable ast (string name)))
  (:method ((ast t) (name string))
    (let ((name (substitute #\_ #\- (string name))))
      (assert (scan "^@?[A-Z0-9_]+$" name) () "Invalid metavariable: ~a" name)
      (if (string^= "@" name) name (fmt "$~a" name)))))

(defgeneric template-subtree (ast thing)
  (:documentation "Convert THING into a subtree.")
  (:method (ast (x integer))
    (princ-to-string x))
  (:method ((ast ast) (x string))
    x)
  (:method ((ast ast) (x terminal-symbol))
    (source-text x))
  (:method ((class t) (x ast))
    x)
  (:method ((ast t) (list list))
    ;; Treat an empty list as equivalent to an empty string. This is
    ;; helpful since an "empty" whatever usually parses as a distinct
    ;; class.
    (cond ((null list) "")
          ((single list)
           (template-subtree ast (car list)))
          ;; For individual subtrees that are strings, rather than
          ;; parsing them on their own we insert them into the template
          ;; (not the AST) so they get parsed in place. But doing the
          ;; same for lists of strings is impractical, since we don't
          ;; know in advance how the items should be separated. (Commas?
          ;; Semicolons? Newlines?) On the other hand, it might turn out
          ;; that for a given language the only places where the grammar
          ;; allows us to splice or match lists of nodes are always
          ;; separated by, say, commas. In that case it would be better
          ;; to insert lists of strings into the template instead of the
          ;; AST.
          (t
           (iter (for item in list)
                 (for subtree = (template-subtree ast item))
                 (collect
                  (cond
                    ((listp subtree)
                     (error "~
Nested lists are not allowed as template arguments:~%~a"
                            subtree))
                    ((stringp subtree)
                     (convert (type-of ast) subtree :deepest t))
                    (t subtree))))))))

(-> parse-ast-template (string symbol list)
    (values string list list list &optional))
(defun parse-ast-template (template class args)
  "Parse TEMPLATE and return four values:
1. The template with placeholders substituted for metavariables.
2. A list of the original metavariable names.
3. A list of placeholders.
4. A list of subtrees."
  (nest
   (if (and args (not (keywordp (car args))))
       (parse-ast-template/positional template class args)
       (parse-ast-template/keywords template class args))))

(defun parse-ast-template/positional (template class args)
  (assert (not (keywordp (first args))))
  (parse-ast-template/keywords
   template class
   (iter (for arg in args)
         (for i from 1)
         (collect (make-keyword (princ-to-string i)))
         (collect arg))))

(defun template-scalar-metavariable (ast symbol)
  (template-metavariable ast (drop-prefix "@" (string symbol))))

(defun template-list-metavariable (ast symbol)
  (template-metavariable ast (ensure-prefix "@" (string symbol))))

(defun canonicalize-kwargs (template class args)
  "Prefix keywords for arguments that should be used as lists with @."
  (with-collectors (out)
    (doplist (kw value args)
      (assert (keywordp kw))
      (let* ((scalar (template-scalar-metavariable class kw))
             (list (template-list-metavariable class kw))
             ;; Need to do a terminated scan since one keyword might
             ;; be a prefix of another.
             (scalar?
              (scan `(:sequence ,scalar (:regex "[^A-Z0-9_]|$"))
                    template))
             (list?
              (scan `(:sequence ,list (:regex "[^A-Z0-9_]|$"))
                    template)))
        (cond ((and scalar? list?)
               (error "~a is used as both a scalar and a list" kw))
              (scalar? (out kw))
              (list? (out (make-keyword (fmt "@~a" kw))))
              (t (error "~a does not occur in template" kw))))
      (out value))))

(defun parse-ast-template/keywords (template class args)
  (assert (or (null args) (keywordp (first args))))
  (nest
   ;; Build tables between names, placeholders, and subtrees.
   (let* ((args (canonicalize-kwargs template class args))
          (dummy (allocate-instance (find-class class)))
          (subs (plist-alist args))
          (names (mapcar #'car subs))
          (subtrees (mapcar #'cdr subs))
          (placeholders
           (mapcar (op (template-placeholder* dummy _)) names))
          (temp-subs (pairlis placeholders names))))
   ;; Wrap the tables with convenience accessors.
   (labels ((name-placeholder (name)
              (rassocar name temp-subs :test #'string=))))
   (values
    ;; Substitute the parseable placeholders for the original names so
    ;; the AST parses correctly.
    (reduce (lambda (template name)
              (string-replace-all (template-metavariable dummy name)
                                  template
                                  (name-placeholder name)))
            ;; Replace the metavariables in descending order of length
            ;; (in case one is a prefix of another).
            (sort names #'length> :key #'string)
            :initial-value template)
    names placeholders subtrees)))

(defun check-ast-template-validity (template class kwargs)
  "Compile-time validity checking for templates."
  (mvlet* ((template names placeholders
            (parse-ast-template template class kwargs))
           (ast (convert class template :deepest t)))
    ;; Check that there are no source text fragments. Note that we
    ;; don't check that there are no parse errors, because the overall
    ;; template can still be valid if the metavariables are the parse
    ;; errors.
    (when-let (fragments (collect-if (of-type 'source-text-fragment) ast))
      (error "Template could not be parsed:~%~a" template))
    ;; Check that the AST is printable.
    (handler-case (source-text ast)
      (error (e)
        (error "Template cannot be printed because: ~a" e)))
    ;; Check that the names and placeholders are the same length.
    (unless (length= names placeholders)
      (error "Length mismatch in template arguments."))
    ;; Check that there are no duplicate placeholders.
    (unless (length= placeholders (nub placeholders))
      (error "Duplicate placeholders: ~a" placeholders))
    ;; Check that no placeholders are split between ASTs.
    (let ((found
           (filter (lambda (p)
                     (find-if (lambda (n)
                                (and (null (children n))
                                     (string*= p (source-text n))))
                              ast))
                   placeholders)))
      (when-let (diff (set-difference placeholders found :test #'equal))
        (simple-style-warning
         "Some placeholders in template were not parsed as ASTs: ~a"
         diff)))
    nil))

(defun check-ast-template (template class kwargs &key tolerant)
  (handler-bind ((error
                  (lambda (e)
                    (declare (ignore e))
                    (when tolerant
                      (unless (string$= ";" template)
                        (handler-case
                            (return-from check-ast-template
                              (check-ast-template-validity
                               (string+ template ";")
                               class
                               kwargs))
                          (error (e)
                            (error e))))))))
    (check-ast-template-validity template class kwargs)))

(define-symbol-macro %tolerant nil)

(define-compiler-macro ast-template (&whole call template class &rest kwargs
                                            &environment env)
  "Compile-time validity checking for AST templates."
  (match (list template class)
    ((list (type string) (list 'quote class))
     (when (subtypep class 'tree-sitter-ast env)
       (check-ast-template template class kwargs
                           :tolerant (macroexpand-1 '%tolerant env)))))
  call)

(defun ast-template (template class &rest args)
  "Create an AST of CLASS from TEMPLATE usings ARGS.

You probably don't want to use this function directly; supported
languages allow you to use a function with the same name as shorthand
for creating a template:

    (python \"$ID = 1\" :id \"x\")
    ≡ (ast-template \"$ID = 1\" 'python-ast :id \"x\")

By default metavariables look like `$X', where the name can contain
only uppercase characters, digits, or underscores. \(Syntax can vary
by language; see `template-metavariable'.)

Metavariables can also look like `@ARGS'. In this case they stand for
a list of ASTs, rather than a single AST.

    (ast-template \"fn(@ARGS)\" :args '(1 2 3))
    => <python-call \"fn(1, 2, 3)\">

There are two syntaxes for ARGS.

ARGS can be keyword arguments, defaults for the corresponding
metavariables (converting dashes to underscores):

    (ast-template \"$LEFT_HAND_SIDE = $RIGHT_HAND_SIDE\" 'python-ast
                  :left-hand-side \"x\"
                  :right-hand-side 1)
    => <python-assignment \"x = 1\">

ARGS can be positional (keywords not allowed!). In this case
metavariables must be numbered (`$1', `$2', etc.):

    (ast-template \"$1 = $2\" 'python-ast \"x\" 1)
    ≡ (ast-template \"$1 = $2\" 'python-ast :1 \"x\" :2 1)

    ;; Also works for list arguments.

    (ast-template \"fn(@1)\" '(1 2 3))
    => <python-call \"fn(1, 2, 3)\">

Values in ARGS must be ASTs, literals, or lists. Lists are processed
recursively \(but only to one level). Atoms that are not ASTs or
literals are converted into ASTs using `template-subtree', a generic
function. Atoms that are ASTs are copied into the resulting tree.
Atoms that are literals (such as strings or integers) are inlined into
the string and parsed in place. \(This is necessary as the AST that
corresponds to a string may only be parseable in context.)

    (ast-template \"$1 = value\" 'python-ast \"x\")
    ≡ (ast-template \"$1 = value\" 'python-ast
                    (convert \"x\" 'python-ast :deepest t))

Both keyword and positional syntaxes (as well as list metavariables
with `@') can also be used as Trivia patterns for destructuring.

    (match (python \"x = 2 + 2\")
      ((python \"$1 = $2\" var (python \"$X + $Y\" :x x :y y))
       (list x y)))
    => (#<python-identifier \"x\"> #<python-integer \"2\">
        #<python-integer \"2\">)"
  (nest
   ;; Build tables between names, placeholders, and subtrees.
   (mvlet* ((template names placeholders subtrees
             (parse-ast-template template class args))
            (dummy (allocate-instance (find-class class)))
            (subs
             (iter (for name in names)
                   (for subtree in subtrees)
                   (collect (cons name (template-subtree dummy subtree)))))
            (names (mapcar #'car subs))
            (temp-subs (pairlis placeholders names))))
   ;; Wrap the tables with convenience accessors.
   (labels ((name-placeholder (name)
              (rassocar name temp-subs :test #'string=))
            (name-subtree (name)
              (assocdr name subs :test #'string=))
            (placeholder-targets (placeholder ast)
              (collect-if (lambda (n)
                            (and (null (children n))
                                 (string= (source-text n) placeholder)))
                          ast))))
   ;; Replace the identifiers with subtrees, taking care to copy
   ;; before and after text.
   (let* ((leading-whitespace
           (take-while #'whitespacep template))
          (template
           ;; Insert subtrees that are strings into the template
           ;; before parsing. This lets us use templates to get ASTs
           ;; can only be parsed in the right context.
           (reduce (lambda (template name)
                     (let ((subtree (name-subtree name)))
                       (if (stringp subtree)
                           (string-replace-all
                            (name-placeholder name)
                            template
                            subtree)
                           template)))
                   (sort names #'length> :key #'string)
                   :initial-value template))
          (ast
           (assure ast
             (convert class template :deepest t)))
          (placeholder-paths
           (iter (for p in placeholders)
                 (for targets = (placeholder-targets p ast))
                 (collect
                  (cons p (mapcar (op (ast-path ast _)) targets))))))
     (setf (before-text ast) leading-whitespace))
   (labels ((placeholder-paths (placeholder)
              (assocdr placeholder placeholder-paths :test #'equal))
            (name-paths (name)
              (placeholder-paths (name-placeholder name)))))
   (labels
       ((slot+offset (step)
          "Return the slot and the offset for STEP, a step in a path."
          (match step
            ((and offset (type number))
             ;; TODO This may no longer be valid if
             ;; functional-trees stops giving the
             ;; children slot special treatment.
             (values 'children offset))
            ((cons slot (and offset (type number)))
             (values slot offset))))
        (insert-subtree (ast path subtree)
          (typecase subtree
            (string
             ;; If the subtree is a string it was
             ;; already inlined during the
             ;; parsing phase.
             ast)
            (list
             (multiple-value-bind (slot offset)
                 (slot+offset (lastcar path))
               (unless slot
                 (error "Attempt to insert a list into a non-list location: ~a"
                        path))
               (let ((parent (lookup ast (butlast path))))
                 (symbol-macrolet ((orig (slot-value parent slot)))
                   (setf orig
                         (append
                          (take offset orig)
                          subtree
                          (drop (1+ offset) orig)))
                   ;; Change the class if necessary.
                   (output-transformation parent)
                   (patch-whitespace parent)
                   ast))))
            (t (with ast path subtree))))
        (copy-subtree (subtree target)
          "Copy SUBTREE, preserving before and after text from TARGET.
If SUBTREE is a list do the same for each element."
          (flet ((cp (subtree)
                   (tree-copy
                    (copy subtree
                          :before-text
                          (before-text target)
                          :after-text
                          (after-text target)))))
            (if (listp subtree)
                (mapcar #'cp subtree)
                (cp subtree))))
        (insert-name-subtrees (ast name)
          ;; Strings have already been inlined.
          (if (stringp (name-subtree name)) ast
              (let* ((paths (name-paths name))
                     (subtree (name-subtree name))
                     (targets (mapcar (op (lookup ast _)) paths))
                     (subtree-copies
                      (mapcar (op (copy-subtree subtree _))
                              targets)))
                (reduce
                 (lambda (ast path.subtree)
                   "Insert SUBTREE into AST at PATH."
                   (destructuring-bind (path . subtree) path.subtree
                     (insert-subtree ast path subtree)))
                 (pairlis paths subtree-copies)
                 :initial-value ast))))))
   (reduce (lambda (ast name)
             (insert-name-subtrees ast name))
           names
           :initial-value ast)))

(define-compiler-macro ast-template* (template class &rest args)
  (with-gensyms (ast)
    `(symbol-macrolet ((%tolerant t))
       (match (ast-template ,template ,class ,@args)
         ((and ,ast (or (type parse-error-ast) (type source-text-fragment)))
          ,(if (eq (uiop/utility:last-char template) #\;)
               ast
               `(match (ast-template* ,(concatenate 'string template ";") ,class ,@args)
                  ;; If we get another error/fragment, then return the original AST w/o the semicolon.
                  ((or (type parse-error-ast) (type source-text-fragment)) ,ast)
                  ((and it) it))))
         ((and ,ast (type ast))
          (first (children ,ast)))))))

(defun ast-template* (template class &rest args)
  "Like `ast-template', but return the first child of the created AST.
This is useful for languages where the parser requires semicolons as
delimiters (such as C or C++)."
  (match (apply #'ast-template template class args)
    ((and ast (or (type parse-error-ast) (type source-text-fragment)))
     (if (eq (uiop/utility:last-char template) #\;)
         ast
         (match (apply #'ast-template* (concatenate 'string template ";") class args)
           ;; If we get another error/fragment, then return the original AST w/o the semicolon.
           ((or (type parse-error-ast) (type source-text-fragment)) ast)
           ((and it) it))))
    ((and ast (type ast))
     (first (children ast)))))

(defpattern ast-template (template class &rest args)
  "Match TEMPLATE as a pattern using CLASS and ARGS.

You probably don't want to use this pattern directly; supported
languages allow you to use a pattern with the same name as shorthand:

    (match x ((python \"$ID = 1\" :id \"x\") ...))
    ≡ (match x ((ast-template \"$ID = 1\" 'python-ast :id \"x\") ...))"
  (check-ast-template template class args)
  (mvlet* ((class
            (match class
              ((list 'quote class) class)
              (otherwise class)))
           (language
            (find-external-symbol (drop-suffix "-AST" (string class))
                                  :sel/sw/ts
                                  :error t))
           (pattern
            (convert 'match template :language language))
           (template names placeholders subtrees
            (parse-ast-template template class args))
           (dummy (allocate-instance (find-class class)))
           (metavars
            (mapcar (op (template-metavariable dummy _))
                    names))
           (metavar-subtrees (pairlis metavars subtrees))
           (tree
            (map-tree
             (lambda (node)
               (if (wildcard? node)
                   (let* ((suffix (drop-prefix "WILD-" (string node)))
                          (key
                           (if (string^= "LIST_" suffix)
                               (string+ "@" (drop-prefix "LIST_" suffix))
                               (string+ "$" suffix))))
                     (assocdr key
                              metavar-subtrees
                              :test #'equal))
                   node))
             pattern))
           ;; If the same name occurs more than once in the pattern,
           ;; ignore all but the first occurrence.
           (tree
            (map-tree
             (let ((name-counts (make-hash-table)))
               (lambda (node)
                 (if (and (symbolp node)
                          (not (keywordp node))
                          (member node names :test #'string=))
                     (let ((count (incf (gethash node name-counts 0))))
                       (if (> count 1)
                           '_
                         node))
                     node)))
             tree
             :traversal :inorder)))
    (declare (ignore placeholders template))
    (sublis '((ellipsis-match . _)) tree)))

(defpattern ast-template* (template class &rest args)
  "Like `ast-template', but take the first child."
  (let ((expansion
         (block expansion
           (handler-bind
               ((error
                 (lambda (e)
                   (declare (ignore e))
                   (when-let (expansion
                              (ignore-errors
                               (pattern-expand-1
                                `(ast-template ,(ensure-suffix template ";")
                                               ,class ,@args))))
                     (return-from expansion expansion)))))
             (pattern-expand-1 `(ast-template ,template ,class ,@args))))))
    (ematch expansion
      ((list* _ :children (list 'list pat) _)
       pat))))

(defmacro ast-from-template-aux (ast-template-fn template class &rest args)
  (let ((temps (make-gensym-list (length args))))
    (ematch class
      ((list 'quote class)
       `(ematch (,ast-template-fn ,template ',class ,@args)
          ((,ast-template-fn ,template ,class ,@temps)
           (values ,@temps)))))))

(defmacro ast-from-template (template class &rest args)
  "In one step, build and destructure a template into ASTs.

Must use the positional syntax of `ast-template'. Returns one value
per metavariable, in numeric order (`$1', `$2', etc.).

This is useful because not every kind of AST node can be parsed
directly as a template. E.g. in Python a tuple, an argument list, and
a parameter list all use the same syntax and can only be distinguished
in context. Or (at the time of writing) the C and C++ parsers for
tree-sitter cannot correctly parse unterminated statements. Using
`ast-from-template' lets you provide throwaway context to the parser
while pulling out only the particular nodes that you want."
  `(ast-from-template-aux ast-template ,template ,class ,@args))

(defmacro ast-from-template* (template class &rest args)
  "Like `ast-from-template', but implicitly passes through to the first child."
  `(ast-from-template-aux ast-template* ,template ,class ,@args))
