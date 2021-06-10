;;; template.lisp --- Templates for de/constructing ASTs
(defpackage :software-evolution-library/software/template
  (:nicknames :sel/software/template :sel/sw/template)
  (:use :gt/full
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/parseable)
  (:import-from :software-evolution-library/software/tree-sitter
                :before-text :after-text)
  (:import-from :software-evolution-library/software/string-clauses
                :ellipsis-match
                :wildcard?)
  (:export :ast-template
           :template-placeholder
           :template-metavariable
           :template-subtree
           :ast-from-template))
(in-package :software-evolution-library/software/template)

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
    (string+ #\_ (substitute #\_ #\- (string name)))))

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
      (assert (scan "^[A-Z0-9_]+$" name) () "Invalid metavariable: ~a" name)
      (fmt "$~a" name))))

(defgeneric template-subtree (ast thing)
  (:documentation "Convert THING into a subtree.")
  (:method (ast (x integer))
    (princ-to-string x))
  (:method ((ast ast) (x string))
    x)
  (:method ((class t) (x ast))
    x)
  (:method ((ast t) (list list))
    ;; It doesn't make sense to try to parse strings in list inline,
    ;; since we need the nodes to be in the AST to know how to
    ;; interleave them. (Or does it?)
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
             (t subtree))))))

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

(defun parse-ast-template/keywords (template class args)
  (assert (or (null args) (keywordp (first args))))
  (nest
   ;; Build tables between names, placeholders, and subtrees.
   (let* ((dummy (allocate-instance (find-class class)))
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
            names
            :initial-value template)
    names placeholders subtrees)))

(defun check-ast-template (template class kwargs)
  "Compile-time validity checking for templates."
  (mvlet* ((template names placeholders
            (parse-ast-template template class kwargs))
           (ast (convert class template)))
    ;; Check that there are no parse errors.
    (when-let (errs (collect-if (of-type 'parse-error-ast) ast))
      (unless (every (op (equal (source-text _) "...")) errs)
        (error "Template contains parse errors:~%~a" template)))
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
    ;; Check that all the placeholders are used.
    (let ((found
           (filter (lambda (p)
                     (find-if (lambda (n)
                                (and (typep n 'identifier-ast)
                                     (string= (source-text n) p)))
                              ast))
                   placeholders)))
      (when-let (diff (set-difference placeholders found :test #'equal))
        (error
         "Some placeholders in template were not parsed as identifiers: ~a"
         diff)))
    nil))

(define-compiler-macro ast-template (&whole call template class &rest kwargs)
  "Compile-time validity checking for AST templates."
  (match (list template class)
    ((list (type string) (list 'quote class))
     (check-ast-template template class kwargs)))
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

Values in ARGS must be either ASTs or strings. Values that are not
ASTs or strings are converted into ASTs using `template-subtree', a
generic function. Values that are ASTs are copied into the resulting
tree. Values that are strings are inlined into the string and parsed
in place.

    (ast-template \"$1 = value\" 'python-ast \"x\")
    ≡ (ast-template \"$1 = value\" 'python-ast
                    (convert \"x\" 'python-ast :deepest t)

Both syntaxes can also be used as Trivia patterns for destructuring.

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
            (subtrees (mapcar #'cdr subs))
            (names (mapcar #'car subs))
            (temp-subs (pairlis placeholders names))))
   ;; Wrap the tables with convenience accessors.
   (labels ((name-placeholder (name)
              (rassocar name temp-subs :test #'string=))
            (name-subtree (name)
              (assocdr name subs :test #'string=))
            (placeholder-targets (placeholder ast)
              (collect-if (lambda (n)
                            (and (typep n 'identifier-ast)
                                 (string= (source-text n)
                                          placeholder)))
                          ast))))
   ;; Replace the identifiers with subtrees, taking care to copy
   ;; before and after text.
   (let* ((template-stripped
           (string-left-trim whitespace template))
          (leading-whitespace
           (take (- (length template)
                    (length template-stripped))
                 template))
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
   (let* ((ast
           (if (notany #'stringp subtrees)
               ast
               (let ((template
                      (reduce (lambda (template name)
                                (let ((subtree (name-subtree name)))
                                  (if (stringp subtree)
                                      (string-replace-all
                                       (name-placeholder name)
                                       template
                                       subtree)
                                      template)))
                              names
                              :initial-value template)))
                 (assure ast
                   (convert class template :deepest t)))))))
   (reduce (lambda (ast name)
             (let* ((paths (name-paths name))
                    (targets (mapcar (op (lookup ast _)) paths))
                    (subtrees
                     (nest
                      (let ((subtree (name-subtree name))))
                      (iter (for target in targets))
                      (flet ((cp (subtree)
                               (tree-copy
                                (copy subtree
                                      :before-text
                                      (before-text target)
                                      :after-text
                                      (after-text target))))))
                      (collect)
                      (if (listp subtree)
                          (mapcar #'cp subtree))
                      (cp subtree))))
               (reduce
                (lambda (ast path.subtree)
                  (destructuring-bind (path . subtree)
                      path.subtree
                    (cond
                      ((stringp subtree)
                       ;; If the subtree is a string it was
                       ;; already inlined during the
                       ;; parsing phase.
                       ast)
                      ((listp subtree)
                       (multiple-value-bind (slot offset)
                           (match (lastcar path)
                             ((and offset (type number))
                              (values 'children offset))
                             ((cons slot (and offset (type number)))
                              (values slot offset)))
                         (if slot
                             (nest
                              (let ((parent (lookup ast (butlast path)))))
                              (symbol-macrolet ((orig (slot-value parent slot)))
                                (setf orig
                                      (append
                                       (take offset orig)
                                       subtree
                                       (drop (1+ offset) orig)))
                                ;; Change the class if necessary.
                                (sel/sw/ts::output-transformation parent)
                                (patch-whitespace parent)
                                ast))
                             (error "Attempt to insert a list into a non-list location: ~a"
                                    path))))
                      (t
                       (with ast path subtree)))))
                (pairlis paths subtrees)
                :initial-value ast)))
           names
           :initial-value ast)))

(defun wildcard? (node)
  "Is NODE a wildcard (symbol that starts with WILD_)?"
  (and (symbolp node)
       (eql (find-package :software-evolution-library/software/string-clauses)
            (symbol-package node))
       (string^= 'wild- node)))

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
                   (assocdr (string+ "$" (drop-prefix "WILD-" (string node)))
                            metavar-subtrees
                            :test #'equal)
                   node))
             pattern)))
    (declare (ignore placeholders template))
    (sublis '((ellipsis-match . _)) tree)))

(defmacro ast-from-template (template class &rest args)
  "In one step, build and destructure a template into ASTs.

Must use the positional syntax of `ast-template'. Returns one value
per metavariable, in numeric order (`$1', `$2', etc.).

Not every kind of AST node can be parsed directly as a template. E.g.
in Python a tuple, an argument list, and a parameter list all use the
same syntax and can only be distinguished in context. Or (at the time
of writing) the C and C++ parsers for tree-sitter cannot correctly
parse unterminated statements. Using `ast-from-template' lets you
provide throwaway context to the parser while pulling out only the
particular nodes that you want."
  (let ((temps (make-gensym-list (length args))))
    (ematch class
      ((list 'quote class)
       `(ematch (ast-template ,template ',class ,@args)
          ((ast-template ,template ,class ,@temps)
           (values ,@temps)))))))
