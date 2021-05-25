;;; parseable.lisp --- Software which may be parsed into ASTs
(defpackage :software-evolution-library/software/template
  (:nicknames :sel/software/template :sel/sw/template)
  (:use :gt/full
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/parseable)
  (:import-from :software-evolution-library/software/tree-sitter
                :before-text :after-text)
  (:import-from :ssr/string-clauses)
  (:export :ast-template
           :template-placeholder
           :template-metavariable
           :template-subtree))
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
    (template-subtree ast (princ-to-string x)))
  (:method ((ast ast) (x string))
    (convert (type-of ast) x :deepest t))
  (:method ((class t) (x ast))
    x))

(-> parse-ast-template (string symbol list &key (:recursive boolean))
    (values string list list list &optional))
(defun parse-ast-template (template class args &key recursive)
  "Parse TEMPLATE and return four values:
1. The template with placeholders substituted for metavariables.
2. A list of the original metavariable names.
3. A list of placeholders.
4. A list of subtrees."
  (nest
   (if (nor recursive (keywordp (car args)))
       ;; Handle the positional syntax.
       (parse-ast-template
        template class
        (iter (for arg in args)
              (for i from 1)
              (collect (make-keyword (princ-to-string i)))
              (collect arg))
        :recursive t))
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
    (when (find-if (of-type 'parse-error-ast) ast)
      (error "Template contains parse errors:~%~a" template))
    ;; Check that the AST is printable.
    (handler-case (source-text ast)
      (error (e)
        (error "Template cannot be printed because: ~a" e)))
    ;; Check that there names and placeholders are the same length.
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

Values in ARGS that are not ASTs are converted into ASTs using
`template-subtree', a generic functon. By default this recursively
calls `convert' on strings (or values with trivial string equivalents,
like integers).

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
            (names (mapcar #'car subs))
            (temp-subs (pairlis placeholders names))))
   ;; Wrap the tables with convenience accessors.
   (labels ((name-placeholder (name)
              (rassocar name temp-subs :test #'string=))
            (name-subtree (name)
              (assocdr name subs :test #'string=))
            (name-targets (name ast)
              (let ((placeholder (name-placeholder name)))
                (collect-if (lambda (n)
                              (and (typep n 'identifier-ast)
                                   (string= (source-text n)
                                            placeholder)))
                            ast)))))
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
             (convert class template :deepest t))))
     (setf (before-text ast) leading-whitespace))
   (reduce (lambda (ast name)
             (let* ((targets (name-targets name ast))
                    (subtrees
                     (let ((subtree (name-subtree name)))
                       (iter (for target in targets)
                             (collect
                              ;; Use tree-copy to force a new SN.
                              ;; Is there some more idiomatic way
                              ;; to do this?
                              (tree-copy
                               (copy subtree
                                     :before-text
                                     (before-text target)
                                     :after-text
                                     (after-text target))))))))
               ;; TODO Handle lists of subtrees as follows: iff the
               ;; path of the target AST ends with a number, find the
               ;; parent and splice into its children. But: how to
               ;; synthesize the "interleaved text" reliably?
               (reduce (lambda (ast target.subtree)
                         (destructuring-bind (target . subtree)
                             target.subtree
                           (with ast
                                 (ast-path ast target)
                                 subtree)))
                       (pairlis targets subtrees)
                       :initial-value ast)))
           names
           :initial-value ast)))

(defun ssr-wildcard? (node)
  "Is NODE an SSR wildcard (symbol that starts with WILD_)?"
  (and (symbolp node)
       (eql (find-package :ssr/string-clauses)
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
           (metavar-subtrees (pairlis metavars subtrees)))
    (declare (ignore placeholders template))
    (map-tree (lambda (node)
                (if (ssr-wildcard? node)
                    (assocdr (string+ "$" (drop-prefix "WILD-" (string node)))
                             metavar-subtrees
                             :test #'equal)
                    node))
              pattern)))
