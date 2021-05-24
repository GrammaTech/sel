;;; parseable.lisp --- Software which may be parsed into ASTs
(defpackage :software-evolution-library/software/template
  (:nicknames :sel/software/template :sel/sw/template)
  (:use :gt/full
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/parseable)
  (:import-from :software-evolution-library/software/tree-sitter
                :before-text :after-text)
  (:export :ast-template
           :template-placeholder
           :template-delimiters))
(in-package :software-evolution-library/software/template)

(defgeneric template-placeholder (ast name)
  (:documentation "Generic so a different syntax can be used per-language.")
  (:method ((ast t) name)
    ;; A reasonable default for most programming languages. Add a
    ;; leading underscore to avoid having to worry about what
    ;; characters a symbol can begin with.
    (string+ #\_ (substitute #\_ #\- (string name)))))

(defun template-placeholder* (ast name)
  "Append a random number to the end of the placeholder."
  (let ((placeholder (template-placeholder ast name)))
    (string+ placeholder (fmt "~x" (random 1000000)))))

(defgeneric template-delimiters (ast)
  (:documentation "Generic so a different syntax can be used per-language.")
  (:method ((ast t))
    (values "{{" "}}")))

(defgeneric template-subtree (ast thing)
  (:documentation "Convert THING into a subtree.")
  ;; TODO Handle single/double floats.
  (:method (ast (x integer))
    (template-subtree ast (princ-to-string x)))
  (:method ((ast ast) (x string))
    (convert (type-of ast) x :deepest t))
  (:method ((class t) (x ast))
    x))

;;; TODO Verify at compile time that the AST with placeholders is valid.

(defun parse-ast-template (template class kwargs)
  "Create an AST of CLASS from TEMPLATE.

For each of the keyword arguments, looks for a delimited occurrence of
the name of the keyword argument, and substitutes the given
subtree.

The default delimiters are {{}}, but this can vary by language."
  (nest
   ;; Build tables between names, placeholders, and subtrees.
   (let* ((dummy (allocate-instance (find-class class)))
          (subs (plist-alist kwargs))
          (subs
           (iter (for (name . value) in subs)
                 (collect (cons (string-invert-case name)
                                value))))
          (names (mapcar #'car subs))
          (subtrees (mapcar #'cdr subs))
          (placeholders
           (mapcar (op (template-placeholder* dummy _)) names))
          (temp-subs (mapcar #'cons placeholders names))))
   ;; Wrap the tables with convenience accessors.
   (labels ((name-placeholder (name)
              (rassocar name temp-subs :test #'string=))))
   ;; Substitute the parseable placeholders for the original names.
   ;; (This may be necessary when, say, using a name like
   ;; `read-function` in Python; we need to substitute it with
   ;; something that Python will treat as a single identifier.
   (mvlet* ((start end
             (assure (values string string &optional)
               (template-delimiters dummy)))
            (template
             (reduce (lambda (template name)
                       (string-replace-all (string+ start name end)
                                           template
                                           (name-placeholder name)))
                     names
                     :initial-value template)))
     (values template names placeholders subtrees))))

(defun check-ast-template (template class kwargs)
  "Compile-time validity checking for templates."
  (mvlet* ((template names placeholders
            (parse-ast-template template class kwargs))
           (ast (convert class template)))
    ;; Check that there are no parse errors.
    (when (find-if (of-type 'parse-error-ast) ast)
      (error "Template contains parse errors:~%~a" template))
    (handler-case (source-text ast)
      (error (e)
        (error "Template cannot be printed because: ~a" e)))
    (unless (length= names placeholders)
      (error "Length mismatch in template arguments."))
    (unless
        ;; Check that the placeholders are parsed as identifiers.
        (every (lambda (p)
                 (find-if (lambda (n)
                            (and (typep n 'identifier-ast)
                                 (string= (source-text n) p)))
                          ast))
               placeholders)
      (error "Some placeholders in template cannot be parsed as identifiers."))
    nil))

(defun ast-template (template class &rest kwargs &key &allow-other-keys)
  "Create an AST of CLASS from TEMPLATE.

For each of the keyword arguments, looks for a delimited occurrence of
the name of the keyword argument, and substitutes the given
subtree.

The default delimiters are {{}}, but this can vary by language."
  (nest
   ;; Build tables between names, placeholders, and subtrees.
   (mvlet* ((template names placeholders subtrees
             (parse-ast-template template class kwargs))
            (dummy (make class))
            (subs (mapcar #'cons names subtrees))
            (subs
             (iter (for (name . value) in subs)
                   (collect (cons name
                                  (template-subtree dummy value)))))
            (names (mapcar #'car subs))
            (temp-subs (mapcar #'cons placeholders names))))
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
                       (mapcar #'cons targets subtrees)
                       :initial-value ast)))
           names
           :initial-value ast)))

(define-compiler-macro ast-template (&whole call template class &rest kwargs)
  (match (list template class)
    ((list (type string) (list 'quote class))
     (check-ast-template template class kwargs)))
  call)

(defpattern ast-template (template class &rest kwargs)
  (declare (ignore template class kwargs))
  (error "Pattern matching on AST templates is not yet supported."))
