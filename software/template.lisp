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

(defun ast-template (template class &rest kwargs &key &allow-other-keys)
  "Create an AST of CLASS from TEMPLATE.

For each of the keyword arguments, looks for a delimited occurrence of
the name of the keyword argument, and substitutes the given
subtree.

The default delimiters are {{}}, but this can vary by language."
  (nest
   ;; Build tables between names, placeholders, and subtrees.
   (let* ((dummy (make class))
          (subs (plist-alist kwargs))
          (subs
           (iter (for (name . value) in subs)
                 (collect (cons (string-invert-case name)
                                (template-subtree dummy value)))))
          (names (mapcar #'car subs))
          (placeholders
           (mapcar (op (template-placeholder dummy _)) names))
          (temp-subs (mapcar #'cons placeholders names))))
   ;; Wrap the tables with convenience accessors.
   (labels ((name-placeholder (name)
              (rassocar name temp-subs :test #'string=))
            (name-subtree (name)
              (assocdr name subs :test #'string=))
            (name-target (name ast)
              (let ((placeholder (name-placeholder name)))
                (find-if (lambda (n)
                           (and (typep n 'identifier-ast)
                                (string= (source-text n)
                                         placeholder)))
                         ast)))))
   ;; Substitute the parseable placeholders for the original names.
   ;; (This may be necessary when, say, using a name like
   ;; `read-function` in Python; we need to substitute it with
   ;; something that Python will treat as a single identifier.
   (mvlet* ((start end
             (assure (values string string &optional)
               (template-delimiters dummy)))
            (template
             (reduce (lambda (template name)
                       (string-replace (string+ start name end)
                                       template
                                       (name-placeholder name)))
                     names
                     :initial-value template))))
   ;; Replace the identifiers with subtrees, taking care to copy
   ;; before and after text.
   (reduce (lambda (ast name)
             (let* ((target (name-target name ast))
                    (subtree
                     (copy (name-subtree name)
                           :before-text (before-text target)
                           :after-text (after-text target))))
               (with ast
                     (ast-path ast target)
                     subtree)))
           names
           :initial-value (convert class template :deepest t))))
