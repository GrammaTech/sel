;;;; string-clauses.lisp -- Support for string AST clauses
(defpackage :software-evolution-library/software/string-clauses
  (:nicknames :sel/software/string-clauses :sel/sw/string-clauses)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/utility/range))
(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

(defparameter *slots-excluded*
  '(sel::oid
    sel/software/parseable:indent-children
    sel/software/parseable:indent-adjustment
    sel/software/parseable:stored-hash
    sel/software/tree-sitter::rule
    sel/software/tree-sitter::pruned-rule
    sel/software/tree-sitter::json-rule
    sel/software/tree-sitter::slot-usage
    sel/software/tree-sitter:choice-superclass
    sel/software/tree-sitter::choice-subclasses
    fset::serial-number
    ;; The next two are specific to CCL.
    fset::next-serial-number
    fset::next-serial-number-lock
    functional-trees:descendant-map
    fset:size
    functional-trees:child-slots
    functional-trees:child-slot-specifiers)
  "List of slots to exclude when converting an AST to a list expression.")

(defparameter *match-refinement-functions* nil)

(defconst +metavariable-prefix+
  "string_clause_metavar_")

(defvar-unbound *annotation-number*
  "Used to generate unique identifiers for annotation slots.")

(defgeneric ellipsis-match-p (node result)
  (:method ((node t) (result t)) nil)
  (:method ((node parse-error-ast) (result list))
    (equal (source-text node) "...")))

(defgeneric ast-for-match (language string &key software context tolerant)
  (:method :around (language string &key software context tolerant)
    (let ((string (disarm-metavariables language string)))
      (if (and software context)
          (parse-in-context software context string)
          (if tolerant
              (parse-tolerant (language-ast-class language)
                              string)
              (call-next-method language string
                                :software software
                                :context context)))))
  (:method (language string &key software context tolerant)
    (declare (ignore software context tolerant))
    (convert (language-ast-class language)
             string
             :deepest t)))

(defgeneric parse-tolerant (class string)
  (:method (class (string string))
    (convert class string :deepest t)))

(defun wildcard? (node)
  "Is NODE a wildcard (symbol that starts with WILD_)?"
  (and (symbolp node)
       (eql (find-package :software-evolution-library/software/string-clauses)
            (symbol-package node))
       (string^= 'wild- node)))

(defun make-wild-symbol (wildcard-name)
  (format-symbol :sel/sw/string-clauses "WILD-~a" wildcard-name))

(defmethod convert ((to-type (eql 'match)) (not-ast t) &key &allow-other-keys) not-ast)
(defmethod convert ((to-type (eql 'match)) (ast ast) &key &allow-other-keys)
  "Convert an AST into a trivia MATCH clause.
NOTE: This is the location at which customizations in the string
pattern matching language should be applied. Including wildcards,
typed wildcards, CL-PPCRE string wildcards, tildes for semantically
similar matches, and elipses for matching series of ASTs."
  (assert (boundp '*annotation-number*))
  (match ast
    ((ast :text (ppcre
                 #.(string+ "^"
                            +metavariable-prefix+
                            "([A-Z0-9_]+)"
                            "(_(call|class|function|string|number|comment))"
                            "?$")
                 wildcard-name _ type))
     (let ((wild-symbol
            (make-wild-symbol wildcard-name)))
       (if type
           (list (intern (string-join (list (string-upcase type) "AST") #\-)
                         (find-package :sel/sw/ts))
                 wild-symbol)
           wild-symbol)))
    ((ast :text (ppcre "^[\"']?=~/(.+)/[\"']?$" re))
     `(ast :text (ppcre ,re)))
    ((string-ast :children (list* (ast :text (ppcre "^[\"']?=~/(.+)/[\"']?$" re)) _))
     `(ast :text (ppcre ,re)))
    ;; For a list metavariable, we want the shallowest match with the
    ;; same source text.
    ((and _
          (type ast)
          (access #'source-text
                  (ppcre #.(string+ "^" +metavariable-prefix+
                                    "(LIST_[A-Z0-9_]+)$")
                         name)))
     (make-wild-symbol name))
    (otherwise
     (let ((result '())
           (slot-names
             (mapcar 'slot-definition-name
                     (class-slots (class-of ast))))
           (class-name (class-name (class-of ast))))
       (push class-name result)
       (dolist (slot-name slot-names)
         (unless (member slot-name *slots-excluded*)
           (let ((val (slot-value ast slot-name)))
             (push (make-keyword slot-name) result)
             (cond                    ; These are the main rules.
               ((eql slot-name 'annotations)
                ;; Preserve the child-order annotation.
                (if (aget :child-order val)
                    ;; TODO Just the child order?
                    (push (format-symbol :sel/sw/string-clauses
                                         "annot~a"
                                         (finc *annotation-number*))
                          result)
                    (pop result)))
               ((null val) (pop result))
               ((and (listp val) (every «and #'stringp #'emptyp» val)) (pop result))
               ((and (stringp val) (emptyp val)) (pop result))
               ((consp val)
                (if (some (op (ellipsis-match-p _ result)) val)
                    (push 'ellipsis-match result)
                    ;; Handle a list match whether it is the sole item
                    ;; or it occurs at the end of the list context.
                    (let ((subpattern (mapcar {convert 'match} val)))
                      (if (and (wildcard? (lastcar subpattern))
                               (string*= "LIST_" (lastcar subpattern)))
                          (push (cons 'list* subpattern) result)
                          (push (cons 'list subpattern) result)))))
               ((match val
                  ((and _
                        (type ast)
                        (access #'source-text
                                (ppcre #.(string+ "^" +metavariable-prefix+
                                                  "(LIST_[A-Z0-9_]+)$")
                                       name)))
                   (push
                    `(access #'children ,(make-wild-symbol name))
                    result))))
               (t (push (convert 'match val) result))))))
       (nreverse result)))))

(defun language-ast-class (language)
  "Get the topmost superclass common to all AST classes for LANGUAGE."
  (intern (string-join (list language "AST") #\-)
          (find-package :sel/sw/ts)))

(defun parse-in-context (software ast clause)
  "Parse CLAUSE, a string, as if it appeared in the same location as
AST.

A string may not parse in the same way at the top level that it does
in context."
  (check-type clause string)
  (let* ((ast-class (language-ast-class (type-of software)))
         (path (ast-path software ast))
         (ranges (ast-source-ranges software))
         (range (assocdr ast ranges))
         (source (source-text (genome software)))
         (start (source-location->position source (begin range)))
         (end (source-location->position source (end range)))
         (new-string
           (concatenate 'string
                        (subseq source 0 start)
                        clause
                        (subseq source end)))
         (new-genome (convert ast-class new-string)))
    (lookup new-genome path)))

(defmethod convert :around
    ((to-type (eql 'match)) (clause string)
     &key language software context (tolerant t) &allow-other-keys)
  (if language
      (convert 'match (ast-for-match language clause
                                     :software software
                                     :context context
                                     :tolerant tolerant))
      (call-next-method)))

(defmethod convert :around
    ((to-type (eql 'match)) (clause t)
     &key)
  "This wrapper methods has two different intended effects:
1. Bind `*annotation-numbers*' around nested methods.
2. Generalize specialized class names with the enclosing `map-tree'."
  (map-tree (lambda (node)                   ; (2)
              (or (and (symbolp node)
                       (tree-sitter-class-name node))
                  node))
            (if (boundp '*annotation-number*) ; (1)
                (call-next-method)
                (let ((*annotation-number* 0))
                  (call-next-method)))))

(defmethod convert ((to-type (eql 'replace)) (not-clause t)
                    &key &allow-other-keys)
  not-clause)
(defmethod convert ((to-type (eql 'replace)) (ast ast) &key &allow-other-keys)
  (convert 'replace (convert 'match ast)))
(defmethod convert ((to-type (eql 'replace)) (clause list)
                    &rest args
                    &key &allow-other-keys)
  "Convert an AST into a trivia REPLACEMENT clause."
  (cond ((eql 'and (car clause))
         (third clause))
        ((and (find-class (car clause) nil) ;E.g. the car might be list*.
              (subclassp (find-class (car clause)) 'ast))
         (list* 'make-instance `',(car clause)
                (mapcar (op (apply #'convert 'replace _ args))
                        (cdr clause))))
        (t
         (list* (car clause)
                (mapcar (op (apply #'convert 'replace _ args))
                        (cdr clause))))))
(defmethod convert :around
    ((to-type (eql 'replace)) (clause string)
     &key language software context &allow-other-keys)
  (cond
    (language (convert 'replace (ast-for-match language clause software context)))
    (t (call-next-method))))

(defgeneric disarm-metavariables (language string)
  (:documentation "Disarm metavariables in STRING.

This also takes a LANGUAGE parameter in case we want to allw for a
different surface syntax for languages that use sigils (Bash, Perl).")
  (:method ((language symbol) string)
    (disarm-metavariables (make language) string))
  (:method ((language class) string)
    (disarm-metavariables (make language) string))
  (:method ((software software) (string string))
    (setf string
          (regex-replace-all "\\@([_A-Z0-9]+)"
                             string
                             #.(string+ +metavariable-prefix+ "LIST_" "\\1"))
          string
          (regex-replace-all "\\$([_A-Z0-9]+)"
                             string
                             #.(string+ +metavariable-prefix+ "\\1")))
    string))
