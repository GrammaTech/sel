(defpackage :software-evolution-library/python/lisp/tree-sitter-class-dump
  (:nicknames :sel/py/lisp/ts-class-dump)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter)
  (:export :run-tree-sitter-class-dump))
(in-package :software-evolution-library/python/lisp/tree-sitter-class-dump)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    '((("help" #\h #\?) :type boolean :optional t
       :documentation "display help output")
      (("languages" #\L) :type string :optional t
       :initial-value "c,cpp,javascript,python"
       :action #'handle-languages-argument
       :documentation
       "comma-delimited source languages of the ASTs to dump"))))

(-> handle-languages-argument (string) list)
(defun handle-languages-argument (languages)
  "Transform the input comma-delimited source languages into a list of
language-specific AST types."
  (nest (mapcar (lambda (language)
          (intern (format nil "~a-AST" (string-upcase (symbol-name language)))
                  :sel/sw/tree-sitter)))
        (mapcar #'sel/command-line::alias-language)
        (split-sequence #\, languages)))

(-> superclasses (class) list)
(defun superclasses (clazz)
  "Return the superclasses of CLAZZ, recursive."
  (labels ((superclasses-helper (clazz)
             (cons clazz
                   (mappend #'superclasses-helper
                            (class-direct-superclasses clazz)))))
    (cdr (remove-duplicates (superclasses-helper clazz)))))

(-> ast-symbol-p (symbol &optional list) (or nil list))
(defun ast-symbol-p (sym &optional (languages (list 'ast)))
  "Return non-nil if SYM is an AST, optionally, of one of the given LANGUAGES."
  (member sym languages :test #'subtypep))

(-> tree-sitter-superclasses (symbol &optional list) list)
(defun tree-sitter-superclasses (sym &optional (ignored-syms '(tree-sitter-ast
                                                               structured-text
                                                               computed-text)))
  "Return the superclasses of SYM in the tree-sitter package."
  (labels ((tree-sitter-class-p (sym)
             (eq (package-name-keyword (symbol-package sym))
                 :software-evolution-library/software/tree-sitter)))
    (nest (mapcar #'symbol-name)
          (remove-if «or [#'not #'tree-sitter-class-p] {member _ ignored-syms}»)
          (mapcar #'class-name)
          (superclasses)
          (find-class sym))))

(define-command tree-sitter-class-dump (&spec +command-line-options+)
  "Command line interface for tree-sitter class dumper."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (when help (show-help-for-tree-sitter-class-dump) (quit 0))

  (encode-json (iter (iter:with ht = (make-hash-table))
                     (for e in (package-exports :sel/sw/tree-sitter))
                     (when (ast-symbol-p e languages)
                       (setf (gethash (symbol-name e) ht)
                             (tree-sitter-superclasses e)))
                     (finally (return ht)))
               *standard-output*))
