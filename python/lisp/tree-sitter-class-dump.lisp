(defpackage :software-evolution-library/python/lisp/tree-sitter-py-generator
  (:nicknames :sel/py/lisp/ts-py-generator)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/python/lisp/utility)
  (:export :run-tree-sitter-py-generator))
(in-package :software-evolution-library/python/lisp/tree-sitter-py-generator)
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

(-> ast-symbol-p (symbol &optional list) boolean)
(defun ast-symbol-p (sym &optional (languages (list 'ast)))
  "Return T if SYM is an AST, optionally, of one of the given LANGUAGES."
  (not (null (member sym languages :test #'subtypep))))

(-> class-superclasses (class) list)
(defun class-superclasses (clazz)
  "Return the superclasses of CLAZZ, recursive."
  (labels ((class-superclasses-helper (clazz)
             (cons clazz
                   (mappend #'class-superclasses-helper
                            (class-direct-superclasses clazz)))))
    (cdr (remove-duplicates (class-superclasses-helper clazz) :from-end t))))

(-> remove-duplicates-from-end (list) t)
(defun remove-duplicates-from-end (l)
  "Remove duplicates from l, from right to left."
  (remove-duplicates l :from-end t))

(defun tree-sitter-symbols ()
  "Return the symbols in the tree-sitter package sorted lexicographically."
  (let ((pkg (find-package :sel/sw/tree-sitter)))
    (sort (nest (remove-if-not [{eq pkg} #'symbol-package])
                (loop for sym being the symbols of pkg collect sym))
          #'string<
          :key #'symbol-name)))

;; (-> tree-sitter-class-p ((or class symbol)) boolean)
(defgeneric tree-sitter-class-p (clazz)
  (:documentation "Return T if CLAZZ is a class in the tree-sitter package.")
  (:method ((clazz class))
    (tree-sitter-class-p (class-name clazz)))
  (:method ((sym symbol))
    (eq (package-name-keyword (symbol-package sym))
        :software-evolution-library/software/tree-sitter)))

;; (-> ignore-class-p ((or class symbol)) boolean)
(defgeneric ignore-class-p (clazz)
  (:documentation "Return T if CLAZZ should be ignored when generating python
classes.")
  (:method ((clazz class))
    (ignore-class-p (class-name clazz)))
  (:method ((sym symbol))
    (not (null (member sym '(tree-sitter-ast structured-text computed-text))))))

;; (-> class-and-python-dependencies ((or class symbol)) list)
(defgeneric class-and-python-dependencies (clazz)
  (:documentation "Return CLAZZ and the python-relevant dependencies of CLAZZ
in top-down order.")
  (:method ((clazz class))
    (append (nest (reverse)
                  (remove-if #'ignore-class-p)
                  (remove-if-not #'tree-sitter-class-p)
                  (class-superclasses clazz))
            (list clazz)))
  (:method ((sym symbol))
    (class-and-python-dependencies (find-class sym))))

(-> python-class-str (class) string)
(defun python-class-str (clazz)
  "Return a python class declaration string for the given common lisp CLAZZ."
  (format nil "class ~a(~{~a~^, ~}):~%    pass~%~%"
          (common-lisp-to-python-type clazz)
          (append (nest (mapcar #'common-lisp-to-python-type)
                        (remove-if #'ignore-class-p)
                        (remove-if-not #'tree-sitter-class-p)
                        (class-direct-superclasses clazz))
                  (list "AST"))))

(define-command tree-sitter-py-generator (&spec +command-line-options+)
  "Command line interface for tree-sitter python class generator."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (when help (show-help-for-tree-sitter-py-generator) (quit 0))

  (format *standard-output* "from .asts import AST~%~%")
  (nest (mapcar [{format *standard-output* "~a"} #'python-class-str])
        (remove-duplicates-from-end)
        (mappend #'class-and-python-dependencies)
        (remove-if-not {ast-symbol-p _ languages})
        (tree-sitter-symbols)))
