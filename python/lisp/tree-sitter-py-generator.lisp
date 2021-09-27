(defpackage :software-evolution-library/python/lisp/tree-sitter-py-generator
  (:nicknames :sel/py/lisp/ts-py-generator)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/python/lisp/utility)
  (:import-from :software-evolution-library/command-line :alias-language)
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
                  (intern (format nil "~a-AST"
                                  (string-upcase (symbol-name language)))
                          :sel/sw/tree-sitter)))
        (mapcar #'alias-language)
        (split-sequence #\, languages)))

(-> ast-symbol-p (symbol &optional list) list)
(defun ast-symbol-p (sym &optional (languages (list 'ast)))
  "Return non-NIL if SYM is an AST, optionally, of one of the given LANGUAGES."
  (member sym languages :test #'subtypep))

(-> remove-duplicates-from-end (list) list)
(defun remove-duplicates-from-end (l)
  "Remove duplicates from l, from right to left."
  (remove-duplicates l :from-end t))

(-> tree-sitter-symbols () list)
(defun tree-sitter-symbols ()
  "Return the symbols in the tree-sitter package sorted lexicographically."
  (let ((pkg (find-package :sel/sw/tree-sitter)))
    (sort (nest (remove-if-not [{eq pkg} #'symbol-package])
                (loop for sym being the symbols of pkg collect sym))
          #'string<
          :key #'symbol-name)))

;; (-> tree-sitter-class-p ((or class symbol)) boolean)
(defgeneric tree-sitter-class-p (class)
  (:documentation "Return T if CLASS is in the tree-sitter package.")
  (:method ((class class))
    (tree-sitter-class-p (class-name class)))
  (:method ((symbol symbol))
    (eq (package-name-keyword (symbol-package symbol))
        :software-evolution-library/software/tree-sitter)))

;; (-> ignore-class-p ((or class symbol)) boolean)
(defgeneric ignore-class-p (class)
  (:documentation "Return non-NIL if CLASS should be ignored when generating
python classes.")
  (:method ((class class))
    (ignore-class-p (class-name class)))
  (:method ((symbol symbol))
    (member symbol '(tree-sitter-ast structured-text computed-text))))

;; (-> class-and-python-dependencies ((or class symbol)) list)
(defgeneric class-and-python-dependencies (class)
  (:documentation "Return CLASS and the python-relevant dependencies of CLASS
in top-down order.")
  (:method ((class class))
    (append (nest (reverse)
                  (remove-if #'ignore-class-p)
                  (remove-if-not #'tree-sitter-class-p)
                  (compute-class-precedence-list class))
            (list class)))
  (:method ((symbol symbol))
    (class-and-python-dependencies (find-class symbol))))

(-> python-class-str (class) string)
(defun python-class-str (class)
  "Return a python class declaration string for the given common lisp CLASS."
  (labels ((python-superclasses (class)
             "Return the superclasses of CLASS to be included in the
              python class definition."
             (append (nest (mapcar #'cl-to-python-type)
                           (remove-if #'ignore-class-p)
                           (remove-if-not #'tree-sitter-class-p)
                           (class-direct-superclasses class))
                     (list "AST")))
           (join-with-newlines (lines)
             "Join the given list of LINES into a single string separated
              by newlines."
             (string-join lines #\Newline))
           (indent (str &optional (indentation 4))
             "Add INDENTATION spaces to the prefix of every line in STR."
             (nest (join-with-newlines)
                   (mapcar (lambda (line)
                             (concatenate 'string
                                          (repeat-sequence " " indentation)
                                          line))
                           (split-sequence #\Newline str))))
           (child-slots-definition (class)
             "Return the slot definition for the \"child-slots\" slot on CLASS."
             (find-if [{eq 'child-slots} #'slot-definition-name]
                      (compute-slots class)))
           (child-slot-ignorable-p (slot)
             "Return non-NIL if the given child slot should not be included
              in the python class definition's properties."
             (or (eq (car slot) 'children) (internal-child-slot-p slot)))
           (python-property-name (slot)
             "Convert the given child SLOT name to a python property name."
             (string-replace-all "-"
                                 (string-downcase (cl-to-python-slot-name slot))
                                 "_"))
           (python-child-slots (class)
             "Return the child slots of CLASS relevant for creating python
              property definitions."
             (when-let ((slot-definition (child-slots-definition class))
                        (_ (null (class-direct-subclasses class))))
               (and (eq (slot-definition-allocation slot-definition) :class)
                    (slot-definition-initform slot-definition)
                    (apply #'remove-if #'child-slot-ignorable-p
                           (cdr (slot-definition-initform slot-definition))))))
           (python-property (slot)
             "Return a string defining a python property for the given SLOT."
             (destructuring-bind (symbol . arity) slot
               (nest (indent)
                     (join-with-newlines)
                     (list "@cached_property"
                           (format nil "def ~a(self) -> ~a:"
                                   (python-property-name symbol)
                                   (if (zerop arity) "List[AST]" "AST"))
                           (format nil "    return self.child_slot(\"~a\")"
                                   (cl-to-python-slot-name symbol))))))
           (python-properties (class)
             "Return a list defining python properties for the given CLASS."
             (nest (mapcar #'indent)
                   (or (mapcar #'python-property (python-child-slots class))
                       '("pass")))))

    (ensure-finalized class)
    (format nil "class ~a(~{~a~^, ~}):~%~{~a~^~%~%~}~%~%~%"
            (cl-to-python-type class)
            (python-superclasses class)
            (python-properties class))))

(define-command tree-sitter-py-generator (&spec +command-line-options+)
  "Command line interface for tree-sitter python class generator."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (when help (show-help-for-tree-sitter-py-generator) (quit 0))

  (format *standard-output* "from functools import cached_property~%")
  (format *standard-output* "from typing import List~%")
  (format *standard-output* "from .asts import AST~%~%")
  (nest (mapcar [{format *standard-output* "~a"} #'python-class-str])
        (remove-duplicates-from-end)
        (mappend #'class-and-python-dependencies)
        (remove-if-not {ast-symbol-p _ languages})
        (tree-sitter-symbols)))
