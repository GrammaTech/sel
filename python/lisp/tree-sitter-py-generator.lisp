(defpackage :software-evolution-library/python/lisp/tree-sitter-py-generator
  (:nicknames :sel/py/lisp/ts-py-generator)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/python
        :software-evolution-library/software/java
        :software-evolution-library/software/javascript
        :software-evolution-library/software/rust
        :software-evolution-library/software/typescript
        :software-evolution-library/python/lisp/utility)
  (:import-from :software-evolution-library/command-line :alias-language)
  (:import-from :software-evolution-library/software/tree-sitter :inner-parent)
  (:export :run-tree-sitter-py-generator))
(in-package :software-evolution-library/python/lisp/tree-sitter-py-generator)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    `((("help" #\h #\?) :type boolean :optional t
       :documentation "display help output"))))

(define-constant +ast-languages+
    '(c-ast cpp-ast java-ast javascript-ast python-ast
      rust-ast typescript-ast typescript-txs-ast)
  :test #'equalp
  :documentation "List of AST language superclasses to generate python for.")

(define-constant +ast-classes-language-agnostic+ '(inner-parent)
  :test #'equalp
  :documentation "List of language-agnostic AST classes which should always
be part of the python API.")

(define-constant +ast-terminals-skip-output+ '(rust-type rust-literal)
  :test #'equalp
  :documentation "List of special case AST terminals to not output to python.")

(-> ast-symbol-p (symbol &optional list) list)
(defun ast-symbol-p (sym &optional (languages +ast-languages+))
  "Return non-NIL if SYM is an AST, optionally, of one of the given LANGUAGES."
  (or (member sym languages :test #'subtypep)
      (member sym +ast-classes-language-agnostic+ :test #'subtypep)))

(-> remove-duplicates-from-end (list) list)
(defun remove-duplicates-from-end (l)
  "Remove duplicates from l, from right to left."
  (remove-duplicates l :from-end t))

(-> remove-special-case-classes (list) list)
(defun remove-special-case-classes (l)
  "Remove special case classes from l."
  (remove-if {member _ +ast-terminals-skip-output+} l :key #'class-name))

(-> tree-sitter-symbols () (values list &optional))
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
           (python-property-name (class slot)
             "Convert the given child SLOT name of CLASS to a python
              property name."
             (string-replace-all "-"
                                 (nest (string-downcase)
                                       (cl-to-python-slot-name class slot))
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
           (python-property (class slot)
             "Return a string defining a python property for the given
              SLOT in CLASS."
             (destructuring-bind (symbol . arity) slot
               (nest (join-with-newlines)
                     (list "@_property"
                           (fmt "def ~a(self) -> ~a:"
                                (python-property-name class symbol)
                                (if (zerop arity) "List[AST]" "AST"))
                           (fmt "    return self.child_slot(\"~a\")  # type: ignore"
                                (cl-to-python-slot-name class symbol))))))
           (python-properties (class)
             "Return a list defining python properties for the given CLASS."
             (or (nest (mapcar #'indent)
                       (mapcar {python-property class})
                       (python-child-slots class))
                 (list (indent "pass")))))

    (ensure-finalized class)
    (fmt "class ~a(~{~a~^, ~}):~%~{~a~^~%~%~}~%~%~%"
         (cl-to-python-type class)
         (python-superclasses class)
         (python-properties class))))

(define-command tree-sitter-py-generator (&spec +command-line-options+)
  "Command line interface for tree-sitter python class generator."
  #.(fmt "~%Built from SEL ~a, and ~a ~a.~%"
         +software-evolution-library-version+
         (lisp-implementation-type) (lisp-implementation-version))
  (when help (show-help-for-tree-sitter-py-generator) (quit 0))

  (format *standard-output* "from typing import List~%")
  (format *standard-output* "from .asts import AST~%~%")
  (format *standard-output* "_property = property~%~%~%")
  (nest (mapcar [{format *standard-output* "~a"} #'python-class-str])
        (remove-special-case-classes)
        (remove-duplicates-from-end)
        (mappend #'class-and-python-dependencies)
        (remove-if-not #'ast-symbol-p)
        (tree-sitter-symbols)))
