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

(defparameter +ast-languages+
  '(c-ast cpp-ast java-ast javascript-ast python-ast
    rust-ast typescript-ast typescript-txs-ast)
  "List of AST language superclasses to generate python for.")

(defparameter +ast-classes-language-agnostic+ '(inner-parent)
  "List of language-agnostic AST classes which should always
be part of the python API.")

(defparameter +ast-terminals-skip-output+ '(rust-type rust-literal)
  "List of special case AST terminals to not output to python.")

(defparameter +ast-classes-extra-methods-path+
    (asdf:system-relative-pathname :software-evolution-library
                                   "python/lisp/data/methods.py")
  "Path to file containing extra Python methods to define
on certain generated classes.")

(-> load-extra-methods () (values hash-table &optional))
#+:TREE-SITTER-PYTHON
(defun load-extra-methods ()
  "Return a hash-table mapping AST classes to user-defined extra Python
methods which should be defined on their generated classes."
  (labels ((python-to-cl-class (class-ast)
             (string-case (source-text (python-name class-ast))
               ("FunctionAST" (find-class 'function-ast))
               ("CallAST" (find-class 'call-ast))))
           (methods (class-ast)
             (children (python-body class-ast))))
    (nest (alist-hash-table)
          (mapcar «cons #'python-to-cl-class #'methods»)
          (remove-if-not #'python-to-cl-class)
          (remove-if-not (of-type 'class-ast))
          (children)
          (convert 'python-ast)
          (read-file-into-string +ast-classes-extra-methods-path+))))
#-:TREE-SITTER-PYTHON
(defun load-extra-methods () (make-hash-table))

(defparameter +ast-classes-extra-methods+ (load-extra-methods)
  "Hash-table mapping AST classes to extra Python methods which
should be defined on their generated classes.")

(-> ast-symbol-p (symbol &optional list) list)
(defun ast-symbol-p (sym &optional (languages +ast-languages+))
  "Return non-NIL if SYM is an AST, optionally, of one of the given LANGUAGES."
  (or (member sym languages :test #'subtypep)
      (member sym +ast-classes-language-agnostic+ :test #'subtypep)))

(-> remove-duplicates-from-end (list) list)
(defun remove-duplicates-from-end (l)
  "Remove duplicates from l, from right to left."
  (remove-duplicates l :from-end t))

(-> remove-special-case-classes (list) (values list &optional))
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

(let ((cache (alist-hash-table '((ast)))))
  (defun python-child-slots (class)
    "Return the child slots of CLASS for use in creating
     python property definitions. Aggregate slots common
     to all subclasses the highest possible superclass."
    (if (nth-value 1 (gethash class cache))
        (gethash class cache)
        (setf (gethash class cache)
              (let ((direct-subclasses (class-direct-subclasses class)))
                (ensure-finalized class)
                (if (null direct-subclasses)
                    (direct-child-slots class)
                    (iter (iter:with common-slots =
                                     (reduce (lambda (slots result)
                                               (intersection slots
                                                             (or result slots)
                                                             :test #'equal))
                                             (mapcar #'python-child-slots
                                                     direct-subclasses)))
                          (for subclass in direct-subclasses)
                          (setf (gethash subclass cache)
                                (set-difference (gethash subclass cache)
                                                common-slots
                                                :test #'equal))
                          (finally (return common-slots)))))))))

(defun direct-child-slots (class)
  "Return the direct child slots of CLASS as specified in the `child-slots` slot
   on CLASS relevant for creating python property definitions."
  (labels ((child-slots-definition (class)
             "Return the slot definition for the \"child-slots\" slot on CLASS."
             (find-if [{eq 'child-slots} #'slot-definition-name]
                      (compute-slots class)))
           (child-slot-ignorable-p (slot)
             "Return non-NIL if the given child slot should not be included
              in the python class definition's properties."
             (or (eq (car slot) 'children) (internal-child-slot-p slot)))
           (python-slotname (slot)
             "Convert the given child SLOT name of CLASS to a python
              property name."
             (destructuring-bind (symbol . arity) slot
               (cons (string-replace-all "-"
                                         (nest (string-downcase)
                                               (cl-to-python-slot-name class symbol))
                                         "_")
                     arity))))
    (when-let ((slot-definition (child-slots-definition class)))
      (and (null (class-direct-subclasses class))
           (eq (slot-definition-allocation slot-definition) :class)
           (slot-definition-initform slot-definition)
           (nest (mapcar #'python-slotname)
                 (apply #'remove-if #'child-slot-ignorable-p
                 (cdr (slot-definition-initform slot-definition))))))))

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
           (python-property (slot)
             "Return a string defining a python property for the given SLOT."
             (destructuring-bind (slotname . arity) slot
               (nest (join-with-newlines)
                     (list "@_property"
                           (fmt "def ~a(self) -> ~a:"
                                slotname
                                (if (zerop arity) "List[AST]" "Optional[AST]"))
                           (fmt "    return self.child_slot(\"~a\")  # type: ignore"
                                slotname)))))
           (python-properties (class)
             "Return a list defining python properties for the given CLASS."
             (nest (mapcar #'indent)
                   (mapcar #'python-property)
                   (python-child-slots class)))
           (python-methods (class)
             "Return a list of user-defined extra methods for the given CLASS."
             (nest (mapcar [#'indent #'source-text])
                   (gethash class +ast-classes-extra-methods+)))
           (python-properties-and-methods (class)
             "Return a list of python properties and methods for the given CLASS."
             (or (append (python-properties class)
                         (python-methods class))
                 (list (indent "pass")))))

    (fmt "class ~a(~{~a~^, ~}):~%~{~a~^~%~%~}~%~%~%"
         (cl-to-python-type class)
         (python-superclasses class)
         (python-properties-and-methods class))))

(define-command tree-sitter-py-generator (&spec +command-line-options+)
  "Command line interface for tree-sitter python class generator."
  #.(fmt "~%Built from SEL ~a, and ~a ~a.~%"
         +software-evolution-library-version+
         (lisp-implementation-type) (lisp-implementation-version))
  (when help (show-help-for-tree-sitter-py-generator) (quit 0))

  (format *standard-output* "from typing import List, Optional~%")
  (format *standard-output* "from .asts import AST, _interface~%~%")
  (format *standard-output* "_property = property~%~%~%")
  (nest (mapcar [{format *standard-output* "~a"} #'python-class-str])
        (remove-special-case-classes)
        (remove-duplicates-from-end)
        (mappend #'class-and-python-dependencies)
        (remove-if-not #'ast-symbol-p)
        (tree-sitter-symbols)))
