;;; utility.lisp - Utility functions shared by the python API cl applications.
(defpackage :software-evolution-library/python/lisp/utility
  (:nicknames :sel/py/lisp/utility)
  (:use :gt/full
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/python
        :software-evolution-library/software/java
        :software-evolution-library/software/javascript
        :software-evolution-library/software/rust
        :software-evolution-library/software/typescript)
  (:export :cl-to-python-type
           :cl-to-python-ast-language
           :python-to-cl-ast-language
           :ast-language
           :internal-child-slot-p
           :cl-to-python-slot-name
           :python-to-cl-slot-name))
(in-package :software-evolution-library/python/lisp/utility)
(in-readtable :curry-compose-reader-macros)

;;;; AST types
;; (-> cl-to-python-type ((or class symbol string)) string)
(defgeneric cl-to-python-type (type)
  (:documentation "Convert the given common lisp TYPE to the corresponding
python type identifier.")
  (:method ((class class))
    (cl-to-python-type (class-name class)))
  (:method ((symbol symbol))
    (cl-to-python-type (symbol-name symbol)))
  (:method ((typename string))
    (labels ((pep8-camelcase (term)
               "Camelcase TERM in accordance to PEP-8 conventions."
               ;; Keep abbreviations uppercase, otherwise camelcase per PEP-8.
               (cond ((member term '("AST" "CPP" "CXX" "ECMA") :test #'string=)
                      term)
                     (t (string-capitalize term)))))
      (nest (apply #'concatenate 'string)
            (mapcar #'pep8-camelcase)
            (split-sequence #\-)
            (python-identifier-chars typename)))))

(defmacro symname-find-replace$ (symname pairs)
  "Build cond clauses from PAIRS specifying find/replace strings for
the trailing characters of the symbol string SYMNAME."
  `(cond ,@(mapcar
             (lambda (pair)
               (let ((regex (fmt "(.*[A-Za-z0-9]-)(?i)(~a)$"
                                 (if (equal (car pair) "\\n")
                                     (car pair)
                                     (quote-meta-chars (car pair)))))
                     (replacement (fmt "\\1~a" (cdr pair))))
                 `((scan ,regex ,symname)
                   (regex-replace ,regex ,symname ,replacement
                                  :preserve-case t))))
             (stable-sort pairs #'> :key [#'length #'car]))
         (t ,symname)))

(-> python-identifier-chars (string) (values string &optional))
(defun python-identifier-chars (typename)
  "Replace special chars in AST TYPENAMEs not valid in python identifiers."
  (symname-find-replace$ (regex-replace "^C/CPP" typename "CXX")
                         (;; Logical operators
                          ("||" . "logical-or")
                          ("&&" . "logical-and")
                          ("!" . "logical-not")
                          ("&&=" . "logical-and-assign")
                          ("||=" . "logical-or-assign")
                          ;; Comparison operators
                          ("<" . "less-than")
                          ("<=" . "less-than-or-equal")
                          (">" . "greater-than")
                          (">=" . "greater-than-or-equal")
                          ("==" . "equal")
                          ("!=" . "not-equal")
                          ;; Bitwise operators
                          ("<<" . "bitshift-left")
                          (">>" . "bitshift-right")
                          ("&" . "bitwise-and")
                          ("|" . "bitwise-or")
                          ("^" . "bitwise-xor")
                          ("~" . "bitwise-not")
                          ("<<=" . "bitshift-left-assign")
                          (">>=" . "bitshift-right-assign")
                          ("&=" . "bitwise-and-assign")
                          ("|=" . "bitwise-or-assign")
                          ("^=" . "bitwise-xor-assign")
                          ;; Arithmetic operators
                          ("+" . "add")
                          ("-" . "subtract")
                          ("*" . "multiply")
                          ("/" . "divide")
                          ("%" . "modulo")
                          ("+=" . "add-assign")
                          ("-=" . "subtract-assign")
                          ("*=" . "multiply-assign")
                          ("/=" . "divide-assign")
                          ("%=" . "module-assign")
                          ("++" . "increment")
                          ("--" . "decrement")
                          ;; Miscellaneous operators
                          ("=" . "assign")
                          ("[" . "open-bracket")
                          ("]" . "close-bracket")
                          ("{" . "open-brace")
                          ("}" . "close-brace")
                          ("(" . "open-parenthesis")
                          (")" . "close-parenthesis")
                          ("," . "comma")
                          ("." . "dot")
                          ("?" . "question")
                          (":" . "colon")
                          (";" . "semicolon")
                          ("->" . "dash-arrow")
                          ("..." . "ellipsis")
                          ("\\n" . "newline")
                          ;; Quote terminals
                          ("'" . "single-quote")
                          ("\"" . "double-quote")
                          ("`" . "back-quote")
                          ("u'" . "unicode-single-quote")
                          ("u\"" . "unicode-double-quote")
                          ("\"\"" . "empty-string")
                          ;; Python terminals
                          (":=" . "walrus")
                          ("<>" . "not-equal-flufl")
                          ("@" . "matrix-multiply")
                          ("**" . "pow")
                          ("//" . "floor-divide")
                          ("@=" . "matrix-multiply-assign")
                          ("**=" . "pow-assign")
                          ("//=" . "floor-divide-assign")
                          ("{{" . "double-open-brace")
                          ("}}" . "double-close-brace")
                          ;; Javascript terminals
                          ("=>" . "equal-arrow")
                          ("?." . "chaining")
                          ("${" . "open-template-literal")
                          ("??" . "nullish-coalescing")
                          ("??=" . "nullish-coalescing-assign")
                          ("===" . "strictly-equal")
                          ("!==" . "strictly-not-equal")
                          ("<<<" . "unsigned-bitshift-left")
                          (">>>" . "unsigned-bitshift-right")
                          ("<<<=" . "unsigned-bitshift-left-assign")
                          (">>>=" . "unsigned-bitshift-right-assign")
                          ;; Java terminals
                          ("@interface" . "at-interface")
                          ;; Rust terminals
                          ("#" . "hash-sign")
                          ("$" . "dollar-sign")
                          ("rules!" . "rules-exclamation")
                          (".." . "dot-dot")
                          ("..=" . "dot-dot-assign")
                          ;; Typescript terminals
                          ("?:" . "opting-type-terminal")
                          ("-?:" . "omitting-type-terminal")
                          ("{|" . "object-type-open")
                          ("|}" . "object-type-close")
                          ;; C/C++ terminals
                          ("[[" . "open-attribute")
                          ("]]" . "close-attribute")
                          ("[]" . "empty-capture-clause")
                          ("()" . "call-operator")
                          ("::" . "scope-resolution")
                          ("<=>" . "spaceship")
                          ("l'" . "wchar-single-quote")
                          ("l\"" . "wchar-double-quote")
                          ("u\'-terminal" . "unsigned-terminal-single-quote")
                          ("u\"-terminal" . "unsigned-terminal-double-quote")
                          ("u8'" . "unsigned-8bit-terminal-single-quote")
                          ("u8\"" . "unsigned-8bit-terminal-double-quote")
                          ("--unaligned" . "underscore-unaligned")
                          (".*" . "pointer-to-member-dot")
                          ("->*" . "pointer-to-member-arrow")
                          ("#define" . "macro-define")
                          ("#include" . "macro-include")
                          ("#ifdef" . "macro-if-defined")
                          ("#ifndef" . "macro-if-not-defined")
                          ("#if" . "macro-if")
                          ("#elif" . "macro-elif")
                          ("#else" . "macro-else")
                          ("#endif" . "macro-end-if")
                          ("#end" . "macro-end"))))

;;;; AST language
(-> cl-to-python-ast-language (string) string)
(defun cl-to-python-ast-language (language)
  "Convert LANGUAGE to its form for the python API with dashes
removed and terms capitalized."
  (nest (apply #'concatenate 'string)
        (mapcar #'string-capitalize)
        (split-sequence #\- language)))

(-> python-to-cl-ast-language (string) string)
(defun python-to-cl-ast-language (language)
  "Convert LANGUAGE to its form for the common lisp API with dashes
added between uppercase terms."
  (string-upcase (regex-replace-all "(?!^)([A-Z]+)" language "-\\1")))

;; (-> ast-language ((or ast class symbol) string)
(defgeneric ast-language (ast)
  (:documentation "Return a string representing the language of the given
AST (instance, type symbol, or class).")
  (:method ((ast ast))
    (ast-language (type-of ast)))
  (:method ((ast-class class))
    (ast-language (class-name ast-class )))
  (:method ((ast-type symbol))
    (when-let ((superclass (find ast-type
                                 '(c-ast cpp-ast python-ast
                                   java-ast javascript-ast rust-ast
                                   typescript-ts-ast typescript-tsx-ast)
                                 :test #'subtypep)))
      (drop-suffix "-AST" (symbol-name superclass)))))

;;;; AST slots
(defparameter +disambiguation-suffix+ "-CHILD-SLOT"
  "Suffix to disambiguate python AST methods and child slot properties where
there is a naming collision.")

(-> internal-child-slot-p (list) list)
(defun internal-child-slot-p (slot)
  "Return non-NIL if the given child slot contains internal ASTs that should
not be exposed by the python API."
  (member (symbol-name (car slot)) '("internal-ast" "before-asts" "after-asts")
          :test (flip #'string-contains-p)))

;; (-> cl-to-python-slot-name ((or ast class symbol string)
;;                             (or symbol string))
;;                             string)
(defgeneric cl-to-python-slot-name (language slot-name)
  (:documentation "Convert SLOT-NAME to its form for the python API with
any LANGUAGE prefix stripped.")
  (:method ((ast ast) (slot-name t))
    (cl-to-python-slot-name (ast-language ast) slot-name))
  (:method ((ast-class class) (slot-name t))
    (cl-to-python-slot-name (ast-language ast-class) slot-name))
  (:method ((ast-type symbol) (slot-name t))
    (cl-to-python-slot-name (ast-language ast-type) slot-name))
  (:method ((language string) (slot-name symbol))
    (cl-to-python-slot-name language (symbol-name slot-name)))
  (:method ((language string) (slot-name string))
    (cond ((passthrough-slot-name-p slot-name)
           (string-upcase slot-name))
          ((existing-method-collision-p slot-name)
           (nest (cl-to-python-slot-name language)
                 (ensure-suffix slot-name +disambiguation-suffix+)))
          (t (drop-prefix (concatenate 'string (string-upcase language) "-")
                          (string-upcase slot-name))))))

;; (-> python-to-cl-slot-name ((or ast class symbol string)
;;                             (or symbol string))
;;                             string)
(defgeneric python-to-cl-slot-name (language slot-name)
  (:documentation "Convert SLOT-NAME to its form for the common lisp API
with a LANGUAGE prefix prepended.")
  (:method ((ast ast) (slot-name t))
    (python-to-cl-slot-name (ast-language ast) slot-name))
  (:method ((ast-class class) (slot-name t))
    (python-to-cl-slot-name (ast-language ast-class) slot-name))
  (:method ((ast-type symbol) (slot-name t))
    (python-to-cl-slot-name (ast-language ast-type) slot-name))
  (:method ((language string) (slot-name symbol))
    (python-to-cl-slot-name language (symbol-name slot-name)))
  (:method ((language string) (slot-name string))
    (cond ((passthrough-slot-name-p slot-name)
           (string-upcase slot-name))
          ((existing-method-collision-p slot-name)
           (nest (python-to-cl-slot-name language)
                 (drop-suffix +disambiguation-suffix+ slot-name)))
          (t (concatenate 'string
                          (string-upcase language)
                          "-"
                          (string-upcase slot-name))))))

(-> passthrough-slot-name-p (string) list)
(defun passthrough-slot-name-p (slot-name)
  "Return non-NIL if SLOT-NAME is a slot which should be passed thru
any cl-to-python or python-to-cl unchanged."
  (member slot-name '("before-asts" "after-asts" "children" "async")
          :test (flip #'string-suffix-p)))

(-> existing-method-collision-p (string) list)
(defun existing-method-collision-p (slot-name)
  "Return T if slot-name collides with an existing method on python ASTs."
  (member slot-name '("size" "language" "source_text")
          :test (flip #'string-suffix-p)))
