;;; utility.lisp - Utility functions shared by the python API cl applications.
(defpackage :software-evolution-library/python/lisp/utility
  (:nicknames :sel/py/lisp/utility)
  (:use :gt/full)
  (:export :cl-to-python-type
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
               (cond ((member term '("AST" "CPP" "CXX") :test #'string=) term)
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
               (let ((regex (format nil "(.*[A-Za-z0-9]-)(?i)(~a)$"
                                        (if (equal (car pair) "\\n")
                                            (car pair)
                                            (quote-meta-chars (car pair)))))
                     (replacement (format nil "\\1~a" (cdr pair))))
                 `((scan ,regex ,symname)
                   (regex-replace ,regex ,symname ,replacement
                                  :preserve-case t))))
             (stable-sort pairs #'> :key [#'length #'car]))
         (t ,symname)))

(-> python-identifier-chars (string) string)
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
                          ("->" . "arrow")
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
                          ("=>" . "arrow")
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
                          ;; C/C++ terminals
                          ("[[" . "open-attribute")
                          ("]]" . "close-attribute")
                          ("[]" . "empty-capture-clause")
                          ("()" . "call-operator")
                          ("::" . "scope-resolution")
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

;;;; AST slots
(-> internal-child-slot-p (list) list)
(defun internal-child-slot-p (slot)
  "Return non-NIL if the given child slot contains internal ASTs that should
not be exposed by the python API."
  (member (symbol-name (car slot)) '("internal-ast" "before-asts" "after-asts")
          :test (flip #'string-contains-p)))

;; (-> cl-to-python-slot-name ((or symbol string)) string)
(defgeneric cl-to-python-slot-name (slot-name)
  (:documentation "Convert SLOT-NAME to its form for the python API with
any language prefix stripped.")
  (:method ((slot-name symbol))
    (cl-to-python-slot-name (symbol-name slot-name)))
  (:method ((slot-name string))
    (cond ((passthrough-slot-name-p slot-name) slot-name)
          (t (string-join (cdr (split-sequence #\- slot-name)) #\-)))))

;; (-> python-to-cl-slot-name (string (or symbol string)) string)
(defgeneric python-to-cl-slot-name (language slot-name)
  (:documentation "Convert SLOT-NAME to its form for the common lisp API
with a language prefix prepended.")
  (:method ((language string) (slot-name symbol))
    (python-to-cl-slot-name language (symbol-name slot-name)))
  (:method ((language string) (slot-name string))
    (cond ((passthrough-slot-name-p slot-name) slot-name)
          (t (concatenate 'string (string-upcase language) "-" slot-name)))))

(-> passthrough-slot-name-p (string) list)
(defun passthrough-slot-name-p (slot-name)
  "Return non-NIL if SLOT-NAME is a slot which should be passed thru
any cl-to-python or python-to-cl unchanged."
  (member slot-name '("children" "async")
          :test (flip #'string-suffix-p)))
