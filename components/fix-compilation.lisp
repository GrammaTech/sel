;;; fix-compilation.lisp --- Functions to fix software object compilation
;;;
;;; The `fix-compilation' function will take a software object and
;;; will try to make any changes necessary for that object to compile
;;; successfully.  This will first employ `clang-tidy' (which calls
;;; the command line utility of the same name).  If that is not
;;; sufficient it will then begin collecting compilation error
;;; messages, and calling the associated element of
;;; `*compilation-fixers*'.
;;;
;;; The `*compilation-fixers*' will be an alist of compiler warning
;;; regular expressions, and associated functions to call over the
;;; match data returned by the regular expression match.  Each fixer
;;; function may modify its argument, and should return non-nil on
;;; success or nil if additional fixers should be applied.
;;;
(defpackage :software-evolution-library/components/fix-compilation
  (:nicknames :sel/components/fix-compilation :sel/cp/fix-compilation)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/utility/range
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-w-fodder
        :software-evolution-library/components/formatting
        :software-evolution-library/components/searchable
        :software-evolution-library/components/fodder-database)
  (:export :register-fixer
           :fix-compilation
           :*compilation-fixers*))
(in-package :software-evolution-library/components/fix-compilation)
(in-readtable :curry-compose-reader-macros)

(defvar *compilation-fixers* nil
  "Alist of compiler warning regular expressions, and associated function.
The function is called over the match data returned by the regular
expression match.  Each fixer function may modify its argument, and
should return non-nil on success or nil if additional fixers should be
applied.")

(defun register-fixer (regex function)
  "Add FUNCTION to `*compilation-fixers*' to be called when REGEX matches."
  (unless (member regex (mapcar #'car *compilation-fixers*) :test #'string=)
    (push (cons regex function) *compilation-fixers*)))

(defmethod fix-compilation ((obj clang) max-attempts &aux matches)
  "Fix compilation errors in clang software object OBJ.
Try to make any changes necessary for that object to compile
successfully.  This will first employ `clang-tidy', which calls the
command line utility of the same name.  If that is not sufficient it
will then begin collecting compilation error messages, and calling the
associated element of `*compilation-fixers*'.

* OBJ Clang software object to fix.
* MAX-ATTEMPTS Maximum number of fix attempts to try."
  (handler-bind
      ;; While compilation is broken we expect all clang calls
      ;; to return non-zero.  Simply restart keeping as many of the
      ;; source ASTs as possible.
      ((mutate
        (lambda (e)
          (if (find-restart 'keep-partial-asts)
              (invoke-restart 'keep-partial-asts)
              (error e)))))
    ;; Tidy
    (clang-tidy obj)
    (loop :for attempt :below max-attempts :do
       ;; Compile
       (with-temporary-file (:pathname bin)
         (multiple-value-bind (bin errno stderr)
             (ignore-phenome-errors
              (phenome obj :bin bin))
           (declare (ignorable bin))
           (when (zerop errno)
             (return))
           ;; Dispatch on the first compiler warnings.
           (block fix
             (loop :for line :in (split-sequence #\Newline stderr) :do
                (loop :for fixer :in *compilation-fixers*
                   :when (setf matches
                               (multiple-value-bind (matchp match-data)
                                   (scan-to-strings (car fixer) line)
                                 (when matchp match-data)))
                   :do
                   (when (funcall (cdr fixer) obj matches)
                     (return-from fix)))))))))
  obj)

;; Fallback strategy: just delete the offending line entirely.
(defmethod delete-line-with-error ((obj clang) match-data)
  "DOCFIXME

* OBJ DOCFIXME
* MATCH-DATA DOCFIXME
"
  (let ((target-line (parse-integer (aref match-data 0))))
    (setf (lines obj)
          (loop :for line :in (lines obj)
             :for line-num :from 1
             :when (not (= target-line line-num))
             :collect line))))


;;; Resolve missing functions by adding #includes.
(defmethod resolve-function ((obj clang) match-data)
  "DOCFIXME

* OBJ DOCFIXME
* MATCH-DATA DOCFIXME
"
  (mapc {add-include obj} (resolve-function-includes (aref match-data 2)))
  obj)


(defvar *resolved-header-files* (make-hash-table :test 'equal)
  "A map from function name to a list of headers where
that function may be declared.")

(defun headers-in-manpage (section name)
  (multiple-value-bind (stdout stderr errno)
      (shell
       "man -P cat ~a ~a | sed -n \"/DESCRIPTION/q;p\" | ~
        grep \"#include\" | cut -d'<' -f 2 | cut -d'>' -f 1"
       section name)
    (declare (ignorable stderr errno))
    (split-sequence #\Newline stdout :remove-empty-subseqs t)))

(defun resolve-function-includes (func)
  (let ((headers (gethash func *resolved-header-files* 'not-found)))
    (mapcar {format nil "<~a>"}
            (if (eq headers 'not-found)
                (setf (gethash func *resolved-header-files*)
                      (or (headers-in-manpage 3 func)
                          (headers-in-manpage 2 func)))
                headers))))

(register-fixer
 "implicit( declaration of built-in|ly declaring library) function (‘|')(\\S+)(’|')"
 #'resolve-function)


;;; Add declaration and initialize uninitialized variables.
(defmethod add-declaration-and-initialize (line-number-index
                                           variable-name-index
                                           (obj clang)
                                           match-data)
  "DOCFIXME
* LINE-NUMBER-INDEX DOCFIXME
* VARIABLE-NAME-INDEX DOCFIXME
* OBJ DOCFIXME
* MATCH-DATA DOCFIXME
"
  ;; TODO: For now we'll just synthesize a random instantiation, in
  ;;       the future we should pull variable names from DeclStmt's,
  ;;       and grab a DeclStmt, possibly of a particular type.
  (let ((line-number (parse-integer (aref match-data line-number-index)))
        (lines (lines obj))
        (random-type (random-elt ; TODO: More types.
                      (mappend (lambda (type)
                                 (list type type
                                       (concatenate 'string type "* ")))
                               +c-numeric-types+)))
        (variable-name (aref match-data variable-name-index)))
    ;; Insert a declaration and initialization
    (setf (lines obj)
          (append (subseq lines 0 (1- line-number))
                  (list (format nil "~a ~a;" random-type variable-name)
                        (format nil "~a = ~a;"
                                variable-name
                                (- (random (expt 2 32)) (expt 2 31))))
                  (subseq lines (1- line-number)))))
  obj)

(register-fixer
 ":(\\d+):\\d+: error: use of undeclared identifier '(\\S+)'"
 {add-declaration-and-initialize 0 1})

;; Replace C++-style casts with C-style casts.
(defmethod c++-casts-to-c-casts ((obj clang) match-data)
  "DOCFIXME

* OBJ DOCFIXME
* MATCH-DATA DOCFIXME
"
  (declare (ignorable match-data))
  (setf (lines obj)
        (loop :for line :in (lines obj)
           :collecting (cl-ppcre:regex-replace-all
                        "(reinterpret|static)_cast<([^>]*)>"
                        line
                        "(\\2)"))))

(register-fixer
 ":(\\d+):(\\d+): error: (‘|')(reinterpret|static)_cast(’|') undeclared"
 #'c++-casts-to-c-casts)

(register-fixer
 ":(\\d+):\\d+: error: (‘|')(\\S+)(’|') undeclared"
 {add-declaration-and-initialize 0 2})


;;; Add declaration and initialize uninitialized variables.
(defmethod expected-expression-before ((obj clang-w-fodder) match-data)
  "DOCFIXME

* OBJ DOCFIXME
* MATCH-DATA
"
  (let* ((line-number (parse-integer (aref match-data 0)))
         (col-number (1- (parse-integer (aref match-data 1))))
         (new-expression
           (bind-vars-in-snippet
             obj
             (car (find-snippets *database*
                                 :ast-class
                                 (random-elt '("FloatingLiteral"
                                               "IntegerLiteral"
                                               "CharacterLiteral"
                                               "StringLiteral"
                                               "ParenExpr"
                                               "DeclRefExpr"
                                               "UnaryExprOrTypeTraitExpr"
                                               "ImplicitCastExpr"
                                               "CStyleCastExpr"))
                                 :limit 1))
             (lastcar (asts-containing-source-location
                       obj (make-instance 'source-location
                                          :line line-number
                                          :column col-number)))))
         (lines (lines obj))
         (orig (nth (1- line-number) lines)))
    (setf (lines obj)
          (append (subseq lines 0 (1- line-number))
                  (list (concatenate 'string
                          (subseq orig 0 col-number)
                          new-expression
                          (subseq orig col-number)))
                  (subseq lines line-number))))
  obj)

(defgeneric ast-source-ranges (software)
  (:documentation "Return (AST . SOURCE-RANGE) for each AST in OBJ.")
  (:method ((obj clang))
    (labels
        ((source-location (line column)
           (make-instance 'source-location :line line :column column))
         (scan-ast (ast line column)
           "Scan entire AST, updating line and column. Return the new values."
           (let* ((begin (source-location line column))
                  (ranges
                   (if (stringp ast)
                       ;; String literal
                       (iter (for char in-string ast)
                             (incf column)
                             (when (eq char #\newline)
                               (incf line)
                               (setf column 1)))

                       ;; Subtree
                       (iter (for child in (ast-children ast))
                             (appending
                              (multiple-value-bind
                                    (ranges new-line new-column)
                                  (scan-ast child line column)
                                (setf line new-line
                                      column new-column)
                                ranges)
                              into child-ranges)
                             (finally
                              (return
                                (cons (cons ast
                                            (make-instance 'source-range
                                              :begin begin
                                              :end (source-location
                                                    line column)))
                                      child-ranges)))))))

             (values ranges line column))))
      (cdr (scan-ast (ast-root obj) 1 1)))))

(defgeneric asts-containing-source-location (software location)
  (:documentation "Return a list of ASTs in SOFTWARE containing LOC.")
  (:method ((obj clang) (loc source-location))
    (when loc
      (mapcar #'car
              (remove-if-not [{contains _ loc} #'cdr]
                             (ast-source-ranges obj))))))

(register-fixer
 ":(\\d+):(\\d+): error: expected expression before ‘(\\S+)’ "
 #'expected-expression-before)

;; #include <stdint.h> when using types like int32_t.
(defmethod require-stdint ((obj clang) match-data)
  "DOCFIXME

* OBJ DOCFIXME
* MATCH-DATA DOCFIXME
"
  (declare (ignorable match-data))
  (add-include obj "stdint.h"))

(register-fixer
 ":(\\d+):(\\d+): error: unknown type name (‘|')(int|uint)(8|16|32|64)_t(’|')"
 #'require-stdint)

;; Macro definitions for int1_t, uint1_t
(defmethod add-int1-macros ((obj clang) match-data)
  "DOCFIXME

* OBJ DOCFIXME
* MATCH-DATA DOCFIXME
"
  (declare (ignorable match-data))
  (add-include obj "stdint.h")
  (add-macro obj (make-clang-macro :name "int1_t"
                                   :body "int1_t int32_t"
                                   :hash 3666623046900672582))
  (add-macro obj (make-clang-macro :name "uint1_t"
                                   :body "uint1_t uint32_t"
                                   :hash 6836836908473106000)))

(register-fixer
 ":(\\d+):(\\d+): error: unknown type name (‘|')(int|uint)1_t(’|')"
 #'add-int1-macros)

(defmethod delete-redefinitions ((obj clang) match-data)
  "DOCFIXME

* OBJ DOCFIXME
* MATCH-DATA
"
  ;; TODO: For now, we just take care of offending structs.
  (multiple-value-bind (new-genome matched)
    (regex-replace (concatenate 'string
                       "struct\\s+"
                       (aref match-data 0)
                       "\\s+\\{.*\\}")
                   (genome-string obj)
                   "")
    (when matched
      (setf (genome-string obj) new-genome))))

(register-fixer
 ": error: redefinition of '(.*)'"
 #'delete-redefinitions)

(defmethod delete-undefined-references ((obj clang) match-data)
  "DOCFIXME

* OBJ DOCFIXME
* MATCH-DATA DOCFIXME
"
  (let ((id (format nil "(|~a|)" (aref match-data 0)))
        (to-delete (make-hash-table :test 'equal)))
    (loop :for ast :in (asts obj)
       :when (find id
                   (append (get-used-variables obj ast)
                           (mapcar #'car (get-unbound-funs obj ast)))
                   :test #'string=)
       :do (setf (gethash (enclosing-full-stmt obj ast)
                          to-delete) t))
    ;; NOTE: Potential bug here where a function is passed nil
    ;;       which requires a number...  Not easy to reproduce.
    ;;
    ;; (mapc (lambda (it) (format t "IT:~S~%" it)))
    ;;
    ;; NOTE: Another potential bug here in which asts are
    ;;       returned instead of numbers and asts can't be
    ;;       compared with `>'.
    (nest (mapc [{apply-mutation obj} {list 'clang-cut} {cons :stmt1}])
          (sort (remove nil (hash-table-keys to-delete)) #'ast-later-p))))

(register-fixer
 ": undefined reference to `(\\S+)'"
 #'delete-undefined-references)

(defmethod declare-var-as-pointer ((obj clang) match-data)
  "DOCFIXME

* OBJ DOCFIXME
* MATCH-DATA DOCFIXME
"
  (let* ((line-number (parse-integer (aref match-data 0)))
         (col-number (parse-integer (aref match-data 1)))
         (variable (scan-to-strings
                     "^[a-zA-Z_][a-zA-Z0-9_]*"
                     (subseq (nth (1- line-number) (lines obj))
                             col-number))))
    (when variable
      (iter (for ast in (reverse (asts obj)))
            (when (and (eq (ast-class ast) :DeclStmt)
                       (scan (concatenate 'string variable "\\s*=")
                             (source-text ast)))
              (replace-ast obj ast
                           (nest (first)
                                 (convert 'clang-ast)
                                 (regex-replace variable
                                                (source-text ast)
                                                (format nil "*~a" variable)))
                           :literal t)
              (return obj))))
    obj))

(register-fixer
 ":(\\d+):(\\d+): error: subscripted value is not an array, pointer, or vector"
 #'declare-var-as-pointer)

(register-fixer
 ":(\\d+):(\\d+): error: indirection requires pointer operand"
 #'declare-var-as-pointer)


;; These fixers just delete the offending line, because there is not much
;; intelligent recovery we can do.
(register-fixer
 ":(\\d+):(\\d+): error: expected identifier or (‘|')\*(’|') before numeric constant"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): error: built-in function (‘|')(\\S+)(’|') declared as non-function"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): error: (‘|')(\\S+)(’|') redeclared as different kind of symbol"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): error: label (‘|')(\\S+)(’|') used but not defined"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): error: too many arguments (for format|to function)"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): error: invalid type argument of"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): error: called object (‘|')(\\S+)(’|') is not a function or function pointer"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): error: duplicate case value"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): 'case' statement not in switch statement"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): 'break' statement not in loop or switch statement"
 #'delete-line-with-error)

(register-fixer
 ":(\\d+):(\\d+): 'continue' statement not in loop statement"
 #'delete-line-with-error)

(register-fixer
 ": error: typedef redefinition with different types"
 #'delete-line-with-error)
