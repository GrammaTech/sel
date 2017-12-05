;;; fix-comp --- Functions to fix software object compilation

;;; Commentary:

;; The `fix-compilation' function will take a software object and will
;; try to make any changes necessary for that object to compile
;; successfully.  This will first employ `clang-tidy' (which calls the
;; command line utility of the same name).  If that is not sufficient
;; it will then begin collecting compilation error messages, and
;; calling the associated element of `*compilation-fixers*'.
;;
;; The `*compilation-fixers*' will be an alist of compiler warning
;; regular expressions, and associated functions to call over the
;; match data returned by the regular expression match.  Each fixer
;; function may modify its argument, and should return non-nil on
;; success or nil if additional fixers should be applied.
;;
;; NOTE: If this is every productized, it may be worthwhile
;; re-implementing it inside of clang-tidy, which does have an
;; extensible mechanism for adding additional fixers.

(in-package :software-evolution-library)
(enable-curry-compose-reader-macros :include-utf8)

(defvar *compilation-fixers* nil
  "Alist of compiler warning regular expressions, and associated function.
The function is called over the match data returned by the regular
expression match.")

(defun register-fixer (regex function)
  "Add FUNCTION to `*compilation-fixers*' to be called when REGEX matches."
  (unless (member regex (mapcar #'car *compilation-fixers*) :test #'string=)
    (push (cons regex function) *compilation-fixers*)))

(defmethod fix-compilation ((obj clang) max-attempts &aux matches)
  ;; Tidy
  (clang-tidy obj)
  (loop :for attempt :below max-attempts :do
     ;; Compile
     (with-temp-file (bin)
       (multiple-value-bind (bin errno stderr)
         (phenome obj :bin bin)
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
                       (return-from fix))))))))
  obj)

;; Fallback strategy: just delete the offending line entirely.
(defmethod delete-line-with-error ((obj clang) match-data)
  (let ((target-line (parse-integer (aref match-data 0))))
    (setf (lines obj)
          (loop :for line :in (lines obj)
             :for line-num :from 1
             :when (not (= target-line line-num))
             :collect line))))


;;; Resolve missing functions by adding #includes.
(defmethod resolve-function ((obj clang) match-data)
  (mapc {add-include obj} (resolve-function-includes (aref match-data 2)))
  obj)

(register-fixer
 "implicit( declaration of built-in|ly declaring library) function (‘|')(\\S+)(’|')"
 #'resolve-function)


;;; Add declaration and initialize uninitialized variables.
(defmethod add-declaration-and-initialize (line-number-index
                                           variable-name-index
                                           (obj clang-w-fodder)
                                           match-data)
  (flet ((random-type ()
           (random-elt ; TODO: More types.
            (mappend (lambda (type)
                       (list type type (concatenate 'string type "* ")))
                     +c-numeric-types+))))
    ;; TODO: For now we'll just synthesize a random instantiation, in
    ;;       the future we should pull variable names from DeclStmt's,
    ;;       and grab a DeclStmt, possibly of a particular type.
    (let ((line-number (parse-integer (aref match-data line-number-index)))
          (variable-name (aref match-data variable-name-index))
          (lines (lines obj)))
      ;; Insert a declaration.
      (setf (lines obj)
            (append (take (1- line-number) lines)
                    (list (format nil "~a ~a;" (random-type) variable-name))
                    (drop (1- line-number) lines)))
      ;; Find the ID of the declaration.
      (let* ((decl-stmt (find-if (lambda (snippet)
                                   (and (eq (ast-class snippet)
                                            :DeclStmt)
                                        (= (->> snippet
                                                (ast-to-source-range obj)
                                                (begin)
                                                (line))
                                           line-number)))
                                 (asts obj)))
             (fodder
              (random-elt
               (remove-if-not [{scan "\\(\\|\\w+\\|\\) = "} {aget :src-text}]
                              (find-snippets *database*
                                :ast-class "BinaryOperator"
                                :limit 512))))
             ;; Find the "assigned-to" free-variable.
             (assigned-variable
              (multiple-value-bind (matchp match-data)
                  (scan-to-strings "(\\(\\|\\w+\\|\\)) = "
                                   (aget :src-text fodder))
                (assert matchp (fodder)
                        "Assignment fodder should assign to a free variable.")
                (aref match-data 0)))
             (scope-vars (mapcar {aget :name}
                                 (get-vars-in-scope obj decl-stmt))))
        ;; Insert a BinaryOperator assignment after the DeclStmt binding
        ;; its first free variable to the newly declared variable.
        (when decl-stmt
          (let* ((unbound (remove-if [{string= assigned-variable} #'car]
                                     (aget :unbound-vals fodder)))
                 (replacements (cons
                                (cons assigned-variable variable-name)
                                (mapcar
                                 (lambda (val-scope-pair)
                                   (cons (car val-scope-pair)
                                         (or (random-elt-with-decay scope-vars
                                                                    0.5)
                                             "/* no bound vars */")))
                                 unbound)))
                 (text (apply-replacements replacements
                                           (aget :src-text fodder)))
                 ;; First full statement after decl.
                 (stmt1 (find-if «and #'ast-full-stmt
                                      {ast-later-p _ decl-stmt}»
                                 (asts obj))))

            (apply-clang-mutate-ops obj
                          `((:insert-value (:stmt1 . ,(ast-counter stmt1))
                                           (:value1 . ,text)))))))))
  obj)

;; For clang software objects with no fodder database,
;; just delete the offending line.
(defmethod add-declaration-and-initialize (line-number-index
                                           variable-name-index
                                           (obj clang)
                                           match-data)
  (declare (ignorable line-number-index)
           (ignorable variable-name-index))
  (delete-line-with-error obj match-data))

(register-fixer
 ":(\\d+):\\d+: error: use of undeclared identifier '(\\S+)'"
 {add-declaration-and-initialize 0 1})

;; Replace C++-style casts with C-style casts.
(defmethod c++-casts-to-c-casts ((obj clang) match-data)
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
  (let* ((line-number (parse-integer (aref match-data 0)))
         (col-number (1- (parse-integer (aref match-data 1))))
         (new-expression
           (bind-vars-in-snippet
             obj
             (->> (find-snippets *database*
                                 :ast-class (->> '("FloatingLiteral"
                                                   "IntegerLiteral"
                                                   "CharacterLiteral"
                                                   "StringLiteral"
                                                   "ParenExpr"
                                                   "DeclRefExpr"
                                                   "UnaryExprOrTypeTraitExpr"
                                                   "ImplicitCastExpr"
                                                   "CStyleCastExpr")
                                                 (random-elt))
                                 :limit 1)
                  (first))
             (lastcar (asts-containing-source-location
                       obj (make-instance 'source-location
                                          :line line-number
                                          :column col-number)))))
         (lines (lines obj))
         (orig (nth (1- line-number) lines)))
    (setf (lines obj)
          (append (take (1- line-number) lines)
                  (list (concatenate 'string
                          (subseq orig 0 col-number)
                          new-expression
                          (subseq orig col-number)))
                  (drop line-number lines))))
  obj)

(register-fixer
 ":(\\d+):(\\d+): error: expected expression before ‘(\\S+)’ "
 #'expected-expression-before)

;; #include <stdint.h> when using types like int32_t.
(defmethod require-stdint ((obj clang) match-data)
  (declare (ignorable match-data))
  (add-include obj "stdint.h"))

(register-fixer
 ":(\\d+):(\\d+): error: unknown type name (‘|')(int|uint)(8|16|32|64)_t(’|')"
 #'require-stdint)

;; Macro definitions for int1_t, uint1_t
(defmethod add-int1-macros ((obj clang) match-data)
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
  (let ((id (format nil "(|~a|)" (aref match-data 0)))
        (to-delete (make-hash-table :test 'equal)))
    (loop :for ast :in (asts obj)
       :when (find id
                   (append (get-used-variables obj ast)
                           (mapcar #'car (get-unbound-funs obj ast)))
                   :test #'string=)
       :do (setf (gethash (enclosing-full-stmt obj ast)
                          to-delete) t))
    (-<>> (hash-table-keys to-delete)
          (remove nil)
          ;; NOTE: Potential bug here where a function is passed nil
          ;;       which requires a number...  Not easy to reproduce.
          ;;
          ;; (mapc (lambda (it) (format t "IT:~S~%" it)))
          (sort <> #'>)
          (remove-if #'zerop)
          (mapc [{apply-mutation obj} {list 'clang-cut} {cons :stmt}]))))

(register-fixer
 ": undefined reference to `(\\S+)'"
 #'delete-undefined-references)

(defmethod declare-var-as-pointer ((obj clang) match-data)
  (let* ((line-number (parse-integer (aref match-data 0)))
         (col-number (parse-integer (aref match-data 1)))
         (variable (scan-to-strings
                     "^[a-zA-Z_][a-zA-Z0-9_]*"
                     (subseq (nth (1- line-number) (lines obj))
                             col-number)))
         (*matching-free-var-retains-name-bias* 1)
         (*matching-free-function-retains-name-bias* 1))
    (when variable
      ;; Run through clang-mutate to get accurate counters
      (update-asts obj)
      (loop :for ast
            :in (reverse (asts obj))
            :when (and (eq (ast-class ast) :DeclStmt)
                       (scan (concatenate 'string variable "\\s*=")
                             (source-text ast)))
            :do (let ((text (regex-replace variable (source-text ast)
                                           (concatenate 'string "*" variable))))
                  (apply-clang-mutate-ops obj
                    `((:set . ((:stmt1 . ,(ast-counter ast))
                               (:value1 . ,text)))))
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
