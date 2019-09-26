;;; clang-tokens.lisp --- Tokenize C/C++ source files
(defpackage :software-evolution-library/components/clang-tokens
  (:nicknames :sel/components/clang-tokens :sel/cp/clang-tokens)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :cl-ppcre
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang)
  (:export
   :tokens))
(in-package :software-evolution-library/components/clang-tokens)
(in-readtable :curry-compose-reader-macros)

(define-condition unhandled-token-class (error)
  ((text :initarg :text :initform nil :reader text))
  (:report (lambda (condition stream)
             (format stream "Tokenization failed: ~a"
                     (text condition))))
  (:documentation "UNHANDLED-TOKEN-CLASS is raised when an
unknown AST class is encountered."))

(defgeneric tokens (software &optional roots)
  (:documentation "Return a list of keyword tokens in SOFTWARE.
Optional argument ROOTS limits to tokens below elements of ROOT ASTs in
SOFTWARE."))

;;;; For reference, list of tokens explicitly introduced:
;;;; && identifier { } ( ) [ ] = break continue case switch default , : ?
;;;; if else while do typedef -> . va-arg return goto for offset-of
;;;; generic sizeof alignof struct union char-literal int-literal string-literal
;;;; float-literal i-literal ... macro
(defmethod tokens ((clang clang-base) &optional (roots (roots clang)))
  "Return a list of keyword tokens representing the ASTs in SOFTWARE below ROOTS.
* CLANG software object to tokenize
* ROOTS limit to tokens below elements of ROOT ASTs in CLANG
"
  (labels
      ((tokenize-children (children)
         (mappend #'tokenize children))
       (split-tokens (str &optional (start 0) (end (length str)))
         (remove-if #'emptyp
                    (split "\\s+|(\\W)" str
                           :with-registers-p t
                           :omit-unmatched-p t
                           :start start
                           :end end)))
       ;; In a list of lists, insert a comma between each pair of lists
       ;; (prepend a comma to each list, then drop the leading comma)
       (comma-sep (ls)
         (cdr (mappend {cons (token-from-string ",")} ls)))
       (idents (clang ast)
         (remove-duplicates
          (append (mapcar #'ast-name (ast-declares ast))
                  (mapcar [#'ast-name {aget :name}]
                          (get-vars-in-scope clang ast))
                  (mapcar [#'ast-name #'car] (get-unbound-funs clang ast))
                  (mapcar [#'ast-name {aget :name}]
                          (get-unbound-vals clang ast)))
          :test #'equal))
       ;; replace occurrences of a list of identifiers with the string
       ;; "identifier"
       (replace-identifiers (identifiers str)
         (iter (for i in identifiers)
               (unless (equal i "") ;; (emptyp i)
                 (setf str (replace-all str i "identifier")))
               (finally (return str))))
       (token-from-string (str)
         (switch (str :test #'equal)
           ("||" :pipe-pipe)
           ("|=" :pipe=)
           ("|"  :pipe)
           (t (make-keyword str))))
       (tokenize (root)
         (let ((children (get-immediate-children clang root)))
           ;; (format t "TOKENIZE:~%root = ~a~%children = ~a~%" root children)
           (switch ((ast-class root) :test #'equal)
             (:AddrLabelExpr
              (assert (<= 2 (length (source-text root))))
              (list (token-from-string "&&") (token-from-string "identifier")))
             (:ArraySubscriptExpr
              (assert (= 2 (length children)))
              (append (tokenize (first children))
                      (list (token-from-string "["))
                      (tokenize (second children))
                      (list (token-from-string "]"))))
             ;; no tokens, just proceed to children
             (:AttributedStmt (tokenize-children children))
             (:BinaryOperator
              (assert (= 2 (length children)))
              (append (tokenize (first children))
                      (list (token-from-string (ast-opcode root)))
                      (tokenize (second children))))
             (:BreakStmt (list (token-from-string "break")))
             (:CallExpr
              (append (tokenize (first children))
                      (list (token-from-string "("))
                      ;; tokenize children and comma-separate
                      (comma-sep (mapcar #'tokenize (cdr children)))
                      ;; right paren
                      (list (token-from-string ")"))))
             (:CaseStmt (append (list (token-from-string "case"))
                                 (tokenize (first children))
                                 (list (token-from-string ":"))
                                 (tokenize-children (cdr children))))
             (:CharacterLiteral (list (token-from-string "char-literal")))
             (:CompoundAssignOperator
              (assert (= 2 (length children)))
              (append (tokenize (first children))
                      (list (token-from-string (ast-opcode root)))
                      (tokenize (second children))))
             ;; TODO: need to pull out the cast part and tokenize the
             ;; children, but some children seem to be duplicated
             (:CompoundLiteralExpr
              (let* ((l-paren (position #\( (source-text root)))
                     (r-paren (position #\) (source-text root)))
                     (cast-expr (split-tokens (source-text root)
                                              (1+ l-paren)
                                              r-paren)))
                (append (list (token-from-string "("))
                        (mapcar #'token-from-string cast-expr)
                        (list (token-from-string ")"))
                        (tokenize-children children))))
             (:CompoundStmt (append (list (token-from-string "{"))
                                    (tokenize-children children)
                                    (list (token-from-string "}"))))
             (:ConditionalOperator
              (assert (= 3 (length children)))
              (append (tokenize (first children))
                      (list (token-from-string "?"))
                      (tokenize (second children))
                      (list (token-from-string ":"))
                      (tokenize (third children))))
             (:ContinueStmt (list (token-from-string "continue")))
             (:CStyleCastExpr
              (let* ((l-paren (position #\( (source-text root)))
                     (r-paren (position #\) (source-text root)))
                     (cast-expr (split-tokens (source-text root)
                                              (1+ l-paren)
                                              r-paren)))
                (append (list (token-from-string "("))
                        ;; TODO: write string->token function
                        (mapcar #'token-from-string cast-expr)
                        (list (token-from-string ")"))
                        (tokenize-children children))))
             (:DeclRefExpr (list (token-from-string "identifier")))
             (:DeclStmt (tokenize-children children))
             (:DefaultStmt (append (list (token-from-string "default")
                                         (token-from-string ":"))
                                   (tokenize-children children)))
             ;; [const or range] = init , last child is init, rest are for array
             ;; .field-ident = init , child is init (none for field)
             ;; [const or range]*.field-ident = init
             (:DesignatedInitExpr
              (let* ((src (source-text root))
                     (eq-index (position #\= src)))
                (append
                 ;; scan characters for [], . , ...
                 (iter (for i from 0 below eq-index)
                       (with child = 0)
                       (switch ((char src i))
                         (#\[ (appending (list (token-from-string "["))
                                         into tokens))
                         ;; #\] collect left child
                         (#\] (appending
                               (append (tokenize (nth child children))
                                       (list (token-from-string "]")))
                               into tokens)
                              (incf child))
                         (#\. (if (and (< i (- eq-index 2))
                                       (eql #\. (char src (1+ i)))
                                       (eql #\. (char src (+ 2 i))))
                                  ;; range: 0...1 collect left child
                                  ;; (right child handled by #\] case)
                                  (progn
                                    (appending
                                     (append (tokenize (nth child children))
                                             (list (token-from-string "...")))
                                     into tokens)
                                    (incf child)
                                    (setf i (+ i 2)))
                                  ;; field: .identifier
                                  (appending
                                   (list (token-from-string ".")
                                         (token-from-string "identifier"))
                                   into tokens)))
                         (t nil))
                       (finally (return tokens)))
                 (list (token-from-string "="))
                 ;; initializer is the last child
                 (tokenize (lastcar children)))))
             (:DoStmt
              (assert (= 2 (length children)))
              (append (list (token-from-string "do"))
                      (tokenize (first children))
                      (list (token-from-string "while")
                            (token-from-string "("))
                      (tokenize (second children))
                      (list (token-from-string ")"))))
             (:Enum
              (let ((has-ident (not (emptyp (first (ast-declares root))))))
                (append (list (token-from-string "enum"))
                        (when has-ident (list (token-from-string "identifier")))
                        (list (token-from-string "["))
                        (comma-sep (mapcar #'tokenize children))
                        (list (token-from-string "]")))))
             (:EnumConstant (list (token-from-string "identifier")))
             (:Field (let* ((src (replace-identifiers (idents clang root)
                                                      (source-text root)))
                            (src (remove #\; src)))
                       (mapcar #'token-from-string (split-tokens src))))
             (:FloatingLiteral (list (token-from-string "float-literal")))
             (:ForStmt
              (append (list (token-from-string "for")
                            (token-from-string "("))
                      (mappend #'tokenize (butlast children))
                      (list (token-from-string ")"))
                      (tokenize (lastcar children))))

             (:Function
              (let* ((sig (take-until
                           {string= "("}
                           (split-tokens
                            (replace-identifiers (idents clang root)
                                                 (source-text root))))))
                (append (mapcar #'token-from-string sig)
                        (list (token-from-string "("))
                        ;; comma-separated ParmVars (all but last child)
                        (comma-sep (mapcar #'tokenize (butlast children)))
                        (list (token-from-string ")"))
                        (tokenize (lastcar children)))))
             ;; _Generic(child0, type: child1, type: child2, ...)
             (:GenericSelectionExpr
              (let* ((comma (position #\, (source-text root)))
                     ;; split on commas to get each (type: child) pairs
                     (a-ls (cdr (split "\\)|,\\s*" (source-text root)
                                       :start comma)))
                     ;; split a-ls on : to get types
                     (types (mapcar [#'token-from-string #'first {split ":\\s*"}]
                                    a-ls)))
                (assert (= (length types) (1- (length children))) ()
                        "Types = ~a, Children = ~a" types children)
                (append (list (token-from-string "generic")
                              (token-from-string "("))
                        (tokenize (first children))
                        (mappend (lambda (type toks)
                                   (append (list (token-from-string ",")
                                                 type
                                                 (token-from-string ":"))
                                           toks))
                                 types
                                 (mapcar #'tokenize
                                         (take (length types)
                                               (cdr children))))
                        (list (token-from-string ")")))))
             (:GotoStmt (list (token-from-string "goto")
                              (token-from-string "identifier")))
             (:IfStmt (append (list (token-from-string "if")
                                    (token-from-string "("))
                              (tokenize (first children))
                              (list (token-from-string ")"))
                              (tokenize (second children))
                              (when (= 3 (length children))
                                (cons (token-from-string "else")
                                      (tokenize (third children))))))
             (:ImaginaryLiteral (list (token-from-string "i-literal")))
             ;; Just tokenize children
             (:ImplicitCastExpr (tokenize-children children))
             (:IndirectGotoStmt (append (list (token-from-string "goto")
                                              (token-from-string "*"))
                                        (tokenize-children children)))
             ;; TODO might be broken: seems that some InitListExprs have a
             ;; child that duplicates the whole InitListExpr?
             (:InitListExpr
              (append (list (token-from-string "{"))
                      (comma-sep (mapcar #'tokenize children))
                      (list (token-from-string "}"))))
             (:IntegerLiteral (list (token-from-string "int-literal")))
             (:LabelStmt (append (list (token-from-string "identifier")
                                       (token-from-string ":"))
                                 (tokenize-children children)))
             (:MacroExpansion (list (token-from-string "macro")))
             ;; x.y or x->y (one child for leftof ->/.)
             (:MemberExpr
              ;; find start of rightmost -> or .
              (let* ((dash (position #\- (source-text root) :from-end t))
                     (dot (position #\. (source-text root) :from-end t))
                     ;; identify (rightmost) -> or .
                     (dash-dot (if (or (and dash dot (= dash (max dash dot)))
                                       (not dot))
                                   (list (token-from-string "->"))
                                   (list (token-from-string ".")))))
                (assert (= 1 (length children)))
                (append (tokenize (first children))
                        dash-dot
                        (list (token-from-string "identifier")))))
             (:NullStmt nil)
             (:OffsetOfExpr (mapcar #'token-from-string
                                    (list "offset-of" "("
                                          "identifier" ","
                                          "identifier" ")")))
             (:ParenExpr (append (list (token-from-string "("))
                                 (tokenize-children children)
                                 (list (token-from-string ")"))))
             (:ParmVar (->> (replace-identifiers (idents clang root)
                                                 (source-text root))
                            (split-tokens)
                            (mapcar #'token-from-string)))
             (:PredefinedExpr (list (token-from-string (source-text root))))
             ;; NOTE: struct, union. May include fields or just be a declaration.
             (:Record
              (let* ((src (replace-identifiers (idents clang root)
                                               (source-text root)))
                     (end (position #\{ src)))
                (assert (or (starts-with-subseq "struct" src)
                            (starts-with-subseq "union" src)))
                (append (mapcar #'token-from-string
                                (split-tokens src 0 (or end (length src))))
                        (when end
                          (append (list (token-from-string "{"))
                                  (tokenize-children children)
                                  (list (token-from-string "}")))))))
             (:ReturnStmt (cons (token-from-string "return")
                                (tokenize-children children)))
             ;; parenthesized CompoundStmt
             (:StmtExpr (append (list (token-from-string "("))
                                (tokenize-children children)
                                (list (token-from-string ")"))))
             (:StringLiteral (list (token-from-string "string-literal")))
             (:SwitchStmt
              (assert (= 2 (length children)))
              (append (list (token-from-string "switch")
                            (token-from-string "("))
                      (tokenize (first children))
                      (list (token-from-string ")"))
                      (tokenize (second children))))
             ;; NOTE: typedef always appears after struct, has no children in
             ;; tree
             (:Typedef (list (token-from-string "typedef")))
             (:UnaryExprOrTypeTraitExpr
              (assert (or (starts-with-subseq "sizeof" (source-text root))
                          (starts-with-subseq "alignof" (source-text root))))
              ;; Split on whitespace or non-alpha chars., preserving
              ;; non-whitespace.
              (let ((tokens (split-tokens (source-text root))))
                (mapcar #'token-from-string tokens)))
             (:UnaryOperator
              (if (starts-with-subseq (ast-opcode root) (source-text root))
                  ;; prefix
                  (cons (token-from-string (ast-opcode root))
                        (tokenize-children children))
                  ;; postfix
                  (append (tokenize-children children)
                          (list (token-from-string (ast-opcode root))))))
             (:VAArgExpr
              (let* ((comma (position #\, (source-text root)))
                     (r-paren (position #\) (source-text root)))
                     (type (split-tokens (source-text root)
                                         (1+ comma)
                                         r-paren)))
                (append (list (token-from-string "va-arg")
                              (token-from-string "(")
                              (token-from-string "identifier"))
                        (mapcar #'token-from-string type)
                        (list (token-from-string ")")))))
             ;; get all tokens from children
             (:Var (let ((idents (idents clang root)))
                     (iter (for item in (ast-children root))
                           (with child = 0)
                           (appending
                            (if (stringp item)
                                (->> (replace-identifiers idents item)
                                     (split-tokens)
                                     (mapcar #'token-from-string))
                                (prog1 (tokenize (nth child children))
                                  (incf child)))))))
             (:WhileStmt
              (assert (= 2 (length children)))
              (append (list (token-from-string "while")
                            (token-from-string "("))
                      (tokenize (first children))
                      (list (token-from-string ")"))
                      (tokenize (second children))))
             (:ConstantExpr (tokenize-children children))
             (t (error
                 (make-condition
                     'unhandled-token-class
                   :text (format nil "Unrecognized AST class ~a"
                                 (ast-class root)))))))))
    (tokenize-children roots)))
