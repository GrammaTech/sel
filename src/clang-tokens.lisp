(in-package :software-evolution)

(define-condition unhandled-token-class (error)
  ((text :initarg :text :initform nil :reader text))
  (:report (lambda (condition stream)
             (format stream "Tokenization failed: ~a"
                     (text condition)))))


(defgeneric tokenize (root asts)
  (:documentation "Return a list of tokens representing the AST whose root
is ROOT and whose descendants are included in the set of ASTS."))



;; For reference, list of tokens:
;; :&& :identifier :l-brace :r-brace :l-paren :r-paren :l-square :r-square
;; (opcodes) := :break :continue :case :switch :default :comma :colon :question
;; :if :else :while :do :typedef :-> :. :va-arg :return :goto :for
;; :offset-of :generic :sizeof :alignof :struct :union
;; :char-literal :int-literal :string-literal :float-literal :i-literal
;; :... :macro
(defmethod tokenize ((root ast-ref) (clang clang))
  (let* ((children (get-immediate-children clang root)))
    (flet ((tokenize-children (clang children)
             (mappend {tokenize _ clang} children))
           (tokenize-nth-child (clang children n)
             (let ((nth-child (nth n children)))
               (if nth-child
                   (tokenize nth-child clang)
                   (error (make-condition
                           'tokenization-failure
                           :text (format
                                  nil
                                  "Expected to find ~a children in AST ~a."
                                  (1+ n) root))))))
           (split-tokens (str &optional (start 0) (end (length str)))
             (remove-if #'emptyp
                        (split "\\s+|(\\W)" str
                               :with-registers-p t
                               :omit-unmatched-p t
                               :start start
                               :end end)))
           ;; in a list of lists, append a comma at the end of each list
           (comma-sep (ls)
             (cdr (mappend {cons :comma} ls)))
           (idents (clang ast)
             (remove-duplicates
              (append (ast-declares ast)
                      (apply #'append (scopes clang ast))
                      (get-unbound-funs clang ast)
                      (get-unbound-vals clang ast))
              :test #'equal))
           (replace-identifiers (identifiers str)
             (iter (for i in identifiers)
                   (setf str (replace-all str i "identifier"))
                   (finally (return str))))
           (token-from-string (str)
             (switch (str :test #'equal)
               (">" :>) ("<" :<) ("<=" :<=) (">=" :>=) ("==" :==) ("!=" :!=)
               ("&&" :&&) ("||" :pipe-pipe) ("&" :&) ("|" :pipe) ("^" :^)
               ("<<" :<<) (">>" :>>) ("+" :+) ("-" :-) ("*" :*) ("/" :/)
               ("%" :%) ("+=" :+=) ("-=" :-=) ("*=" :*=) ("/=" :/=)
               ("%=" :%=) ("++" :++) ("--" :--) ("<<=" :<<=) (">>=" :>>=)
               ("&=" :&=) ("|=" :pipe=) ("^=" :^=) ("!" :!) ("~" :~) ("=" :=)
               ("," :comma) ("->" :->) ("." :.) ("(" :l-paren) (")" :r-paren)
               ("[" :l-square) ("]" :r-square) ("{" :l-brace) ("}" :r-brace)
               ("identifier" :identifier) ("..." :...)
               ("sizeof" :sizeof) ("alignof" :alignof)
               ("struct" :struct) ("union" :union)
               (t (intern str)))))
      (switch ((ast-class root) :test #'equal)
        ("AddrLabelExpr"
         (assert (<= 2 (length (source-text root))))
         (list :&& :identifier))
        ("ArraySubscriptExpr"
         (assert (= 2 (length children)))
         (append (tokenize-nth-child clang children 0)
                 (list :l-square)
                 (tokenize-nth-child clang children 1)
                 (list :r-square)))
        ;; no tokens, just proceed to children
        ("AttributedStmt" (tokenize-children clang children))
        ("BinaryOperator"
         (assert (= 2 (length children)))
         (append (tokenize-nth-child clang children 0)
                 (list (token-from-string (ast-opcode root)))
                 (tokenize-nth-child clang children 1)))
        ("BreakStmt" (list :break))
        ("CallExpr"
         (append (tokenize-nth-child clang children 0)
                 (list :l-paren)
                 ;; separate by commas then remove trailing comma
                 (comma-sep (mapcar {tokenize-nth-child clang children}
                                    (iota (1- (length children)) :start 1)))
                 ;; right paren
                 (list :r-paren)))
        ("CaseStmt" (append (list :case)
                            (tokenize-nth-child clang children 0)
                            (list :colon)
                            (tokenize-children clang (cdr children))))
        ("CharacterLiteral" (list :char-literal))
        ("CompoundAssignOperator"
         (assert (= 2 (length children)))
         (append (tokenize-nth-child clang children 0)
                 (list (token-from-string (ast-opcode root)))
                 (tokenize-nth-child clang children 1)))
        ;; TODO: need to pull out the cast part and tokenize the
        ;; children, but some children seem to be duplicated
        ("CompoundLiteralExpr"
         (let* ((l-paren (position #\( (source-text root)))
                (r-paren (position #\) (source-text root)))
                (cast-expr (split-tokens (source-text root)
                                         (1+ l-paren)
                                         r-paren)))
           (append (list :l-paren)
                   ;; TODO: write string->token function
                   (mapcar #'token-from-string cast-expr)
                   (list :r-paren)
                   (tokenize-children clang children))))
        ("CompoundStmt" (append (list :l-brace)
                                (tokenize-children clang children)
                                (list :r-brace)))
        ("ConditionalOperator"
         (assert (= 3 (length children)))
         (append (tokenize-nth-child clang children 0)
                 (list :question)
                 (tokenize-nth-child clang children 1)
                 (list :colon)
                 (tokenize-nth-child clang children 2)))
        ("ContinueStmt" (list :continue))
        ("CStyleCastExpr"
         (let* ((l-paren (position #\( (source-text root)))
                (r-paren (position #\) (source-text root)))
                (cast-expr (split-tokens (source-text root)
                                         (1+ l-paren)
                                         r-paren)))
           (append (list :l-paren)
                   ;; TODO: write string->token function
                   (mapcar #'token-from-string cast-expr)
                   (list :r-paren)
                   (tokenize-children clang children))))
        ("DeclRefExpr" (list :identifier))
        ("DeclStmt" (tokenize-children clang children))
        ("DefaultStmt" (append (list :default :colon)
                               (tokenize-children clang children)))
        ;; [const or range] = init , last child is init, rest are for array
        ;; .field-ident = init , child is init (none for field)
        ;; [const or range]*.field-ident = init
        ("DesignatedInitExpr"
         (let* ((src (source-text root))
                (eq-index (position #\= src)))
           (append
            ;; scan characters for [], . , ...
            (iter (for i from 0 below eq-index)
                  (with child = 0)
                  (switch ((char src i))
                    (#\[ (appending (list :l-square) into tokens))
                    ;; #\] collect left child
                    (#\] (appending
                           (append (tokenize-nth-child clang children child)
                                   (list :r-square))
                           into tokens)
                         (incf child))
                    (#\. (if (and (< i (- eq-index 2))
                                  (eql #\. (char src (1+ i)))
                                  (eql #\. (char src (+ 2 i))))
                           ;; range: 0...1 collect left child
                           ;; (right child handled by #\] case)
                           (progn
                             (appending
                               (append (tokenize-nth-child clang children child)
                                       (list :...))
                               into tokens)
                             (incf child)
                             (setf i (+ i 2)))
                           ;; field: .identifier
                           (progn (appending (list :. :identifier)
                                    into tokens))))
                    (t nil))
                  (finally (return tokens)))
            (list :=)
            ;; initializer is the last child
            (tokenize-nth-child clang children (1- (length children))))))
        ("DoStmt"
         (assert (= 2 (length children)))
         (append (list :do)
                 (tokenize-nth-child clang children 0)
                 (list :while :l-paren)
                 (tokenize-nth-child clang children 1)
                 (list :r-paren)))
        ("Enum" (let ((has-ident (not (emptyp (first (ast-declares root))))))
                  (append (list :enum)
                          (when has-ident (list :identifier))
                          (list :l-brace)
                          (comma-sep (mapcar {tokenize-nth-child clang children}
                                             (iota (length children))))
                          (list :r-brace))))
        ("EnumConstant" (list :identifier))
        ("Field" (let* ((src (replace-identifiers (idents clang root)
                                                  (source-text root)))
                        (src (remove #\; src)))
                   (mapcar #'token-from-string (split-tokens src))))
        ("FloatingLiteral" (list :float-literal))
        ("ForStmt"
         (append (list :for :l-paren)
                 (mappend {tokenize-nth-child clang children}
                          (iota (1- (length children))))
                 (list :r-paren)
                 (tokenize-nth-child clang children (1- (length children)))))

        ("Function"
         (let* ((sig (take-until {string= "("}
                       (split-tokens
                        (replace-identifiers (idents clang root)
                                             (source-text root))))))
           (append (mapcar #'token-from-string sig)
                   (list :l-paren)
                   ;; comma-separated ParmVars (all but last child)
                   (comma-sep (mapcar {tokenize-nth-child clang children}
                                      (iota (1- (length children)))))
                   (list :r-paren)
                   (tokenize-nth-child clang children (1- (length children))))))
        ;; _Generic(child0, type: child1, type: child2, ...)
        ("GenericSelectionExpr"
         (let* ((comma (position #\, (source-text root)))
                ;; split on commas to get each (type: child) pairs
                (a-ls (cdr (split "\\)|,\\s*" (source-text root)
                                  :start comma)))
                ;; split a-ls on : to get types
                (types (mapcar [#'token-from-string #'first {split ":\\s*"}]
                               a-ls))
                ;; indices of children to tokenize
                (types-children (iota (length types) :start 1)))
           (assert (= (length types) (1- (length children))))
           (append (list :generic :l-paren)
                   (tokenize-nth-child clang children 0)
                   (mappend (lambda (type toks)
                              (append (list :comma type :colon)
                                      toks))
                            types
                            (mapcar {tokenize-nth-child clang children}
                                    types-children))
                   (list :r-paren))))
        ("GotoStmt" (list :goto :identifier))
        ("IfStmt" (append (list :if :l-paren)
                          (tokenize-nth-child clang children 0)
                          (list :r-paren)
                          (tokenize-nth-child clang children 1)
                          (when (= 3 (length children))
                            (cons :else
                                  (tokenize-nth-child clang children 2)))))
        ("ImaginaryLiteral" (list :i-literal))
        ;; Just tokenize children
        ("ImplicitCastExpr" (tokenize-children clang children))
        ("IndirectGotoStmt" (append (list :goto :*)
                                    (tokenize-children clang children)))
        ;; TODO might be broken: seems that some InitListExprs have a
        ;; child that duplicates the whole InitListExpr?
        ("InitListExpr"
         (append (list :l-brace)
                 (comma-sep (mapcar {tokenize-nth-child clang children}
                                    (iota (length children))))
                 (list :r-brace)))
        ("IntegerLiteral" (list :int-literal))
        ("LabelStmt" (append (list :identifier :colon)
                             (tokenize-children clang children)))
        ("MacroExpansion" (list :macro))
        ;; x.y or x->y (one child for leftof ->/.)
        ("MemberExpr"
         ;; find start of rightmost -> or .
         (let* ((dash (position #\- (source-text root) :from-end t))
                (dot (position #\. (source-text root) :from-end t))
                ;; identify (rightmost) -> or .
                (dash-dot (if (or (and dash dot (= dash (max dash dot)))
                                  (not dot))
                              (list :->)
                              (list :.))))
           (assert (= 1 (length children)))
           (append (tokenize-nth-child clang children 0)
                   dash-dot
                   (list :identifier))))
        ("NullStmt" nil)
        ("OffsetOfExpr" (list :offset-of
                              :l-paren
                              :identifier
                              :comma
                              :identifier
                              :r-paren))
        ("ParenExpr" (append (list :l-paren)
                             (tokenize-children clang children)
                             (list :r-paren)))
        ("ParmVar" (->> (replace-identifiers (idents clang root)
                                             (source-text root))
                        (split-tokens)
                        (mapcar #'token-from-string)))
        ("PredefinedExpr" (list (token-from-string (source-text root))))
        ;; NOTE: struct, union. May include fields or just be a declaration
        ("Record"
         (let* ((src (replace-identifiers (idents clang root)
                                          (source-text root)))
                (end (position #\{ src)))
           (assert (or (starts-with-subseq "struct" src)
                       (starts-with-subseq "union" src)))
           (append (mapcar #'token-from-string
                           (split-tokens src 0 (or end (length src))))
                   (when end
                     (append (list :l-brace)
                             (tokenize-children clang children)
                             (list :r-brace))))))
        ("ReturnStmt" (cons :return
                            (tokenize-children clang children)))
        ;; parenthesized CompoundStmt
        ("StmtExpr" (append (list :l-paren)
                            (tokenize-children clang children)
                            (list :r-paren)))
        ("StringLiteral" (list :string-literal))
        ("SwitchStmt"
         (assert (= 2 (length children)))
         (append (list :switch :l-paren)
                 (tokenize-nth-child clang children 0)
                 (list :r-paren)
                 (tokenize-nth-child clang children 1)))
        ;; NOTE: typedef always appears after struct, has no children in tree
        ("Typedef" (list :typedef))
        ("UnaryExprOrTypeTraitExpr"
         (assert (or (starts-with-subseq "sizeof" (source-text root))
                     (starts-with-subseq "alignof" (source-text root))))
         ;; split on whitespace or non-alpha chars., preserving non-whitespace
         (let ((tokens (split-tokens (source-text root))))
           (mapcar #'token-from-string tokens)))
        ("UnaryOperator"
         (if (starts-with-subseq (ast-opcode root) (source-text root))
             ;; prefix
             (cons (token-from-string (ast-opcode root))
                   (tokenize-children clang children))
             ;; postfix
             (append (tokenize-children clang children)
                     (list (token-from-string (ast-opcode root))))))
        ("VAArgExpr"
         (let* ((comma (position #\, (source-text root)))
                (r-paren (position #\) (source-text root)))
                (type (split-tokens (source-text root)
                                    (1+ comma)
                                    r-paren)))
           (append (list :va-arg :l-paren :identifier)
                   (mapcar #'token-from-string type)
                   (list :r-paren))))
        ;; get all tokens from children
        ("Var" (let ((ast-ls (ast-ref-ast root))
                     (idents (idents clang root)))
                 (iter (for item in (cdr ast-ls))
                       (with child = 0)
                       (appending
                        (if (stringp item)
                            (->> (replace-identifiers idents item)
                                 (split-tokens)
                                 (mapcar #'token-from-string))
                            (prog1 (tokenize-nth-child clang children child)
                                   (incf child)))))))
        ("WhileStmt"
         (assert (= 2 (length children)))
         (append (list :while :l-paren)
                 (tokenize-nth-child clang children 0)
                 (list :r-paren)
                 (tokenize-nth-child clang children 1)))
        (t (error
            (make-condition
             'unhandled-token-class
             :text (format nil "Unrecognized AST class ~a"
                           (ast-class root)))))))))
