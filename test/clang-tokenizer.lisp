;;;; clang-tokenizer.lisp --- Clang tokenizer tests.
(defpackage :software-evolution-library/test/clang-tokenizer
  (:nicknames :sel/test/clang-tokenizer)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/components/clang-tokens)
  (:export :test-clang-tokenizer))
(in-package :software-evolution-library/test/clang-tokenizer)
(in-readtable :curry-compose-reader-macros)
(defsuite test-clang-tokenizer "Clang representation." (clang-available-p))

(deftest (case-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((root (stmt-with-text *variety* "case 1" :at-start t))
           (tokens (clang-tokens *variety* (list root)))
           (switch-tokens
            (mapcar #'make-keyword
                    ;; case 1:
                    (list "case" "int-literal" ":"
                          ;; printf("%d\n", argc
                          "identifier" "(" "string-literal" "," "identifier"
                          ;; + argc);
                          "+" "identifier" ")"))))
      (is (equal tokens switch-tokens)))))

(deftest (do-while-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((root (stmt-with-text *variety* "do {" :at-start t))
           (tokens (clang-tokens *variety* (list root))))
      (is (equal tokens
                 ;; do {
                 (mapcar #'make-keyword
                         (list "do" "{"
                               ;; run.foo++
                               "identifier" "." "identifier" "++"
                               ;; } while (run.foo
                               "}" "while" "(" "identifier" "." "identifier"
                               ;; < 8)
                               "<" "int-literal" ")")))))))

(deftest (function-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((root (stmt-with-text *variety* "int add3" :at-start t))
           (tokens (clang-tokens *variety* (list root))))
      (is (equal
           tokens
           ;; int add3(int x) {
           (mapcar #'make-keyword
                   (list "int" "identifier" "(" "int" "identifier"
                         ")" "{"
                         ;; printf("...", __func__)
                         "identifier" "(" "string-literal" "," "__func__" ")"
                         ;; return x + 3 }
                         "return" "identifier" "+" "int-literal" "}")))))))

(deftest (mixed-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((root (stmt-with-text *variety* "tun->foo" :at-start t))
           (tokens (clang-tokens *variety* (list root))))
      (is
       (equal
        tokens
        (mapcar #'make-keyword
                (list
                 ;; tun -> foo = _Generic(
                 "identifier" "->" "identifier" "=" "generic" "("
                 ;; tun -> foo, int: 12
                 "identifier" "->" "identifier" "," "int" ":" "int-literal"
                 ;; , char: 'q')
                 "," "char" ":" "char-literal" ")")))))))

(deftest (memberexpr-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((roots (remove-if-not {eq :MemberExpr}
                                 (asts *variety*)
                                 :key #'ast-class))
           (token-lists (mapcar [{clang-tokens *variety*} #'list] roots)))
      (is (every [{<= 3} #'length] token-lists))
      (is (every (lambda (ls)
                   (or (equal ls (mapcar #'make-keyword
                                         (list "identifier" "." "identifier")))
                       (equal ls (mapcar #'make-keyword
                                         (list "identifier" "->" "identifier")))
                       (equal ls (mapcar #'make-keyword
                                         (list "identifier" "[" "int-literal"
                                               "]" "." "identifier")))))
                 token-lists)))))

(deftest (parenexpr-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((roots (remove-if-not {eq :ParenExpr}
                                 (asts *variety*)
                                 :key #'ast-class))
           (token-lists (mapcar [{clang-tokens *variety*} #'list] roots)))
      (is (= 1 (length token-lists)))
      (is (every [{<= 3} #'length] token-lists))
      (is (every [{eql (make-keyword "(")} #'first] token-lists))
      (is (every [{eql (make-keyword ")")} #'lastcar] token-lists)))))

(deftest (parmvar-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((parmvars (remove-if-not {eq :ParmVar}
                                    (asts *variety*)
                                    :key #'ast-class))
           (token-lists (mapcar [{clang-tokens *variety*} #'list] parmvars))
           (argv-tokens (mapcar #'make-keyword
                                (list "char" "*" "*" "identifier")))
           (int-tokens (mapcar #'make-keyword
                               (list "int" "identifier"))))
      (is (= 5 (length token-lists)))
      (is (every [{<= 2} #'length] token-lists))
      (is (member int-tokens token-lists :test #'equal))
      (is (member argv-tokens token-lists :test #'equal)))))

(deftest (record-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((records (remove-if-not {eq :Record}
                                   (asts *variety*)
                                   :key #'ast-class))
           (token-lists (mapcar [{clang-tokens *variety*} #'list] records))
           (union-tokens (mapcar #'make-keyword
                                 (list "union" "{"
                                       "int" "identifier"
                                       "char" "identifier" "}")))
           (struct-tokens (mapcar #'make-keyword
                                  (list "struct" "identifier" "{"
                                        "double" "identifier"
                                        "double" "identifier" "}"))))
      (is (= 2 (length token-lists)))
      (is (member union-tokens token-lists :test #'equal))
      (is (member struct-tokens token-lists :test #'equal)))))

(deftest (while-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((root (remove-if-not {eq :WhileStmt}
                                (asts *variety*)
                                :key #'ast-class))
           (tokens (clang-tokens *variety* root))
           (while-tokens
            (mapcar #'make-keyword
                    ;; while((next = va_arg(nums, int))
                    (list "while" "(" "(" "identifier" "=" "macro" ")"
                          ;; && num_args > 0) {
                          "&&" "identifier" ">" "int-literal" ")" "{"
                          ;; sum += next
                          "identifier" "+=" "identifier"
                          ;; num_args-- }
                          "identifier" "--" "}"))))
      (is (equal tokens while-tokens)))))
