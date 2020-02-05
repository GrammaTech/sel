;;;; clang-tokenizer.lisp --- Clang tokenizer tests.
(defpackage :software-evolution-library/test/clang-tokenizer
  (:nicknames :sel/test/clang-tokenizer)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/ast
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/software/new-clang
   :software-evolution-library/components/clang-tokens
   :software-evolution-library/components/fault-loc)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-clang-tokenizer))
(in-package :software-evolution-library/test/clang-tokenizer)
(in-readtable :curry-compose-reader-macros)
(defsuite test-clang-tokenizer "Clang representation." (clang-mutate-available-p))

(defvar *test-suite* nil "Holds condition synthesis test suite object.")

(define-constant +fl-tiny-dir+ (append +etc-dir+ (list "fl-test"))
  :test #'equalp
  :documentation "Path to condition fault localization example.")

(defun fl-tiny-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +fl-tiny-dir+))

(defixture fl-tiny-clang
  (:setup (setf *soft*
                (from-file (make-clang)
                           (fl-tiny-dir "tiny-test.c")))
          (setf *test-suite*
                (make-instance 'test-suite
                  :test-cases
                  (iter (for i below 6)
                        (collecting
                         (make-instance 'test-case
                           :program-name (namestring (fl-tiny-dir "fitness.sh"))
                           :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(deftest (case-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((root (stmt-starting-with-text *variety* "case 1"))
           (tokens (tokens *variety* (list root)))
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
    (let* ((root (stmt-starting-with-text *variety* "do {"))
           (tokens (tokens *variety* (list root))))
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
    (let* ((root (stmt-starting-with-text *variety* "int add3"))
           (tokens (tokens *variety* (list root))))
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
    (let* ((root (stmt-starting-with-text *variety* "tun->foo"))
           (tokens (tokens *variety* (list root))))
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
           (token-lists (mapcar [{tokens *variety*} #'list] roots)))
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
           (token-lists (mapcar [{tokens *variety*} #'list] roots)))
      (is (= 1 (length token-lists)))
      (is (every [{<= 3} #'length] token-lists))
      (is (every [{eql (make-keyword "(")} #'first] token-lists))
      (is (every [{eql (make-keyword ")")} #'lastcar] token-lists)))))

(deftest (parmvar-tokens :long-running) ()
  (with-fixture variety-clang
    (let* ((parmvars (remove-if-not {eq :ParmVar}
                                    (asts *variety*)
                                    :key #'ast-class))
           (token-lists (mapcar [{tokens *variety*} #'list] parmvars))
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
           (token-lists (mapcar [{tokens *variety*} #'list] records))
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
           (tokens (tokens *variety* root))
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

                                        ; def this with lables
                                        ;(defun

(deftest annotations-with-fault-loc ()
  (labels ((ast-start-line (ast)
             (let ((loc (new-clang-range-begin (ast-range ast))))
               (if (numberp loc)
                   nil  ; The root node has "0" for a range.
                   (new-clang-loc-line loc)))))
    (setf *new-clang?* t)
    (with-fixture gcd-clang
      (decorate-with-annotations *gcd* (make-pathname :directory +gcd-dir+
                                                      :name "gcd-fault-loc"))
      (let* ((bad-stmts-weights (fault-loc-only-on-bad-traces *gcd*))
             (bad-stmts (remove nil (loop for tup in bad-stmts-weights
                                        ;when weighted at 1.0
                                       collect (when (equal (cdr tup) 1.0)
                                                 (car tup)))))
             (bad-lines (remove-duplicates (sort (mapcar #'ast-start-line bad-stmts) #'<))))
        (setf *new-clang?* nil) ; set back
        (is (equal bad-lines (list 16 17 18 19 20 21 24)))))))

#+nil
(deftest rinard-fault-loc ()
  (with-fixture fl-tiny-clang
    (with-temp-file (trace-file)
      (let* ((copy *soft*)
             (instrumented
              (instrument copy :trace-file trace-file))
             ;; pick first test to be "negative", arbitrarily
             (made-up-bad-test (list (nth 4 (test-cases *test-suite*))
                                     (nth 5 (test-cases *test-suite*))))
             (read-trace-fn
              (lambda (accumulated-results is-good-trace &optional test_id)
                (if (probe-file trace-file)
                    (with-open-file (trace-stream trace-file)
                      (prog1 (rinard-incremental
                              trace-stream
                              accumulated-results
                              is-good-trace
                              test_id)
                        ;; instrumentation appends to trace file for
                        ;; some reason. Remove it before each use to
                        ;; start with an empty trace.
                        (ignore-errors (delete-file trace-file))))
                    (error "Something went wrong with trace file: ~a"
                           trace-file)))))
        (with-temp-file (bin)
          (multiple-value-bind (bin phenome-exit stderr stdout src)
              (phenome instrumented :bin bin)
            (declare (ignorable stdout src))

            (let* ((trace-results (collect-fault-loc-traces
                                   bin
                                   *test-suite*
                                   read-trace-fn
                                   made-up-bad-test))
                   ;; Should be only 9 statements, only AST ids.
                   ;; The first 5 elements are always the same, in
                   ;; order.  The rest may be in an arbitrary
                   ;; order.  Compare only the first 5.
                   (bad-stmts (mapcar #'cdar (rinard 5 instrumented
                                                     trace-results)))
                   (gold-set-prefix (list 54 23 12 10 4)))
              (is (equal bad-stmts gold-set-prefix)))))))))
