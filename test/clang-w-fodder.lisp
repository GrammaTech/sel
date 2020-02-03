;;;; clang-w-fodder.lisp --- Clang w/ mutation fodder representation.
(defpackage :software-evolution-library/test/clang-w-fodder
  (:nicknames :sel/test/clang-w-fodder)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric))
(in-package :software-evolution-library/test/clang-w-fodder)
(in-readtable :curry-compose-reader-macros)

(clang-mutate-available-p))


(deftest (clang-parse-source-snippet-body-statement :long-running) ()
  (with-fixture gcd-clang
    (let ((asts (parse-source-snippet
                 :clang
                 "x + y"
                 :unbound-vals `(("x" ,(type-from-trace-string "int"))
                                 ("y" ,(type-from-trace-string "char"))))))
      (is (eq 1 (length asts)))
      (is (eq :BinaryOperator (ast-class (car asts))))
      (is (equalp '(((:name . "y")) ((:name . "x")))
                  (get-unbound-vals *gcd* (car asts)))))))

(deftest clang-parse-source-snippet-handles-includes ()
  (let ((asts (parse-source-snippet
               :clang
               "printf(\"hello\")"
               :unbound-vals nil
               :includes '("<stdio.h>"))))
    (is (eq 1 (length asts)))
    (is (eq :CallExpr (ast-class (car asts))))
    (is (equalp '("<stdio.h>")
                (ast-includes (car asts))))))

(deftest clang-parse-source-snippet-multiple-statements ()
  (let ((asts (parse-source-snippet
               :clang
               "x = 1; y = 1"
               :unbound-vals `(("x" ,(type-from-trace-string "int"))
                               ("y" ,(type-from-trace-string "char")))
               :includes nil)))
    (is (eq 2 (length asts)))
    (is (eq :BinaryOperator (ast-class (first asts))))
    (is (eq :BinaryOperator (ast-class (second asts))))))

(deftest clang-parse-source-snippet-top-level ()
  (let ((asts (parse-source-snippet
               :clang
               "int foo() { return 1; }"
               :unbound-vals nil
               :top-level t)))
    (is (eq 1 (length asts)))
    (is (eq :Function (ast-class (car asts))))
    (is (eq :CompoundStmt (ast-class (function-body (make-clang)
                                                    (car asts)))))))

(deftest clang-parse-source-snippet-preamble ()
  (let ((asts (parse-source-snippet
               :clang
               "int *p = A + 10;"
               :unbound-vals nil
               :preamble "static int A[10];")))
    (is (eq :DeclStmt (ast-class (first asts))))))

(deftest (clang-parse-source-snippet-keep-comments :long-running) ()
  (let ((asts1 (parse-source-snippet
                :clang
                "/*POTENTIAL FLAW */ strlen(0);"
                :unbound-vals nil
                :includes '("<string.h>")
                :keep-comments t))
        (asts2 (parse-source-snippet
                :clang
                (format nil "// POTENTIAL FLAW~% strlen(0);")
                :unbound-vals nil
                :includes '("<string.h>")
                :keep-comments t)))
    (is (not (null (search "/*POTENTIAL FLAW */"
                           (source-text (first asts1))))))
    (is (not (null (search "// POTENTIAL FLAW"
                           (source-text (first asts2))))))))

(deftest simply-able-to-load-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (is (not (null *hello-world*)))))

(deftest (insert-fodder-decl-mutation-throws-error-if-no-targets-test
          :long-running) ()
  (with-fixture no-insert-fodder-decl-mutation-targets-clang
    (signals no-mutation-targets
             (apply-mutation *soft* (make-instance 'insert-fodder-decl
                                      :object *soft*)))))

(deftest
    (insert-decl-lengthens-a-clang-w-fodder-software-object :long-running) ()
  (handler-bind ((mutate ; TODO: Maybe better to fix the hello-world C file.
                  (lambda (e)
                    (if (find-restart 'keep-partial-asts)
                        (invoke-restart 'keep-partial-asts)
                        (error e)))))
    (with-fixture hello-world-clang-w-fodder
      (let ((variant (copy *hello-world*)))
        (apply-mutation variant (make-instance 'insert-fodder-decl
                                  :object variant))
        (is (> (size variant)
               (size *hello-world*)))
        (is (string/= (genome variant)
                      (genome *hello-world*)))))))

(deftest (insert-decl-rename-lengthens-and-insinuates-a-clang-w-fodder
          :long-running) ()
  (with-fixture gcd-clang-w-fodder
    (let ((var (copy *gcd*))
          (mut (make-instance 'insert-fodder-decl-rep :object *gcd*)))
      (apply-mutation var mut)
      (is (string/= (genome var) (genome *gcd*))
          "Genome of *gcd* changed by INSERT-FODDER-DECL-REP.")
      (is
       (> (length (split (aget :new-var (aget :rename-variable (targets mut)))
                         (genome var)))
          2)
       "New decl variable appears in more than just the declaring ast."))))

(deftest insert-value-lengthens-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*))
          (target (stmt-with-text *hello-world* "return 0;")))
      (apply-mutation variant
                      `(clang-insert (:stmt1 . ,target)
                                     (:value1 . ,target)))
      (is (> (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest (set-value-changes-a-clang-w-fodder-software-object
          :long-running) ()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant
                      `(clang-replace
                        (:stmt1 . ,(find-if [{eq :StringLiteral} #'ast-class]
                                            (asts variant)))
                        (:literal1 . ,(to-ast 'clang-ast `(:StringLiteral "Hello, mutate!")))))
      (is (= (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest (insert-fodder-lengthens-a-clang-w-fodder-software-object
          :long-running) ()
  (with-fixture gcd-clang-w-fodder
    (let ((variant (copy *gcd*)))
      (handler-case
          (progn (apply-mutation variant (make-instance 'insert-fodder
                                           :object variant))
                 (is (> (size variant) (size *gcd*)))
                 (is (string/= (genome variant) (genome *gcd*))))

        (mutate (e)
          ;; Fodder mutations may fail when bad variable bindings make
          ;; the snippet unparseable.
          (is (search "Failed to parse fodder" (text e))))))))

(deftest (pick-bad-fodder-works :long-running) ()
  (with-fixture hello-world-clang-w-fodder
    (let ((mut (make-instance 'insert-fodder :object *hello-world*)))
      (is (not (null (targets mut))))
      (is (not (null (aget :stmt1 (targets mut)))))
      (is (not (null (aget :value1 (targets mut))))))))

(deftest (pick-decl-fodder-works :long-running) ()
  (with-fixture hello-world-clang-w-fodder
    (let ((mut (make-instance 'insert-fodder-decl :object *hello-world*)))
      (is (not (null (targets mut))))
      (is (not (null (aget :stmt1 (targets mut)))))
      (is (not (null (aget :value1 (targets mut))))))))
