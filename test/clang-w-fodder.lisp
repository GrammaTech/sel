;;;; clang-w-fodder.lisp --- Clang w/ mutation fodder representation.
(defpackage :software-evolution-library/test/clang-w-fodder
  (:nicknames :sel/test/clang-w-fodder)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/software/clang-w-fodder
   :software-evolution-library/components/fodder-database
   :software-evolution-library/components/json-fodder-database)
  (:export :test-clang-w-fodder))
(in-package :software-evolution-library/test/clang-w-fodder)
(in-readtable :curry-compose-reader-macros)
(defsuite test-clang-w-fodder "Clang representation." (clang-available-p))

(defixture no-insert-fodder-decl-mutation-targets-clang
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example.json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in)))
   (setf *soft* (from-file (make-instance 'clang-w-fodder)
                           (lisp-bugs-dir
                            "no-insert-fodder-decl-mutation-targets.c"))))
  (:teardown
   (setf *database* nil)
   (setf *soft* nil)))

(defixture hello-world-clang-w-fodder
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example.json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in)))
   (setf *hello-world*
         (from-file (make-instance 'clang-w-fodder :compiler "clang"
                                   :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))))
  (:teardown
   (setf *database* nil)
   (setf *hello-world* nil)))

(defixture gcd-clang-w-fodder
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example"
                                            :type "json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in)))
   (setf *gcd*
         (from-file
          (make-instance 'clang-w-fodder
            :flags (list
                    "-I"
                    (namestring (make-pathname :directory +etc-dir+))))
          (gcd-dir "gcd.c"))))
  (:teardown
   (setf *database* nil)
   (setf *gcd* nil)))

(deftest (clang-convert-snippet-body-statement :long-running) ()
  (with-fixture gcd-clang
    (let ((asts (convert
                 'clang-ast
                 "x + y"
                 :unbound-vals `(("x" ,(type-from-trace-string "int"))
                                 ("y" ,(type-from-trace-string "char"))))))
      (is (eq 1 (length asts)))
      (is (eq :BinaryOperator (ast-class (car asts))))
      (is (equal '("y" "x")
                 (mapcar [#'ast-name {aget :name}]
                         (get-unbound-vals *gcd* (car asts))))))))

(deftest clang-convert-source-snippet-handles-includes ()
  (let ((asts (convert
               'clang-ast
               "printf(\"hello\")"
               :unbound-vals nil
               :includes '("<stdio.h>"))))
    (is (eq 1 (length asts)))
    (is (eq :CallExpr (ast-class (car asts))))
    (is (equalp '("<stdio.h>")
                (ast-includes nil (car asts))))))

(deftest clang-convert-source-snippet-multiple-statements ()
  (let ((asts (convert
               'clang-ast
               "x = 1; y = 1"
               :unbound-vals `(("x" ,(type-from-trace-string "int"))
                               ("y" ,(type-from-trace-string "char")))
               :includes nil)))
    (is (eq 2 (length asts)))
    (is (eq :BinaryOperator (ast-class (first asts))))
    (is (eq :BinaryOperator (ast-class (second asts))))))

(deftest clang-convert-source-snippet-top-level ()
  (let ((asts (convert
               'clang-ast
               "int foo() { return 1; }"
               :unbound-vals nil
               :top-level t)))
    (is (eq 1 (length asts)))
    (is (eq :Function (ast-class (car asts))))
    (is (eq :CompoundStmt (ast-class (function-body (make-instance 'clang)
                                                    (car asts)))))))

(deftest clang-convert-source-snippet-preamble ()
  (let ((asts (convert
               'clang-ast
               "int *p = A + 10;"
               :unbound-vals nil
               :preamble "static int A[10];")))
    (is (eq :DeclStmt (ast-class (first asts))))))

(deftest (clang-convert-source-snippet-keep-comments :long-running) ()
  (let ((asts1 (convert
                'clang-ast
                "/*POTENTIAL FLAW */ strlen(0);"
                :unbound-vals nil
                :includes '("<string.h>")
                :keep-comments t))
        (asts2 (convert
                'clang-ast
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
      ;; FIXME: Variable rebinding may overwrite the variable
      ;; declaration. This mutation was broken prior to the commit
      ;; associated with this comment, and the below test appears to
      ;; have succeeded (and continues to succeed) mainly by coincidence.
      (is
       (> (length (split (nest (ast-name)
                               (aget :new-var)
                               (aget :rename-variable)
                               (targets mut))
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
                        (:stmt1 .
                                ,(find-if [{eq :StringLiteral} #'ast-class]
                                          (asts variant)))
                        (:literal1 .
                                   ,(convert 'clang-ast
                                             `(:StringLiteral "Hello, mutate!")))))
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
