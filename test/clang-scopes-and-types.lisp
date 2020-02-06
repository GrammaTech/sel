;;;; clang-scopes-and-types.lisp --- Clang scope and type tests.
(defpackage :software-evolution-library/test/clang-scopes-and-types
  (:nicknames :sel/test/clang-scopes-and-types)
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
   :software-evolution-library/components/fodder-database)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-clang-scopes-and-types))
(in-package :software-evolution-library/test/clang-scopes-and-types)
(in-readtable :curry-compose-reader-macros)
(defsuite test-clang-scopes-and-types "Clang representation." (clang-available-p))

(defun compare-scopes (result expected)
  (is (equal (length result) (length expected)))
  (is (every (lambda (a b)
               (and (equal (length a) (length b))
                    (every #'name= a b)))
             (mapcar {mapcar {aget :name}} result)
             expected))
  (iter (for var-info in (apply #'append result))
        (is (aget :type var-info))
        (is (aget :decl var-info))
        (is (aget :scope var-info))))

(define-constant +unbound-fun-dir+ (append +etc-dir+ (list "unbound-fun"))
  :test #'equalp
  :documentation "Location of the unbound-fun example directory")

(defun unbound-fun-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +unbound-fun-dir+))

(defixture unbound-fun-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang)
                    (unbound-fun-dir "unbound-fun.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture scopes-type-field-clang
  (:setup
   (setf *scopes*
         (from-file (make-instance 'clang
                     :compiler "clang" :flags '("-g -m32 -O0"))
                    (scopes-dir "scopes-type-field.c"))))
  (:teardown
   (setf *scopes* nil)))

(defixture scopes2-clang
  (:setup
   (setf *scopes*
         (from-file (make-instance 'clang-control-picks
                     :compiler "clang" :flags '("-g -m32 -O0"))
                    (scopes-dir "scopes2.c"))))
  (:teardown
   (setf *scopes* nil)))

(defixture scopes-cxx-clang
  (:setup
   (setf *scopes*
         (from-file (make-instance 'clang-control-picks :compiler "clang")
                    (scopes-dir "scopes.cxx"))))
  (:teardown
   (setf *scopes* nil)))

(deftest scopes-are-correct ()
  (with-fixture scopes2-clang
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int b;"))
                    '(nil
                      ("a")
                      ("global")))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int c;"))
                    '(("b")
                      ("a")
                      ("global")))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "char d;"))
                    '(nil
                      ("c" "b")
                      ("a")
                      ("global")))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "return;"))
                    '(("c" "b")
                      ("a")
                      ("global")))))

(deftest cxx-method-scopes-are-correct ()
  (with-fixture scopes-cxx-clang
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int y;"))
                    '(nil
                      ("x")
                      nil))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "int z;"))
                    '(nil
                      ("y")
                      ("x")
                      nil))
    (compare-scopes (scopes *scopes* (stmt-with-text *scopes* "y = 0;"))
                    '(("z")
                      ("y")
                      ("x")
                      nil))))

(deftest types-are-correct ()
  (with-fixture scopes2-clang
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-with-text *scopes*
                                                      "int global;")))
               '("int")))
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-with-text *scopes*
                                                      "char d;")))
               '("char")))
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-starting-with-text *scopes*
                                                               "void foo")))
               '("char" "int")))))

(deftest unbound-vals-are-correct ()
  (flet
      ((compare-vals (result expected)
         (is (null (set-exclusive-or (mapcar {aget :name} result)
                                     expected
                                     :test #'name=)))
         (iter (for var-info in result)
               (is (aget :type var-info))
               (is (aget :decl var-info))
               (is (aget :scope var-info)))))

    (with-fixture scopes2-clang
      (is (null (get-unbound-vals *scopes*
                                  (stmt-with-text *scopes* "int global;"))))
      (compare-vals (get-unbound-vals *scopes*
                                      (stmt-starting-with-text *scopes* "c ="))
                    '("global" "b" "a" "c"))
      (compare-vals (get-unbound-vals *scopes*
                                      (stmt-starting-with-text *scopes* "b ="))
                    '("b"))
      (compare-vals (get-unbound-vals *scopes*
                                      (stmt-starting-with-text *scopes* "d ="))
                    '("d"))

      (compare-vals (get-unbound-vals *scopes*
                                      (nest (stmt-starting-with-text *scopes* )
                                            "void foo"))
                    '("global")))))

(defun unbound-funs-equal (result expected)
  (and (= (length result) (length expected))
       (every
        (lambda (r e)
          (and (consp r) (consp e)
               (equal (cdr r) (cdr e))
               (name= (car r) (car e))))
        result expected)))

(deftest unbound-funs-are-correct ()
  (with-fixture scopes2-clang
    (is (null (get-unbound-funs *scopes*
                                (stmt-with-text *scopes* "int global;"))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
                           (stmt-with-text *scopes* "foo(0);"))
         '(("foo" t nil 1))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
                           (stmt-with-text *scopes* "bar();"))
         '(("bar" t nil 0))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
                           (stmt-starting-with-text *scopes* "void bar"))
         '(("foo" t nil 1)
           ("bar" t nil 0))))))

(deftest unbound-fun-not-defined ()
  (with-fixture unbound-fun-clang
    (is (unbound-funs-equal
         (get-unbound-funs *soft*
                           (stmt-with-text *soft* "g();"))
         '(("g" nil nil 0))))))

(deftest scopes-type-field-is-correct ()
  (with-fixture scopes-type-field-clang
    (is (equal "char"
               (nest (type-name)
                     (find-type *scopes*)
                     (aget :type)
                     (first)
                     (lastcar)
                     (scopes *scopes* (stmt-with-text *scopes* "return 0;"))))
        "char variable should have been found at the global scope")))

(deftest get-vars-in-scope-keep-globals-flag ()
  (with-fixture scopes-type-field-clang
    (is (name= "time_args"
               (nest (aget :name)
                     (first)
                     (get-vars-in-scope *scopes*
                                        (stmt-with-text *scopes* "return 0;")
                                        t)))
        "time_args variable should have been found at the global scope")
    (is (equal nil (get-vars-in-scope *scopes*
                                      (stmt-with-text *scopes* "return 0;")
                                      nil))
        "no variables should have been found outside the global scope")))

(deftest move-statement-updates-scopes ()
  (with-fixture scopes2-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (apply-mutation *scopes*
                      `(clang-swap (:stmt1 . ,(stmt-with-text *scopes* "int c;"))
                                   (:stmt2 . ,(stmt-with-text *scopes* "b = 0;")))))
    (compare-scopes (scopes *scopes* (stmt-starting-with-text *scopes* "b ="))
                    '(("b")
                      ("a")
                      ("global")))))

(deftest cut-decl-updates-scopes ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "int global;"))))
    (compare-scopes (scopes *scopes* (stmt-starting-with-text *scopes* "b ="))
                    '(("c" "b")
                      ("a")
                      nil))))

(deftest insert-decl-updates-types ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-insert (:stmt1 . ,(stmt-with-text *scopes* "foo(0);"))
                                   (:stmt2 . ,(stmt-with-text *scopes* "int b;"))))
    (is (equal (mapcar [#'type-name {find-type *scopes*}]
                       (get-ast-types *scopes*
                                      (stmt-starting-with-text *scopes*
                                                               "void bar")))
               '("int")))))

(deftest cut-statement-updates-unbound-funs ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "foo(0);"))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
                           (stmt-starting-with-text *scopes* "void bar"))
         '(("bar" t nil 0))))))

(deftest insert-statement-updates-unbound-funs ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-insert (:stmt1 . ,(stmt-with-text *scopes* "int b;"))
                                   (:stmt2 . ,(stmt-with-text *scopes*
                                                              "bar();"))))
    (is (unbound-funs-equal
         (get-unbound-funs *scopes*
                           (stmt-starting-with-text *scopes* "void foo"))
         '(("bar" t nil 0))))))

(deftest cut-statement-updates-unbound-vals ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-cut (:stmt1 . ,(stmt-starting-with-text *scopes*
                                                                    "c ="))))
    (is (null (get-unbound-vals *scopes*
                                (stmt-starting-with-text *scopes*
                                                         "void foo"))))))

(deftest cut-decl-updates-unbound-vals ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-cut (:stmt1 . ,(stmt-with-text *scopes* "int b;"))))
    (let ((unbound (get-unbound-vals *scopes*
                                     (stmt-starting-with-text *scopes*
                                                              "void foo"))))
      (is (fully-every #'name= (mapcar {aget :name} unbound) '("global" "b")))
      (is (aget :decl (find-if [{name= "global"} {aget :name}] unbound)))
      ;; b is now undeclared
      (is (not (aget :decl (find-if [{name= "b"} {aget :name}] unbound)))))))

(deftest insert-statement-updates-unbound-vals ()
  (with-fixture scopes2-clang
    (apply-mutation *scopes*
                    `(clang-insert (:stmt1 . ,(stmt-with-text *scopes*
                                                              "foo(0);"))
                                   (:stmt2 . ,(stmt-with-text *scopes*
                                                              "b = 0;"))))
    ;; "b" is not defined in this context so it will be rebound
    (let ((unbound (get-unbound-vals *scopes*
                                     (stmt-starting-with-text *scopes*
                                                              "void bar"))))
      (is (eq 1 (length unbound)))
      (is (name= "global" (aget :name (car unbound))))
      (is (equalp (stmt-with-text *scopes* "int global;")
                  (aget :decl (car unbound))))
      (is (eq (ast-root *scopes*) (aget :scope (car unbound))))
      (is (aget :type (car unbound))))))
