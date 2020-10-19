;;;; clang-crossover.lisp --- Crossover tests.
(defpackage :software-evolution-library/test/clang-crossover
  (:nicknames :sel/test/clang-crossover)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang)
  (:export :test-clang-crossover))
(in-package :software-evolution-library/test/clang-crossover)
(in-readtable :curry-compose-reader-macros)
(defsuite test-clang-crossover "Crossover tests." (clang-available-p))

(defvar *collatz* nil "Holds the collatz software object.")

(define-constant +collatz-dir+
    (append +etc-dir+ (list "collatz"))
  :test #'equalp
  :documentation "Location of the collatz example dir")

(defun collatz-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +collatz-dir+))

(defixture collatz-clang
  (:setup
   (setf *collatz*
         (from-file (make-instance 'clang
                     :compiler "clang"
                     :flags '("-O0" "-g" "-c"))
                    (collatz-dir "collatz.c"))))
  (:teardown
   (setf *collatz* nil)))

(defixture crossover-no-compound-stmt-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang
                     :compiler "clang"
                     :flags '("-O0" "-g"))
                    (clang-crossover-dir
                     "crossover-no-compound-stmt.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture crossover-switch-stmt-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang
                     :compiler "clang"
                     :flags '("-O0" "-g"))
                    (clang-crossover-dir
                     "crossover-switch-stmt.c"))))
  (:teardown
   (setf *soft* nil)))

(defun select-intraprocedural-pair-with-adjustments-test (obj)
  (let ((function (first (functions obj))))
    (loop :for i :from 0 :to 25
       :do (progn (multiple-value-bind (pt1 pt2)
                      (select-intraprocedural-pair obj)
                    (multiple-value-bind (stmt1 stmt2)
                        (adjust-stmt-range obj pt1 pt2)
                      (is (<= (1+ (first (stmt-range obj function)))
                              stmt1
                              (second (stmt-range obj function))))
                      (is (<= (1+ (first (stmt-range obj function)))
                              stmt2
                              (second (stmt-range obj function))))
                      (is (ast-full-stmt (ast-at-index obj stmt1)))
                      (is (ast-full-stmt (ast-at-index obj stmt2)))))))))

(deftest select-intraprocedural-pair-with-adjustments-collatz-test ()
  (with-fixture collatz-clang
    (select-intraprocedural-pair-with-adjustments-test *collatz*)))

(deftest select-intraprocedural-pair-with-adjustments-fib-test ()
  (with-fixture fib-clang
    (select-intraprocedural-pair-with-adjustments-test *fib*)))

(deftest select-intraprocedural-pair-with-adjustments-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (select-intraprocedural-pair-with-adjustments-test *soft*)))

(deftest select-intraprocedural-pair-with-adjustments-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (select-intraprocedural-pair-with-adjustments-test *soft*)))

(deftest nesting-relation-same-scope-test ()
  (with-fixture fib-clang
    (is (equal '(0 . 0)
               (nesting-relation *fib*
                                 (stmt-with-text *fib* "int t = x;")
                                 (stmt-with-text *fib* "x = x + y;")))
        (equal '(0 . 0)
               (nesting-relation *fib*
                                 (stmt-with-text *fib* "int t = x;")
                                 (stmt-with-text *fib* "int t = x;")))
        (equal '(0 . 0)
               (nesting-relation *fib*
                                 (aget :counter (first (stmt-asts *fib*)))
                                 (aget :counter (first (stmt-asts *fib*))))))))

(deftest nesting-relation-increasing-scope-fib-test ()
  (with-fixture fib-clang
    (is (equal '(0 . 1)
               (nesting-relation *fib*
                                 (stmt-with-text *fib* "int x = 0")
                                 (stmt-with-text *fib* "int t = x"))))))

(deftest nesting-relation-decreasing-scope-fib-test ()
  (with-fixture fib-clang
    (is (equal '(1 . 0)
               (nesting-relation *fib*
                                 (stmt-with-text *fib* "int t = x")
                                 (stmt-with-text *fib* "return x;"))))))

(deftest nesting-relation-increasing-scope-collatz-test ()
  (with-fixture collatz-clang
    (is (equal '(0 . 2)
               (nesting-relation *collatz*
                                 (stmt-with-text *collatz* "int k = 0")
                                 (stmt-with-text *collatz* "m /= 2;"))))))

(deftest nesting-relation-decreasing-scope-collatz-test ()
  (with-fixture collatz-clang
    (is (equal '(2 . 0)
               (nesting-relation *collatz*
                                 (stmt-with-text *collatz* "m /= 2;")
                                 (stmt-with-text *collatz* "return k;"))))))

(deftest nesting-relation-increasing-scope-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal '(0 . 2)
               (nesting-relation *soft*
                                 (stmt-with-text *soft* "int i")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);"))))))

(deftest nesting-relation-decreasing-scope-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal '(2 . 0)
               (nesting-relation *soft*
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);")
                                 (stmt-with-text *soft*
                                                 "return 0;"))))))

(deftest nesting-relation-increasing-scope-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal '(0 . 1)
               (nesting-relation *soft*
                                 (nest (stmt-with-text *soft*)
                                       "printf(\"%d\\n\", argc);")
                                 (nest (stmt-with-text *soft*)
                                       "printf(\"%d\\n\", argc + argc);"))))))

(deftest nesting-relation-decreasing-scope-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal '(1 . 0)
               (nesting-relation *soft*
                                 (nest (stmt-with-text *soft*)
                                       "printf(\"%d\\n\", argc + argc);")
                                 (nest (stmt-with-text *soft*)
                                       "return 0;"))))))

(deftest common-ancestor-fib-test ()
  (with-fixture fib-clang
    (is (equalp (function-body (stmt-starting-with-text *fib* "int fib"))
                (common-ancestor *fib*
                                 (stmt-with-text *fib* "int x = 0")
                                 (stmt-with-text *fib* "int y = 1"))))
    (is (equalp (function-body (stmt-starting-with-text *fib* "int fib"))
                (common-ancestor *fib*
                                 (stmt-with-text *fib* "int x = 0")
                                 (stmt-with-text *fib* "int t = x"))))
    (is (equalp (nest (second)
                      (child-asts)
                      (stmt-starting-with-text *fib* "while"))
                (common-ancestor *fib*
                                 (stmt-with-text *fib* "int t = x")
                                 (stmt-with-text *fib* "x = x + y;"))))))

(deftest common-ancestor-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *collatz* "int collatz"))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "int k = 0")
                                 (stmt-with-text *collatz* "return k;"))))
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *collatz* "int collatz"))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "int k = 0")
                                 (stmt-with-text *collatz* "m /= 2;"))))
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *collatz* "int collatz"))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "m /= 2;")
                                 (stmt-with-text *collatz* "return k;"))))
    (is (equalp (stmt-starting-with-text *collatz* "if")
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "m /= 2;")
                                 (stmt-with-text *collatz* "m = 3*m + 1;"))))
    (is (equalp (nest (second)
                      (child-asts)
                      (stmt-starting-with-text *collatz* "while"))
                (common-ancestor *collatz*
                                 (stmt-with-text *collatz* "m /= 2;")
                                 (stmt-with-text *collatz* "++k;"))))
    (is (equalp (nest (second)
                      (child-asts)
                      (stmt-starting-with-text *collatz* "while"))
                (common-ancestor *collatz*
                                 (nest (second)
                                       (child-asts)
                                       (stmt-starting-with-text *collatz* "while"))
                                 (stmt-starting-with-text *collatz* "if"))))))

(deftest common-ancestor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *soft* "int main"))
                (common-ancestor *soft*
                                 (stmt-with-text *soft* "int i")
                                 (stmt-with-text *soft* "return 0;"))))
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *soft* "int main"))
                (common-ancestor *soft*
                                 (stmt-with-text *soft*
                                                 "int i")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (common-ancestor *soft*
                                 (stmt-starting-with-text *soft* "for (i = 0")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (common-ancestor *soft*
                                 (stmt-starting-with-text *soft* "for (j = 0")
                                 (stmt-with-text *soft*
                                                 "printf(\"%d\\n\", i+j);"))))))

(deftest common-ancestor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *soft* "int main"))
                (common-ancestor *soft*
                                 (nest (stmt-with-text *soft*)
                                       "printf(\"%d\\n\", argc);")
                                 (nest (stmt-with-text *soft*)
                                       "printf(\"%d\\n\", argc + argc);"))))
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *soft* "int main"))
                (common-ancestor *soft*
                                 (nest (stmt-with-text *soft*)
                                       "printf(\"%d\\n\", argc + argc);")
                                 (nest (stmt-with-text *soft*)
                                       "return 0;"))))
    (is (equalp (nest (second)
                      (child-asts)
                      (stmt-starting-with-text *soft* "switch"))
                (common-ancestor *soft*
                                 (nest (stmt-with-text *soft*)
                                       "printf(\"%d\\n\", argc + argc);")
                                 (nest (stmt-with-text *soft*)
                                       "printf(\"%d\\n\", argc * argc);"))))
    (is (equalp (stmt-starting-with-text *soft* "case 1")
                (common-ancestor *soft*
                                 (nest (stmt-with-text *soft*)
                                       "printf(\"%d\\n\", argc + argc);")
                                 (stmt-starting-with-text *soft* "case 1"))))))

(deftest ancestor-after-fib-test ()
  (with-fixture fib-clang
    (is (equalp (stmt-with-text *fib* "int x = 0;")
                (ancestor-after *fib*
                                (nest (function-body)
                                      (stmt-starting-with-text *fib*)
                                      "int fib")
                                (stmt-with-text *fib* "int x = 0;"))))
    (is (equalp (nest (first)
                      (remove-if-not [{eq :WhileStmt} #'ast-class])
                      (stmt-asts *fib*))
                (ancestor-after *fib*
                                (nest (function-body)
                                      (stmt-starting-with-text *fib*)
                                      "int fib")
                                (stmt-with-text *fib* "int t = x;"))))
    (is (equalp (stmt-with-text *fib* "x = x + y;")
                (ancestor-after *fib*
                                (nest (second)
                                      (child-asts)
                                      (stmt-starting-with-text *fib*)
                                      "while ")
                                (stmt-with-text *fib* "x = x + y;"))))))

(deftest ancestor-after-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (stmt-with-text *collatz* "int k = 0;")
                (ancestor-after *collatz*
                                (nest (function-body)
                                      (stmt-starting-with-text *collatz*)
                                      "int collatz")
                                (stmt-with-text *collatz* "int k = 0;"))))
    (is (equalp (stmt-with-text *collatz* "return k;")
                (ancestor-after *collatz*
                                (nest (function-body)
                                      (stmt-starting-with-text *collatz*)
                                      "int collatz")
                                (stmt-with-text *collatz* "return k;"))))
    (is (equalp (nest (first)
                      (remove-if-not [{eq :WhileStmt} #'ast-class])
                      (stmt-asts *collatz*))
                (ancestor-after *collatz*
                                (nest (function-body)
                                      (stmt-starting-with-text *collatz*)
                                      "int collatz")
                                (stmt-with-text *collatz* "m /= 2;"))))
    (is (equalp (nest (first)
                      (remove-if-not [{eq :IfStmt} #'ast-class])
                      (stmt-asts *collatz*))
                (ancestor-after *collatz*
                                (nest (second)
                                      (child-asts)
                                      (stmt-starting-with-text *collatz*)
                                      "while")
                                (stmt-with-text *collatz* "m /= 2;"))))
    (is (equalp (nest (first)
                      (remove-if-not [{eq :IfStmt} #'ast-class])
                      (stmt-asts *collatz*))
                (ancestor-after *collatz*
                                (nest (second)
                                      (child-asts)
                                      (stmt-starting-with-text *collatz*)
                                      "while")
                                (stmt-with-text
                                 *collatz* "m = 3*m + 1;"))))
    (is (equalp (stmt-with-text *collatz* "++k;")
                (ancestor-after *collatz*
                                (nest (second)
                                      (child-asts)
                                      (stmt-starting-with-text *collatz*)
                                      "while")
                                (stmt-with-text *collatz* "++k;"))))))

(deftest ancestor-after-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (ancestor-after *soft*
                                (stmt-starting-with-text
                                 *soft* "for (i = 0")
                                (stmt-with-text
                                 *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
                (ancestor-after *soft*
                                (stmt-starting-with-text *soft*
                                                         "for (j = 0")
                                (stmt-with-text
                                 *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (ancestor-after *soft*
                                (nest (function-body)
                                      (stmt-starting-with-text *soft*)
                                      "int main")
                                (stmt-with-text
                                 *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equalp (stmt-with-text *soft* "return 0;")
                (ancestor-after *soft*
                                (nest (function-body)
                                      (stmt-starting-with-text *soft*)
                                      "int main")
                                (stmt-with-text *soft* "return 0;"))))))

(deftest ancestor-after-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "case 2")
                (ancestor-after *soft*
                                (nest (second)
                                      (child-asts)
                                      (stmt-starting-with-text *soft*)
                                      "switch")
                                (nest (stmt-with-text *soft*)
                                      "printf(\"%d\\n\", argc * argc);"))))))

(deftest enclosing-full-stmt-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (stmt-with-text *collatz* "m = 3*m + 1;")
                (enclosing-full-stmt *collatz*
                                     (stmt-with-text *collatz* "3"))))
    (is (equalp (stmt-with-text *collatz* "m = 3*m + 1;")
                (enclosing-full-stmt *collatz*
                                     (stmt-with-text *collatz*
                                                     "m = 3*m + 1;"))))
    (is (equalp (stmt-with-text *collatz* "int k = 0;")
                (enclosing-full-stmt *collatz*
                                     (stmt-with-text
                                      *collatz* "int k = 0;"))))))

(deftest enclosing-full-stmt-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
                (nest (enclosing-full-stmt *soft*)
                      (stmt-with-text *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equalp (nest (first)
                      (remove-if-not [{eq :ForStmt} #'ast-class]
                                     (stmt-asts *soft*)))
                (enclosing-full-stmt *soft*
                                     (nest (first)
                                           (remove-if-not [{eq :ForStmt}
                                                           #'ast-class]
                                                          (stmt-asts *soft*))))))
    (is (equalp (nest (second)
                      (remove-if-not [{eq :ForStmt} #'ast-class]
                                     (stmt-asts *soft*)))
                (enclosing-full-stmt *soft*
                                     (nest (second)
                                           (remove-if-not [{eq :ForStmt}
                                                           #'ast-class]
                                                          (stmt-asts *soft*))))))
    (is (equalp (stmt-with-text *soft* "int i;")
                (enclosing-full-stmt *soft*
                                     (stmt-with-text *soft* "int i;"))))))

(deftest enclosing-full-stmt-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
                (nest (enclosing-full-stmt *soft*)
                      (stmt-with-text *soft* "printf(\"%d\\n\", argc);"))))
    (is (equalp (nest (first)
                      (remove-if-not [{eq :SwitchStmt} #'ast-class]
                                     (stmt-asts *soft*)))
                (enclosing-full-stmt *soft*
                                     (nest (first)
                                           (remove-if-not [{eq :SwitchStmt}
                                                           #'ast-class]
                                                          (stmt-asts *soft*))))))
    (is (equalp (nest (first)
                      (remove-if-not [{eq :CaseStmt} #'ast-class]
                                     (stmt-asts *soft*)))
                (enclosing-full-stmt *soft*
                                     (nest (first)
                                           (remove-if-not [{eq :CaseStmt}
                                                           #'ast-class]
                                                          (stmt-asts *soft*))))))))

(deftest enclosing-block-collatz-test ()
  (with-fixture collatz-clang
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *collatz* "int collatz"))
                (enclosing-block *collatz* (stmt-with-text *collatz*
                                                           "int k = 0;"))))
    (is (equalp (nest (second)
                      (child-asts)
                      (find-if [{eq :WhileStmt} #'ast-class])
                      (stmt-asts *collatz*))
                (enclosing-block *collatz* (stmt-with-text *collatz* "++k;"))))
    (is (equalp (nest (second)
                      (child-asts)
                      (find-if [{eq :IfStmt} #'ast-class])
                      (stmt-asts *collatz*))
                (enclosing-block *collatz*
                                 (stmt-with-text *collatz* "m /= 2;"))))
    (is (null (nest (enclosing-block *collatz*)
                    (function-body)
                    (stmt-starting-with-text *collatz* "int collatz"))))))

(deftest (enclosing-block-no-compound-stmt-test :long-running) ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equalp (stmt-starting-with-text *soft* "for (j = 0")
                (nest (enclosing-block *soft*)
                      (stmt-with-text *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *soft* "int main"))
                (enclosing-block *soft*
                                 (stmt-starting-with-text *soft*
                                                          "for (i = 0"))))
    (is (equalp (stmt-starting-with-text *soft* "for (i = 0")
                (enclosing-block *soft* (stmt-starting-with-text
                                         *soft* "for (j = 0"))))
    (is (null (enclosing-block *soft*
                               (nest (function-body)
                                     (stmt-starting-with-text *soft* "int main")))))))

(deftest enclosing-block-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equalp (nest (function-body)
                      (stmt-starting-with-text *soft* "int main"))
                (nest (enclosing-block *soft*)
                      (stmt-with-text *soft* "printf(\"%d\\n\", argc);"))))
    (is (equalp (nest (second)
                      (child-asts)
                      (find-if [{eq :SwitchStmt} #'ast-class])
                      (stmt-asts *soft*))
                (nest (enclosing-block *soft*)
                      (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);"))))
    (is (equalp (nest (second)
                      (child-asts)
                      (find-if [{eq :SwitchStmt} #'ast-class])
                      (stmt-asts *soft*))
                (nest (enclosing-block *soft*)
                      (first)
                      (remove-if-not [{eq :CaseStmt} #'ast-class])
                      (stmt-asts *soft*))))))

(deftest block-p-collatz-test ()
  (with-fixture collatz-clang
    (loop :for ast
       :in (stmt-asts *collatz*)
       :do (is (equal (eq :CompoundStmt (ast-class ast))
                      (block-p ast))))))

(deftest block-p-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (loop :for ast
       :in (stmt-asts *soft*)
       :do (is (equal (or (eq :CompoundStmt (ast-class ast))
                          (eq :ForStmt (ast-class ast)))
                      (block-p ast))))))

(deftest block-p-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (loop :for ast
       :in (stmt-asts *soft*)
       :do (is (equal (eq :CompoundStmt (ast-class ast))
                      (block-p ast))))))

(deftest block-successor-collatz-test ()
  (with-fixture collatz-clang
    (is (eq :WhileStmt
            (nest (ast-class)
                  (block-successor *collatz* (stmt-with-text *collatz*
                                                             "int k = 0;")))))
    (is (eq :ReturnStmt
            (nest (ast-class)
                  (block-successor *collatz*
                                   (stmt-with-text *collatz*
                                                   "printf(\"%d\\n\", k);")))))
    (is (equal nil
               (block-successor *collatz* (stmt-with-text *collatz*
                                                          "m /= 2;"))))
    (is (equal nil
               (block-successor *collatz* (stmt-with-text *collatz*
                                                          "++k;"))))
    (is (equal nil
               (block-successor *collatz* (stmt-with-text *collatz*
                                                          "return k;"))))))

(deftest block-successor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal nil
               (nest (block-successor *soft*)
                     (stmt-with-text *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equal nil
               (block-successor *soft*
                                (stmt-starting-with-text *soft* "for (j = 0"))))
    (is (equalp (stmt-with-text *soft* "return 0;")
                (block-successor *soft* (stmt-starting-with-text
                                         *soft* "for (i = 0"))))))

(deftest block-successor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal nil
               (nest (block-successor *soft*)
                     (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);"))))
    (is (equalp (stmt-starting-with-text *soft* "default:")
                (nest (block-successor *soft*)
                      (stmt-starting-with-text *soft* "case 2:"))))))

(deftest block-predeccessor-collatz-test ()
  (with-fixture collatz-clang
    (is (equal nil
               (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                             "int k = 0;"))))
    (is (eq :WhileStmt
            (nest (ast-class)
                  (block-predeccessor *collatz*
                                      (stmt-with-text *collatz*
                                                      "printf(\"%d\\n\", k);")))))
    (is (equal nil
               (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                             "m /= 2;"))))
    (is (eq :IfStmt
            (nest (ast-class)
                  (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                                "++k;")))))
    (is (eq :CallExpr
            (nest (ast-class)
                  (block-predeccessor *collatz* (stmt-with-text *collatz*
                                                                "return k;")))))))

(deftest block-predeccessor-no-compound-stmt-test ()
  (with-fixture crossover-no-compound-stmt-clang
    (is (equal nil
               (nest (block-predeccessor *soft*)
                     (stmt-with-text *soft* "printf(\"%d\\n\", i+j);"))))
    (is (equal nil
               (block-successor *soft*
                                (stmt-starting-with-text *soft* "for (j = 0"))))
    (is (equalp (stmt-with-text *soft* "int j;")
                (block-predeccessor *soft* (stmt-starting-with-text
                                            *soft* "for (i = 0"))))))

(deftest block-predeccessor-switch-stmt-test ()
  (with-fixture crossover-switch-stmt-clang
    (is (equal nil
               (nest (block-predeccessor *soft*)
                     (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);"))))
    (is (equalp (stmt-starting-with-text *soft* "case 1:")
                (nest (block-predeccessor *soft*)
                      (stmt-starting-with-text *soft* "case 2:"))))))

(deftest (crossover-2pt-outward-fib-test :long-running) ()
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *fib*)
                                         (copy *fib*)
                                         (stmt-with-text *fib* "x = x + y;")
                                         (stmt-with-text *fib* "return x;")
                                         (stmt-with-text *fib* "x = x + y;")
                                         (stmt-with-text *fib* "return x;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "x = x + y;")
                          (stmt-with-text *fib* "return x;"))
                    effective-a-pts)))))
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *fib*)
           (copy *fib*)
           (stmt-with-text *fib* "int t = x;")
           (nest (first)
                 (remove-if-not [{eq :WhileStmt}
                                 #'ast-class])
                 (stmt-asts *fib*))
           (stmt-with-text *fib* "int t = x;")
           (nest (first)
                 (remove-if-not [{eq :WhileStmt}
                                 #'ast-class])
                 (stmt-asts *fib*)))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int t = x;")
                          (nest (first)
                                (remove-if-not [{eq :WhileStmt}
                                                #'ast-class])
                                (stmt-asts *fib*)))
                    effective-a-pts))))))

(deftest (crossover-2pt-outward-collatz-test :long-running) ()
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "m /= 2;")
                                         (stmt-with-text *collatz* "++k;")
                                         (stmt-with-text *collatz* "m /= 2;")
                                         (stmt-with-text *collatz* "++k;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (equalp (cons (stmt-with-text *collatz* "m /= 2;")
                          (stmt-with-text *collatz* "++k;"))
                    effective-a-pts)))))
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "++k;")
                                         (stmt-with-text *collatz* "return k;")
                                         (stmt-with-text *collatz* "++k;")
                                         (stmt-with-text *collatz* "return k;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "++k;")
                          (stmt-with-text *collatz* "return k;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-outward-no-compound-stmt-test :long-running) ()
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
           (stmt-with-text *soft* "return 0;")
           (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-starting-with-text *soft* "for (j = 0")
           (stmt-with-text *soft* "return 0;")
           (stmt-starting-with-text *soft* "for (j = 0")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-starting-with-text *soft* "for (j = 0")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-outward-switch-stmt-test :long-running) ()
  (with-fixture crossover-switch-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
           (stmt-with-text *soft* "return 0;")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-inward-fib-test :long-running) ()
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *fib*)
                                         (copy *fib*)
                                         (stmt-with-text *fib* "int x = 0;")
                                         (stmt-with-text *fib* "return x;")
                                         (stmt-with-text *fib* "int x = 0;")
                                         (stmt-with-text *fib* "return x;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int x = 0;")
                          (stmt-with-text *fib* "return x;"))
                    effective-a-pts)))))
  (with-fixture fib-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *fib*)
                                         (copy *fib*)
                                         (stmt-with-text *fib* "int x = 0;")
                                         (stmt-with-text *fib* "x = x + y;")
                                         (stmt-with-text *fib* "int x = 0;")
                                         (stmt-with-text *fib* "x = x + y;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *fib*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *fib* "int x = 0;")
                          (stmt-with-text *fib* "x = x + y;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-inward-collatz-test :long-running) ()
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz* "return k;")
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz* "return k;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "int k = 0;")
                          (stmt-with-text *collatz* "return k;"))
                    effective-a-pts)))))
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz*
                                                         "m = 3*m + 1;")
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz*
                                                         "m = 3*m + 1;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "int k = 0;")
                          (stmt-with-text *collatz* "m = 3*m + 1;"))
                    effective-a-pts)))))
  (with-fixture collatz-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover (copy *collatz*)
                                         (copy *collatz*)
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz* "++k;")
                                         (stmt-with-text *collatz* "int k = 0;")
                                         (stmt-with-text *collatz* "++k;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *collatz*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *collatz* "int k = 0;")
                          (stmt-with-text *collatz* "++k;"))
                    effective-a-pts))))))

(deftest (crossover-2pt-inward-no-compound-stmt-test :long-running) ()
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "int i;")
           (stmt-with-text *soft* "return 0;")
           (stmt-with-text *soft* "int i;")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i;")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "int i;")
           (stmt-with-text *soft* "printf(\"%d\\n\", i+j);")
           (stmt-with-text *soft* "int i;")
           (stmt-with-text *soft* "printf(\"%d\\n\", i+j);"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i;")
                          (stmt-with-text *soft* "printf(\"%d\\n\", i+j);"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "int i;")
           (stmt-starting-with-text *soft* "for (i = 0")
           (stmt-with-text *soft* "int i;")
           (stmt-starting-with-text *soft* "for (i = 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i;")
                          (stmt-starting-with-text *soft* "for (i = 0"))
                    effective-a-pts)))))
  (with-fixture crossover-no-compound-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "int i;")
           (stmt-starting-with-text *soft* "for (j = 0")
           (stmt-with-text *soft* "int i;")
           (stmt-starting-with-text *soft* "for (j = 0"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "int i;")
                          (stmt-starting-with-text *soft* "for (j = 0"))
                    effective-a-pts))))))

(deftest (crossover-2pt-inward-switch-stmt-test :long-running) ()
  (with-fixture crossover-switch-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc * argc);")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc + argc);")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc * argc);"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft*
                                          "printf(\"%d\\n\", argc + argc);")
                          (stmt-with-text *soft*
                                          "printf(\"%d\\n\", argc * argc);"))
                    effective-a-pts)))))
  (with-fixture crossover-switch-stmt-clang
    (let ((*matching-free-var-retains-name-bias* 1.0))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover
           (copy *soft*)
           (copy *soft*)
           (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
           (stmt-with-text *soft* "return 0;")
           (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
           (stmt-with-text *soft* "return 0;"))
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (stmt-asts *soft*))
               (length (stmt-asts variant))))
        (is (equalp (cons (stmt-with-text *soft* "printf(\"%d\\n\", argc);")
                          (stmt-with-text *soft* "return 0;"))
                    effective-a-pts))))))

(deftest (basic-2pt-crossover-works :long-running) ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int d;"))
           (a-stmt2 (stmt-with-text *scopes* "d = 5;"))
           (b-stmt1 (stmt-with-text *scopes* "int e;"))
           (b-stmt2 (stmt-with-text *scopes* "c = 10;"))
           (target-a-pts (cons a-stmt1 a-stmt2)))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (phenome-p variant))
        (is (equalp effective-a-pts target-a-pts))))))

(deftest (crossover-can-match-nesting :long-running) ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "--d;"))
           (a-stmt2 (stmt-with-text *scopes* "int e;"))
           (b-stmt1 (stmt-with-text *scopes* "c = 6;"))
           (b-stmt2 (stmt-with-text *scopes* "e = 8;"))
           (target-a-pts
            (cons (stmt-starting-with-text *scopes* "while (d > 0)")
                  a-stmt2)))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable variant a-pts b-pts))
        (is ok)
        (is (equalp effective-a-pts target-a-pts)))))
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "--d;"))
           (a-stmt2 (stmt-with-text *scopes* "int e;"))
           (b-stmt1 (stmt-with-text *scopes* "a = 13;"))
           (b-stmt2 (stmt-with-text *scopes* "c = 15;"))
           (target-a-pts
            (cons (stmt-starting-with-text *scopes* "for (b = 2;")
                  (stmt-starting-with-text *scopes* "if (b == 7)"))))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable variant a-pts b-pts))
        (is ok)
        (is (equalp effective-a-pts target-a-pts))))))

(deftest (crossover-can-rebind-text :long-running) ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int b;"))
           (a-stmt2 (stmt-with-text *scopes* "int c;"))
           (b-stmt1 (stmt-with-text *scopes* "a = 13;"))
           (b-stmt2 (stmt-with-text *scopes* "a = 13;")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant)))))
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int b;"))
           (a-stmt2 (stmt-with-text *scopes* "int c;"))
           (b-stmt1 (stmt-with-text *scopes* "d = 5;"))
           (b-stmt2 (stmt-with-text *scopes* "--d;")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))))))

(deftest (crossover-entire-text-of-a-function :long-running) ()
  ;; Entire text of a function
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int a;"))
           (a-stmt2 (stmt-with-text *scopes* "return a + b + c;"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (is (with-retries (100)
            ;; NOTE: Without the retries recontectualization can fail
            ;;       stochastically, not all possible rebinding
            ;;       variables work.
            ;;
            ;; TODO: Refine recontextualization with type information
            ;;       so that this *always* works.
            (for (values variant a-pts b-pts ok effective-a-pts) =
                 (intraprocedural-2pt-crossover *scopes* *scopes*
                                                a-stmt1 a-stmt2
                                                b-stmt1 b-stmt2))
            (declare (ignorable a-pts b-pts effective-a-pts))
            (when (and ok
                       (phenome-p variant)
                       (= (length (asts *scopes*))
                          (length (asts variant))))
              (return t)))
          "Able to crossover entire text of a function with 100 tries."))))

(deftest (crossover-a-single-statement-the-first :long-running) ()
  ;; A single statement (the first one)
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int a;"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant))))))))

(deftest (crossover-a-single-statement-the-last :long-running) ()
  ;; A single statement (the last one)
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "return a + b + c;"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant))))))))

(deftest (crossover-a-single-statement-complex :long-running) ()
  ;; A single complex statement
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant))))))))

(deftest (crossover-a-statement-and-descendants :long-running) ()
  ;; A statement and one of its descendants
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (a-stmt2 (stmt-with-text *scopes* "c = 4;"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant))))))))

(deftest (crossover-a-statement-and-one-ancestor :long-running) ()
  ;; A statement and one of its ancestors
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "c = 4;"))
           (a-stmt2 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (is (with-retries (100)
            ;; NOTE: Without the retries recontectualization can fail
            ;;       stochastically, not all possible rebinding
            ;;       variables work.
            ;;
            ;; TODO: Refine recontextualization with type information
            ;;       so that this *always* works.
            (for (values variant a-pts b-pts ok effective-a-pts) =
                 (intraprocedural-2pt-crossover *scopes* *scopes*
                                                a-stmt1 a-stmt2
                                                b-stmt1 b-stmt2))
            (declare (ignorable a-pts b-pts effective-a-pts))
            (when (and ok
                       (phenome-p variant)
                       (= (length (asts *scopes*))
                          (length (asts variant))))
              (return t)))
          "Able to crossover a statement and a ancestor with 100 tries."))))

(deftest (crossover-a-statement-with-multiple-statements :long-running) ()
  ;; Replace a single statement with multiple statements
  (with-fixture scopes-clang
    (let ((a-stmt (stmt-with-text *scopes* "int b;"))
          (b-stmt1 (stmt-with-text *scopes* "c = 10;"))
          (b-stmt2 (stmt-with-text *scopes* "g = 12;")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt a-stmt
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (> (length (asts variant))
               (length (asts *scopes*))))
        ;; a is the only var in scope so all these assignments should
        ;; be rebound.
        (is (stmt-with-text variant "a = 10;" :no-error t))
        (is (stmt-with-text variant "a = 11;" :no-error t))
        (is (stmt-with-text variant "a = 12;" :no-error t))
        (is (not (stmt-with-text variant "int b;" :no-error t)))))))

(deftest (crossover-a-multiple-statements-with-a-single-statement
          :long-running) ()
  ;; Replace multiple statements with a single statement
  (with-fixture scopes-clang
    (let ((a-stmt1 (stmt-with-text *scopes* "int b;"))
          (a-stmt2 (stmt-with-text *scopes* "b = 1;"))
          (b-stmt (stmt-with-text *scopes* "e = 8;")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt b-stmt)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (phenome-p variant))
        (is (< (length (asts variant))
               (length (asts *scopes*))))
        ;; a is the only var in scope so this assignment should
        ;; be rebound.
        (is (stmt-with-text variant "a = 8;" :no-error t))
        (is (not (stmt-with-text variant "int b;" :no-error t)))
        (is (not (stmt-with-text variant "b = 1;" :no-error t)))))))

(defixture intraprocedural-2pt-crossover-bug-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang :compiler "clang"
                                   :flags '("-g" "-O0"))
                    (clang-crossover-dir
                     "intraprocedural-2pt-crossover-bug.c"))))
  (:teardown
   (setf *soft* nil)))

(deftest (intraprocedural-2pt-crossover-does-not-crash :long-running) ()
  (with-fixture intraprocedural-2pt-crossover-bug-clang
    (let ((variant (intraprocedural-2pt-crossover
                    (copy *soft*)
                    (copy *soft*)
                    (stmt-with-text *soft* "printf(\"%d\\n\", argc + 1 < NUM);")
                    (stmt-with-text *soft* "argc--;")
                    (stmt-with-text *soft* "printf(\"%d\\n\", argc + 1 < NUM);")
                    (stmt-with-text *soft* "argc--;"))))
      (is (string/= (genome-string variant) "")))))
