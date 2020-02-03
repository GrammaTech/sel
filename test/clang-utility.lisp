;;;; clang-utility.lisp --- Clang utility methods.
(defpackage :software-evolution-library/test/clang-utility
  (:nicknames :sel/test/clang-utility)
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
   :defmethod :defgeneric)
  (:export :clang-utility))
(in-package :software-evolution-library/test/clang-utility)
(in-readtable :curry-compose-reader-macros)
(defsuite clang-utility)

(defvar *hello-world* nil "Holds the hello world software object.")
(defvar *soft* nil "Software used in tests.")

(deftest asts-populated-on-creation ()
  (with-fixture hello-world-clang
    (is (= 10 (length (asts *hello-world*))))))

(deftest parent-ast-p-true-test()
  (with-fixture hello-world-clang
    (is (parent-ast-p *hello-world*
                      (stmt-with-text *hello-world* "return 0;")
                      (stmt-with-text *hello-world* "0")))))

(deftest parent-ast-p-false-test()
  (with-fixture hello-world-clang
    (is (not (parent-ast-p *hello-world*
                           (stmt-with-text *hello-world* "0")
                           (stmt-with-text *hello-world* "return 0;"))))))

(deftest (tidy-a-clang-software-object :long-running) ()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*)))
      (clang-tidy variant)
      (is (= (size variant)
             (size *hello-world*))))))

(deftest (tidy-adds-braces :long-running) ()
  (with-fixture tidy-adds-braces-clang
    (let ((variant (copy *soft*)))
      (clang-tidy variant)
      (is (= 2 (->> (stmt-asts variant)
                    (remove-if-not [{eq :CompoundStmt} #'ast-class])
                    (length)))))))

(deftest (format-a-clang-software-object :long-running) ()
  (flet ((run (obj)
           (with-temp-file (bin)
             (phenome obj :bin bin)
             (shell bin))))
    (with-fixture hello-world-clang
      (multiple-value-bind (obj errno) (clang-format (copy *hello-world*))
        (is (zerop errno))
        (is (string= (run *hello-world*) (run obj)))))))

(deftest find-var-type-returns-correct-type ()
  (flet
      ((get-var-type (var stmt-text)
         (some->> (stmt-with-text *soft* stmt-text)
                  (get-vars-in-scope *soft*)
                  (find-if [{name= var} {aget :name}])
                  (find-var-type *soft*))))
    (with-fixture type-of-var-clang
      (let ((var-type1 (get-var-type "a" "return 0;"))
            (var-type2 (get-var-type "a" "return 1;"))
            (var-type3 (get-var-type "a" "return 2;"))
            (var-type4 (get-var-type "a" "int a[N][N];"))
            (var-type5 (get-var-type "b" "return 2;")))
        (is (null var-type4))
        (is (equal (if *new-clang?* "[10]" "[10][10]") (type-array var-type1)))
        (is (equal ""         (type-array var-type2)))
        (is (equal "[10][10]" (type-array var-type3)))
        (is (equal ""         (type-array var-type5)))
        (is (equal nil        (type-pointer var-type1)))
        (is (equal t          (type-pointer var-type2)))
        (is (equal nil        (type-pointer var-type3)))
        (is (equal t          (type-pointer var-type5)))
        (is (equal (if *new-clang?* "int (*)" "int") (type-name var-type1)))
        (is (equal "int"      (type-name var-type2)))
        (is (equal "int"      (type-name var-type3)))
        (is (equal "int*"     (remove #\Space (type-name var-type5))))))))

(deftest typedef-type-returns-correct-type ()
  (with-fixture typedef-type-clang
    (let ((type1 (some->> (stmt-with-text *soft* "gint a;")
                          (get-ast-types *soft*)
                          (car)
                          (find-type *soft*)
                          (typedef-type *soft*)))
          (type2 (some->> (stmt-with-text *soft* "gchar *b;")
                          (get-ast-types *soft*)
                          (car)
                          (find-type *soft*)
                          (typedef-type *soft*)))
          (type3 (some->> (stmt-with-text *soft* "gcharp p;")
                          (get-ast-types *soft*)
                          (car)
                          (find-type *soft*)
                          (typedef-type *soft*))))
      (is (equal "int"  (type-name type1)))
      (is (equal nil    (type-pointer type1)))
      (is (equal "char" (type-name type2)))
      (is (equal t      (type-pointer type2)))
      (is (equal "char" (type-name type3)))
      (is (equal t      (type-pointer type3))))))

(deftest apply-replacements-test ()
  (is (string= "Hello, world!"
               (apply-replacements nil
                                   "Hello, world!")))
  (is (string= "Hello, world!"
               (apply-replacements `((nil . nil))
                                   "Hello, world!")))
  (is (string= "Hello, world!"
               (apply-replacements `((nil . "Goodbye"))
                                   "Hello, world!")))
  (is (string= "Hello, world!"
               (apply-replacements `(("Hello" . nil))
                                   "Hello, world!")))
  (is (string= "Goodbye, world!"
               (apply-replacements `(("Hello" . "Goodbye"))
                                   "Hello, world!")))
  (is (string= "Goodbye, earth!"
               (apply-replacements `(("Hello" . "Goodbye") ("world" . "earth"))
                                   "Hello, world!"))))

(deftest rebind-vars-in-macro-test ()
  (with-fixture assert-clang
    (labels ((peel-banana-tree (tree)
               (if *new-clang?*
                   (cond ((stringp tree)
                          (peel-bananas tree))
                         ((consp tree)
                          (let ((car (peel-banana-tree (car tree)))
                                (cdr (peel-banana-tree (cdr tree))))
                            (if (and (eql car (car tree)) (eql cdr (cdr tree)))
                                tree
                                (cons car cdr))))
                         (t tree))
                   tree)))
      (let* ((copy (copy *soft*))
             (stmt (stmt-with-text copy "assert(argc > 0);")))
        (is (equalp (peel-banana-tree "assert((|someVal|) > 0);")
                    (->> (rebind-vars stmt
                                      (peel-banana-tree
                                       '(("(|argc|)" "(|someVal|)")))
                                      nil)
                         (source-text)))
            "rebind-vars did not rebind a variable within a macro")))))


;;;; Java representation.
(defun java-mutate-available-p ()
  (zerop (nth-value 2 (shell "which java-mutator"))))
