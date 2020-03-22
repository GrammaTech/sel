;;;; clang-utility.lisp --- Clang utility methods.
(defpackage :software-evolution-library/test/clang-utility
  (:nicknames :sel/test/clang-utility)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/components/formatting
   :software-evolution-library/components/fodder-database
   :software-evolution-library/components/json-fodder-database)
  (:import-from :arrow-macros :some->>) ; FIXME: Remove.
  (:export :test-clang-utility))
(in-package :software-evolution-library/test/clang-utility)
(in-readtable :curry-compose-reader-macros)
(defsuite test-clang-utility "Clang representation." (clang-available-p))

(define-constant +typedef-type-dir+
    (append +etc-dir+ (list "typedef-type"))
  :test #'equalp
  :documentation "Path to the typedef-type program example")

(defun typedef-type-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +typedef-type-dir+))

(defixture tidy-adds-braces-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g"))
                    (clang-tidy-dir "tidy-adds-braces.c"))))
  (:teardown
   (setf *soft* nil)))

(define-constant +type-of-var-dir+
    (append +etc-dir+ (list "type-of-var"))
  :test #'equalp
  :documentation "Location of the type-of-var example dir")

(defun type-of-var-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +type-of-var-dir+))

(defixture type-of-var-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g"))
                    (type-of-var-dir "type-of-var.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture typedef-type-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang)
                    (typedef-type-dir "typedef-type.c"))))
  (:teardown
   (setf *soft* nil)))

(define-constant +assert-dir+ (append +etc-dir+ (list "assert"))
  :test #'equalp
  :documentation "Path to assert example.")

(defun assert-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +assert-dir+))

(defixture assert-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang)
                    (assert-dir "assert.c"))))
  (:teardown
   (setf *soft* nil)))

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
      (is (= 2 (nest (length)
                     (remove-if-not [{eq :CompoundStmt} #'ast-class])
                     (stmt-asts variant)))))))

(deftest (format-a-clang-software-object :long-running) ()
  (flet ((run (obj)
           (with-temporary-file (:pathname bin)
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
        (is (equal "[10]"     (type-array var-type1)))
        (is (equal ""         (type-array var-type2)))
        (is (equal "[10][10]" (type-array var-type3)))
        (is (equal ""         (type-array var-type5)))
        (is (equal nil        (type-pointer var-type1)))
        (is (equal t          (type-pointer var-type2)))
        (is (equal nil        (type-pointer var-type3)))
        (is (equal t          (type-pointer var-type5)))
        (is (equal "int (*)"  (type-name var-type1)))
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
    (let* ((copy (copy *soft*))
           (stmt (stmt-with-text copy "assert(argc > 0);")))
      (is (equalp "assert(someVal > 0);"
                  (source-text (rebind-vars stmt
                                            '(("argc" "someVal"))
                                            nil)))
          "rebind-vars did not rebind a variable within a macro"))))
