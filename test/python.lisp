;;;; python.lisp --- Python representation.
(defpackage :software-evolution-library/test/python
  (:nicknames :sel/test/python)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/python
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-python))
(in-package :software-evolution-library/test/python)
(in-readtable :curry-compose-reader-macros)
(defsuite test-python "Python representation." (python3.8-available-p))

(defixture hello-world-python
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"hello-world/hello-world.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture fib-python
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"fib/fib.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture empty-python
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"empty/empty.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture multibyte-python1
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"unicode/unicode1.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture multibyte-python2
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"unicode/unicode2.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture multibyte-python3
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"unicode/unicode3.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture dos-python
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"dos/fib.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture formatting-python
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"formatting/formatting.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture rebind-python
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"rebind-vars/rebind-vars.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture unbound-python
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"unbound-vals-and-funs/unbound.py"))))
  (:teardown
   (setf *soft* nil)))

(defixture type-comments-python
  (:setup
   (setf *soft*
         (from-file (make-instance 'python)
                    (python-dir #P"type-comments/type-comments.py"))))
  (:teardown
   (setf *soft* nil)))

(deftest simply-able-to-load-a-python-software-object ()
  (with-fixture hello-world-python
    (is (not (null *soft*)))))

(deftest can-parse-a-python-software-object ()
  (with-fixture hello-world-python
    (is (= 4 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*))))
  (with-fixture fib-python
    (is (= 29 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest can-handle-empty-file-python ()
  (with-fixture empty-python
    (is (= 0 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest can-handle-multibyte-characters-python ()
  (with-fixture multibyte-python1
    (is (= 10 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*))))
  (with-fixture multibyte-python2
    (is (= 8 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*))))
  (with-fixture multibyte-python3
    (is (= 3 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest can-handle-dos-format-python ()
  (with-fixture dos-python
    (is (= 29 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest python-stmt-ast-has-newline ()
  (with-fixture fib-python
    (is (typep (stmt-with-text *soft* (concatenate 'string "return b"
                                                           (list #\Newline)))
               'py-return)))
  (with-fixture dos-python
    (is (typep (stmt-with-text *soft* (concatenate 'string "return b"
                                                           (list #\Linefeed
                                                                 #\Newline)))
               'py-return))))

(deftest python-ast-source-ranges ()
  (with-fixture hello-world-python
    (is (equalp (mapcar [#'range-to-list #'cdr] (ast-source-ranges *soft*))
                '(((1 . 1) (2 . 1))
                  ((1 . 1) (2 . 1))
                  ((1 . 1) (1 . 22))
                  ((1 . 1) (1 . 6))
                  ((1 . 7) (1 . 21)))))))

(deftest can-handle-type-comments-python ()
  (with-fixture type-comments-python
    (is (= 8 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))
    (is (nest (stmt-with-text *soft*)
              (format nil "x = 12 # type: int~%")))
    (is (equal "int"
               (nest (aget :type-comment)
                     (ast-annotations)
                     (stmt-with-text *soft*)
                     (format nil "x = 12 # type: int~%"))))))

(deftest python-convert-source-snippet-works ()
  (let ((ast (convert 'python-ast "j = 0")))
    (is (equal 4 (size ast)))
    (is (equal "j = 0" (source-text ast)))
    (is (find-if {typep _ 'py-assign} ast))))

(deftest (can-format-a-python-software-object :long-running) ()
  (with-fixture formatting-python
    (when (which "yapf")
      (is (not (string= (genome-string (copy *soft*))
                        (genome-string (yapf (copy *soft*))))))
      (is (not (string= (genome-string (copy *soft*))
                        (genome-string (format-genome (copy *soft*))))))
      (is (string= (genome-string (yapf (copy *soft*)))
                   (genome-string (format-genome (copy *soft*))))))))

(deftest python-can-rebind-vars ()
  (with-fixture rebind-python
    (is (string= "b = 0"
                 (nest (trim-whitespace)
                       (source-text)
                       (rebind-vars (stmt-starting-with-text *soft* "a = 0")
                                    (list (list "a" "b"))
                                    nil))))))

(deftest python-get-unbound-vals ()
  (with-fixture unbound-python
    (is (equal `((:name . "i") (:name . "j"))
               (nest (get-unbound-vals *soft*)
                     (stmt-with-text *soft*)
                     (format nil "f(i, j)~%"))))
    (is (equal `((:name . "obj") (:name . "i") (:name . "j"))
               (nest (get-unbound-vals *soft*)
                     (stmt-with-text *soft*)
                     (format nil "obj.function(i, j)~%"))))
    (is (equal `((:name . "x") (:name . "y"))
               (nest (get-unbound-vals *soft*)
                     (stmt-with-text *soft*)
                     (format nil "return x * y~%"))))
    (is (equal `((:name . "__name__") (:name . "obj")
                 (:name . "i") (:name . "j"))
               (nest (get-unbound-vals *soft*)
                     (stmt-starting-with-text *soft*)
                     (format nil "if __name__ == '__main__':~%"))))))

(deftest python-get-unbound-funs ()
  (with-fixture unbound-python
    (is (equal `(("f" nil nil 2))
               (nest (get-unbound-funs *soft*)
                     (stmt-with-text *soft*)
                     (format nil "f(i, j)~%"))))
    (is (equal `(("function" nil nil 2))
               (nest (get-unbound-funs *soft*)
                     (stmt-with-text *soft*)
                     (format nil "obj.function(i, j)~%"))))
    (is (equal `(("Obj" nil nil 0))
               (nest (get-unbound-funs *soft*)
                     (stmt-with-text *soft*)
                     (format nil "obj = Obj()~%"))))
    (is (equal `(("Obj" nil nil 0) ("function" nil nil 2) ("f" nil nil 2))
               (nest (get-unbound-funs *soft*)
                     (stmt-starting-with-text *soft*)
                     (format nil "if __name__ == '__main__':~%"))))))

(deftest python-scopes ()
  (labels ((scopes-test-dir (path)
             (merge-pathnames-as-file
              (make-pathname :directory (append +python-dir+ (list "scopes")))
              path)))
    (let ((global (from-file (make-instance 'python)
                             (scopes-test-dir #P"global.py"))))
      (is (equal `((((:name  . "b")
                     (:decl  . ,(nest (stmt-with-text global)
                                      (format nil "global a, b~%")))
                     (:scope . ,(genome global)))
                    ((:name  . "a")
                     (:decl  . ,(nest (stmt-with-text global)
                                      (format nil "global a, b~%")))
                     (:scope . ,(genome global)))))
                 (nest (scopes global)
                       (stmt-with-text global)
                       (format nil "b = 1~%")))))

    (let ((non-local (from-file (make-instance 'python)
                                (scopes-test-dir #P"non-local.py"))))
      (is (equal `((((:name  . "b")
                     (:decl  . ,(nest (stmt-with-text non-local)
                                      (format nil "nonlocal a, b~%")))
                     (:scope . ,(genome non-local)))
                    ((:name  . "a")
                     (:decl  . ,(nest (stmt-with-text non-local)
                                      (format nil "nonlocal a, b~%")))
                     (:scope . ,(genome non-local)))))
                 (nest (scopes non-local)
                       (stmt-with-text non-local)
                       (format nil "b = 1~%")))))
    (let ((assign (from-file (make-instance 'python)
                             (scopes-test-dir #P"assign.py"))))
      (is (equal `((((:name  . "b")
                     (:decl  . ,(nest (stmt-with-text assign)
                                      (format nil "a, b = 0, 1~%")))
                     (:scope . ,(genome assign)))
                    ((:name  . "a")
                     (:decl  . ,(nest (stmt-with-text assign)
                                      (format nil "a, b = 0, 1~%")))
                     (:scope . ,(genome assign)))))
                 (nest (scopes assign)
                       (stmt-with-text assign)
                       (format nil "b = 1~%")))))

    (let ((ann-assign (from-file (make-instance 'python)
                                 (scopes-test-dir #P"ann-assign.py"))))
      (is (equal `((((:name  . "b")
                     (:decl  . ,(nest (stmt-with-text ann-assign)
                                      (format nil "b:int = 2~%")))
                     (:scope . ,(genome ann-assign)))
                    ((:name  . "a")
                     (:decl  . ,(nest (stmt-with-text ann-assign)
                                      (format nil "a:int = 3~%")))
                     (:scope . ,(genome ann-assign)))))
                 (nest (scopes ann-assign)
                       (stmt-with-text ann-assign)
                       (format nil "b = 1~%")))))

    (let ((function-def (from-file (make-instance 'python)
                                   (scopes-test-dir #P"function-def.py"))))
      (is (equal `((((:name  . "y")
                     (:decl  . ,(nest (stmt-with-text function-def)
                                      (format nil "y = 12~%")))
                     (:scope . ,(stmt-starting-with-text function-def
                                                         "def f(x):")))
                    ((:name  . "x")
                     (:decl  . ,(stmt-with-text function-def "x"))
                     (:scope . ,(stmt-starting-with-text function-def
                                                         "def f(x):"))))
                    nil)
                 (nest (scopes function-def)
                       (stmt-with-text function-def)
                       (format nil "return x~%")))))

    (let ((if-else (from-file (make-instance 'python)
                              (scopes-test-dir #P"if-else.py"))))
      (is (equal `((((:name  . "a")
                     (:decl  . ,(nest (stmt-with-text if-else)
                                      (format nil "a = 2~%")))
                     (:scope . ,(nest (get-parent-ast if-else)
                                      (stmt-with-text if-else)
                                      (format nil "a = 2~%")))))
                   (((:name  . "x")
                     (:decl  . ,(nest (stmt-with-text if-else)
                                      (format nil "x = 1~%")))
                     (:scope . ,(genome if-else)))))
                 (nest (scopes if-else)
                       (stmt-with-text if-else)
                       (format nil "print(a)~%"))))
      (is (equal `((((:name  . "b")
                     (:decl  . ,(nest (stmt-with-text if-else)
                                      (format nil "b = 3~%")))
                     (:scope . ,(nest (get-parent-ast if-else)
                                      (stmt-with-text if-else)
                                      (format nil "b = 3~%")))))
                   (((:name  . "x")
                     (:decl  . ,(nest (stmt-with-text if-else)
                                      (format nil "x = 1~%")))
                     (:scope . ,(genome if-else)))))
                 (nest (scopes if-else)
                       (stmt-with-text if-else)
                       (format nil "print(b)~%"))))
      (is (equal `((((:name  . "c")
                     (:decl  . ,(nest (stmt-with-text if-else)
                                      (format nil "c = 4~%")))
                     (:scope . ,(nest (get-parent-ast if-else)
                                      (stmt-with-text if-else)
                                      (format nil "c = 4~%")))))
                   (((:name  . "x")
                     (:decl  . ,(nest (stmt-with-text if-else)
                                      (format nil "x = 1~%")))
                     (:scope . ,(genome if-else)))))
                 (nest (scopes if-else)
                       (stmt-with-text if-else)
                       (format nil "print(c)~%"))))
      (is (equal `((((:name  . "d")
                     (:decl  . ,(nest (stmt-with-text if-else)
                                      (format nil "d = 5~%")))
                     (:scope . ,(nest (get-parent-ast if-else)
                                      (stmt-with-text if-else)
                                      (format nil "d = 5~%")))))
                   (((:name  . "c")
                     (:decl  . ,(nest (stmt-with-text if-else)
                                      (format nil "c = 4~%")))
                     (:scope . ,(nest (get-parent-ast if-else)
                                      (stmt-with-text if-else)
                                      (format nil "c = 4~%")))))
                   (((:name  . "x")
                     (:decl  . ,(nest (stmt-with-text if-else)
                                      (format nil "x = 1~%")))
                     (:scope . ,(genome if-else)))))
                 (nest (scopes if-else)
                       (stmt-with-text if-else)
                       (format nil "print(d)~%")))))

    (let ((cmplx (from-file (make-instance 'python)
                            (scopes-test-dir #P"complex.py"))))
      (is (equal `((((:name  . "y")
                     (:decl  . ,(nest (stmt-with-text cmplx)
                                      (format nil "y = g ** 2~%")))
                     (:scope . ,(stmt-starting-with-text cmplx "if g > 0:"))))
                   (((:name  . "x")
                     (:decl  . ,(stmt-with-text cmplx "x"))
                     (:scope . ,(stmt-starting-with-text cmplx "def f(x):"))))
                   (((:name  . "g")
                     (:decl  . ,(nest (stmt-with-text cmplx)
                                      (format nil "global g~%")))
                     (:scope . ,(genome cmplx)))))
                 (nest (scopes cmplx)
                       (stmt-with-text cmplx)
                       (format nil "return x~%")))))))

(deftest (python-parsing-test :long-running) ()
  (labels ((parsing-test-dir (path)
             (merge-pathnames-as-file
              (make-pathname :directory (append +python-dir+ (list "parsing")))
              path))
           (parse-test (path &rest ast-types)
             (let ((soft (from-file (make-instance 'python)
                                    (parsing-test-dir path))))
               (is (not (zerop (size soft))))
               (is (equal (genome-string soft)
                          (file-to-string (parsing-test-dir path))))
               (iter (for ast-type in ast-types)
                     (is (find-if {typep _ ast-type} (genome soft)))))))
    (mapc {apply #'parse-test}
          '((#P"function-def.py" py-function-def)
            (#P"async-function-def.py" py-async-function-def)
            (#P"class-def.py" py-class-def)
            (#P"return.py" py-return)
            (#P"delete.py" py-delete)
            (#P"assign.py" py-assign)
            (#P"aug-assign.py" py-aug-assign)
            (#P"ann-assign.py" py-ann-assign)
            (#P"for.py" py-for)
            (#P"async-for.py" py-async-for)
            (#P"while.py" py-while)
            (#P"if.py" py-if)
            (#P"with.py" py-with)
            (#P"async-with.py" py-async-with)
            (#P"raise.py" py-raise)
            (#P"try.py" py-try)
            (#P"assert.py" py-assert)
            (#P"import.py" py-import)
            (#P"import-from.py" py-import-from)
            (#P"global.py" py-global)
            (#P"non-local.py" py-nonlocal)
            (#P"pass.py" py-pass)
            (#P"break.py" py-break)
            (#P"continue.py" py-continue)
            (#P"bool-op.py" py-bool-op)
            (#P"named-expr.py" py-named-expr)
            (#P"bin-op.py" py-bin-op)
            (#P"unary-op.py" py-unary-op)
            (#P"lambda.py" py-lambda)
            (#P"if-exp.py" py-if-exp)
            (#P"dict.py" py-dict)
            (#P"set.py" py-set)
            (#P"list-comp.py" py-list-comp)
            (#P"set-comp.py" py-set-comp)
            (#P"dict-comp.py" py-dict-comp)
            (#P"generator-exp.py" py-generator-exp)
            (#P"await.py" py-await)
            (#P"yield.py" py-yield)
            (#P"yield-from.py" py-yield-from)
            (#P"compare.py" py-compare)
            (#P"call.py" py-call)
            (#P"joined-str.py" py-joined-str)
            (#P"attribute.py" py-attribute)
            (#P"starred.py" py-starred)
            (#P"list.py" py-list)
            (#P"tuple.py" py-tuple)
            (#P"slice.py" py-subscript py-slice py-ext-slice py-index)))))
