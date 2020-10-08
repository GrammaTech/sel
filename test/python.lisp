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

(define-constant +scopes-dir+ (append +python-dir+ (list "scopes"))
  :test #'equalp
  :documentation "Path to directory holding scopes files.")

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

(defmacro with-scopes-file ((filename software-var genome-var)
                              &body body)
  `(let* ((,software-var (from-file
                          (make-instance 'python)
                          (make-pathname :name ,filename
                                         :type "py"
                                         :directory +scopes-dir+)))
          (,genome-var (genome ,software-var)))
     ,@body))

(defmacro with-util-file ((filename software-var genome-var)
                              &body body)
  `(let* ((,software-var (from-file
                          (make-instance 'python)
                          (make-pathname :name ,filename
                                         :type "py"
                                         :directory +python-utility-dir+)))
          (,genome-var (genome ,software-var)))
     ,@body))


(defun scope-contains-string-p (scope string)
  "Return the variable alist associated with STRING if it exists in SCOPE."
  (find-if [{equalp string} {aget :name}] scope))

(defun scopes-contains-string-p (scopes string)
  "Return the variable alist associated with STRING if it exists in SCOPES."
  (mappend {scope-contains-string-p _ string} scopes))


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

;; Currently fails with indentation due to the
;; comment.
#+nil
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

;; Currently fails with indentation due to the
;; comments.
#+nil
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

(deftest python-can-cut-last-child ()
  (with-fixture fib-python
    (nest (apply-mutation *soft*)
          (make-instance 'parseable-cut :targets)
          (list (cons :stmt1 (stmt-with-text *soft* "return b"))))
    (is (null (stmt-with-text *soft* "return b" :no-error t))
        "'return b' was not removed from the program.")))

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

(deftest python-scopes-1 ()
  "scopes gets the initial binding of a global statement."
  (with-util-file ("nested-global" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'py-return} genome)))
           (global-alist (scopes-contains-string-p scopes "a"))
           (expected-assign (find-if {typep _ 'py-assign} genome)))
      (is (eq (aget :decl global-alist) expected-assign)
          "~A did not contain the expected variable 'a' assignment, ~a."
          global-alist expected-assign))))

(deftest python-scopes-2 ()
  "scopes gets the initial binding of a local statement."
  (with-util-file ("local-shadow" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'py-call} genome)))
           (nonlocal-alist (scopes-contains-string-p scopes "a"))
           (expected-assign (cadr (collect-if {typep _ 'py-assign} genome))))
      (is (eq (aget :decl nonlocal-alist) expected-assign)
          "~A did not contain the expected variable 'a' assignment, ~a."
          nonlocal-alist expected-assign))))

(deftest python-scopes-4 ()
  "scopes gets the bindng from a function definition."
  (with-util-file ("global" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'py-return} genome)))
           (def-alist (scopes-contains-string-p scopes "test"))
           (expected-def (find-if {typep _ 'py-function-def} genome)))
      (is (eq (aget :decl def-alist) expected-def)
          "~A did not contain the expected variable 'test' assignment, ~a."
          def-alist expected-def))))

(deftest python-scopes-5 ()
  "scopes gets the bindng from an import."
  (with-util-file ("import" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'py-return} genome)))
           (import-alist (scopes-contains-string-p scopes "a"))
           (expected-import (find-if {typep _ 'py-import} genome)))
      (is (eq (aget :decl import-alist) expected-import)
          "~A did not contain the expected variable 'a' assignment, ~a."
          import-alist expected-import))))

(deftest python-scopes-6 ()
  "scopes gets the bindng from function parameters."
  (with-util-file ("parameter" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'py-pass} genome)))
           (parameter-alist (scopes-contains-string-p scopes "a"))
           (expected-parameter (find-if {typep _ 'py-arg} genome)))
      (is (eq (aget :decl parameter-alist) expected-parameter)
          "~A did not contain the expected variable 'a' assignment, ~a."
          parameter-alist expected-parameter))))

(deftest python-scopes-7 ()
  "scopes gets the bindngs local to a namespace."
  (with-util-file ("local-2" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'py-return} genome)))
           (local-alist (scopes-contains-string-p scopes "a"))
           (expected-local (find-if {typep _ 'py-assign} genome)))
      (is (eq (aget :decl local-alist) expected-local)
          "~A did not contain the expected variable 'a' assignment, ~a."
          local-alist expected-local))))

(deftest python-scopes-8 ()
  "scopes gets the bindings from multiple assignment statement."
  (with-scopes-file ("assign" soft genome)
    (is (equal `((((:decl  . ,(nest (stmt-with-text soft)
                                    (format nil "a, b = 0, 1~%")))
                   (:name  . "a")
                   (:scope . ,genome))
                  ((:decl  . ,(nest (stmt-with-text soft)
                                    (format nil "a, b = 0, 1~%")))
                   (:name  . "b")
                   (:scope . ,genome))))
               (nest (scopes soft)
                     (stmt-with-text soft)
                     (format nil "b = 1~%"))))))

(deftest python-scopes-9 ()
  "scopes gets the bindings from annotated assignment statements."
  (with-scopes-file ("ann-assign" soft genome)
    (is (equal `((((:decl  . ,(nest (stmt-with-text soft)
                                    (format nil "a:int = 3~%")))
                   (:name  . "a")
                   (:scope . ,genome))
                  ((:decl  . ,(nest (stmt-with-text soft)
                                    (format nil "b:int = 2~%")))
                   (:name  . "b")
                   (:scope . ,genome))))
               (nest (scopes soft)
                     (stmt-with-text soft)
                     (format nil "b = 1~%"))))))

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
            ;; Currently doesn't work with indentation.
            ;;(#P"if.py" py-if)
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
