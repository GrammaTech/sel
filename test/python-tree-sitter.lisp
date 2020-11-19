;;;; python-tree-sitter.lisp --- Python tree-sitter representation.
(defpackage :software-evolution-library/test/python-tree-sitter
  (:nicknames :sel/test/python-tree-sitter :sel/test/py-ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-python-tree-sitter))
(in-package :software-evolution-library/test/python-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-python-tree-sitter "Python tree-sitter representation."
  (python-tree-sitter-available-p))


;;; Utility
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


;;; Tests
(deftest simply-able-to-load-a-python-software-object ()
  (with-fixture hello-world-python
    (is (not (null *soft*)))))

(deftest can-parse-a-python-software-object ()
  (with-fixture hello-world-python
    (is (= 5 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*))))
  (with-fixture fib-python
    (is (= 44 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest (can-format-a-python-software-object :long-running) ()
  (with-fixture formatting-python
    (when (which "yapf")
      (is (not (string= (genome-string (copy *soft*))
                        (genome-string (yapf (copy *soft*))))))
      (is (not (string= (genome-string (copy *soft*))
                        (genome-string (format-genome (copy *soft*))))))
      (is (string= (genome-string (yapf (copy *soft*)))
                   (genome-string (format-genome (copy *soft*))))))))

(deftest can-handle-empty-file-python ()
  (with-fixture empty-python
    (is (= 0 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest (python-tree-sitter-parsing-test :long-running) ()
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
               (is (not (find-if {typep _ 'python-error} (genome soft))))
               (iter (for ast-type in ast-types)
                     (is (find-if {typep _ ast-type} (genome soft)))))))
    (mapc {apply #'parse-test}
          '(#+nil
            ;; NOTE: positional-only parameters appear to be
            ;;       unsupported by tree-sitter currently.
            (#P"function-def.py" python-function-definition)
            ;; NOTE: async is in interleaved text.
            (#P"async-function-def.py" python-function-definition)
            (#P"class-def.py" python-class-definition)
            (#P"return.py" python-return-statement)
            (#P"delete.py" python-delete-statement)
            (#P"assign.py" python-assignment)
            (#P"aug-assign.py" python-augmented-assignment)
            ;; NOTE: stores annotation in the type slot.
            (#P"ann-assign.py" python-type)
            (#P"for.py" python-for-statement)
            ;; NOTE: async is in interleaved text.
            (#P"async-for.py" python-for-statement)
            (#P"while.py" python-while-statement)
            (#P"if.py" python-if-statement)
            (#P"with.py" python-with-statement)
            ;; NOTE: async is in interleaved text.
            (#P"async-with.py" python-with-statement)
            (#P"raise.py" python-raise-statement)
            (#P"try.py" python-try-statement)
            (#P"assert.py" python-assert-statement)
            (#P"import.py" python-import-statement)
            (#P"import-from.py" python-import-from-statement)
            (#P"global.py" python-global-statement)
            (#P"non-local.py" python-nonlocal-statement)
            (#P"pass.py" python-pass-statement)
            (#P"break.py" python-break-statement)
            (#P"continue.py" python-continue-statement)
            (#P"bool-op.py" python-boolean-operator)
            (#P"named-expr.py" python-named-expression)
            (#P"bin-op.py" python-binary-operator)
            (#P"unary-op.py" python-unary-operator)
            (#P"lambda.py" python-lambda)
            (#P"if-exp.py" python-conditional-expression)
            (#P"dict.py" python-dictionary)
            (#P"set.py" python-set)
            (#P"list-comp.py" python-list-comprehension)
            (#P"set-comp.py" python-set-comprehension)
            (#P"dict-comp.py" python-dictionary-comprehension)
            (#P"generator-exp.py" python-generator-expression)
            (#P"await.py" python-await)
            (#P"yield.py" python-yield)
            ;; NOTE: from is in interleaved text.
            (#P"yield-from.py" python-yield)
            (#P"compare.py" python-comparison-operator)
            (#P"call.py" python-call)
            (#P"joined-str.py" python-interpolation)
            (#P"attribute.py" python-attribute)
            (#P"starred.py" python-list-splat-pattern)
            (#P"list.py" python-list)
            (#P"tuple.py" python-tuple)
            (#P"slice.py" python-subscript python-slice)))))
