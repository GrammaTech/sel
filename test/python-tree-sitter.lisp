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
   :software-evolution-library/components/formatting
   :software-evolution-library/utility/range)
  (:import-from :asdf/system
                :system-relative-pathname)
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

(defun is-gets-vars (expected-vars result-vars)
  "Test that EXPECTED-VARS matches RESULT-VARS."
  (let ((result-var-names (mapcar {aget :name} result-vars)))
    (is (= (length expected-vars)
           (length result-var-names)))
    (mapcar
     (lambda (variable)
       (is (member variable result-var-names
                   :test #'equal)))
     expected-vars)))

(defun is-get-vars-scope (obj ast result-vars &key scope-fun)
  "Test that the var alists in RESULT-VARS all occur in the expected scope."
  (let ((scope (if scope-fun
                   (find-if scope-fun ast)
                   (enclosing-scope obj ast))))
    (mapcar
     (lambda (var-scope)
       (is (eq scope var-scope)))
     (mapcar {aget :scope} result-vars))))

(defun is-get-vars-test (filename class-name expected-vars &key scope-fun)
  "Test that get-vars returns the expected information."
  (with-util-file (filename soft genome)
    (let* ((target-ast (find-if {typep _ class-name} genome))
           (result-vars (get-vars soft target-ast)))
      (is-gets-vars expected-vars result-vars)
      (is-get-vars-scope soft target-ast result-vars
                         :scope-fun scope-fun))))

(defun is-maps-args-to-params (arguments-to-parameters strings-alist)
  (labels ((convert-asts-to-strings ()
             "Convert arguments-to-parameters into an alist of
              strings."
             (mapcar (lambda (mapping)
                       (cons (source-text (car mapping))
                             (source-text (cdr mapping))))
                     arguments-to-parameters))
           (is-equalp (mapping string-mapping
                       &aux (parameter (cdr mapping))
                         (expected-parameter
                          (aget (car mapping) strings-alist
                                :test #'equalp)))
             "Test if CONS1 and CONS2 map to the same
              parameter."
             (is (equalp parameter expected-parameter)
                 "\"~a\" did not match expected \"~a\" in \"~a\""
                 parameter expected-parameter string-mapping)))
    (let ((string-mapping (convert-asts-to-strings)))
      (mapcar {is-equalp _ string-mapping} string-mapping))))


;;; Tests
(deftest python-simply-able-to-load-a-software-object ()
  (with-fixture hello-world-python
    (is (not (null *soft*)))))

(deftest python-can-parse-a-software-object ()
  (with-fixture hello-world-python
    (is (= 5 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*))))
  (with-fixture fib-python
    (is (= 37 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest python-can-handle-empty-file ()
  (with-fixture empty-python
    (is (= 0 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest python-can-handle-multibyte-characters ()
  (with-fixture multibyte-python1
    (is (= 17 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*))))
  (with-fixture multibyte-python2
    (is (= 11 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*))))
  (with-fixture multibyte-python3
    (is (= 5 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest python-can-handle-dos-format ()
  (with-fixture dos-python
    (is (= 37 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
               (genome-string *soft*)))))

(deftest python-can-copy-tree ()
  (let* ((text "
(a + b) / 2
")
         (new-source-text
           (source-text (tree-copy (convert 'python-ast text)))))
    (is (not (emptyp new-source-text)))
    (is (not (emptyp (trim-whitespace new-source-text))))
    (is (search text new-source-text))))

;;;(deftest python-stmt-ast-has-newline ()
(deftest py-stmt-ast-has-newline ()
  (with-fixture fib-python
    (is (typep (stmt-with-text *soft* (concatenate 'string "return b"
                                                           (list #\Newline)))
               'python-return-statement)))
  (with-fixture dos-python
    (is (typep (stmt-with-text *soft* (concatenate 'string "return b"
                                                           (list #\Linefeed
                                                                 #\Newline)))
               'python-return-statement))))

;;;(deftest python-ast-source-ranges ()
(deftest py-ast-source-ranges ()
  (with-fixture hello-world-python
    (is (equalp (mapcar [#'range-to-list #'cdr] (ast-source-ranges *soft*))
                '(((1 . 1) (2 . 1))
                  ((1 . 1) (2 . 1))
                  ((1 . 1) (1 . 22))
                  ((1 . 1) (1 . 6))
                  ((1 . 6) (1 . 22))
                  ((1 . 7) (1 . 21)))))))

;;;(deftest python-convert-source-snippet-works ()
(deftest py-convert-source-snippet-works ()
  (let ((ast (convert 'python-ast "j = 0")))
    (is (equal 5 (size ast)))
    (is (equal "j = 0" (source-text ast)))
    (is (find-if {typep _ 'python-assignment} ast))))

(deftest (python-can-format-a-software-object :long-running) ()
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

(deftest python-get-unbound-vals-works ()
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

(deftest python-get-unbound-funs-works ()
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
    (let* ((scopes (scopes soft (find-if {typep _ 'python-return-statement}
                                         genome)))
           (global-alist (scopes-contains-string-p scopes "a"))
           (expected-assign (find-if {typep _ 'python-assignment} genome)))
      (is (eq (aget :decl global-alist) expected-assign)
          "~A did not contain the expected variable 'a' assignment, ~a."
          global-alist expected-assign))))

(deftest python-scopes-2 ()
  "scopes gets the initial binding of a local statement."
  (with-util-file ("local-shadow" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'python-call} genome)))
           (nonlocal-alist (scopes-contains-string-p scopes "a"))
           (expected-assign (cadr (collect-if {typep _ 'python-assignment}
                                              genome))))
      (is (eq (aget :decl nonlocal-alist) expected-assign)
          "~A did not contain the expected variable 'a' assignment, ~a."
          nonlocal-alist expected-assign))))

(deftest python-scopes-3 ()
  "scopes gets the bindng from a function definition."
  (with-util-file ("global" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'python-return-statement}
                                         genome)))
           (def-alist (scopes-contains-string-p scopes "test"))
           (expected-def (find-if {typep _ 'python-function-definition} genome)))
      (is (eq (aget :decl def-alist) expected-def)
          "~A did not contain the expected variable 'test' assignment, ~a."
          def-alist expected-def))))

(deftest python-scopes-4 ()
  "scopes gets the bindng from an import."
  (with-util-file ("import" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'python-return-statement}
                                         genome)))
           (import-alist (scopes-contains-string-p scopes "a"))
           (expected-import (find-if {typep _ 'python-import-statement}
                                     genome)))
      (is (eq (aget :decl import-alist) expected-import)
          "~A did not contain the expected variable 'a' assignment, ~a."
          import-alist expected-import))))

(deftest python-scopes-5 ()
  "scopes gets the bindng from function parameters."
  (with-util-file ("parameter" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'python-pass-statement}
                                         genome)))
           (parameter-alist (scopes-contains-string-p scopes "a"))
           (expected-parameters (cadr (collect-if {typep _ 'python-parameters}
                                                  genome))))
      (is (eq (aget :decl parameter-alist) expected-parameters)
          "~A did not contain the expected variable 'a' assignment, ~a."
          parameter-alist expected-parameters))))

(deftest python-scopes-6 ()
  "scopes gets the bindngs local to a namespace."
  (with-util-file ("local-2" soft genome)
    (let* ((scopes (scopes soft (find-if {typep _ 'python-return-statement}
                                         genome)))
           (local-alist (scopes-contains-string-p scopes "a"))
           (expected-local (find-if {typep _ 'python-assignment} genome)))
      (is (eq (aget :decl local-alist) expected-local)
          "~A did not contain the expected variable 'a' assignment, ~a."
          local-alist expected-local))))

(deftest python-scopes-7 ()
  "scopes gets the bindings from multiple assignment statement."
  (with-scopes-file ("assign" soft genome)
    (is (equal `((((:decl  . ,(nest (car)
                                    (python-children)
                                    (stmt-with-text soft)
                                    (format nil "a, b = 0, 1~%")))
                   (:name  . "a")
                   (:scope . ,genome))
                  ((:decl  . ,(nest (car)
                                    (python-children)
                                    (stmt-with-text soft)
                                    (format nil "a, b = 0, 1~%")))
                   (:name  . "b")
                   (:scope . ,genome))))
               (nest (scopes soft)
                     (stmt-with-text soft)
                     (format nil "b = 1~%"))))))

(deftest python-scopes-8 ()
  "scopes gets the bindings from annotated assignment statements."
  (with-scopes-file ("ann-assign" soft genome)
    (is (equal `((((:decl  . ,(nest (car)
                                    (python-children)
                                    (stmt-with-text soft)
                                    (format nil "a:int = 3~%")))
                   (:name  . "a")
                   (:scope . ,genome))
                  ((:decl  . ,(nest (car)
                                    (python-children)
                                    (stmt-with-text soft)
                                    (format nil "b:int = 2~%")))
                   (:name  . "b")
                   (:scope . ,genome))))
               (nest (scopes soft)
                     (stmt-with-text soft)
                     (format nil "b = 1~%"))))))

(deftest python-get-vars-assignment-1 ()
  "get-vars gets variables from python-assignment."
  (is-get-vars-test "assign-1" 'python-assignment '("a" "b" "c")))

(deftest python-get-vars-class-definition-1 ()
  "get-vars gets variables from python-class-definition."
  (is-get-vars-test "class-def-1" 'python-class-definition '("Test")))

(deftest python-get-vars-except-clause-1 ()
  "get-vars gets variables from python-except-clause."
  (is-get-vars-test "except-handler-1" 'python-except-clause '("e")))

(deftest python-get-vars-for-statement-1 ()
  "get-vars gets variables from python-for-statement."
  (is-get-vars-test "for-1" 'python-for-statement '("i")))

(deftest python-get-vars-function-definition-1 ()
  "get-vars gets variables from python-function-definition."
  (with-util-file ("function-def-1" soft genome)
    (let* ((target-ast (find-if {typep _ 'python-function-definition} genome))
           (result-vars (get-vars soft target-ast)))
      (is-gets-vars '("test" "a" "b" "c") result-vars)
      (is-get-vars-scope
       soft target-ast (remove-if (lambda (alist)
                                    (equalp '(:function)
                                            (aget :attributes alist)))
                                  result-vars)
       :scope-fun {typep _ 'python-function-definition})
      (is-get-vars-scope
       soft target-ast (remove-if-not (lambda (alist)
                                        (equalp '(:function)
                                                (aget :attributes alist)))
                                      result-vars)))))

;;; Both imports share the same code.
(deftest python-get-vars-import-from-statement-1 ()
  "get-vars gets variables from py-import-from with an 'as'."
  (is-get-vars-test "import-from-1" 'python-import-from-statement '("z")))

(deftest python-get-vars-import-from-statement-2 ()
  "get-vars gets variables from py-import-from without an 'as'."
  (is-get-vars-test "import-from-2" 'python-import-from-statement '("y")))

(deftest python-get-vars-lambda-function-1 ()
  "get-vars gets variables from python-lambda and has the correct scope."
  (is-get-vars-test "lambda-1" 'python-lambda '("x")
                    :scope-fun {typep _ 'python-lambda}))

;;; Comps and generators share the same code.
(deftest python-get-vars-list-comprehension-1 ()
  "get-vars gets variables from py-list-comp with multiple generators and has
the correct scope."
  (is-get-vars-test "list-comp-1" 'python-list-comprehension '("x" "y")
                    :scope-fun {typep _ 'python-list-comprehension}))

(deftest python-get-vars-list-comprehension-2 ()
  "get-vars gets variables from py-list-comp and has the correct scope."
  (is-get-vars-test "list-comp-2" 'python-list-comprehension '("x")
                    :scope-fun {typep _ 'python-list-comprehension}))

(deftest python-get-vars-with-statement-1 ()
  "get-vars gets variables from python-with-statement."
  (is-get-vars-test "with-1" 'python-with-statement '("x")))

;;;(deftest python-collect-var-uses-1 ()
(deftest py-collect-var-uses-1 ()
  "collect-var-uses collects global variable usages and ignores local bindings."
  (with-util-file ("global" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'python-identifier)
                                     (equalp "a" (source-text ast))))
                              genome))))
      (is (= 4 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

;;;(deftest python-collect-var-uses-2 ()
(deftest py-collect-var-uses-2 ()
  "collect-var-uses collects global variable usages when a local binding
appears in a scope above the global usage."
  (with-util-file ("nested-global" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'python-identifier)
                                     (equalp "a" (source-text ast))))
                              genome))))
      (is (= 3 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

;;;(deftest python-collect-var-uses-3 ()
(deftest py-collect-var-uses-3 ()
  "collect-var-uses collects nested local variable usages."
  (with-util-file ("local" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'python-identifier)
                                     (equalp "a" (source-text ast))))
                              genome))))
      (is (= 4 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

;;;(deftest python-collect-var-uses-4 ()
(deftest py-collect-var-uses-4 ()
  "collect-var-uses collects local variable usages and ignores global
bindings when shadowed."
  (with-util-file ("local-shadow" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (cadr
                      (collect-if (lambda (ast)
                                    (and (typep ast 'python-identifier)
                                         (equalp "a" (source-text ast))))
                                  genome)))))
      (is (= 3 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

;;;(deftest python-collect-var-uses-5 ()
(deftest py-collect-var-uses-5 ()
  "collect-var-uses doesn't include parameters as uses when targeting
a variable in a scope above it."
  (with-util-file ("parameter" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'python-identifier)
                                     (equalp "a" (source-text ast))))
                              genome))))
      (is (= 2 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

;;;(deftest python-collect-var-uses-6 ()
(deftest py-collect-var-uses-6 ()
  "collect-var-uses doesn't include parameters of functions defined
in the same sub-tree as its namespace."
  (with-util-file ("same-variable-name" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'python-identifier)
                                     (typep (get-parent-ast soft ast)
                                            'python-assignment)
                                     (string= "a" (source-text ast))))
                              genome))))
      (is (= 3 (length var-uses))
          "~A did not contain the expected number of uses" var-uses)
      (is (not (find-if
                (lambda (var-use)
                  (find-if-in-parents {typep _ 'python-parameters} soft var-use))
                var-uses))
          "~A contained an unexpected parameter" var-uses))))

;;;(deftest python-collect-fun-uses-1 ()
(deftest py-collect-fun-uses-1 ()
  "collect-fun-uses doesn't collect uses of shadowed functions."
  (with-util-file ("function-shadow" soft genome)
    (let ((fun-uses (collect-fun-uses
                     soft
                     (find-if {typep _ 'python-function-definition} genome))))
      (is (= 5 (length fun-uses))
          "~A did not contain the expected number of uses" fun-uses))))

;;;(deftest python-map-arguments-to-parameters-1 ()
(deftest py-map-arguments-to-parameters-1 ()
  "map-arguments-to-parameters handles positional parameters
and positional parameters with defaults."
  (with-util-file ("positional" soft genome)
    (is-maps-args-to-params
     (map-arguments-to-parameters
      soft
      (find-if {typep _ 'python-call} genome))
     '(("1" . "a")
       ("2" . "b")
       ("3" . "c")))))

;;;(deftest python-map-arguments-to-parameters-2 ()
(deftest py-map-arguments-to-parameters-2 ()
  "map-arguments-to-parameters handles keyword parameters
and keyword parameters with defaults."
  (with-util-file ("keyword" soft genome)
    (is-maps-args-to-params
     (map-arguments-to-parameters
      soft
      (find-if {typep _ 'python-call} genome))
     '(("1" . "key1")
       ("2" . "key2")
       ("3" . "key3")))))

;;;(deftest python-map-arguments-to-parameters-3 ()
(deftest py-map-arguments-to-parameters-3 ()
  "map-arguments-to-parameters handles var args."
  (with-util-file ("variable-arg" soft genome)
    (is-maps-args-to-params
     (map-arguments-to-parameters
      soft
      (find-if {typep _ 'python-call} genome))
     '(("1" . "a")
       ("2" . "b")
       ("(3, 4, 5)" . "args")))))

;;;(deftest python-map-arguments-to-parameters-4 ()
(deftest py-map-arguments-to-parameters-4 ()
  "map-arguments-to-parameters handles keyword args."
  (with-util-file ("keyword-arg" soft genome)
    (is-maps-args-to-params
     (map-arguments-to-parameters
      soft
      (find-if {typep _ 'python-call} genome))
     '(("1" . "a")
       ("2" . "required")
       ("3" . "b")
       ("{\"d\" : 5, \"c\" : 4}" . "args")))))

(deftest (test-python-source-ranges :long-running t) ()
  (let ((py-files (remove "empty" (expand-wildcard #p"python/*/*.py")
                          :test #'equal :key #'pathname-name)))
    ;; We ignore whitespace here because
    (test-ast-source-ranges-for-files 'python py-files
                                      :ignore-indentation t)))

(deftest python-test-end-of-parameter-list ()
  (with-util-file ("functions" sw ast)
    (let ((nodes (iter (for node in-tree ast)
                   (when (typep node 'parseable-function)
                     (collect node)))))
      (is (length= 3 nodes))
      (destructuring-bind (f1 f2 f3) nodes
        (is (equal "function1" (function-node-name f1)))
        (is (equal? (make 'source-location :line 3 :column 16)
                    (end-of-parameter-list sw f1)))
        (is (equal "function2" (function-node-name f2)))
        (is (equal? (make 'source-location :line 6 :column 20)
                    (end-of-parameter-list sw f2)))
        (is (equal "function3" (function-node-name f3)))
        (is (equal? (make 'source-location :line 9 :column 22)
                    (end-of-parameter-list sw f3)))))))

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
