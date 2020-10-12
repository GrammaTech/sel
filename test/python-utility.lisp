;;;; python-utility.lisp --- Python utility.
;;;;
;;;; This file tests the functions that operate on
;;;; python and python-ast objects.
(defpackage :software-evolution-library/test/python-utility
  (:nicknames :sel/test/python-utility
              :sel/test/py-util)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/python
   :software-evolution-library/software/non-homologous-parseable
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-python-utility))
(in-package :software-evolution-library/test/python-utility)
(in-readtable :curry-compose-reader-macros)
(defsuite test-python-utility "Python utility." (python3.8-available-p))



;;; Utility
(defmacro with-software-file ((filename software-var genome-var)
                              &body body)
  `(let* ((,software-var (from-file
                          (make-instance 'python)
                          (make-pathname :name ,filename
                                         :type "py"
                                         :directory +python-utility-dir+)))
          (,genome-var (genome ,software-var)))
     ,@body))

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
  (with-software-file (filename soft genome)
    (let* ((target-ast (find-if {typep _ class-name} genome))
           (result-vars (get-vars soft target-ast)))
      (is-gets-vars expected-vars result-vars)
      (is-get-vars-scope soft target-ast result-vars
                         :scope-fun scope-fun))))


;;; Tests
(deftest python-collect-var-uses-1 ()
  "collect-var-uses collects global variable usages and ignores local bindings."
  (with-software-file ("global" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'py-name)
                                     (equalp "a" (ast-annotation ast :id))))
                              genome))))
      (is (= 4 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

(deftest python-collect-var-uses-2 ()
  "collect-var-uses collects global variable usages when a local binding
appears in a scope above the global usage."
  (with-software-file ("nested-global" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'py-name)
                                     (equalp "a" (ast-annotation ast :id))))
                              genome))))
      (is (= 3 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

(deftest python-collect-var-uses-3 ()
  "collect-var-uses collects nested local variable usages."
  (with-software-file ("local" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'py-name)
                                     (equalp "a" (ast-annotation ast :id))))
                              genome))))
      (is (= 4 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

(deftest python-collect-var-uses-4 ()
  "collect-var-uses collects local variable usages and ignores global
bindings when shadowed."
  (with-software-file ("local-shadow" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (cadr
                      (collect-if (lambda (ast)
                                    (and (typep ast 'py-name)
                                         (equalp "a" (ast-annotation ast :id))))
                                  genome)))))
      (is (= 3 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

(deftest python-collect-var-uses-5 ()
  "collect-var-uses doesn't include parameters as uses when targeting
a variable in a scope above it."
  (with-software-file ("parameter" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'py-name)
                                     (equalp "a" (ast-annotation ast :id))))
                              genome))))
      (is (= 2 (length var-uses))
          "~A did not contain the expected number of uses" var-uses))))

(deftest python-collect-var-uses-6 ()
  "collect-var-uses doesn't include parameters of functions defined
in the same sub-tree as its namespace."
  (with-software-file ("same-variable-name" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'py-name)
                                     (typep (get-parent-ast soft ast) 'py-assign)
                                     (string= "a" (ast-annotation ast :id))))
                              genome))))
      (is (= 3 (length var-uses))
          "~A did not contain the expected number of uses" var-uses)
      (is (not (find-if {typep _ 'py-arg} var-uses))
          "~A contained an unexpected py-arg" var-uses))))

(deftest python-map-arguments-to-parameters-1 ()
  "map-arguments-to-parameters handles positional parameters
and positional parameters with defaults."
  (with-software-file ("positional" soft genome)
    (is-maps-args-to-params
     (map-arguments-to-parameters
      soft
      (find-if {typep _ 'py-call} genome))
     '(("1" . "a")
       ("2" . "b")
       ("3" . "c")))))

(deftest python-map-arguments-to-parameters-2 ()
  "map-arguments-to-parameters handles keyword parameters
and keyword parameters with defaults."
  (with-software-file ("keyword" soft genome)
    (is-maps-args-to-params
     (map-arguments-to-parameters
      soft
      (find-if {typep _ 'py-call} genome))
     '(("1" . "key1")
       ("2" . "key2")
       ("3" . "key3")))))

(deftest python-map-arguments-to-parameters-3 ()
  "map-arguments-to-parameters handles var args."
  (with-software-file ("variable-arg" soft genome)
    (is-maps-args-to-params
     (map-arguments-to-parameters
      soft
      (find-if {typep _ 'py-call} genome))
     '(("1" . "a")
       ("2" . "b")
       ("(3, 4, 5)" . "args")))))

(deftest python-map-arguments-to-parameters-4 ()
  "map-arguments-to-parameters handles keyword args."
  (with-software-file ("keyword-arg" soft genome)
    (is-maps-args-to-params
     (map-arguments-to-parameters
      soft
      (find-if {typep _ 'py-call} genome))
     '(("1" . "a")
       ("2" . "required")
       ("3" . "b")
       ("{\"d\" : 5, \"c\" : 4}" . "args")))))

(deftest python-collect-fun-uses-1 ()
  "collect-fun-uses doesn't collect uses of shadowed functions."
  (with-software-file ("function-shadow" soft genome)
    (let ((fun-uses (collect-fun-uses
                     soft
                     (find-if {typep _ 'py-function-def} genome))))
      (is (= 5 (length fun-uses))
          "~A did not contain the expected number of uses" fun-uses))))

(deftest python-get-vars-ann-assign-1 ()
  "get-vars gets variables from py-ann-assign."
  (is-get-vars-test "ann-assign-1" 'py-ann-assign '("a")))

(deftest python-get-vars-assign-1 ()
  "get-vars gets variables from py-assign."
  (is-get-vars-test "assign-1" 'py-assign '("a" "b" "c")))

(deftest python-get-vars-class-def-1 ()
  "get-vars gets variables from py-class-def."
  (is-get-vars-test "class-def-1" 'py-class-def '("Test")))

(deftest python-get-vars-except-handler-1 ()
  "get-vars gets variables from py-except-handler."
  (is-get-vars-test "except-handler-1" 'py-except-handler '("e")))

;;; Async-for and for share the same code.
(deftest python-get-vars-for-1 ()
  "get-vars gets variables from py-for."
  (is-get-vars-test "for-1" 'py-for '("i")))

;;; Async fun and fun share the same code.
(deftest python-get-vars-function-def-1 ()
  "get-vars gets variables from py-function-def."
  (with-software-file ("function-def-1" soft genome)
    (let* ((target-ast (find-if {typep _ 'py-function-def} genome))
           (result-vars (get-vars soft target-ast)))
      (is-gets-vars '("test" "a" "b" "c") result-vars)
      (is-get-vars-scope
       soft target-ast (remove-if (lambda (alist)
                                    (equalp '(:function)
                                            (aget :attributes alist)))
                                  result-vars)
       :scope-fun {typep _ 'py-function-def})
      (is-get-vars-scope
       soft target-ast (remove-if-not (lambda (alist)
                                        (equalp '(:function)
                                                (aget :attributes alist)))
                                      result-vars)))))

;;; Both imports share the same code.
(deftest python-get-vars-import-from-1 ()
  "get-vars gets variables from py-import-from with an 'as'."
  (is-get-vars-test "import-from-1" 'py-import-from '("z")))

(deftest python-get-vars-import-from-2 ()
  "get-vars gets variables from py-import-from without an 'as'."
  (is-get-vars-test "import-from-2" 'py-import-from '("y")))

(deftest python-get-vars-lambda-1 ()
  "get-vars gets variables from py-lambda and has the correct scope."
  (is-get-vars-test "lambda-1" 'py-lambda '("x")
                    :scope-fun {typep _ 'py-lambda}))

;;; Comps and generators share the same code.
(deftest python-get-vars-list-comp-1 ()
  "get-vars gets variables from py-list-comp with multiple generators and has
the correct scope."
  (is-get-vars-test "list-comp-1" 'py-list-comp '("x" "y")
                    :scope-fun {typep _ 'py-list-comp}))

(deftest python-get-vars-list-comp-2 ()
  "get-vars gets variables from py-list-comp and has the correct scope."
  (is-get-vars-test "list-comp-2" 'py-list-comp '("x")
                    :scope-fun {typep _ 'py-list-comp}))

;;; Async with and with share the same code.
(deftest python-get-vars-with-1 ()
  "get-vars gets variables from py-with."
  (is-get-vars-test "with-1" 'py-with '("x")))
