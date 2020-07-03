;;;; javascript.lisp --- Javascript representation.
(defpackage :software-evolution-library/test/javascript
  (:nicknames :sel/test/javascript)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/javascript
   :software-evolution-library/software/json
   :software-evolution-library/components/formatting
   :software-evolution-library/components/test-suite)
  (:export :test-javascript))
(in-package :software-evolution-library/test/javascript)
(in-readtable :curry-compose-reader-macros)
(defsuite test-javascript "Javascript representation." (acorn-available-p))

(defixture hello-world-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript)
                    (javascript-dir #P"hello-world/hello-world.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture fib-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript)
                    (javascript-dir #P"fib/fib.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture trivial-json
  (:setup
   (setf *soft*
         (from-file (make-instance 'json) (javascript-dir #P"trivial.json"))))
  (:teardown
   (setf *soft* nil)))

(deftest simply-able-to-load-a-javascript-software-object ()
  (with-fixture hello-world-javascript
    (is (not (null *soft*)))))

(deftest (can-parse-a-javascript-software-object :long-running) ()
  (with-fixture hello-world-javascript
    (is (= 6 (size *soft*)))
    (is (equal (file-to-string (javascript-dir #P"hello-world/hello-world.js"))
               (genome-string *soft*))))
  (with-fixture fib-javascript
    (is (= 40 (size *soft*)))
    (is (equal (file-to-string (javascript-dir #P"fib/fib.js"))
               (genome-string *soft*)))))

(deftest can-parse-a-json-software-object ()
  (with-fixture trivial-json
    (is (not (zerop (size *soft*))))
    (is (typep (genome *soft*) 'js-object-expression))))

(deftest cut-shortens-a-javascript-software-object ()
  (with-fixture fib-javascript
    (let* ((variant (copy *soft*))
           (stmt1 (stmt-with-text variant "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (not (equal (genome-string variant)
                      (genome-string *soft*))))
      (is (< (size variant)
             (size *soft*))))))

(deftest insert-lengthens-a-javascript-software-object ()
  (with-fixture fib-javascript
    (let* ((variant (copy *soft*))
           (stmt1 (stmt-with-text variant "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-insert
                        :targets (list (cons :stmt1 stmt1)
                                       (cons :value1 stmt1))))
      (is (not (equal (genome-string variant)
                      (genome-string *soft*))))
      (is (not (equal? (genome variant)
                       (genome *soft*))))
      (is (> (size variant)
             (size *soft*))))))

(deftest swap-changes-a-javascript-software-object ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;"))
          (stmt2 (stmt-with-text *soft* "num--;")))
      (apply-mutation variant
                      (make-instance 'parseable-swap
                        :targets (list (cons :stmt1 stmt1)
                                       (cons :stmt2 stmt2))))
      (is (not (equal (genome-string variant)
                      (genome-string *soft*))))
      (is (= (size variant)
             (size *soft*))))))

(deftest javascript-copies-are-independent ()
  (with-fixture fib-javascript
    (let ((orig-genome-string (genome-string *soft*))
          (variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (string= (genome-string *soft*) orig-genome-string))
      (is (not (string= (genome-string variant) orig-genome-string))))))

(deftest javascript-copies-share-asts ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*)))
      (is (eq (genome *soft*) (genome variant)))
      (is (> (size variant) 0)))))

(deftest javascript-mutation-preserves-unmodified-subtrees ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (equal? (stmt-with-text *soft* "num--;")
                  (stmt-with-text variant "num--;"))))))

(deftest javascript-convert-source-snippet-works ()
  (let ((ast (convert 'javascript-ast "j = 0")))
    (is (equal 5 (size ast)))
    (is (equal "j = 0" (source-text ast)))
    (is (find-if {typep _ 'js-expression-statement} ast))))

(deftest (can-format-a-javascript-software-object :long-running) ()
  (with-fixture fib-javascript
    (is (not (string= (genome-string (copy *soft*))
                      (genome-string (prettier (copy *soft*))))))
    (is (not (string= (genome-string (copy *soft*))
                      (genome-string (format-genome (copy *soft*))))))
    (is (string= (genome-string (prettier (copy *soft*)))
                 (genome-string (format-genome (copy *soft*)))))))

(deftest javascript-can-rebind-vars ()
  (with-fixture fib-javascript
    (let ((rebound (rebind-vars (stmt-with-text *soft* "temp = a;")
                                (list (list "a" "b"))
                                nil)))
      (is (string= "temp = b;" (trim-whitespace (source-text rebound)))
          "`rebind-vars` did not properly rebind 'a' to 'b'")
      (is (find-if [{equal "b"} {ast-annotation _ :name}] rebound)
          "`rebind-vars` did not properly rebind 'a' to 'b'")))
  (with-fixture fib-javascript
    (let ((rebound (rebind-vars (stmt-with-text *soft* "fibonacci(10);")
                                (list (list "b" "G19"))
                                nil)))
      (is (string= "fibonacci(10);" (trim-whitespace (source-text rebound)))
          "`rebind-vars` improperly rebound 'b' to 'G19' in 'fibonacci(10)'")
      (is (null (find-if [{equal "b"} {ast-annotation _ :name}] rebound))
          "`rebind-vars` improperly rebound 'b' to 'G19' in 'fibonacci(10)'"))))

(deftest javascript-get-vars-in-scope ()
  (with-fixture fib-javascript
    (is (set-equal (list "temp" "b" "a" "num")
                   (nest (mapcar {aget :name})
                         (get-vars-in-scope *soft*)
                         (stmt-with-text *soft* "temp = a;"))
                   :test #'string=))))

(deftest javascript-get-unbound-vals ()
  (with-fixture fib-javascript
    (is (equal `((:name . "temp") (:name . "a"))
               (nest (get-unbound-vals *soft*)
                     (stmt-with-text *soft* "temp = a;"))))))

(deftest javascript-get-unbound-funs ()
  (with-fixture fib-javascript
    (is (equal `(("fibonacci" nil nil 1))
               (nest (get-unbound-funs *soft*)
                     (stmt-with-text *soft* "fibonacci(10);"))))))

(deftest (javascript-parsing-test :long-running) ()
  (labels ((parse-test (path &rest ast-classes)
             (let ((soft (from-file (make-instance 'javascript)
                                    (javascript-dir path))))
               (is (not (zerop (size soft))))
               (is (equal (genome-string soft)
                          (file-to-string (javascript-dir path))))
               (mapc (lambda (ast-class)
                       (is (find ast-class (genome soft) :key #'ast-class)))
                     ast-classes))))
    (mapc {apply #'parse-test}
          '((#P"parsing/array-destructuring.js" :ArrayPattern)
            (#P"parsing/arrow-function-expression.js" :ArrowFunctionExpression)
            (#P"parsing/await-expression.js" :AwaitExpression)
            (#P"parsing/class-declaration.js" :ClassDeclaration)
            (#P"parsing/class-expression.js" :ClassExpression)
            (#P"parsing/conditional-expression.js" :ConditionalExpression)
            (#P"parsing/debugger-statement.js" :DebuggerStatement)
            (#P"parsing/empty-statement.js" :EmptyStatement)
            (#P"parsing/export-specifier.js" :ExportSpecifier)
            (#P"parsing/expression-statement.js" :ExpressionStatement)
            (#P"parsing/function-declaration.js" :FunctionDeclaration)
            (#P"parsing/function-expression.js" :FunctionExpression)
            (#P"parsing/if.js" :IfStatement)
            (#P"parsing/import-specifier.js" :ImportSpecifier)
            (#P"parsing/labeled-statement.js" :LabeledStatement)
            (#P"parsing/loops.js"
             :ForStatement :ForInStatement :ForOfStatement
             :WhileStatement :DoWhileStatement)
            (#P"parsing/new-expression.js" :NewExpression)
            (#P"parsing/object-destructuring.js" :ObjectPattern)
            (#P"parsing/object-expression.js" :ObjectExpression)
            (#P"parsing/property.js" :Property)
            (#P"parsing/sequence-expression.js" :SequenceExpression)
            (#P"parsing/spread-element.js" :SpreadElement)
            (#P"parsing/switch.js" :SwitchStatement)
            (#P"parsing/tagged-template-expression.js"
             :TaggedTemplateExpression)
            (#P"parsing/try-catch-throw.js"
             :TryStatement :CatchClause :ThrowStatement)
            (#P"parsing/with-statement.js" :WithStatement)
            (#P"parsing/yield-expression.js" :YieldExpression)))))

(deftest array-destructuring-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/array-destructuring.js"))))
    (is (set-equal (list "d" "c" "b" "a" "arr")
                   (nest (mapcar {aget :name})
                         (get-vars-in-scope soft)
                         (stmt-starting-with-text soft "console.log("))
                   :test #'string=))))

(deftest object-destructuring-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/object-destructuring.js"))))
    ;; FIXME: Does the order matter for these in scope variables?  I'm
    ;;        now consistently returning them reversed from how they
    ;;        were returned previously.
    (is (set-equal (list "q" "p" "o")
                   (nest (mapcar {aget :name})
                         ;; FIXME: It would be nice to try to remove
                         ;;        this requirement of passing the
                         ;;        full software object as a first
                         ;;        argument as much as possible.  It
                         ;;        often isn't used with functional
                         ;;        trees and we should be able to
                         ;;        dispatch on the class of the AST.
                         (get-vars-in-scope soft)
                         (stmt-starting-with-text soft "console.log("))
                   :test #'string=))))

(deftest for-in-loop-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/loops.js"))))
    (is (find "i" (nest (mapcar {aget :name})
                        (get-vars-in-scope soft)
                        (stmt-with-text soft "console.log(arr[i]);"))
              :test #'equal))))

(deftest for-of-loop-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/loops.js"))))
    (is (find "val" (nest (mapcar {aget :name})
                          (get-vars-in-scope soft)
                          (stmt-with-text soft "console.log(val);"))
              :test #'equal))))

(deftest function-identifier-ast-present ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/function-declaration.js"))))
    (is (stmt-with-text soft "foo"))
    (is (equal :Identifier (ast-class (stmt-with-text soft "foo"))))))

(deftest javascript.newline.post-processing.1 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "") nil)
      "position-after-leading-newline on empty string"))

(deftest javascript.newline.post-processing.2 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "x") nil)
      "position-after-leading-newline on string with no whitespace or newline"))

(deftest javascript.newline.post-processing.3 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "   ") nil)
      "position-after-leading-newline on string with whitespace only, no newline"))

(deftest javascript.newline.post-processing.4 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline " x") nil)
      "position-after-leading-newline on string with whitespace, no newline"))

(deftest javascript.newline.post-processing.5 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline (string #\Newline)) 1)
      "position-after-leading-newline on newline"))

(deftest javascript.newline.post-processing.6 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               (concatenate 'string (string #\Newline) "x"))
              1)
      "position-after-leading-newline on newline + other stuff"))

(deftest javascript.newline.post-processing.7 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               (concatenate 'string (string #\Newline) "// foo "))
              1)
      "position-after-leading-newline on newline, comment"))

(deftest javascript.newline.post-processing.8 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               (concatenate 'string  "// foo " (string #\Newline) "   "))
              8)
      "position-after-leading-newline on comment, newline "))

(deftest javascript.newline.post-processing.9 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               "  // foo ")
              nil)
      "position-after-leading-newline on comment"))

(deftest javascript.newline.post-processing.10 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "/")
              nil)
      "position-after-leading-newline slash at EOL not a comment"))

(deftest javascript.newline.post-processing.11 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline " / ")
              nil)
      "position-after-leading-newline slash not at EOL not a comment"))

(defun to-js-ast (tree)
  (labels ((to-js-ast- (tree)
             (assert (or (stringp tree)
                         (and (listp tree) (keywordp (car tree))))
                     (tree)
                     "Every subtree must be a string or start with AST keyword")
             (nest
              (if (stringp tree)
                  tree)
              (case (car tree)
                (:j (make-instance 'javascript-ast
                     :children (mapcar #'to-js-ast- (cdr tree))))
                (:c (make-instance 'conflict-ast
                     :child-alist (mapcar
                                   (lambda (pair)
                                     (destructuring-bind (key . value) pair
                                       (cons key (when value
                                                   (to-js-ast- value)))))
                                   (second tree))
                     :children (mapcar #'to-js-ast- (cddr tree))))))))
    (to-js-ast- tree)))

(defixture javascript-ast-w-conflict
  (:setup
   (setf *soft* (make-instance 'javascript
                  :genome (to-js-ast '(:j "top"
                                       (:j "left"
                                        (:c ((:old . nil) (:my . "a") (:your . "b"))))
                                       (:j "right"))))))
  (:teardown
   (setf *soft* nil)))

(deftest javascript-and-conflict-basic-parseable-ast-functionality ()
  (with-fixture javascript-ast-w-conflict
    ;; We actually have ASTs.
    (is (typep (genome *soft*) 'javascript-ast))
    ;; Non-root asts have path.
    (is (equal (size *soft*)
               (count-if {ast-path *soft*} (genome *soft*))))
    ;; Copy works.
    (is (typep (copy (genome *soft*)) 'javascript-ast)))
  (with-fixture javascript-ast-w-conflict
    ;; Access ASTs.
    (is (string= "top" (@ *soft* '(0))))
    (is (typep (@ *soft* '(1)) 'javascript-ast))
    (is (string= "left" (@ *soft* '(1 0))))
    (is (typep (@ *soft* '(2)) 'javascript-ast))
    (is (string= "right" (@ *soft* '(2 0))))
    ;; Set AST with (replace-ast ...).
    (replace-ast *soft* '(2 0) "RIGHT")
    (is (string= "RIGHT" (@ *soft* '(2 0))))
    (replace-ast *soft* '(1) (make-instance 'javascript-ast :class :foo))
    (is (eql :foo (ast-class (@ *soft* '(1))))))
  (with-fixture javascript-ast-w-conflict
    (replace-ast *soft* '(1) (make-instance 'javascript-ast :class :foo))
    (is (eql :foo (ast-class (@ *soft* '(1)))))))

(deftest javascript-and-conflict-replace-ast ()
  (with-fixture javascript-ast-w-conflict
    (let ((cnf (find-if {typep _ 'conflict-ast} (genome *soft*))))
      (replace-ast *soft*
                   (ast-path *soft* cnf)
                   (aget :my (conflict-ast-child-alist cnf))))

    (is (equal (size *soft*) (count-if {ast-path *soft*} (genome *soft*))))
    (is (string= (genome-string *soft*) "topleftaright"))))

(deftest test-json-preserves-trailing-whitespace ()
  (let* ((ws (fmt "     ~%"))
         (genome (string+ "{\"x\": 1}" ws))
         (json (make-instance 'json :genome genome)))
    (is (string$= ws (genome-string json)))))
