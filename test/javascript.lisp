;;;; javascript.lisp --- Javascript representation.
(defpackage :software-evolution-library/test/javascript
  (:nicknames :sel/test/javascript)
  (:use
   :gt/full
   :trace-db
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/javascript
   :software-evolution-library/software/json
   :software-evolution-library/components/formatting
   :software-evolution-library/components/instrument
   :software-evolution-library/components/javascript-instrument
   :software-evolution-library/components/traceable
   :software-evolution-library/components/test-suite)
  (:export :test-javascript))
(in-package :software-evolution-library/test/javascript)
(in-readtable :curry-compose-reader-macros)
(defsuite test-javascript "Javascript representation." (acorn-available-p))

(define-software javascript-traceable  (javascript sexp-traceable) ())

(defixture hello-world-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript-traceable)
                    (javascript-dir #P"hello-world/hello-world.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture fib-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript-traceable)
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
    (is (= 6 (length (asts *soft*))))
    (is (equal (file-to-string (javascript-dir #P"hello-world/hello-world.js"))
               (genome *soft*))))
  (with-fixture fib-javascript
    (is (= 40 (length (asts *soft*))))
    (is (equal (file-to-string (javascript-dir #P"fib/fib.js"))
               (genome *soft*)))))

(deftest can-parse-a-json-software-object ()
  (with-fixture trivial-json
    (is (not (null (asts *soft*))))
    (is (eq :OBJECTEXPRESSION
            (ast-class (ast-node (ast-root *soft*)))))))

(deftest cut-shortens-a-javascript-software-object ()
  (with-fixture fib-javascript
    (let* ((variant (copy *soft*))
           (stmt1 (stmt-with-text variant "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (not (equal (genome variant)
                      (genome *soft*))))
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
      (is (not (equal (genome variant)
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
      (is (not (equal (genome variant)
                      (genome *soft*))))
      (is (= (size variant)
             (size *soft*))))))

(deftest javascript-copies-are-independent ()
  (with-fixture fib-javascript
    (let ((orig-genome (genome *soft*))
          (variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (string= (genome *soft*) orig-genome))
      (is (not (string= (genome variant) orig-genome))))))

(deftest javascript-copy-clears-genome-slot ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*)))
      (is (null (slot-value (copy *soft*) 'genome)))
      (is (string= (genome *soft*)
                   (genome variant))))))

(deftest javascript-copies-share-asts ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*)))
      (is (ast-equal-p (ast-root *soft*)
                       (ast-root variant)))
      (is (> (size variant) 0)))))

(deftest javascript-mutation-preserves-unmodified-subtrees ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (ast-equal-p (stmt-with-text *soft* "num--;")
                       (stmt-with-text variant "num--;"))))))

(deftest javascript-convert-source-snippet-works ()
  (is (equal 1 (length (convert 'javascript-ast "j = 0")))))

(deftest (can-format-a-javascript-software-object :long-running) ()
  (with-fixture fib-javascript
    (is (not (string= (genome (copy *soft*))
                      (genome (prettier (copy *soft*))))))
    (is (not (string= (genome (copy *soft*))
                      (genome (format-genome (copy *soft*))))))
    (is (string= (genome (prettier (copy *soft*)))
                 (genome (format-genome (copy *soft*)))))))

(deftest javascript-can-rebind-vars ()
  (with-fixture fib-javascript
    (is (string= "temp = b;"
                 (nest (trim-whitespace)
                       (source-text)
                       (rebind-vars (stmt-with-text *soft* "temp = a;")
                                    (list (list "a" "b"))
                                    nil))))))

(deftest javascript-get-vars-in-scope ()
  (with-fixture fib-javascript
    (is (equal (list "temp" "b" "a" "num")
               (nest (mapcar {aget :name})
                     (get-vars-in-scope *soft*)
                     (stmt-with-text *soft* "temp = a;"))))))

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

(deftest (javascript-instrument-and-collect-traces :long-running) ()
  (with-fixture fib-javascript
    (let ((instrumented (instrument *soft*)))
      (collect-traces instrumented
                      (make-instance 'test-suite
                        :test-cases (list (make-instance 'test-case
                                            :program-name "node"
                                            :program-args (list :bin)))))
      (is (equal 1 (n-traces (traces instrumented))))
      (is (equal '((:TRACE ((:C . 0))  ((:C . 36)) ((:C . 3))
                    ((:C . 12)) ((:C . 17)) ((:C . 21))
                    ((:C . 27)) ((:C . 31)) ((:C . 17))
                    ((:C . 21)) ((:C . 27)) ((:C . 31))
                    ((:C . 17)) ((:C . 21)) ((:C . 27))
                    ((:C . 31)) ((:C . 17)) ((:C . 21))
                    ((:C . 27)) ((:C . 31)) ((:C . 17))
                    ((:C . 21)) ((:C . 27)) ((:C . 31))
                    ((:C . 17)) ((:C . 21)) ((:C . 27))
                    ((:C . 31)) ((:C . 17)) ((:C . 21))
                    ((:C . 27)) ((:C . 31)) ((:C . 17))
                    ((:C . 21)) ((:C . 27)) ((:C . 31))
                    ((:C . 17)) ((:C . 21)) ((:C . 27))
                    ((:C . 31)) ((:C . 17)) ((:C . 21))
                    ((:C . 27)) ((:C . 31)) ((:C . 34)))
                   (:INPUT "node" :BIN))
                 (get-trace (traces instrumented) 0))))))

(deftest (javascript-instrument-and-collect-traces-with-vars :long-running) ()
  (with-fixture fib-javascript
    (let ((instrumented
           (instrument *soft*
                       :functions
                       (list (lambda (instrumenter ast)
                               (var-instrument
                                {get-vars-in-scope (software instrumenter)}
                                instrumenter
                                ast))))))
      (collect-traces instrumented
                      (make-instance 'test-suite
                        :test-cases (list (make-instance 'test-case
                                            :program-name "node"
                                            :program-args (list :bin)))))
      (is (equalp 1 (n-traces (traces instrumented))))
      (is (equalp '((:C . 21)(:SCOPES #("temp" "number" 1 nil)
                              #("b" "number" 0 nil)
                              #("a" "number" 1 nil)
                              #("num" "number" 10 nil)))
                  (nth 5 (aget :trace (get-trace (traces instrumented) 0))))))))

(deftest (javascript-parsing-test :long-running) ()
  (labels ((parse-test (path &rest ast-classes)
             (let ((soft (from-file (make-instance 'javascript)
                                    (javascript-dir path))))
               (is (not (null (asts soft))))
               (is (equal (genome soft) (file-to-string (javascript-dir path))))
               (mapc (lambda (ast-class)
                       (is (find ast-class (asts soft) :key #'ast-class)))
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
    (is (equal (list "d" "c" "b" "a" "arr")
               (nest (mapcar {aget :name})
                     (get-vars-in-scope soft)
                     (lastcar)
                     (remove-if-not {traceable-stmt-p soft})
                     (asts soft))))))

(deftest object-destructuring-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/object-destructuring.js"))))
    (is (equal (list "q" "p" "o")
               (nest (mapcar {aget :name})
                     (get-vars-in-scope soft)
                     (lastcar)
                     (remove-if-not {traceable-stmt-p soft})
                     (asts soft))))))

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
                (:j (make-javascript-ast
                     :node (make-javascript-ast-node)
                     :children (mapcar #'to-js-ast- (cdr tree))))
                (:c (make-conflict-ast
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
                  :ast-root (to-js-ast '(:j "top"
                                         (:j "left"
                                          (:c ((:old . nil) (:my . "a") (:your . "b"))))
                                         (:j "right"))))))
  (:teardown
   (setf *soft* nil)))

(deftest javascript-and-conflict-basic-parseable-ast-functionality ()
  (with-fixture javascript-ast-w-conflict
    (is (javascript-ast-p (ast-root *soft*)))         ; We actually have ASTs.
    (is (every #'ast-path (cdr (asts *soft*))))       ; Non-root ast have path.
    (is (javascript-ast-p (copy (ast-root *soft*))))) ; Copy works.
  (with-fixture javascript-ast-w-conflict
    ;; Access ASTs.
    (is (string= "top" (get-ast *soft* '(0))))
    (is (javascript-ast-p (get-ast *soft* '(1))))
    (is (string= "left" (get-ast *soft* '(1 0))))
    (is (javascript-ast-p (get-ast *soft* '(2))))
    (is (string= "right" (get-ast *soft* '(2 0))))
    ;; Set AST with (setf (get-ast ...) ...).
    (setf (get-ast *soft* '(2 0)) "RIGHT")
    (is (string= "RIGHT" (get-ast *soft* '(2 0))))
    (setf (get-ast *soft* '(1)) (make-javascript-ast
                                 :node (make-javascript-ast-node :class :foo)))
    (is (eql :foo (ast-class (get-ast *soft* '(1))))))
  (with-fixture javascript-ast-w-conflict
    (replace-ast *soft* '(1)
                 (make-javascript-ast
                  :node (make-javascript-ast-node :class :foo)))
    (is (eql :foo (ast-class (get-ast *soft* '(1)))))))

(deftest javascript-and-conflict-replace-ast ()
  (with-fixture javascript-ast-w-conflict
    (let ((cnf (find-if {typep _ 'conflict-ast} ; [{subtypep _ 'conflict-ast} #'type-of]
                        (asts *soft*))))
      (setf (get-ast *soft* (ast-path cnf))
            (aget :my (conflict-ast-child-alist cnf))))

    (is (equalp (mapc-ast (ast-root *soft*) #'ast-path)
                '(NIL ((1)) ((2)))))
    (is (string= (source-text (ast-root *soft*)) "topleftaright"))))
