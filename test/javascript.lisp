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
   :software-evolution-library/components/file
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

(defixture multibyte-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript)
                    (javascript-dir #P"unicode/unicode.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture newlines-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript)
                    (javascript-dir #P"newlines/newlines.js"))))
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
    (is (= 41 (size *soft*)))
    (is (equal (file-to-string (javascript-dir #P"fib/fib.js"))
               (genome-string *soft*)))))

(deftest can-handle-multibyte-characters-javascript ()
  (with-fixture multibyte-javascript
    (is (= 6 (size *soft*)))
    (is (equal (file-to-string (original-path *soft*))
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

(deftest (javascript-parsing-test :long-running) ()
  (labels ((parsing-test-dir (path)
             (merge-pathnames-as-file (nest (make-pathname :directory)
                                   (append +javascript-dir+)
                                   (list "parsing"))
                             path))
           (parse-test (path &rest ast-types)
             (let ((soft (from-file (make-instance 'javascript)
                                    (parsing-test-dir path))))
               (is (not (zerop (size soft))))
               (is (equal (genome-string soft)
                          (file-to-string (original-path soft))))
               (iter (for ast-type in ast-types)
                     (is (find-if {typep _ ast-type} (genome soft)))))))
    (mapc {apply #'parse-test}
          '((#P"array-destructuring.js" js-array-pattern)
            (#P"arrow-function-expression.js" js-arrow-function-expression)
            (#P"await-expression.js" js-await-expression)
            (#P"class-declaration.js" js-class-declaration)
            (#P"class-expression.js" js-class-expression)
            (#P"conditional-expression.js" js-conditional-expression)
            (#P"debugger-statement.js" js-debugger-statement)
            (#P"empty-statement.js" js-empty-statement)
            (#P"export-specifier.js" js-export-specifier)
            (#P"expression-statement.js" js-expression-statement)
            (#P"function-declaration.js" js-function-declaration)
            (#P"function-expression.js" js-function-expression)
            (#P"if.js" js-if-statement)
            (#P"import-specifier.js" js-import-specifier)
            (#P"labeled-statement.js" js-labeled-statement)
            (#P"loops.js"
             js-for-statement js-for-in-statement js-for-of-statement
             js-while-statement js-do-while-statement)
            (#P"new-expression.js" js-new-expression)
            (#P"object-destructuring.js" js-object-pattern)
            (#P"object-expression.js" js-object-expression)
            (#P"property.js" js-property)
            (#P"sequence-expression.js" js-sequence-expression)
            (#P"spread-element.js" js-spread-element)
            (#P"switch.js" js-switch-statement)
            (#P"tagged-template-expression.js"
             js-tagged-template-expression)
            (#P"try-catch-throw.js"
             js-try-statement js-catch-clause js-throw-statement)
            (#P"with-statement.js" js-with-statement)
            (#P"yield-expression.js" js-yield-expression)))))

(deftest array-destructuring-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/array-destructuring.js"))))
    (is (equal (list "d" "c" "b" "a" "arr")
               (nest (mapcar {aget :name})
                     (get-vars-in-scope soft)
                     (stmt-starting-with-text soft "console.log("))))))

(deftest object-destructuring-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/object-destructuring.js"))))
    (is (equal (list "q" "p" "o")
               (nest (mapcar {aget :name})
                     (get-vars-in-scope soft)
                     (stmt-starting-with-text soft "console.log("))))))

(deftest ensure-do-not-duplicate-property-in-genome-string ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/object-destructuring.js"))))
    (nest (is) (string= "p") (source-text) (@ soft)
          '((js-body . 1) (js-declarations . 0) js-id (js-properties . 0)))
    (nest (is) (string= "p") (source-text) (@ soft)
          '((js-body . 1) (js-declarations . 0) js-id (js-properties . 0)))))

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
    (is (typep (stmt-with-text soft "foo") 'js-identifier))))

(deftest javascript-ast-source-ranges ()
  (with-fixture hello-world-javascript
    (is (equalp (mapcar [#'range-to-list #'cdr] (ast-source-ranges *soft*))
                '(((1 . 1) (2 . 1))
                  ((1 . 1) (2 . 1))
                  ((1 . 1) (1 . 27))
                  ((1 . 1) (1 . 12))
                  ((1 . 1) (1 . 8))
                  ((1 . 9) (1 . 12))
                  ((1 . 13) (1 . 26)))))))

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

#+broken
(deftest javascript-newline-included-in-ast-test ()
  (with-fixture newlines-javascript
    (mapc (lambda (text)
            (is (stmt-with-text *soft* text :no-error t :trim nil)
                "Failed to find ~S in ~S" text (original-path *soft*)))
          (list (format nil "import \"module-name\"~%")
                (format nil "export { f, g }~%")
                (format nil "var a = 1, b = 0, temp;~%")
                (format nil "temp = a + b // comment~%")
                (format nil "console.log(\"Hello world!\"); /* comment 2 */~%")
                (format nil "debugger;~%")
                (format nil "await f()~%")
                (format nil "temp = await f();~%")
                (format nil "f()~%")
                (format nil "return 0~%")
                (format nil "yield 0~%")))))

(defixture javascript-ast-w-conflict
  (:setup (nest (setf *soft*)
                (with (from-file (make-instance 'javascript)
                                 (javascript-dir #P"fib/fib.js"))
                      ;; The "b" in 'a = a + b'.
                      '((js-body . 0) js-body
                        (js-body . 1) js-body
                        (js-body . 1) js-expression js-right js-right))
                (flet ((js-identifier (name)
                         (make-instance 'js-identifier
                          :name name :interleaved-text (list name)))))
                ;; A new conflict AST.
                (make-instance 'conflict-ast :child-alist)
                `((:old . nil)
                  (:my . (,(js-identifier "temp")))
                  (:your . (,(js-identifier "a"))))))
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
    (is (string= "fibonacci"
                 (ast-annotation (@ *soft* '((js-body . 0) js-id)) :name)))
    (is (string= "b"
                 (ast-annotation (@ *soft* '((js-body . 0) js-body
                                             (js-body . 1) js-body (js-body . 2)
                                             js-expression js-left))
                                 :name)))
    (is (string= "temp"
                 (ast-annotation (@ *soft* '((js-body . 0) js-body
                                             (js-body . 1) js-body (js-body . 2)
                                             js-expression js-right))
                                 :name)))
    ;; Set AST with (with ...).
    (let ((path '((js-body . 0) js-body (js-body . 1) js-body (js-body . 2)
                   js-expression js-left)))
      (with *soft* path
            (make-instance 'js-identifier
             :name "RIGHT"
             :interleaved-text (list "RIGHT")))
      (is (typep (@ *soft* path) 'js-identifier))
      (is (string= "RIGHT" (ast-annotation (@ *soft* path) :name)))
      (with *soft* path
            (make-instance 'js-identifier
             :name "LEFT"
             :interleaved-text (list "LEFT")))
      (is (typep (@ *soft* path) 'js-identifier))
      (is (string= "LEFT" (ast-annotation (@ *soft* path) :name))))))

(deftest javascript-and-conflict-replace-ast ()
  (with-fixture javascript-ast-w-conflict
    (let ((cnf (find-if {typep _ 'conflict-ast} *soft*)))
      (with *soft*
            cnf
            (car (aget :my (conflict-ast-child-alist cnf)))))

    (is (equal (size *soft*) (count-if {ast-path *soft*} (genome *soft*))))
    (is (string= (source-text (@ *soft* '((js-body . 0) js-body
                                          (js-body . 1) js-body
                                          (js-body . 1) js-expression)))
                 "a = a + temp"))))

(deftest test-json-preserves-trailing-whitespace ()
  (let* ((ws (fmt "     ~%"))
         (genome (string+ "{\"x\": 1}" ws))
         (json (make-instance 'json :genome genome)))
    (is (string$= ws (genome-string json)))))

;;; Test case for BI failure
(deftest javascript-insert-test ()
  (with-fixture fib-javascript
    (let* ((s (genome *soft*)))
      (is (not (eql (slot-value s 'ft::transform) s)))
      (is (eql (length (slot-value s 'sel/sw/javascript::js-body)) 2)
          "Body had two children before insert")
      (let* ((stmt-ast (convert 'javascript-ast "j = 0"))
             (new-soft (insert s '((js-body . 1)) stmt-ast)))
        (let ((body (slot-value new-soft 'sel/sw/javascript::js-body)))
          (is (= (length body) 3) "Body has three children after insert"))))))
