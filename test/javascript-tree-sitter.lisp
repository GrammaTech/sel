;;;; javascript-tree-sitter.lisp --- Javascript tree-sitter representation.
(defpackage :software-evolution-library/test/javascript-tree-sitter
  (:nicknames :sel/test/javascript-tree-sitter :sel/test/js-ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-javascript-tree-sitter))
(in-package :software-evolution-library/test/javascript-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-javascript-tree-sitter "Javascript tree-sitter representation."
  (javascript-tree-sitter-available-p))


;;; Utility
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

(defixture surrogate-javascript
    (:setup
     (setf *soft*
           (from-file (make-instance 'javascript)
                      (javascript-dir #P"unicode/unicode-offsets.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture newlines-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript)
                    (javascript-dir #P"newlines/newlines.js"))))
  (:teardown
   (setf *soft* nil)))


;;; Tests
;;; TODO: ask about this.
#+nil
(deftest test-js-string-length ()
  (= 24 (js-string-length "̉mủt̉ả̉̉̉t̉ẻd̉W̉ỏ̉r̉̉d̉̉")))

#+nil
(deftest test-lone-surrogate-error ()
  "Test that a malformed surrogate pair in a string literal, where
  what was intended to be a low surrogate is still within the range
  for a valid \"lone surrogate\" (which is valid JavaScript but not
  valid JSON), signals the right error."
  (signals mutate
           (genome
            (from-string (make 'javascript)
                         "\"\\ud800-\\udbff\""))))

(deftest javascript-simply-able-to-load-a-software-object ()
  (with-fixture hello-world-javascript
    (is (not (null *soft*)))))

(deftest (javascript-can-parse-a-software-object :long-running) ()
  (with-fixture hello-world-javascript
    (is (= 7 (size *soft*)))
    (is (equal (file-to-string (javascript-dir #P"hello-world/hello-world.js"))
               (genome-string *soft*))))
  (with-fixture fib-javascript
    (is (= 44 (size *soft*)))
    (is (equal (file-to-string (javascript-dir #P"fib/fib.js"))
               (genome-string *soft*)))))

(deftest javascript-can-handle-multibyte-characters ()
  (with-fixture multibyte-javascript
    (is (= 7 (size *soft*)))
    (is (equal* (file-to-string (original-path *soft*))
                (genome-string *soft*)
                (source-text (genome *soft*))))))

(deftest javascript-can-handle-surrogate-characters ()
  (with-fixture surrogate-javascript
    (is (equal* (file-to-string (original-path *soft*))
                (genome-string *soft*)
                (source-text (genome *soft*))))))

(deftest javascript-cut-shortens-a-software-object ()
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

(deftest javascript-insert-lengthens-a-software-object ()
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

(deftest javascript-swap-changes-a-software-object ()
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

;;;(deftest javascript-copies-are-independent ()
(deftest js-copies-are-independent ()
  (with-fixture fib-javascript
    (let ((orig-genome-string (genome-string *soft*))
          (variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                        :targets (list (cons :stmt1 stmt1))))
      (is (string= (genome-string *soft*) orig-genome-string))
      (is (not (string= (genome-string variant) orig-genome-string))))))

;;;(deftest javascript-copies-share-asts ()
(deftest js-copies-share-asts ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*)))
      (is (eq (genome *soft*) (genome variant)))
      (is (> (size variant) 0)))))

;;;(deftest javascript-mutation-preserves-unmodified-subtrees ()
(deftest js-mutation-preserves-unmodified-subtrees ()
  (with-fixture fib-javascript
    (let ((variant (copy *soft*))
          (stmt1 (stmt-with-text *soft* "temp = a;")))
      (apply-mutation variant
                      (make-instance 'parseable-cut
                                     :targets (list (cons :stmt1 stmt1))))
      (is (equal? (stmt-with-text *soft* "num--;")
                  (stmt-with-text variant "num--;"))))))

;;;(deftest javascript-convert-source-snippet-works ()
(deftest js-convert-source-snippet-works ()
  (let ((ast (convert 'javascript-ast "j = 0")))
    (is (typep (ast-hash ast) '(integer 0)))
    (is (equal 5 (size ast)))
    (is (equal "j = 0" (source-text ast)))
    (is (find-if {typep _ 'javascript-expression-statement} ast))))

(deftest (javascript-can-format-a-software-object :long-running) ()
  (with-fixture fib-javascript
    (is (not (string= (genome-string (copy *soft*))
                      (genome-string (prettier (copy *soft*))))))
    (is (not (string= (genome-string (copy *soft*))
                      (genome-string (format-genome (copy *soft*))))))
    (is (string= (genome-string (prettier (copy *soft*)))
                 (genome-string (format-genome (copy *soft*)))))))

;;;(deftest javascript-can-rebind-vars ()
(deftest js-can-rebind-vars ()
  (with-fixture fib-javascript
    (let ((rebound (rebind-vars (stmt-with-text *soft* "temp = a;")
                                (list (list "a" "b"))
                                nil)))
      (is (string= "temp = b;" (trim-whitespace (source-text rebound)))
          "`rebind-vars` did not properly rebind 'a' to 'b'")
      (is (find-if [{equal "b"} #'source-text] rebound)
          "`rebind-vars` did not properly rebind 'a' to 'b'")))
  (with-fixture fib-javascript
    (let ((rebound (rebind-vars (stmt-with-text *soft* "fibonacci(10);")
                                (list (list "b" "G19"))
                                nil)))
      (is (string= "fibonacci(10);" (trim-whitespace (source-text rebound)))
          "`rebind-vars` improperly rebound 'b' to 'G19' in 'fibonacci(10)'")
      (is (null (find-if [{equal "b"} #'source-text] rebound))
          "`rebind-vars` improperly rebound 'b' to 'G19' in 'fibonacci(10)'"))))

;;;(deftest javascript-get-vars-in-scope ()
(deftest js-get-vars-in-scope ()
  (with-fixture fib-javascript
    (is (equal (list "temp" "b" "a" "num")
               (nest (mapcar {aget :name})
                     (get-vars-in-scope *soft*)
                     (stmt-with-text *soft* "temp = a;"))))))

;;;(deftest javascript-get-unbound-vals ()
(deftest js-get-unbound-vals ()
  (with-fixture fib-javascript
    (is (equal `((:name . "temp") (:name . "a"))
               (nest (get-unbound-vals *soft*)
                     (stmt-with-text *soft* "temp = a;"))))))

;;;(deftest javascript-get-unbound-funs ()
(deftest js-get-unbound-funs ()
  (with-fixture fib-javascript
    (is (equal `(("fibonacci" nil nil 1))
               (nest (get-unbound-funs *soft*)
                     (stmt-with-text *soft* "fibonacci(10);"))))))

(deftest javascript-array-destructuring-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/array-destructuring.js"))))
    (is (equal (list "d" "c" "b" "a" "arr")
               (nest (mapcar {aget :name})
                     (get-vars-in-scope soft)
                     (stmt-starting-with-text soft "console.log("))))))

(deftest javascript-object-destructuring-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/object-destructuring.js"))))
    (is (equal (list "q" "p" "o")
               (nest (mapcar {aget :name})
                     (get-vars-in-scope soft)
                     (stmt-starting-with-text soft "console.log("))))))

;;; FIXME
#+nil
(deftest javascript-ensure-do-not-duplicate-property-in-genome-string ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/object-destructuring.js"))))
    (nest (is) (string= "p") (source-text) (@ soft)
          '((js-body . 1) (js-declarations . 0) js-id (js-properties . 0)))
    (nest (is) (string= "p") (source-text) (@ soft)
          '((js-body . 1) (js-declarations . 0) js-id (js-properties . 0)))))

(deftest javascript-for-in-loop-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/loops.js"))))
    (is (find "i" (nest (mapcar {aget :name})
                        (get-vars-in-scope soft)
                        (stmt-with-text soft "console.log(arr[i]);"))
              :test #'equal))))

(deftest javascript-for-of-loop-get-vars-in-scope-test ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/loops.js"))))
    (is (find "val" (nest (mapcar {aget :name})
                          (get-vars-in-scope soft)
                          (stmt-with-text soft "console.log(val);"))
              :test #'equal))))

(deftest javascript-function-identifier-ast-present ()
  (let ((soft (from-file (make-instance 'javascript)
                         (javascript-dir #P"parsing/function-declaration.js"))))
    (is (stmt-with-text soft "foo"))
    (is (typep (stmt-with-text soft "foo") 'javascript-identifier))))

;;;(deftest javascript-ast-source-ranges ()
;;; FIXME
#+nil
(deftest js-ast-source-ranges ()
  (with-fixture hello-world-javascript
    (is (equalp (mapcar [#'range-to-list #'cdr] (ast-source-ranges *soft*))
                '(((1 . 1) (2 . 1))
                  ((1 . 1) (2 . 1))
                  ((1 . 1) (1 . 27))
                  ((1 . 1) (1 . 12))
                  ((1 . 1) (1 . 8))
                  ((1 . 9) (1 . 12))
                  ((1 . 13) (1 . 26)))))))

;;; FIXME: fix all of these js.newline.post-processing tests.
;;;(deftest javascript.newline.post-processing.1 ()
#+nil
(deftest js.newline.post-processing.1 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "") nil)
      "position-after-leading-newline on empty string"))

;;;(deftest javascript.newline.post-processing.2 ()
#+nil
(deftest js.newline.post-processing.2 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "x") nil)
      "position-after-leading-newline on string with no whitespace or newline"))

;;;(deftest javascript.newline.post-processing.3 ()
#+nil
(deftest js.newline.post-processing.3 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "   ") nil)
      "position-after-leading-newline on string with whitespace only, no newline"))

;;;(deftest javascript.newline.post-processing.4 ()
#+nil
(deftest js.newline.post-processing.4 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline " x") nil)
      "position-after-leading-newline on string with whitespace, no newline"))

;;;(deftest javascript.newline.post-processing.5 ()
#+nil
(deftest js.newline.post-processing.5 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline (string #\Newline)) 1)
      "position-after-leading-newline on newline"))

;;;(deftest javascript.newline.post-processing.6 ()
#+nil
(deftest js.newline.post-processing.6 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               (concatenate 'string (string #\Newline) "x"))
              1)
      "position-after-leading-newline on newline + other stuff"))

;;;(deftest javascript.newline.post-processing.7 ()
#+nil
(deftest js.newline.post-processing.7 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               (concatenate 'string (string #\Newline) "// foo "))
              1)
      "position-after-leading-newline on newline, comment"))

;;;(deftest javascript.newline.post-processing.8 ()
#+nil
(deftest js.newline.post-processing.8 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               (concatenate 'string  "// foo " (string #\Newline) "   "))
              8)
      "position-after-leading-newline on comment, newline "))

;;;(deftest javascript.newline.post-processing.9 ()
#+nil
(deftest js.newline.post-processing.9 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline
               "  // foo ")
              nil)
      "position-after-leading-newline on comment"))

;;;(deftest javascript.newline.post-processing.10 ()
#+nil
(deftest js.newline.post-processing.10 ()
  (is (equalp (sel/sw/javascript::position-after-leading-newline "/")
              nil)
      "position-after-leading-newline slash at EOL not a comment"))

;;;(deftest javascript.newline.post-processing.11 ()
#+nil
(deftest js.newline.post-processing.11 ()
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

;;; FIXME
#+nil
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

;;; FIXME
#+nil
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

;;; FIXME
#+nil
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

;;; Test case for BI failure
;;;(deftest javascript-insert-test ()
;;; FIXME
#+nil
(deftest js-insert-test ()
  (with-fixture fib-javascript
    (let* ((s (genome *soft*)))
      (is (not (eql (slot-value s 'ft::transform) s)))
      (is (eql (length (slot-value s 'sel/sw/javascript::js-body)) 2)
          "Body had two children before insert")
      (let* ((stmt-ast (convert 'javascript-ast "j = 0"))
             (new-soft (insert s '((js-body . 1)) stmt-ast)))
        (let ((body (slot-value new-soft 'sel/sw/javascript::js-body)))
          (is (= (length body) 3) "Body has three children after insert"))))))


;;; Tests
(deftest (javascript-tree-sitter-parsing-test :long-running) ()
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
               (is (not (find-if {typep _ 'javascript-error} (genome soft))))
               (iter (for ast-type in ast-types)
                     (is (find-if {typep _ ast-type} (genome soft)))))))
    (mapc {apply #'parse-test}
          '((#P"array-destructuring.js" javascript-array-pattern)
            (#P"arrow-function-expression.js" javascript-arrow-function)
            (#P"await-expression.js" javascript-await-expression)
            (#P"class-declaration.js" javascript-class-declaration)
            (#P"class-expression.js" javascript-class)
            (#P"conditional-expression.js" javascript-ternary-expression)
            (#P"debugger-statement.js" javascript-debugger-statement)
            (#P"empty-statement.js" javascript-empty-statement)
            (#P"export-specifier.js" javascript-export-specifier)
            (#P"expression-statement.js" javascript-expression-statement)
            (#P"function-declaration.js" javascript-function-declaration)
            (#P"function-expression.js" javascript-function)
            (#P"if.js" javascript-if-statement)
            (#P"import-specifier.js" javascript-import-specifier)
            (#P"labeled-statement.js" javascript-labeled-statement)
            (#P"loops.js"
             javascript-for-statement javascript-for-in-statement
             ;; NOTE: the 'of' in the for-of is in the interleaved-text
             javascript-while-statement javascript-do-statement)
            (#P"new-expression.js" javascript-new-expression)
            (#P"object-destructuring.js" javascript-object-pattern)
            (#P"object-expression.js" javascript-object)
            (#P"property.js" javascript-pair)
            (#P"sequence-expression.js" javascript-sequence-expression)
            (#P"spread-element.js" javascript-spread-element)
            (#p"super.js" javascript-super)
            (#P"switch.js" javascript-switch-statement)
            (#P"tagged-template-expression.js"
             javascript-template-string)
            (#P"try-catch-throw.js"
             javascript-try-statement javascript-catch-clause
             javascript-throw-statement)
            (#P"with-statement.js" javascript-with-statement)
            (#P"yield-expression.js" javascript-yield-expression)))))
