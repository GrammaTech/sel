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


;;; Tests
;;(deftest (javascript-tree-sitter-parsing-test :long-running) ()
(deftest javascript-tree-sitter-parsing-test ()
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
             ;; TODO: it doesn't appear to have this class; is this an issue?
             #+nil js-for-of-statement
             javascript-while-statement javascript-do-statement)
            (#P"new-expression.js" javascript-new-expression)
            (#P"object-destructuring.js" javascript-object-pattern)
            (#P"object-expression.js" javascript-object)
            ;; TODO: figure out what this is supposed to be looking for.
            #+nil
            (#P"property.js" javascript-property)
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
