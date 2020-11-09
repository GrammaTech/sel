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


;;; Tests
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
               (iter (for ast-type in ast-types)
                     (is (find-if {typep _ ast-type} (genome soft)))))))
    (mapc {apply #'parse-test}
          '((#P"function-def.py" python-function-definition)
            #+nil
            (#P"async-function-def.py" python-async)
            (#P"class-def.py" python-class-definition)
            (#P"return.py" python-return-statement)
            (#P"delete.py" python-delete-statement)
            (#P"assign.py" python-assignment)
            (#P"aug-assign.py" python-augmented-assignment)
            ;; NOTE: stores annotation in the type slot.
            #+nil
            (#P"ann-assign.py" py-ann-assign)
            (#P"for.py" python-for-statement)
            #+nil
            (#P"async-for.py" py-async-for)
            (#P"while.py" python-while-statement)
            ;; Currently doesn't work with indentation.
            ;;(#P"if.py" py-if)
            (#P"with.py" python-with-statement)
            #+nil
            (#P"async-with.py" py-async-with)
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
            #+nil
            (#P"yield-from.py" py-yield-from)
            (#P"compare.py" python-comparison-operator)
            (#P"call.py" python-call)
            ;; TODO: confirm this is what's intended.
            (#P"joined-str.py" python-interpolation)
            (#P"attribute.py" python-attribute)
            (#P"starred.py" python-list-splat-pattern)
            (#P"list.py" python-list)
            (#P"tuple.py" python-tuple)
            (#P"slice.py" python-subscript python-slice)))))
