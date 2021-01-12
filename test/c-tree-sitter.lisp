;;;; c-tree-sitter.lisp --- C tree-sitter representation.
(defpackage :software-evolution-library/test/c-tree-sitter
  (:nicknames :sel/test/c-tree-sitter :sel/test/c-ts)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/test/util-clang
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-c-tree-sitter))
(in-package :software-evolution-library/test/c-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-c-tree-sitter "C tree-sitter representation."
  (c-tree-sitter-available-p))


;;; Utility
(define-constant +c-tree-sitter-dir+ (append +etc-dir+ (list "c-tree-sitter"))
  :test #'equalp
  :documentation "Path to directory holding C files for tree-sitter tests.")

(defixture unicode
  (:setup
   (setf *soft*
         (from-file (make-instance 'c)
                    (make-pathname :name "unicode"
                                   :type "c"
                                   :directory +unicode-dir+)))))


;;; Tests
(deftest comment-inheritance-works-as-expected ()
  (is (subtypep 'c-comment 'comment-ast)))

(deftest c-tree-sitter-can-parse-file ()
  (finishes
    (from-file
     (make-instance 'c)
     (make-pathname :name "test"
                    :type "c"
                    :directory +c-tree-sitter-dir+))))

(deftest c-tree-sitter-parses-with-errors ()
  "c-tree-sitter-ast parses ASTs even if there's errors."
  (let ((ast (convert 'c-ast "a = 1")))
    (is (find-if {typep _ 'c-error} ast))
    (is (find-if {typep _ 'c-assignment-expression} ast))))

(deftest c-tree-sitter-parses-from-list ()
  (let ((ast (convert
              'c-ast
              '((:class . :declaration)
                (:declarator
                 ((:class . :init-declarator)
                  (:declarator
                   (:class . :identifier)
                   (:interleaved-text "a"))
                  (:value
                   (:class . :number-literal)
                   (:interleaved-text "0"))
                  (:interleaved-text "" " = " "")))
                (:type
                 (:class . :primitive-type)
                 (:interleaved-text "int"))
                (:interleaved-text "" " " ";")))))
    (is (find-if {typep _ 'c-declaration} ast))
    (is (find-if {typep _ 'c-init-declarator} ast))
    (is (find-if {typep _ 'c-identifier} ast))
    (is (find-if {typep _ 'c-number-literal} ast))
    (is (equal "int a = 0;" (source-text ast)))))

(deftest c-tree-sitter-handles-unicode ()
  "c-ast successfully parses unicode."
  (with-fixture unicode
    (is (stmt-starting-with-text *soft* "int x = 0"))
    (is (stmt-starting-with-text *soft* "\"2 bytes: Δ\""))
    (is (stmt-starting-with-text *soft* "int y = 1"))
    (is (not (find-if {typep _ 'c-error} (genome *soft*))))))

(deftest test-c-source-ranges ()
  ;; There are a lot of C source files and parsing them is slow
  ;; so set a limit. Note the files actually tested are chosen at
  ;; random from the set of all files.
  (let ((c-files (expand-wildcard #p"*/*.c")))
    (test-ast-source-ranges-for-files
     'c c-files :limit 10 :ignore-indentation t)))

(defun parsing-test-dir (path)
  (merge-pathnames-as-file
   (make-pathname :directory (append +c-tree-sitter-dir+
                                     (list "parsing")))
   path))

(defun parse-test (path &rest ast-types)
  (let ((soft (from-file (make-instance 'c)
                         (parsing-test-dir path))))
    (is (not (zerop (size soft))))
    (is (equal (genome-string soft)
               (file-to-string (parsing-test-dir path))))
    (is (not (find-if {typep _ 'c-error} (genome soft))))
    (is (find-if
         (lambda (ast)
           (typep ast `(and ,@ast-types)))
         (genome soft))
        (format nil "Found ASTS of types (and ~{~a~^ ~})" ast-types))
    soft))

(deftest (c-tree-sitter-parsing-test :long-running) ()
  (mapc {apply #'parse-test}
        '((#P"abstract-array-declarator.c" c-abstract-array-declarator)
          (#P"abstract-pointer-declarator.c" c-abstract-pointer-declarator)
          (#P"argument-list.c" c-argument-list)
          (#P"array-declarator.c" c-array-declarator)
          (#P"assignment-expression.c" c-assignment-expression)
          (#P"attribute-specifier.c" c-attribute-specifier)
          (#P"binary-expression.c" c-binary-expression)
          (#P"bitfield-clause.c" c-bitfield-clause)
          (#P"break-statement.c" c-break-statement statement-ast)
          (#P"call-expression.c" c-call-expression)
          (#P"case-statement.c" c-case-statement statement-ast)
          (#P"cast-expression.c" c-cast-expression)
          (#P"char-literal.c" c-char-literal)
          (#P"comma-expression.c" c-comma-expression)
          (#P"compound-literal-expression.c" c-compound-literal-expression)
          (#P"compound-statement.c" c-compound-statement)
          (#P"concatenated-string.c" c-concatenated-string)
          (#P"conditional-expression.c" c-conditional-expression)
          (#P"continue-statement.c" c-continue-statement statement-ast)
          (#P"declaration.c" c-declaration)
          (#P"do-statement.c" c-do-statement statement-ast)
          (#P"enum.c" c-enum-specifier)
          (#P"enum.c" c-enumerator)
          (#P"enum.c" c-enumerator-list)
          (#P"binary-expression.c" c-expression-statement statement-ast)
          (#P"bitfield-clause.c" c-field-declaration)
          (#P"bitfield-clause.c" c-field-declaration-list)
          (#P"field-designator.c" c-field-designator)
          (#P"field-expression.c" c-field-expression)
          (#P"for-statement.c" c-for-statement statement-ast)
          (#P"function-declarator.c" c-function-declarator)
          (#P"function-definition.c" c-function-definition statement-ast)
          (#P"goto-statement.c" c-goto-statement)
          (#P"if-statement.c" c-if-statement statement-ast)
          (#P"field-designator.c" c-init-declarator)
          (#P"field-designator.c" c-initializer-list)
          (#P"field-designator.c" c-initializer-pair)
          (#P"goto-statement.c" c-labeled-statement statement-ast)
          (#P"function-definition.c" c-parameter-declaration)
          (#P"function-definition.c" c-parameter-list)
          (#P"parenthesized-declarator.c" c-parenthesized-declarator)
          (#P"parenthesized-expression.c" c-parenthesized-expression)
          (#P"pointer-declarator.c" c-pointer-declarator)
          (#P"pointer-expression.c" c-pointer-expression)
          ;; NOTE: currently an issue with having just a #define alone
          ;;       in a file. This is probably a tree-sitter issue?
          (#P"preproc-def.c" c-preproc-def)
          (#P"preproc-defined.c" c-preproc-defined)
          (#P"preproc-if.c" c-preproc-elif)
          (#P"preproc-if.c" c-preproc-else)
          (#P"preproc-function-def.c" c-preproc-function-def)
          (#P"preproc-if.c" c-preproc-if)
          (#P"preproc-ifdef.c" c-preproc-ifdef)
          (#P"preproc-include.c" c-preproc-include)
          (#P"preproc-function-def.c" c-preproc-params)
          (#P"function-definition.c" c-return-statement statement-ast)
          (#P"sized-type-specifier.c" c-sized-type-specifier)
          (#P"sizeof-expression.c" c-sizeof-expression)
          (#P"storage-class-specifier.c" c-storage-class-specifier)
          (#P"concatenated-string.c" c-string-literal)
          (#P"bitfield-clause.c" c-struct-specifier)
          (#P"subscript-designator.c" c-subscript-designator)
          (#P"subscript-expression.c" c-subscript-expression)
          (#P"case-statement.c" c-switch-statement statement-ast)
          (#P"type-definition.c" c-type-definition)
          (#P"cast-expression.c" c-type-descriptor)
          (#P"unary-expression.c" c-unary-expression)
          (#P"union-specifier.c" c-union-specifier)
          (#P"update-expression.c" c-update-expression)
          (#P"continue-statement.c" c-while-statement statement-ast))))

(defixture factorial.c
  (:setup (setf *soft* (from-file (make-instance 'c)
                                  (asdf:system-relative-pathname
                                   :software-evolution-library
                                   "test/etc/factorial.c"))))
  (:teardown (setf *soft* nil)))

(deftest test-comments-for ()
  (is (= 3 (length (comments-for *soft* (find-if {typep _ 'c-while-statement} *soft*)))))
  (is (= 1 (length (comments-for *soft* (stmt-starting-with-text *soft* "printf"))))))
