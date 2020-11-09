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

(defmacro with-software-file ((directory filename software-var genome-var)
                              &body body)
  `(let* ((,software-var (from-file
                          (make-instance 'c)
                          (make-pathname :name ,filename
                                         :type "c"
                                         :directory ,directory)))
          (,genome-var (genome ,software-var)))
     (declare (ignorable ,genome-var))
     ,@body))


;;; Tests
(deftest c-tree-sitter-can-parse-file ()
  (finishes
    (with-software-file (+c-tree-sitter-dir+ "test" software genome))))

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
    (is (find-if {typep _'c-declaration} ast))
    (is (find-if {typep _ 'c-init-declarator} ast))
    (is (find-if {typep _ 'c-identifier} ast))
    (is (find-if {typep _ 'c-number-literal} ast))
    (is (equal "int a = 0;" (source-text ast)))))

(deftest c-tree-sitter-handles-unicode ()
  "c-tree-sitter-ast successfully parses unicode."
  (with-software-file (+unicode-dir+ "unicode" software genome)
    (is (stmt-starting-with-text software "int x = 0"))
    (is (stmt-starting-with-text software "\"2 bytes: Δ\""))
    (is (stmt-starting-with-text software "int y = 1"))
    (is (not (find-if {typep _ 'c-error} genome)))))

;;; TODO: something similar to Python's parsing test.
(deftest c-tree-sitter-parsing-test ()
  (labels ((parsing-test-dir (path)
             (merge-pathnames-as-file
              (make-pathname :directory (append +c-tree-sitter-dir+
                                                (list "parsing")))
              path))
           (parse-test (path &rest ast-types)
             (let ((soft (from-file (make-instance 'c)
                                    (parsing-test-dir path))))
               (is (not (zerop (size soft))))
               (is (equal (genome-string soft)
                          (file-to-string (parsing-test-dir path))))
               (is (not (find-if {typep _ 'c-error} (genome soft))))
               (is (find-if
                    (lambda (ast)
                      (typep ast `(and ,@ast-types)))
                    (genome soft))))))
    (mapc {apply #'parse-test}
          ;; TODO: might also need to check that statements have newlines on the
          ;;       ends? Maybe this is a different test?
          ;; TODO: maybe add other super classes to (functions, expressions,
          ;;       etc.) for the list of injected super classes and test here?
          ;; TODO: add more files?
          '((#P"function-definition.c" c-function-definition statement)
            (#P"if-statement.c" c-if-statement statement)))))
