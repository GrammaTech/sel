;;; test.lisp --- tests for the `software-evolution-library' package
(defpackage :software-evolution-library/test
  (:nicknames :sel/test)
  (:use :common-lisp
        :named-readtables
        :curry-compose-reader-macros
        #+gt :testbot
        :software-evolution-library/stefil-plus
        :software-evolution-library/test/adaptive-mutation
        :software-evolution-library/test/asm-super-mutant
        :software-evolution-library/test/asm
        :software-evolution-library/test/bear
        :software-evolution-library/test/clang-ancestry
        :software-evolution-library/test/clang-crossover
        :software-evolution-library/test/clang-expression
        :software-evolution-library/test/clang-mutations
        :software-evolution-library/test/clang-project
        :software-evolution-library/test/clang-scopes-and-types
        :software-evolution-library/test/clang-super-mutants
        :software-evolution-library/test/clang-syntactic-contexts
        :software-evolution-library/test/clang-tokenizer
        :software-evolution-library/test/clang-utility
        :software-evolution-library/test/clang-w-fodder
        :software-evolution-library/test/clang
        :software-evolution-library/test/command-line
        :software-evolution-library/test/components
        :software-evolution-library/test/conflict-ast
        :software-evolution-library/test/coq
        :software-evolution-library/test/cpp-scan
        :software-evolution-library/test/csurf-asm-ancestry
        :software-evolution-library/test/csurf-asm
        :software-evolution-library/test/database
        :software-evolution-library/test/declaration-type-databases
        :software-evolution-library/test/diff
        :software-evolution-library/test/elf
        :software-evolution-library/test/fix-compilation
        :software-evolution-library/test/javascript-project
        :software-evolution-library/test/javascript
        :software-evolution-library/test/lisp
        :software-evolution-library/test/lisp-bindings
        :software-evolution-library/test/python
        :software-evolution-library/test/python-project
        :software-evolution-library/test/misc-mutations
        :software-evolution-library/test/mutation-analysis
        :software-evolution-library/test/population
        :software-evolution-library/test/range-representation
        :software-evolution-library/test/rest
        :software-evolution-library/test/selection
        :software-evolution-library/test/serapi
        :software-evolution-library/test/sexp
        :software-evolution-library/test/style-features
        :software-evolution-library/test/task-runner
        :software-evolution-library/test/type-traces
        :software-evolution-library/test/utility)
  (:import-from :software-evolution-library
                :+software-evolution-library-branch+)
  (:import-from :software-evolution-library/test/util :test)
  (:export :test :batch-test :testbot-test))
(in-package :software-evolution-library/test)
(in-readtable :curry-compose-reader-macros)

#-gt
(progn
  ;;; External replacement for GT-specific test submission helpers
  (defvar *success* nil "Variable indicating test success or failure.")
  (defun batch-test (test project branch &optional args)
    "Run tests in 'batch' mode, printing results as a string."
    (declare (ignorable project branch args))

    (let* ((stefil::*test-progress-print-right-margin* (expt 2 20))
           (failures (coerce (stefil::failure-descriptions-of
                              (without-debugging (funcall test)))
                             'list)))
      (setf *success*
            (if failures
                (prog1 nil
                  (format *error-output* "FAILURES~%")
                  (mapc [{format *error-output* "  ~a~%"}
                         #'stefil::name-of
                         #'stefil::test-of
                         #'car #'stefil::test-context-backtrace-of]
                        failures))
                (prog1 t
                  (format *error-output* "SUCCESS~%")))))))

(defun run-batch (&rest a)
  (declare (ignorable a))
  #+ccl (setf ccl::*interactive-streams-initialized* nil)
  (setf sel/stefil+:*long-tests* t)
  (batch-test #'test "SEL" +software-evolution-library-branch+))
