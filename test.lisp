;;; test.lisp --- tests for the `software-evolution-library' package
(defpackage :software-evolution-library/test
  (:nicknames :sel/test)
  (:use :common-lisp
        :named-readtables
        :curry-compose-reader-macros
        #+gt :testbot
        :stefil+
        :software-evolution-library/test/adaptive-mutation
        :software-evolution-library/test/all-tree-sitter
        :software-evolution-library/test/ancestral-git
        :software-evolution-library/test/asm-super-mutant
        :software-evolution-library/test/asm
        :software-evolution-library/test/bear
        :software-evolution-library/test/c-tree-sitter
        :software-evolution-library/test/cpp-tree-sitter
        :software-evolution-library/test/cl-tree-sitter
        :software-evolution-library/test/c-project
        :software-evolution-library/test/clang-ancestry
        :software-evolution-library/test/clang-crossover
        :software-evolution-library/test/clang-expression
        :software-evolution-library/test/clang-mutations
        :software-evolution-library/test/clang-project
        :software-evolution-library/test/compilation-database-project
        :software-evolution-library/test/clang-scopes-and-types
        :software-evolution-library/test/clang-super-mutants
        :software-evolution-library/test/clang-syntactic-contexts
        :software-evolution-library/test/clang-tokenizer
        :software-evolution-library/test/clang-utility
        :software-evolution-library/test/clang-w-fodder
        :software-evolution-library/test/clang
        :software-evolution-library/test/command-line
        :software-evolution-library/test/components
        :software-evolution-library/test/configuration
        :software-evolution-library/test/conflict-ast
        :software-evolution-library/test/coq
        :software-evolution-library/test/cpp-scan
        :software-evolution-library/test/cpp-project
        :software-evolution-library/test/database
        :software-evolution-library/test/declaration-type-databases
        :software-evolution-library/test/diff
        :software-evolution-library/test/directory
        :software-evolution-library/test/fix-compilation
        :software-evolution-library/test/go-tree-sitter
        :software-evolution-library/test/indentation
        :software-evolution-library/test/java-project
        :software-evolution-library/test/java-tree-sitter
        :software-evolution-library/test/javascript-project
        :software-evolution-library/test/javascript-tree-sitter
        :software-evolution-library/test/typescript-tree-sitter
        :software-evolution-library/test/json
        :software-evolution-library/test/lisp
        :software-evolution-library/test/lisp-bindings
        :software-evolution-library/test/misc-mutations
        :software-evolution-library/test/mutation-analysis
        :software-evolution-library/test/python-tree-sitter
        :software-evolution-library/test/python-project
        :software-evolution-library/test/parseable
        :software-evolution-library/test/population
        :software-evolution-library/test/range-representation
        :software-evolution-library/test/rest
        :software-evolution-library/test/rust-project
        :software-evolution-library/test/rust-tree-sitter
        :software-evolution-library/test/selection
        :software-evolution-library/test/serapi
        :software-evolution-library/test/sexp
        :software-evolution-library/test/simple
        :software-evolution-library/test/style-features
        :software-evolution-library/test/task-runner
        :software-evolution-library/test/template
        :software-evolution-library/test/tree-sitter
        :software-evolution-library/test/utility)
  #+gt (:shadowing-import-from :testbot :batch-test)
  (:import-from :software-evolution-library
                :+software-evolution-library-branch+)
  (:import-from :software-evolution-library/test/util :test)
  (:export :test :batch-test :testbot-test))
(in-package :software-evolution-library/test)
(in-readtable :curry-compose-reader-macros)
