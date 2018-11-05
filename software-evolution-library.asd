(defsystem "software-evolution-library"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "programmatic modification and evaluation of extant software"
  :long-description "A common interface abstracts over multiple
types of software objects including abstract syntax trees parsed from
source code, LLVM IR, compiled assembler, and linked ELF binaries.
Mutation and evaluation methods are implemented on top of this
interface supporting Search Based Software Engineering (SBSE)
techniques."
  :version "0.0.0"
  :depends-on (software-evolution-library/software-evolution-library)
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :in-order-to ((test-op (test-op "software-evolution-library/test"))))

(register-system-packages
 "software-evolution-library/software-evolution-library"
 '(:software-evolution-library))
(register-system-packages "ast-diff/ast-diff" '(:ast-diff))


;;;; Tests and binaries.
(defsystem "software-evolution-library/test"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  :perform
  (test-op (o c) (symbol-call :software-evolution-library/test '#:run-batch)))

(defsystem "software-evolution-library/run-clang-diff"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Calculate difference between two C/C++ programs."
  :version "0.0.0"
  :depends-on (software-evolution-library/ast-diff/clang)
  :build-operation "asdf:program-op"
  :build-pathname "bin/clang-diff"
  :entry-point "software-evolution-library/ast-diff/clang::run-clang-diff")

(defsystem "software-evolution-library/run-lisp-diff"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Calculate difference between two Lisp programs."
  :version "0.0.0"
  :depends-on (software-evolution-library/ast-diff/lisp)
  :build-operation "asdf:program-op"
  :build-pathname "bin/lisp-diff"
  :entry-point "software-evolution-library/ast-diff/lisp::run-lisp-diff")
