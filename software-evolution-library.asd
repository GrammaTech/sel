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
(register-system-packages "eclector" '(:eclector.parse-result))

;; This one needs a defsystem to ensure fare-quasiquote-extras is loaded.
(defsystem "software-evolution-library/components/serapi-io"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  ;; This is the reason for this whole defsystem.
  :depends-on (fare-quasiquote-extras))


;;;; Tests and binaries.
(defsystem "software-evolution-library/test"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  :perform
  (test-op (o c) (symbol-call :software-evolution-library/test '#:run-batch)))

(defsystem "software-evolution-library/run-rest-server"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Calculate difference between two programs."
  :version "0.0.0"
  :depends-on (software-evolution-library/rest)
  :build-operation "asdf:program-op"
  :build-pathname "bin/rest-server"
  :entry-point "software-evolution-library/rest::run-rest-server")

(defsystem "software-evolution-library/run-new-clang-round-trip"
    :author "Eric Schulte and GrammaTech"
    :licence "GPL V3"
    :description "Test harness for clang parser/unparser"
    :version "0.0.0"
    :depends-on (software-evolution-library/components/new-clang-round-trip)
    :build-operation "asdf:program-op"
    :build-pathname "test/commands/new-clang-round-trip"
    :entry-point "software-evolution-library/components/new-clang-round-trip::run-new-clang-round-trip")

(defsystem "software-evolution-library/run-clang-diff-test"
    :author "Eric Schulte and GrammaTech"
    :licence "GPL V3"
    :description "Differential tester for old/new clang front ends"
    :version "0.0.0"
    :depends-on (software-evolution-library/components/clang-diff-test)
    :build-operation "asdf:program-op"
    :build-pathname "test/commands/clang-diff-test"
    :entry-point "software-evolution-library/components/clang-diff-test::run-clang-diff-test")

(defsystem "software-evolution-library/run-dump-store"
    :author "Eric Schulte and GrammaTech"
    :licence "GPL V3"
    :description "Dump store file for a program"
    :version "0.0.0"
    :depends-on (software-evolution-library/components/dump-store)
    :build-operation "asdf:program-op"
    :build-pathname "bin/dump-store"
    :entry-point "software-evolution-library/components/dump-store::run-dump-store")
