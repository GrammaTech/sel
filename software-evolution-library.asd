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
(register-system-packages "eclector" '(:eclector.parse-result))

;; This one needs a defsystem to ensure fare-quasiquote-extras is loaded.
(defsystem "software-evolution-library/components/serapi-io"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  ;; This is the reason for this whole defsystem.
  :depends-on (fare-qusiquote-extras))


;;;; Tests and binaries.
(defsystem "software-evolution-library/test"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  :perform
  (test-op (o c) (symbol-call :software-evolution-library/test '#:run-batch)))

(defsystem "software-evolution-library/run-ast-diff"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Calculate difference between two programs."
  :version "0.0.0"
  :depends-on (software-evolution-library/ast-diff/commands)
  :build-operation "asdf:program-op"
  :build-pathname "bin/ast-diff"
  :entry-point "software-evolution-library/ast-diff/commands::run-ast-diff")

(defsystem "software-evolution-library/run-ast-merge"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Compute the merge of two programs that diverge from a common ancestral version."
  :version "0.0.0"
  :depends-on (software-evolution-library/ast-diff/commands)
  :build-operation "asdf:program-op"
  :build-pathname "bin/ast-merge"
  :entry-point "software-evolution-library/ast-diff/commands::run-ast-merge")

(defsystem "software-evolution-library/run-rest-server"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Calculate difference between two programs."
  :version "0.0.0"
  :depends-on (software-evolution-library/rest)
  :build-operation "asdf:program-op"
  :build-pathname "bin/rest-server"
  :entry-point "software-evolution-library/rest::run-rest-server")
