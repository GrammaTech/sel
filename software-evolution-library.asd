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
  :in-order-to ((test-op (load-op "software-evolution-library/test")))
  :perform
  (test-op (o c) (symbol-call :software-evolution-library/test '#:run-batch)))

(register-system-packages
 "software-evolution-library/software-evolution-library"
 '(:software-evolution-library))
(register-system-packages "eclector" '(:eclector.parse-result
                                       :eclector.readtable
                                       :eclector.reader))

;; This one needs a defsystem to ensure fare-quasiquote-extras is loaded.
(defsystem "software-evolution-library/components/serapi-io"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  ;; This is the reason for this whole defsystem.
  :depends-on (fare-quasiquote-extras)
  :components ((:file "components/serapi-io")))

(defsystem "software-evolution-library/terminal"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Primitives for inspecting and working with a terminal."
  :depends-on (:gt :cffi :cl-interpol)
  :defsystem-depends-on (:cffi-grovel)
  :serial t
  :components ((:file "terminal-package")
               (:cffi-grovel-file "terminal-grovel")
               (:file "terminal-impl")))


;;;; Tests and binaries.

(defsystem "software-evolution-library/run-tree-sitter-interface"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Command-line interface to SEL's tree-sitter ASTs."
  :version "0.0.0"
  :depends-on (software-evolution-library/python/tree-sitter-interface)
  :build-operation "asdf:program-op"
  :build-pathname "bin/tree-sitter-interface"
  :entry-point "software-evolution-library/python/tree-sitter-interface:run-tree-sitter-interface")

(defsystem "software-evolution-library/deploy-tree-sitter-interface"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Command-line interface to SEL's tree-sitter ASTs.
Deployed as a stand-alone bundle with all libraries required by the
executable including the including all tree-sitter libraries."
  :version "0.0.0"
  :defsystem-depends-on (:deploy)
  :depends-on (software-evolution-library/python/tree-sitter-interface)
  :build-operation "deploy-op"
  :build-pathname "../python/asts/tree-sitter-interface"
  :entry-point "software-evolution-library/python/tree-sitter-interface:run-tree-sitter-interface")

(defsystem "software-evolution-library/run-test-parse"
  :author "Paul Diet and GrammaTech"
  :license "GPL V3"
  :description "Test CLI for use with creduce"
  :depends-on (software-evolution-library/components/test-parse)
  :build-operation "asdf:program-op"
  :build-pathname "bin/test-parse"
  :entry-point "software-evolution-library/components/test-parse:run-test-parse")

(defsystem "software-evolution-library/run-rest-server"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Run a REST server exposing SEL APIs."
  :version "0.0.0"
  :depends-on (software-evolution-library/rest)
  :build-operation "asdf:program-op"
  :build-pathname "bin/rest-server"
  :entry-point "software-evolution-library/rest::run-rest-server")

(defsystem "software-evolution-library/run-dump-store"
    :author "Eric Schulte and GrammaTech"
    :licence "GPL V3"
    :description "Dump store file for a program"
    :version "0.0.0"
    :depends-on (software-evolution-library/components/dump-store)
    :build-operation "asdf:program-op"
    :build-pathname "bin/dump-store"
    :entry-point "software-evolution-library/components/dump-store::run-dump-store")
