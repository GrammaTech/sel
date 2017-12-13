(defsystem "software-evolution-library"
  :description "programmatic modification and evaluation of extant software"
  :long-description "A common interface abstracts over multiple
types of software objects including abstract syntax trees parsed from
source code, LLVM IR, compiled assembler, and linked ELF binaries.
Mutation and evaluation methods are implemented on top of this
interface supporting Search Based Software Engineering (SBSE)
techniques."
  :version "0.0.0"
  :licence "GPL V3"
  :build-operation "asdf:program-op"
  :build-pathname "bin/clang-instrument"
  :entry-point "software-evolution-library::main"
  :in-order-to ((test-op (test-op "software-evolution-library/test")))
  ;; :homepage "http://GrammaTech.github.io/sel"
  :depends-on (alexandria
               closer-mop
               uiop
               bordeaux-threads
               cl-arrows
               cl-custom-hash-table
               cl-json
               cl-ppcre
               cl-fad
               curry-compose-reader-macros
               diff
               elf
               iterate
               metabang-bind
               software-evolution-library/utility
               split-sequence
               usocket
               trivial-utf-8
               fast-io
               trace-db)
  :in-order-to ((test-op (test-op software-evolution-library/test)))
  :components
  ((:module base
            :pathname ""
            :components
            ((:file "package")
             (:file "software-evolution-library" :depends-on ("package"))))
   (:module software
            :depends-on (base)
            :pathname "software"
            :components
            ((:file "lisp")
             (:file "expression" :depends-on ("lisp"))
             (:file "simple")
             (:file "diff" :depends-on ("simple"))
             (:file "asm"  :depends-on ("simple"))
             (:file "csurf-asm" :depends-on ("asm"))
             (:file "elf"  :depends-on ("diff"))
             (:file "elf-cisc" :depends-on ("elf"))
             (:file "elf-risc" :depends-on ("elf"))
             (:file "elf-mips" :depends-on ("elf-risc"))
             (:file "ast")
             (:file "cil" :depends-on ("ast"))
             (:file "clang" :depends-on ("ast"))
             (:file "clang-expression" :depends-on ("clang" "expression"))
             (:file "clang-w-fodder" :depends-on ("clang"))
             (:file "llvm" :depends-on ("ast"))
             (:file "project")
             (:file "clang-project" :depends-on ("project" "clang"))))
   (:module src
            :depends-on (base software)
            :pathname "src"
            :components
            ((:file "ancestral")
             (:file "clang-instrument")
             (:file "traceable" :depends-on ("test-suite"))
             (:file "fix-compilation")
             (:file "adaptive-mutation")
             (:file "searchable")
             (:file "fodder-database" :depends-on ("searchable"))
             (:file "in-memory-fodder-database" :depends-on ("fodder-database"))
             (:file "json-fodder-database" :depends-on ("in-memory-fodder-database"))
             (:file "lexicase")
             (:file "pliny-fodder-database" :depends-on ("fodder-database"))
             (:file "test-suite")
             (:file "condition-synthesis" :depends-on ("test-suite"))
             (:file "fault-loc" :depends-on ("test-suite"))
             (:file "generate-helpers")
             (:file "style-features")
             (:file "multi-objective")
             (:file "clang-tokens")))))

(defsystem "software-evolution-library/clang-instrument"
  :description "Compiled clang-instrument binary from SEL."
  :build-operation "asdf:program-op"
  :build-pathname "bin/clang-instrument"
  :entry-point "software-evolution-library::run-clang-instrument")


;;;; Tests.
(defsystem "software-evolution-library/test"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               closer-mop
               cl-arrows
               curry-compose-reader-macros
               cxml
               iterate
               metabang-bind
               software-evolution-library
               software-evolution-library/utility
               split-sequence
               stefil
               uuid
               testbot
               trace-db
               trivial-shell
               uiop)
  :components
  ((:module test
            :pathname "test/src"
            :components
            ((:file "package")
             (:file "software-evolution-library-test"
                    :depends-on ("package")))))
  :perform (test-op (o c)
                    (symbol-call :software-evolution-library/test '#:test)))

(defsystem "software-evolution-library/run-test"
  :description "Compiled basic test binary for SEL."
  :build-operation "asdf:program-op"
  :build-pathname "bin/sel-test"
  :entry-point "software-evolution-library/test::run-batch")

(defsystem "software-evolution-library/run-testbot-test"
  :description "Compiled basic test binary for SEL."
  :build-operation "asdf:program-op"
  :build-pathname "bin/sel-testbot"
  :entry-point "software-evolution-library/test::run-testbot")


;;;; Subsystems.
(defsystem "software-evolution-library/utility"
  :description "Utility functions for the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               uiop
               ;; https://gitlab.common-lisp.net/asdf/asdf-encodings
               asdf-encodings
               cffi ; Work around a bug in which :babel isn't found for :osicat.
               osicat
               metabang-bind
               curry-compose-reader-macros
               bordeaux-threads
               cl-arrows
               iterate
               split-sequence
               trivial-shell
               cl-ppcre
               cl-store
               cl-dot
               diff)
  :components
  ((:module utility
            :pathname "utility"
            :components
            ((:file "package")
             (:file "utility" :depends-on ("package"))))))

(defsystem "software-evolution-library/view"
  :description "Viewing functions for the SOFTWARE-EVOLUTION-LIBRARY."
  :version "0.0.0"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               cl-arrows
               iterate
               split-sequence
               trivial-shell
               cl-ppcre
               cl-store
               cl-dot
               diff
               software-evolution-library
               software-evolution-library/utility
               bordeaux-threads
               cl-interpol)
  :components
  ((:module view
            :pathname "view"
            :components
            ((:file "package")
             (:file "view" :depends-on ("package"))))))

(defsystem "software-evolution-library/mongo"
  :description "Mongo database functions for the SOFTWARE-EVOLUTION-LIBRARY."
  :version "0.0.0"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               cl-arrows
               split-sequence
               cl-ppcre
               cl-store
               cl-mongo
               software-evolution-library
               software-evolution-library/utility)
  :components
  ((:module mongo
            :pathname "mongo"
            :components
            ((:file "package")
             (:file "mongo-fodder-database" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "software-evolution-library/mongo-test"))))

(defsystem "software-evolution-library/mongo-test"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY/MONGO package."
  :version "0.0.0"
  :depends-on (alexandria
               closer-mop
               cl-arrows
               curry-compose-reader-macros
               cxml
               metabang-bind
               software-evolution-library
               software-evolution-library/utility
               software-evolution-library/mongo
               software-evolution-library/test
               split-sequence
               stefil
               uuid
               testbot)
  :components
  ((:module test-mongo
            :pathname "test/src"
            :components
            ((:file "package-mongo")
             (:file "software-evolution-library/mongo-test"
                    :depends-on ("package-mongo")))))
  :perform (test-op (o c)
                    (symbol-call :software-evolution-library/mongo-test
                                 '#:test)))
