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
  ;; :homepage "http://GrammaTech.github.io/sel"
  :depends-on (alexandria
               closer-mop
               uiop
               bordeaux-threads
               arrow-macros
               cl-json
               cl-ppcre
               cl-store
               named-readtables
               curry-compose-reader-macros
               diff
               elf
               iterate
               metabang-bind
               software-evolution-library/utility
               software-evolution-library/serapi-io
               software-evolution-library/ast-diff
               split-sequence
               usocket
               babel
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
            ((:file "simple")
             (:file "lisp" :depends-on ("simple"))
             ;; (:file "with-exe")
             ;; (:file "lisp-ext" :depends-on ("lisp" "simple"))
             (:file "expression" :depends-on ("lisp"))
             (:file "diff" :depends-on ("simple"))
             (:file "asm"  :depends-on ("simple"))
             (:file "asm-heap" :depends-on ("asm"))
	     (:file "asm-super-mutant" :depends-on ("asm-heap"))
             (:file "csurf-asm" :depends-on ("asm-heap"))
             (:file "elf"  :depends-on ("diff"))
             (:file "elf-cisc" :depends-on ("elf"))
             (:file "elf-risc" :depends-on ("elf"))
             (:file "elf-mips" :depends-on ("elf-risc"))
             (:file "source")
             (:file "ast")
             (:file "parseable" :depends-on ("source" "ast"))
             (:file "cil" :depends-on ("source"))
             (:file "java" :depends-on ("source"))
             (:file "clang" :depends-on ("parseable" "ast"))
             (:file "clang-expression" :depends-on ("clang" "expression"))
             (:file "clang-w-fodder" :depends-on ("clang"))
             (:file "forth" :depends-on ("simple"))
             (:file "llvm" :depends-on ("source"))
             (:file "project")
             (:file "clang-project" :depends-on ("project" "clang"))
             (:file "java-project" :depends-on ("project" "java"))
             (:file "super-mutant")
             (:file "adaptive-mutation")
             (:file "styleable" :depends-on ("project"))
             (:file "ancestral" :depends-on ("clang"))
             (:file "coq")
             (:file "coq-project" :depends-on ("project" "coq"))))
   (:module src
            :depends-on (base software)
            :pathname "src"
            :components
            ((:file "instrument")
             (:file "clang-instrument" :depends-on ("instrument"))
             (:file "java-instrument" :depends-on ("instrument"))
             (:file "traceable" :depends-on ("test-suite"))
             (:file "fix-compilation")
             (:file "searchable")
             (:file "fodder-database" :depends-on ("searchable"))
             (:file "in-memory-fodder-database" :depends-on ("fodder-database"))
             (:file "json-fodder-database" :depends-on ("in-memory-fodder-database"))
             (:file "lexicase")
             (:file "pliny-fodder-database" :depends-on ("fodder-database"))
             (:file "test-suite")
             (:file "condition-synthesis" :depends-on ("test-suite"))
             (:file "fault-loc" :depends-on ("test-suite"))
             (:file "multi-objective")
             (:file "clang-tokens")))))

(defsystem "software-evolution-library/clang-instrument"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Compiled clang-instrument binary from SEL."
  :depends-on (software-evolution-library)
  :build-operation "asdf:program-op"
  :build-pathname "bin/clang-instrument"
  :entry-point "software-evolution-library::run-clang-instrument")


;;;; Tests.
(defsystem "software-evolution-library/test"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  :depends-on (alexandria
               closer-mop
               arrow-macros
               cl-store
               named-readtables
               curry-compose-reader-macros
               cxml
               iterate
               metabang-bind
               software-evolution-library
               software-evolution-library/utility
               software-evolution-library/ast-diff
               split-sequence
	       software-evolution-library/stefil-plus
               uuid
               #+gt testbot
               trace-db
               uiop
               optima
               fare-quasiquote
               fare-quasiquote-extras)
  :components
  ((:module test
            :pathname "test/src"
            :components
            ((:file "package")
             (:file "software-evolution-library-test"
                    :depends-on ("package")))))
  :perform (test-op (o c)
                    (symbol-call :software-evolution-library/test
                                 '#:run-batch)))


;;;; Subsystems.
(defsystem "software-evolution-library/utility"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Utility functions for the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  :depends-on (alexandria
               closer-mop
               uiop
               ;; https://gitlab.common-lisp.net/asdf/asdf-encodings
               asdf-encodings
               cffi ; Work around a bug in which :babel isn't found for :osicat.
               osicat
               #+sbcl sb-posix
               #+sbcl sb-introspect
               #+sbcl sb-sprof
               metabang-bind
               named-readtables
               curry-compose-reader-macros
               bordeaux-threads
               arrow-macros
               iterate
               split-sequence
               cl-ppcre
               cl-store
               cl-dot
               diff)
  :components
  ((:module utility
            :pathname "utility"
            :components
            ((:file "package")
             (:file "utility" :depends-on ("package"))
             (:file "task" :depends-on ("package"))))))

(defsystem "software-evolution-library/view"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Viewing functions for the SOFTWARE-EVOLUTION-LIBRARY."
  :version "0.0.0"
  :depends-on (alexandria
               metabang-bind
               named-readtables
               curry-compose-reader-macros
               arrow-macros
               iterate
               split-sequence
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

(defsystem "software-evolution-library/serapi-io"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "serialization interface for Coq"
  :version "0.0.0"
  :depends-on (alexandria
               metabang-bind
               named-readtables
               curry-compose-reader-macros
               arrow-macros
               iterate
               split-sequence
               cl-ppcre
               cl-store
               cl-dot
               diff
               software-evolution-library/utility
               bordeaux-threads
               optima
               fare-quasiquote
               fare-quasiquote-extras)
  :components ((:file "src/serapi-io")))

(defsystem "software-evolution-library/mongo"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Mongo database functions for the SOFTWARE-EVOLUTION-LIBRARY."
  :version "0.0.0"
  :depends-on (alexandria
               metabang-bind
               named-readtables
               curry-compose-reader-macros
               arrow-macros
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
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY/MONGO package."
  :version "0.0.0"
  :depends-on (alexandria
               closer-mop
               arrow-macros
               named-readtables
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
               #+gt testbot)
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

(defsystem "software-evolution-library/ast-diff"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Compute differences between ASTs and other tree structures."
  :version "0.0.0"
  :depends-on (alexandria
               closer-mop
               arrow-macros
               named-readtables
               curry-compose-reader-macros
               iterate
               cl-heap
               software-evolution-library/utility)
  :in-order-to ((test-op (test-op "software-evolution-library/test")))
  :components ((:file "ast-diff/ast-diff")))

(defsystem "software-evolution-library/ast-diff-html"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Compute differences between ASTs and other tree structures."
  :version "0.0.0"
  :depends-on (software-evolution-library/ast-diff cl-who)
  :components ((:file "ast-diff/ast-diff-html")))

(defsystem "software-evolution-library/clang-diff"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Calculate difference between two C/C++ programs."
  :version "0.0.0"
  :depends-on (alexandria
               closer-mop
               arrow-macros
               named-readtables
               curry-compose-reader-macros
               metabang-bind
               iterate
               uiop
               split-sequence
               software-evolution-library/view
               software-evolution-library/utility
               software-evolution-library/ast-diff
               software-evolution-library)
  :build-operation "asdf:program-op"
  :build-pathname "bin/clang-diff"
  :entry-point "software-evolution-library/clang-diff::run-clang-diff"
  :components ((:file "ast-diff/clang-diff")))

(defsystem "software-evolution-library/lisp-diff"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Calculate difference between two lisp programs."
  :version "0.0.0"
  :depends-on (alexandria
               closer-mop
               arrow-macros
               named-readtables
               curry-compose-reader-macros
               metabang-bind
               iterate
               uiop
               software-evolution-library/utility
               software-evolution-library/ast-diff
               eclector-concrete-syntax-tree
               concrete-syntax-tree)
  :build-operation "asdf:program-op"
  :build-pathname "bin/lisp-diff"
  :entry-point "software-evolution-library/lisp-diff::run-lisp-diff"
  :components ((:file "ast-diff/lisp-diff")))

(defsystem "software-evolution-library/stefil-plus"
  :author "Eric Schulte and GrammaTech"
  :licence "GPL V3"
  :description "Adds some enhancements to the STEFIL test framework."
  :version "0.0.0"
  :depends-on (iterate
	       uiop
	       stefil
	       software-evolution-library/utility)
  :components
  ((:module stefil-plus
            :pathname "test/src"
            :components
            ((:file "stefil-plus")))))
