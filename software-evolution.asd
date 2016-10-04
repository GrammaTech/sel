(defsystem :software-evolution
  :description "programmatic modification and evaluation of extant software"
  :long-description "A common interface abstracts over multiple
types of software objects including abstract syntax trees parsed from
source code, LLVM IR, compiled assembler, and linked ELF binaries.
Mutation and evaluation methods are implemented on top of this
interface supporting Search Based Software Engineering (SBSE)
techniques."
  :version "0.0.0"
  :licence "GPL V3"
  ;; :homepage "http://eschulte.github.io/software-evolution/index.html"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               cl-arrows
               iterate
               split-sequence
               cl-json
               cl-ppcre
               cl-mongo
               usocket
               diff
               elf
               memoize
               software-evolution-utility)
  :in-order-to ((test-op (test-op software-evolution-test)))
  :components
  ((:module base
            :pathname ""
            :components
            ((:file "package")
             (:file "software-evolution" :depends-on ("package"))))
   (:module software
            :depends-on (base)
            :pathname "software"
            :components
            ((:file "lisp")
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
             (:file "clang-expression" :depends-on ("clang" "lisp"))
             (:file "clang-w-fodder" :depends-on ("clang"))
             (:file "llvm" :depends-on ("ast"))))
   (:module src
            :depends-on (base software)
            :pathname "src"
            :components
            ((:file "ancestral")
             (:file "clang-instrument")
             (:file "fix-compilation")
             (:file "fodder-database")
             (:file "json-fodder-database" :depends-on ("fodder-database"))
             (:file "lexicase")
             (:file "mongo-fodder-database" :depends-on ("fodder-database"))
             (:file "pliny-fodder-database" :depends-on ("fodder-database"))))))
