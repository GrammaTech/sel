(defsystem :software-evolution
  :description "evolution of extant software."
  :long-description ""
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               cl-ppcre
               elf
               software-evolution-utility)
  :components
  ((:file "package")
   (:file "software-evolution" :depends-on ("package"))
   ;; genomes
   (:file "genomes/genome"
    :depends-on ("package" "software-evolution"))
   (:file "genomes/cons"
    :depends-on ("package" "software-evolution"))
   (:file "genomes/tree"
    :depends-on ("package" "software-evolution"))
   (:file "genomes/vector"
    :depends-on ("package" "software-evolution"))
   ;; software
   ;; (:file "software/lisp"
   ;;  :depends-on ("package" "software-evolution"))
   (:file "software/elf"
    :depends-on ("package" "software-evolution"))
   (:file "software/simple"
    :depends-on ("package" "software-evolution"))
   (:file "software/asm"
    :depends-on ("package" "software-evolution" "software/simple"))
   (:file "software/ast"
    :depends-on ("package" "software-evolution"))
   (:file "software/cil"
    :depends-on ("package" "software-evolution" "software/ast"))
   (:file "software/clang"
    :depends-on ("package" "software-evolution" "software/ast"))
   (:file "software/llvm"
    :depends-on ("package" "software-evolution" "software/ast"))))

(defsystem :software-evolution-test
  :description "Test the evolution of extant software."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               stefil
               software-evolution
               software-evolution-utility)
  :components
  ((:static-file "COPYING")
   (:module "test"
            :components
            ((:file "package")
             (:file "tests" :depends-on ("package"))))))
