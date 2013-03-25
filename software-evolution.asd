(defsystem :software-evolution
  :description "evolution of extant software."
  :long-description ""
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               split-sequence
               metabang-bind
               curry-compose-reader-macros
               trivial-shell
               cl-ppcre
               elf)
  :components
  ((:static-file "COPYING")
   (:file "package")
   (:file "utility" :depends-on ("package"))
   (:file "software-evolution" :depends-on ("package" "utility"))
   ;; genomes
   (:file "genomes/genome"
    :depends-on ("package" "utility" "software-evolution"))
   (:file "genomes/cons"
    :depends-on ("package" "utility" "software-evolution"))
   (:file "genomes/tree"
    :depends-on ("package" "utility" "software-evolution"))
   (:file "genomes/vector"
    :depends-on ("package" "utility" "software-evolution"))
   ;; software
   (:file "software/lisp"
    :depends-on ("package" "utility" "software-evolution"))
   (:file "software/elf"
    :depends-on ("package" "utility" "software-evolution"))
   (:file "software/asm"
    :depends-on ("package" "utility" "software-evolution"))
   (:file "software/ast"
    :depends-on ("package" "utility" "software-evolution"))
   (:file "software/cil"
    :depends-on ("package" "utility" "software-evolution" "software/ast"))
   (:file "software/clang"
    :depends-on ("package" "utility" "software-evolution" "software/ast"))))

(defsystem :software-evolution-test
  :description "Test the evolution of extant software."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on
(alexandria metabang-bind curry-compose-reader-macros stefil software-evolution)
  :components
  ((:static-file "COPYING")
   (:module "test"
            (:file "package")
            (:file "tests" :depends-on ("package")))))
