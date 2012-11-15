(defsystem :software-evolution
  :description "evolution of extant software."
  :long-description ""
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               split-sequence
               metabang-bind
               cl-store
               trivial-shell
               trivial-timeout
               cl-ppcre
               ;; cl-quickcheck
               eager-future2
               memoize
               )
  :components
  ((:static-file "COPYING")
   (:static-file "Makefile")
   (:static-file "test/tests.lisp")
   (:file       "package")
   (:file       "utility"
    :depends-on ("package"))
   (:file       "software-evolution"
    :depends-on ("package" "utility"))
   (:file       "software/asm"
    :depends-on ("package" "utility" "software-evolution"))
   (:file       "software/ast"
    :depends-on ("package" "utility" "software-evolution"))
   (:file       "software/cil"
    :depends-on ("package" "utility" "software-evolution" "software/ast"))
   (:file       "software/clang"
    :depends-on ("package" "utility" "software-evolution" "software/ast"))
   (:file       "evolution/neutral"
    :depends-on ("package" "utility"))
   (:file       "evolution/by-function"
    :depends-on ("package" "utility"))
   ))
