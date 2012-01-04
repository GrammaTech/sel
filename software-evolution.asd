(defsystem :software-evolution
  :description "software-evolution enables evolution of extant software."
  :long-description ""
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               split-sequence
               metabang-bind
               cl-store
               trivial-shell
               trivial-timeout
               cl-ppcre)
  :components
  ((:static-file "COPYING")
   (:static-file "Makefile")
   (:static-file "test/tests.lisp")
   (:file "package")
   (:file "utility"       :depends-on ("package"))
   (:file "software"      :depends-on ("package" "utility"))
   (:file "software/asm"  :depends-on ("package" "utility" "software"))
   (:file "software/elf"  :depends-on ("package" "utility" "software"))
   (:file "software/lisp" :depends-on ("package" "utility" "software"))
   (:file "evolution"     :depends-on ("package" "utility" "software"))
   (:file "executable"    :depends-on ("package" "utility" "software"
                                       "evolution"))))
