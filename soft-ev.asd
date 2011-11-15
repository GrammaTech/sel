(defsystem :soft-ev
  :description "Soft-ev enables to evolution of existing software artifacts."
  :long-description ""
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               split-sequence
               metabang-bind
               cl-store
               trivial-shell
               cl-ppcre
               zmsg)
  :components
  ((:static-file "COPYING")
   (:static-file "Makefile")
   (:static-file "tests.lisp")
   (:file "package")
   (:file "util"     :depends-on ("package"))
   (:file "soft"     :depends-on ("package" "util"))
   (:file "soft-asm" :depends-on ("package" "util" "soft"))
   (:file "soft-elf" :depends-on ("package" "util" "soft"))
   (:file "ev"       :depends-on ("package" "util" "soft" "soft-elf"
                                  "soft-asm"))
   (:file "main"     :depends-on ("ev" "package" "util" "soft" "soft-elf"
                                       "soft-asm"))
   (:file "share"    :depends-on ("ev" "package" "util" "soft" "soft-elf"
                                   "soft-asm"))))
