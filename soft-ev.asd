(defsystem :soft-ev
  :description "Soft-ev enables to evolution of existing software artifacts."
  :long-description ""
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               split-sequence
               metabang-bind
               cl-store)
  :components
  ((:static-file "COPYING")
   (:static-file "Makefile")
   (:static-file "tests.lisp")
   (:file "package")
   (:file "soft" :depends-on ("package"))
   (:file "soft-asm" :depends-on ("package" "soft"))
   (:file "soft-elf" :depends-on ("package" "soft"))
   (:file "ev" :depends-on ("package" "soft" "soft-elf" "soft-asm"))))
