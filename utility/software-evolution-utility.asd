(defsystem :software-evolution-utility
  :description "Utility functions for extant software evolution."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               split-sequence
               trivial-shell
               cl-ppcre
               cl-store)
  :components
  ((:file "package")
   (:file "utility" :depends-on ("package"))))
