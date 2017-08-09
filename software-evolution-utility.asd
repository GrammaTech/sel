(defsystem :software-evolution-utility
  :description "Utility functions for the SOFTWARE-EVOLUTION package."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               uiop
               babel
               osicat
               metabang-bind
               curry-compose-reader-macros
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
