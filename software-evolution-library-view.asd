(defsystem :software-evolution-library-view
  :description "Viewing functions for the SOFTWARE-EVOLUTION-LIBRARY."
  :version "0.0.0"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               cl-arrows
               iterate
               split-sequence
               trivial-shell
               cl-ppcre
               cl-store
               cl-dot
               diff
               software-evolution-library
               software-evolution-library-utility
               bordeaux-threads
               cl-interpol)
  :components
  ((:module view
            :pathname "view"
            :components
            ((:file "package")
             (:file "view" :depends-on ("package"))))))
