(defsystem :software-evolution-view
  :description "Viewing functions for the SOFTWARE-EVOLUTION library."
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
               software-evolution
               software-evolution-utility
               bordeaux-threads
               flexi-streams
               cl-interpol)
  :components
  ((:module view
            :pathname "view"
            :components
            ((:file "package")
             (:file "view" :depends-on ("package"))))))
