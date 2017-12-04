(defsystem :software-evolution-mongo
  :description "Mongo database functions for the SOFTWARE-EVOLUTION library."
  :version "0.0.0"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               cl-arrows
               split-sequence
               cl-ppcre
               cl-store
               cl-mongo
               software-evolution
               software-evolution-utility)
  :components
  ((:module mongo
            :pathname "mongo"
            :components
            ((:file "package")
             (:file "mongo-fodder-database" :depends-on ("package"))))))
