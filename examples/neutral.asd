(defsystem :neutral
  :description "generate neutral variants of software against a test suite"
  :licence "GPL V3"
  :depends-on (alexandria
               split-sequence
               metabang-bind
               curry-compose-reader-macros
               software-evolution
               software-evolution-utility
               software-evolution-command-line)
  :components ((:file "neutral")))
