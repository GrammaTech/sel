(defsystem "repair"
  :description "repair software"
  :licence "GPL V3"
  :depends-on (alexandria
               split-sequence
               metabang-bind
               curry-compose-reader-macros
               cl-store
               bordeaux-threads
               software-evolution-library
               software-evolution-library/utility
               software-evolution-library/command-line)
  :components ((:file "repair")))
