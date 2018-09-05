(defsystem "repair"
  :description "repair software"
  :licence "GPL V3"
  :depends-on (alexandria
               split-sequence
               metabang-bind
               named-readtables
               curry-compose-reader-macros
               cl-store
               bordeaux-threads
               uiop
               software-evolution-library
               software-evolution-library/utility)
  :components ((:file "repair")))
