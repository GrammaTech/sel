(defsystem "neutral"
  :description "generate neutral variants of software against a test suite"
  :licence "GPL V3"
  :depends-on (alexandria
               split-sequence
               metabang-bind
               cl-ppcre
               named-readtables
               curry-compose-reader-macros
               software-evolution-library
               software-evolution-library/utility)
  :components ((:file "neutral")))
