(defsystem :software-evolution-library-test
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               closer-mop
               cl-arrows
               curry-compose-reader-macros
               cxml
               iterate
               metabang-bind
               software-evolution-library
               software-evolution-library-utility
               split-sequence
               stefil
               uuid
               testbot
               trace-db
               trivial-shell
               uiop)
  :components
  ((:module test
            :pathname "test/src"
            :components
            ((:file "package")
             (:file "software-evolution-library-test" :depends-on ("package"))))))
