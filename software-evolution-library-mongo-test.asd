(defsystem :software-evolution-library-mongo-test
  :description "Test the SOFTWARE-EVOLUTION-LIBRARY-MONGO package."
  :version "0.0.0"
  :depends-on (alexandria
               closer-mop
               cl-arrows
               curry-compose-reader-macros
               cxml
               metabang-bind
               software-evolution-library
               software-evolution-library-utility
               software-evolution-library-mongo
               software-evolution-library-test
               split-sequence
               stefil
               uuid
               testbot)
  :components
  ((:module test-mongo
            :pathname "test/src"
            :components
            ((:file "package-mongo")
             (:file "software-evolution-library-mongo-test"
                    :depends-on ("package-mongo"))))))
