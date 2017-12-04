(defsystem :software-evolution-mongo-test
  :description "Test the SOFTWARE-EVOLUTION-MONGO package."
  :version "0.0.0"
  :depends-on (alexandria
               closer-mop
               cl-arrows
               curry-compose-reader-macros
               cxml
               metabang-bind
               software-evolution
               software-evolution-utility
               software-evolution-mongo
               software-evolution-test
               split-sequence
               stefil
               uuid
               testbot)
  :components
  ((:module test-mongo
            :pathname "test/src"
            :components
            ((:file "package-mongo")
             (:file "software-evolution-mongo-test"
                    :depends-on ("package-mongo"))))))
