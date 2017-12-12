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
             (:file "software-evolution-library-test"
                    :depends-on ("package"))))))

(defsystem :software-evolution-library-test/test
  :description "Compiled basic test binary for SEL."
  :build-operation "asdf:program-op"
  :build-pathname "bin/sel-test"
  :entry-point "software-evolution-library-test::run-batch")

(defsystem :software-evolution-library-test/testbot-test
  :description "Compiled basic test binary for SEL."
  :build-operation "asdf:program-op"
  :build-pathname "bin/sel-testbot"
  :entry-point "software-evolution-library-test::run-testbot")
