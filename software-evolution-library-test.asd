(defsystem :software-evolution-test
  :description "Test the SOFTWARE-EVOLUTION package."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               closer-mop
               cl-arrows
               curry-compose-reader-macros
               cxml
               iterate
               metabang-bind
               software-evolution
               software-evolution-utility
               split-sequence
               stefil
               uuid
               testbot
               libtrace
               trivial-shell
               uiop)
  :components
  ((:module test
            :pathname "test/src"
            :components
            ((:file "package")
             (:file "software-evolution-test" :depends-on ("package"))))))
