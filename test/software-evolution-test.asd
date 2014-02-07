(defsystem :software-evolution-test
  :description "Test the SOFTWARE-EVOLUTION package."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               stefil
               software-evolution
               software-evolution-utility)
  :components
  ((:file "package")
   (:file "tests" :depends-on ("package"))))
