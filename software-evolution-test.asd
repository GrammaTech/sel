(defsystem :software-evolution-test
  :description "Test the evolution of extant software."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on
(alexandria metabang-bind curry-compose-reader-macros stefil software-evolution)
  :components
  ((:static-file "COPYING")
   (:file       "package-test")
   (:file       "software-evolution-test" :depends-on ("package-test"))))
