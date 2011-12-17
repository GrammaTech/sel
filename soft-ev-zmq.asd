(defsystem :soft-ev-zmq
  :description "Soft-ev with sharing enabled through ∅MQ"
  :long-description ""
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (soft-ev zmsg)
  :components
  ((:static-file "COPYING")
   (:static-file "Makefile")
   (:file "package-zmq")
   (:file "soft-ev-zmq" :depends-on ("package-zmq"))))
