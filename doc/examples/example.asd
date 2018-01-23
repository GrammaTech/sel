(defsystem :example
  :description "Generate repair for a C program, shown in view"
  :license "GPL V3"
  :depends-on (alexandria
               software-evolution-library
               software-evolution-library/utility
               stefil
               cl-ppcre)
  :build-operation "asdf:program-op"
  :build-pathname "bin/example"
  :entry-point "example::test-examples"
  :components ((:file "examples-test")))
