(defsystem :repair-clang
  :description "Generate repair for a C program, shown in view"
  :license "GPL V3"
  :depends-on (alexandria
               software-evolution-library
               software-evolution-library/utility
               software-evolution-library/view
               uiop
               command-line)
  :build-operation "asdf:program-op"
  :build-pathname "bin/repair-clang"
  :entry-point "repair-clang::run-repair-clang"
  :components ((:file "repair-clang")))
