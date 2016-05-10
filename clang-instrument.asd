(defsystem :clang-instrument
  :description "Instrument C-language source files"
  :version "0.0.0"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               cl-arrows
               iterate
               split-sequence
               software-evolution
               software-evolution-utility)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "clang-instrument" :depends-on ("package"))))))
