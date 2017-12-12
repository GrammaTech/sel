(defsystem :software-evolution-library-utility
  :description "Utility functions for the SOFTWARE-EVOLUTION-LIBRARY package."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               uiop
               ;; https://gitlab.common-lisp.net/asdf/asdf-encodings
               asdf-encodings
               osicat
               metabang-bind
               curry-compose-reader-macros
               bordeaux-threads
               cl-arrows
               iterate
               split-sequence
               trivial-shell
               cl-ppcre
               cl-store
               cl-dot
               diff)
  :components
  ((:module utility
            :pathname "utility"
            :components
            ((:file "package")
             (:file "utility" :depends-on ("package"))))))
