(defpackage :software-evolution-utility
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :split-sequence
   :trivial-shell
   :cl-ppcre)
  (:export
   ;; OS
   :file-to-string
   :file-to-bytes
   :string-to-file
   :temp-file-name
   :with-temp-file
   :with-temp-file-of
   :*work-dir*
   :*shell-debug*
   :shell
   :parse-number
   ;; forensic
   :show-it
   :equal-it
   :count-cons
   ;; simple utility
   :indexed
   :different-it
   :aget
   :getter
   :transpose
   :interleave
   :mapconcat
   :drop
   :take
   :levenshtein-distance
   ;; debugging
   :*note-level*
   :*note-out*
   :note
   ;; gdb functions
   :gdb-disassemble
   :addrs
   :function-lines
   :calculate-addr-map
   ;; oprofile
   :samples-from-oprofile-file
   :samples-from-tracer-file))
