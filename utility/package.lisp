(defpackage :software-evolution-utility
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :split-sequence
   :trivial-shell
   :cl-ppcre
   :cl-store)
  (:export
   :infinity
   ;; OS
   :file-to-string
   :file-to-bytes
   :string-to-file
   :bytes-to-file
   :temp-file-name
   :with-temp-file
   :with-temp-file-of
   :from-bytes
   :to-bytes
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
   :counts
   :proportional-pick
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
