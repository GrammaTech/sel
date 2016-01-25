(defpackage :software-evolution-utility
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :split-sequence
   :trivial-shell
   :cl-ppcre
   :cl-store
   :diff)
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
   :with-temp-file-of-bytes
   ;; :from-bytes
   ;; :to-bytes
   :*work-dir*
   :*shell-debug*
   :shell
   :parse-number
   :parse-numbers
   ;; forensic
   :show-it
   :equal-it
   :count-cons
   ;; simple utility
   :repeatedly
   :range
   :indexed
   :different-it
   :plist-get
   :plist-drop
   :counts
   :proportional-pick
   :random-bool
   :random-elt-with-decay
   :random-hash-table-key
   :uniform-probability
   :cdf
   :random-pick
   :apply-replacements
   :peel-bananas
   :replace-all
   :aget
   :alist
   :getter
   :transpose
   :interleave
   :mapconcat
   :drop
   :drop-while
   :drop-until
   :take
   :take-while
   :take-until
   :chunks
   :<and>
   :<or>
   ;;; Source and binary locations and ranges
   :source-location
   :line
   :column
   :source-range
   :range
   :begin
   :end
   :source-<
   :source-<=
   :source->
   :source->=
   :contains
   :intersects
   :levenshtein-distance
   :intercalate
   :unlines
   :keep-lines-after-matching
   :resolve-function-headers
   ;; hash tables
   :ht->list
   :list->ht
   :merge-hash-tables
   :ht-intersect
   :ht-copy
   :ht-keys
   :remhash-non-destructive
   ;; debugging
   :*note-level*
   :*note-out*
   :note
   ;; diff computing
   :diff-scalar
   ;; gdb functions
   :gdb-disassemble
   :addrs
   :function-lines
   :calculate-addr-map
   ;; oprofile
   :samples-from-oprofile-file
   :samples-from-tracer-file))

#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))

