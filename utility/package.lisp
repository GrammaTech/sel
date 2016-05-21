(defpackage :software-evolution-utility
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :trivial-shell
   :cl-ppcre
   :cl-store
   :diff)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:export
   :infinity
   ;; OS
   :file-to-string
   :file-to-bytes
   :string-to-file
   :bytes-to-file
   :getenv
   :temp-file-name
   :with-temp-file
   :with-temp-file-of
   :with-temp-file-of-bytes
   :ensure-path-is-string
   ;; :from-bytes
   ;; :to-bytes
   :*work-dir*
   :*shell-debug*
   :*shell-error-codes*
   :ignore-shell-error
   :shell-command-failed
   :shell
   :shell-with-input
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
   :plist-keys
   :plist-drop-if
   :plist-drop
   :plist-merge
   :counts
   :proportional-pick
   :random-bool
   :random-elt-with-decay
   :random-hash-table-key
   :uniform-probability
   :cdf
   :random-pick
   :random-subseq
   :apply-replacements
   :peel-bananas
   :replace-all
   :aget
   :alist
   :alist-merge
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
   :binary-search
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
   :unlines
   :keep-lines-after-matching
   :resolve-function-includes
   ;; debugging
   :*note-level*
   :*note-out*
   :note
   :trace-memory
   :*shell-count*
   ;; diff computing
   :diff-scalar
   ;; gdb functions
   :gdb-disassemble
   :addrs
   :function-lines
   :calculate-addr-map
   ;; oprofile
   :samples-from-oprofile-file
   :samples-from-tracer-file
   ;; iterate helpers
   :concatenating))

#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
