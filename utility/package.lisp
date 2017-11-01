(defpackage :software-evolution-utility
  (:nicknames :se-utility)
  (:use
   :common-lisp
   :alexandria
   :uiop
   :osicat-posix
   :metabang-bind
   :curry-compose-reader-macros
   :bordeaux-threads
   :iterate
   :split-sequence
   :trivial-shell
   :cl-ppcre
   :cl-store
   :cl-dot
   :diff)
  (:shadow :quit :read)
  (:shadowing-import-from :trivial-shell :with-timeout)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:shadowing-import-from
   :osicat-posix
   :write
   :truncate
   :open
   :sleep
   :chdir
   :time
   :col
   :exit
   :getcwd
   :getenv
   :ftruncate
   :link
   :close)
  (:export
   :infinity
   ;; OS
   :file-to-string
   :use-encoding
   :file-to-bytes
   :string-to-file
   :bytes-to-file
   :stream-to-string
   :getenv
   :quit
   :current-git-commit
   :current-git-branch
   :*temp-dir*
   :temp-file-name
   :with-temp-file
   :with-temp-fifo
   :with-temp-file-of
   :with-temp-file-of-bytes
   :with-temp-files
   :with-temp-dir
   :with-cwd
   :ensure-path-is-string
   :in-directory
   :*work-dir*
   :*shell-debug*
   :*shell-error-codes*
   :*shell-non-error-codes*
   :ignore-shell-error
   :shell-command-failed
   :shell
   :shell-with-input
   :write-shell-file
   :read-shell-file
   :*bash-shell*
   :read-shell
   :xz-pipe
   :parse-number
   :parse-numbers
   :trim-whitespace
   :make-terminal-raw
   :which
   ;; forensic
   :show-it
   :equal-it
   :count-cons
   ;; simple utility
   :repeatedly
   :indexed
   :different-it
   :plist-get
   :plist-keys
   :plist-drop-if
   :plist-drop
   :plist-merge
   :counts
   :proportional-pick
   :partition
   :random-bool
   :random-elt-with-decay
   :random-hash-table-key
   :uniform-probability
   :normalize-probabilities
   :cumulative-distribution
   :un-cumulative-distribution
   :random-pick
   :random-subseq
   :random-sample-with-replacement
   :random-sample-without-replacement
   :apply-replacements
   :peel-bananas
   :unpeel-bananas
   :replace-all
   :aget
   :alist
   :alist-merge
   :alist-filter
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
   :pad
   :chunks
   :binary-search
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
   :replace-stdout-in-note-targets
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
   :concatenating
   ;; Profiling helpers
   :*profile-dot-min-ratio*
   :profile-to-dot-graph
   :profile-to-flame-graph
   ))
#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
