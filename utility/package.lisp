(defpackage :software-evolution-library/utility
  (:nicknames :sel/utility)
  (:use
   :common-lisp
   :alexandria
   :uiop
   :osicat-posix
   :metabang-bind
   :named-readtables
   :curry-compose-reader-macros
   :bordeaux-threads
   :iterate
   :split-sequence
   :cl-ppcre
   :cl-store
   :cl-dot
   :diff)
  (:shadow :read)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:shadowing-import-from :uiop/run-program :run-program)
  (:shadowing-import-from :uiop :quit)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:shadowing-import-from :osicat :pathname-as-directory)
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
   :with-temp-dir-of
   :with-cwd
   :pwd
   :cd
   :ensure-path-is-string
   :in-directory
   :directory-p
   ;; Process wrapper
   :process
   :os-process
   :process-id
   :process-input-stream
   :process-output-stream
   :process-error-stream
   :process-exit-code
   :process-status
   :signal-process
   ;; Shell execution
   :*shell-debug*
   :*shell-error-codes*
   :*shell-non-error-codes*
   :ignore-shell-error
   :shell-command-failed
   :shell
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
   :arglist
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
   :tails
   :pairs
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
   :enhanced-copy-seq
   ;; jobs
   :task-runner
   :*task-runner*
   :task-runner-jobs
   :task-runner-workers
   :task-runner-workers-count
   :task-runner-results
   :task-runner-completed-jobs
   :task-runner-completed-tasks
   :task-runner-remaining-jobs
   :task-runner-init-jobs
   :task-runner-stop-jobs
   :task-runner-add-job
   :task-runner-create-worker
   :task
   :task-job
   :process-task
   :task-object
   :task-save-result
   :run-task
   ))
#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
