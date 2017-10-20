;;; traceable --- instrumentable software objects with dynamic traces
(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)

(defvar *trace-open-timeout* 2
  "Timeout (in seconds) when opening pipe to collect traces.")

(defclass traceable (software)
  ((traces :initarg :traces :accessor traces :initform nil
           :documentation "Execution traces from execution of the software."))
  (:documentation
   "Instrumentable software with support for collecting dynamic traces."))

(define-condition trace-error (error)
  ((text :initarg :text :initform nil :reader text)
   (obj  :initarg :obj  :initform nil :reader obj)
   (bin  :initarg :bin  :initform nil :reader bin))
  (:report (lambda (condition stream)
             (if (bin condition)
                 (format stream "Trace error: ~a while tracing ~S in binary ~S"
                         (text condition) (obj condition) (bin condition))
                 (format stream "Trace error: ~a while tracing ~S"
                         (text condition) (obj condition))))))


;;; Execution with trace collection.
(defgeneric collect-traces (software inputs &key predicate max bin)
  (:documentation
   "Execute instrumented SOFTWARE on INPUTS collecting dynamic traces.
See the documentation of `collect-trace' for information on the
PREDICATE MAX and BIN keyword arguments."))

(defmethod collect-traces ((obj software) (test-suite test-suite)
                           &key predicate max (bin (temp-file-name))
                           &aux (args (list :bin bin)) (delete-bin-p t))
  (when predicate (setf args (append args (list :predicate predicate))))
  (when max (setf args (append args (list :max max))))
  (if (probe-file bin)
      (setf delete-bin-p nil)
      (restart-case
          (unless (phenome obj :bin bin)
            (error (make-condition 'trace-error
                                  :text "Unable to compile software."
                                  :obj obj
                                  :bin bin)))
        (continue-collecting ()
          :report "Ignore and continue collecting traces.")))
  (unwind-protect
       (setf (traces obj)
             (mappend
               (lambda-bind ((i test-case))
                 (note 2 "Collect traces from input ~a of ~a"
                       (1+ i) (length (test-cases test-suite)))
                 (apply #'collect-trace obj test-case args))
               (indexed (test-cases test-suite))))
    (when (and delete-bin-p (probe-file bin)) (delete-file bin))))

(defgeneric collect-trace (software input &key predicate max bin)
  (:documentation
   "Execute instrumented SOFTWARE on INPUT collecting a dynamic trace.
INPUT should be a program and sequence of arguments.  The special
element :BIN in INPUT will be replaced with the name of the compiled
instrumented binary.  PREDICATE may be a function in which case only
trace points for which PREDICATE returns true are recorded.  Max
specifies the maximum number of trace points to record.  BIN specifies
the name of an already-compiled binary to use.

Returns a list of traces, which may contains multiple elements if
executing a test script which runs the traceable program multiple
times."))

(defmethod collect-trace
    ((obj software) (test-case test-case)
     &key (predicate #'identity) (max infinity) (bin (temp-file-name))
     &aux (delete-bin-p t))
  (if (probe-file bin)
      (setf delete-bin-p nil)
      (restart-case
          (unless (phenome obj :bin bin)
            (error (make-condition 'trace-error
                                   :text "Unable to compile software."
                                   :obj obj
                                   :bin bin)))
        (continue-collecting ()
          :report "Ignore and continue collecting traces.")))
  (unwind-protect
       (with-temp-fifo (pipe)
         ;; Start running the test case.
         (let ((proc (start-test bin test-case
                                 :env (list (cons *instrument-log-env-name*
                                                  pipe))
                                 :wait nil)))
           (iter (for trace = (and (eq :running (process-status proc))
                                   (read-trace pipe *trace-open-timeout*
                                               :predicate predicate
                                               :max max)))
                 (while trace)
                 (collect
                     (list
                      ;; keep :bin symbol if present
                      (cons :input
                            (cons (program-name test-case)
                                  (program-args test-case)))
                      (cons :trace trace))
                   into traces)
                 (finally
                  (finish-test proc :kill-signal 15)
                  (restart-case
                      ;; This usually indicates a problem with the test script
                      ;; or the instrumentation
                      (assert (not (emptyp traces)) (test-case)
                              "No traces collected for test case ~s" test-case)
                    (continue-collecting ()
                      :report "Ignore and continue collecting traces."))
                  (return traces)))))
    (when (and delete-bin-p (probe-file bin)) (delete-file bin))))
