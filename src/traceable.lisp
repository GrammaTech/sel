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
      (assert (phenome obj :bin bin) (obj)
              "Unable to compile software ~a" obj))
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
      (assert (phenome obj :bin bin) (obj)
              "Unable to compile software ~a" obj))
  (unwind-protect
       (with-temp-fifo (pipe)
         ;; Start running the test case.
         (let ((proc (start-test bin test-case
                                 :env (list (cons *instrument-log-env-name*
                                                  pipe))
                                 :wait nil)))
           (iter (for in = (open-file-timeout pipe *trace-open-timeout*))
                 (while in)
                 (collect
                     (list
                      ;; keep :bin symbol if present
                      (cons :input
                            (cons (program-name test-case)
                                  (program-args test-case)))
                           (cons :trace
                                 (unwind-protect
                                      (read-trace-stream
                                       in :predicate predicate :max max)
                                   (close in))))
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

(defun read-trace-file (file-name &key (predicate #'identity) (max infinity))
  "Read a trace from FILE-NAME with `read-trace-stream'."
  (if (equalp (pathname-type file-name) "xz")
      (read-shell-file (in file-name "unxz")
        (read-trace-stream in :predicate predicate :max max))
      (with-open-file (in file-name)
        (read-trace-stream in :predicate predicate :max max))))

(defun read-trace-stream
    (in &key (predicate #'identity) (max infinity) &aux (collected 0))
  "Read a trace from the IN.
Keyword argument PREDICATE limits collected trace points and MAX
limits number of trace points to collect."
  (iter (for trace-point =
             (restart-case (read in nil :eof)
               (ignore-rest-of-stream ()
                 :report "Ignore error and skip remainder of stream"
                 :eof)))
        (while (and (not (eq trace-point :eof))
                    (< collected max)))
        (when (funcall predicate trace-point)
          (incf collected)
          (collect trace-point))))
