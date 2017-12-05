;;; traceable --- instrumentable software objects with dynamic traces
(in-package :software-evolution-library)
(enable-curry-compose-reader-macros :include-utf8)

(defvar *trace-open-timeout* 2
  "Timeout (in seconds) when opening pipe to collect traces.")

(defvar *process-kill-timeout* 10
  "Timeout (in seconds) before killing a process with SIGKILL")

(define-software traceable (software)
  ((traces :initarg :traces :accessor traces :initform nil :copier :direct
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
        (skip-trace-collection ()
          :report "Skip trace collection and leave object unchanged."
          (return-from collect-traces nil))
        (nil-traces ()
          :report "Set object traces to NIL and continue."
          (setf (traces obj) (repeatedly (length (test-cases test-suite)) nil))
          (return-from collect-traces nil))))
  (unwind-protect
       (setf (traces obj)
             (mappend (lambda (test-case)
                        (apply #'collect-trace obj test-case args))
                      (test-cases test-suite)))
    (when-let ((probe (and delete-bin-p (probe-file bin))))
      (if (uiop:directory-pathname-p probe)
          (uiop:delete-directory-tree probe :validate #'probe-file)
          (delete-file probe)))))

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
        (skip-test-case ()
          :report "Skip trace collection for test case and return NIL."
          (return-from collect-trace nil))))
  (unwind-protect
    (with-temp-fifo (pipe)
      (with-temp-file (handshake-file) ;; Start running the test case.
        (let ((proc (start-test bin test-case
                                :env (list (cons *instrument-log-env-name*
                                                 pipe)
                                           (cons *instrument-handshake-env-name*
                                                 handshake-file))
                                :wait nil)))
          (flet ((handshake (&aux (start-time (get-internal-real-time)))
                   ;; Create the handshake file, which indicates that we
                   ;; are ready to read traces. The file contents don't
                   ;; actually matter.
                   (with-output-to-file (out handshake-file)
                     (format out "ready"))
                   (iter (while (< (/ (- (get-internal-real-time) start-time)
                                      internal-time-units-per-second)
                                   *trace-open-timeout*))
                         (unless (eq :running (process-status proc))
                           (note 3 "Test process exited")
                           (return nil))

                         ;; The instrumented process will complete the
                         ;; handshake by deleting the file.
                         (unless (probe-file handshake-file)
                           (return t))

                         (finally (note 3 "No handshake after ~d seconds"
                                        *trace-open-timeout*)))))
            (iter (while (handshake))
                  (collect (list ;; keep :bin symbol if present
                                 (cons :input
                                       (cons (program-name test-case)
                                             (program-args test-case)))
                                 (cons :trace
                                       (read-trace pipe
                                                   *trace-open-timeout*
                                                   :predicate predicate
                                                   :max max)))
                    into traces)
                  (finally
                   (finish-test proc :kill-signal 15
                                     :timeout *process-kill-timeout*)
                   (restart-case
                       ;; This usually indicates a problem with the
                       ;; test script or the instrumentation
                       (when (emptyp traces)
                         (error (make-condition 'trace-error
                                 :text (format nil
                                         "No traces collected for test case ~s ~s."
                                         test-case
                                         (cons (program-name test-case)
                                               (program-args test-case)))
                                 :obj test-case
                                 :bin bin)))
                      (ignore-empty-trace ()
                        :report "Ignore empty trace"))
                   (return traces)))))))
    (when-let ((probe (and delete-bin-p (probe-file bin))))
      (if (cl-fad:directory-pathname-p probe)
          (uiop:delete-directory-tree probe :validate #'probe-file)
          (delete-file probe)))))
