;;; traceable.lisp --- Instrumentable software objects with dynamic traces.
(defpackage :software-evolution-library/components/traceable
  (:nicknames :sel/components/traceable :sel/cp/traceable)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility)
  (:export :traceable
           :binary-traceable
           :sexp-traceable
           :traces
           :collect-trace
           :collect-traces
           :read-trace-file
           :read-trace-stream
           :type-decl-string
           :type-trace-string
           :type-from-trace-string
           :trace-error
           :bin))
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defvar *trace-open-timeout* 35
  "Timeout (in seconds) when opening pipe to collect traces.")

(define-software traceable (software)
  ((traces :initarg :traces :accessor traces :initform nil :copier :direct
           :documentation "Execution traces from execution of the software."))
  (:documentation
   "Instrumentable software with support for collecting dynamic traces."))

(define-software binary-traceable (traceable) ()
  (:documentation "Instrumentable software with support for collecting dynamic
traces in a proprietary binary format."))

(define-software sexp-traceable (traceable) ()
  (:documentation "Instrumentable software with support for collecting dynamic
traces in a s-expression format."))

(define-condition trace-error (error)
  ((text :initarg :text :initform nil :reader text)
   (obj  :initarg :obj  :initform nil :reader obj)
   (bin  :initarg :bin  :initform nil :reader bin))
  (:report (lambda (condition stream)
             (if (bin condition)
                 (format stream "Trace error: ~a while tracing ~S in binary ~S"
                         (text condition) (obj condition) (bin condition))
                 (format stream "Trace error: ~a while tracing ~S"
                         (text condition) (obj condition)))))
  (:documentation "Error thrown when trace collection fails unexpectedly"))


;;; Trace collection interface
(defgeneric collect-traces (software test-suite &key max bin num-threads)
  (:documentation
   "Execute instrumented SOFTWARE on TEST-SUITE collecting dynamic traces.
See the documentation of `collect-trace' for information on the
MAX, BIN, and NUM-THREADS keyword arguments."))

(defgeneric collect-trace (software input &key max bin)
  (:documentation
   "Execute instrumented SOFTWARE on TEST-CASE collecting a dynamic trace.
MAX specifies the maximum number of trace points to record.  BIN specifies
the name of an already-compiled binary to use.

Returns a list of traces, which may contains multiple elements if
executing a test script which runs the traceable program multiple
times."))


;;; Trace collection with proprietary binary format
(defmethod collect-traces ((obj binary-traceable) (test-suite test-suite)
                           &key max (bin (temp-file-name)) (num-threads 1)
                           &aux (args (list :bin bin)) (delete-bin-p t))
  "Execute instrumented OBJ on TEST-SUITE collecting dynamic traces.
* OBJ Instrumented software object suitable for trace collection
* TEST-SUITE suite of test case to execute for trace collection
* MAX maximum number of trace points to record
* BIN compiled binary with instrumentation to use for trace collection
* NUM-THREADS number of threads to use in trace collection
"
  (when max (setf args (append args (list :max max))))
  (if (probe-file bin)
      (setf delete-bin-p nil)
      (restart-case
          (phenome obj :bin bin)
        (skip-trace-collection ()
          :report "Skip trace collection and leave object unchanged."
          (return-from collect-traces (traces obj)))
        (nil-traces ()
          :report "Set object traces to NIL and continue."
          (setf (traces obj) nil)
          (return-from collect-traces (traces obj)))))
  (setf (traces obj) (make-instance 'binary-trace-db))
  (unwind-protect
       (task-map num-threads
                 (lambda (test-case)
                   (apply #'collect-trace obj test-case args))
                 (test-cases test-suite))
    (when-let ((probe (and delete-bin-p (probe-file bin))))
      (if (directory-pathname-p probe)
          (delete-directory-tree probe :validate #'probe-file)
          (delete-file probe))))
  (traces obj))

(defmethod collect-trace ((obj binary-traceable) (test-case test-case)
                          &key max (bin (temp-file-name))
                          &aux (delete-bin-p t))
    "Execute instrumented OBJ on TEST-CASE collecting dynamic traces.
* OBJ Instrumented software object suitable for trace collection
* TEST-CASE test case to execute for trace collection
* MAX maximum number of trace points to record
* BIN compiled binary with instrumentation to use for trace collection
"
  (if (probe-file bin)
      (setf delete-bin-p nil)
      (restart-case
          (phenome obj :bin bin)
        (skip-test-case ()
          :report "Skip trace collection for test case and return NIL."
          (return-from collect-trace nil))))
  (unwind-protect
      (with-temp-file (handshake-file) ;; Start running the test case.
        (let ((proc (start-test bin test-case
                                :env (list (cons *instrument-handshake-env-name*
                                                 handshake-file))
                                :wait nil)))
          (labels ((timeout-p (start-time)
                     (> (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)
                        *trace-open-timeout*))
                   (handshake (pipe &aux (start-time (get-internal-real-time)))
                     ;; Create the handshake file, which indicates that we
                     ;; are ready to read traces. Write the pipe name to the
                     ;; handshake file to begin trace collection.
                     (iter (while (not (timeout-p start-time)))
                           (handler-case
                               (progn
                                 (with-output-to-file (out handshake-file)
                                   (format out "~a" pipe)
                                   (finish-output out))
                                 (finish))
                             (error (e)
                               (declare (ignorable e))
                               (sleep 1))))

                     (iter (while (not (timeout-p start-time)))
                           ;; The instrumented process will complete the
                           ;; handshake by deleting the file.
                           (handler-case
                               (unless (probe-file handshake-file)
                                 (return t))
                             (error (e)
                               ;; A race condition exists in the SBCL 1.4.7
                               ;; `probe-file' where if the handshake
                               ;; file is deleted during the execution
                               ;; of the function, an error will be thrown.
                               (declare (ignorable e))))

                           (unless (process-running-p proc)
                             (note 4 "Test process exited")
                             (return nil))

                           (finally (note 3 "No handshake after ~d seconds"
                                          *trace-open-timeout*)))))
            (iter (for i upfrom 0)
                  (while (with-temp-fifo (pipe)
                           (when (handshake pipe)
                             (add-trace (traces obj) pipe *trace-open-timeout*
                                        (list ;; keep :bin symbol if present
                                         (cons :input
                                               (cons (program-name test-case)
                                                     (program-args test-case))))
                                        :max max))))
                  (finally
                   (finish-test proc)
                   (restart-case
                       ;; This usually indicates a problem with the
                       ;; test script or the instrumentation
                       (when (zerop i)
                         (error (make-condition 'trace-error
                                 :text (format nil
                                         "No traces collected for test case ~s ~s."
                                         test-case
                                         (cons (program-name test-case)
                                               (program-args test-case)))
                                 :obj test-case
                                 :bin bin)))
                      (ignore-empty-trace ()
                        :report "Ignore empty trace")))))))
    (when-let ((probe (and delete-bin-p (probe-file bin))))
        (if (directory-pathname-p probe)
          (delete-directory-tree probe :validate #'probe-file)
          (delete-file probe)))))


;;; Trace collection with legacy s-expression format
(defmethod collect-traces ((obj sexp-traceable) (test-suite test-suite)
                           &key max (bin (temp-file-name)) (num-threads 1)
                           &aux (args (list :bin bin)) (delete-bin-p t))
  "Execute instrumented OBJ on TEST-SUITE collecting dynamic traces.
* OBJ Instrumented software object suitable for trace collection
* TEST-SUITE suite of test case to execute for trace collection
* MAX maximum number of trace points to record
* BIN compiled binary with instrumentation to use for trace collection
* NUM-THREADS number of threads to use in trace collection
"
  (when max (setf args (append args (list :max max))))
  (if (probe-file bin)
      (setf delete-bin-p nil)
      (restart-case
          (phenome obj :bin bin)
        (skip-trace-collection ()
          :report "Skip trace collection and leave object unchanged."
          (return-from collect-traces (traces obj)))
        (nil-traces ()
          :report "Set object traces to NIL and continue."
          (setf (traces obj) nil)
          (return-from collect-traces (traces obj)))))
  (setf (traces obj) (make-instance 'sexp-trace-db))
  (unwind-protect
       (task-map num-threads
                 (lambda (test-case)
                   (apply #'collect-trace obj test-case args))
                 (test-cases test-suite))
    (when-let ((probe (and delete-bin-p (probe-file bin))))
      (if (directory-pathname-p probe)
          (delete-directory-tree probe :validate #'probe-file)
          (delete-file probe))))
  (traces obj))

(defmethod collect-trace ((obj sexp-traceable) (test-case test-case)
                          &key (max infinity) (bin (temp-file-name))
                          &aux (delete-bin-p t))
    "Execute instrumented OBJ on TEST-CASE collecting dynamic traces.
* OBJ Instrumented java software object suitable for trace collection
* TEST-CASE test case to execute for trace collection
* MAX maximum number of trace points to record
* BIN compiled binary with instrumentation to use for trace collection
"
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
        ;; Start run on the input.
        (let ((proc (start-test bin test-case
                                :env (list (cons *instrument-log-env-name*
                                                 pipe))
                                :wait nil)))
          (restart-case
              (unless (add-trace (traces obj)
                                 pipe
                                 *trace-open-timeout*
                                 (list (cons :input
                                       (cons (program-name test-case)
                                             (program-args test-case))))
                                 :max max)
                ;; This usually indicates a problem with the
                ;; test script or the instrumentation
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
            (finish-test proc)))
    (when-let ((probe (and delete-bin-p (probe-file bin))))
      (if (directory-pathname-p probe)
          (delete-directory-tree probe :validate #'probe-file)
          (delete-file probe)))))

