;;; traceable --- instrumentable software objects with dynamic traces
(in-package :software-evolution)

(defclass traceable (software)
  ((traces :initarg :traces :accessor traces :initform nil
           :documentation "Execution traces from execution of the software."))
  (:documentation
   "Instrumentable software with support for collecting dynamic traces."))


;;; Execution with trace collection.
(defgeneric collect-trace (software input &key predicate max)
  (:documentation
   "Execute instrumented SOFTWARE on INPUT collecting a dynamic trace."))

(defmethod collect-trace
    ((obj software) input &key (predicate #'identity) (max infinity))
  (with-temp-file (bin)
    (assert (phenome obj :bin bin) (obj) "Unable to compile software ~a" obj)
    (with-temp-fifo (pipe)
      ;; Start run on the input.
      (let ((proc
             #+sbcl
              (sb-ext:run-program bin input
                                  :environment
                                  (cons (concatenate 'string
                                          *instrument-log-env-name* "=" pipe)
                                        (sb-ext:posix-environ))
                                  :wait nil)
              #+ccl
              (ccl:run-program bin input
                               :env (list (cons *instrument-log-env-name* pipe))
                               :wait nil)
              #-(or sbcl ccl)
              (error "Implement for non-SBCL (requires non-blocking shell).")))
        (list (cons :input input)
              (cons :trace (unwind-protect
                                ;; Read trace output from fifo.
                                (with-open-file (in pipe)
                                  (read-trace-stream
                                   in :predicate predicate :max max))
                             #+sbcl (sb-ext:process-close proc)
                             #+ccl (ccl:process-kill proc)
                             #-(or sbcl ccl)
                             (error "`collect-trace' needs SBCL or CCL."))))))))

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
