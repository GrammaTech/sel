;;; traceable --- instrumentable software objects with dynamic traces
(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)

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

(defmethod collect-traces ((obj software) inputs
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
             (mapcar
               (lambda-bind ((i input))
                 (note 2 "Collect trace ~a of ~a" (1+ i) (length inputs))
                 (apply #'collect-trace obj input args))
               (indexed inputs)))
    (when (and delete-bin-p (probe-file bin)) (delete-file bin))))

(defgeneric collect-trace (software input &key predicate max bin)
  (:documentation
   "Execute instrumented SOFTWARE on INPUT collecting a dynamic trace.
INPUT should be a program and sequence of arguments.  The special
element :BIN in INPUT will be replaced with the name of the compiled
instrumented binary.  PREDICATE may be a function in which case only
trace points for which PREDICATE returns true are recorded.  Max
specifies the maximum number of trace points to record.  BIN specifies
the name of an already-compiled binary to use."))

(defmethod collect-trace
    ((obj software) input
     &key (predicate #'identity) (max infinity) (bin (temp-file-name))
     &aux (delete-bin-p t))
  (if (probe-file bin)
      (setf delete-bin-p nil)
      (assert (phenome obj :bin bin) (obj)
              "Unable to compile software ~a" obj))
  (unwind-protect
       (with-temp-fifo (pipe)
         ;; Start run on the input.
         (let* ((real-input (mapcar (lambda (it)
                                      (if (eq :bin it) (namestring bin) it))
                                    input))
                (proc
                 #+sbcl
                  (sb-ext:run-program (car real-input) (cdr real-input)
                                      :environment
                                      (cons (concatenate 'string
                                                         *instrument-log-env-name* "=" pipe)
                                            (sb-ext:posix-environ))
                                      :wait nil)
                  #+ccl
                  (ccl:run-program (car real-input) (cdr real-input)
                                   :env (list
                                         (cons *instrument-log-env-name* pipe))
                                   :wait nil)
                  #-(or sbcl ccl)
                  (error "Implement for lisps other than SBCL and CCL.")))
           (list (cons :input input)    ; keep :bin symbol if present
                 (cons :trace (unwind-protect
                                   ;; Read trace output from fifo.
                                   (with-open-file (in pipe)
                                     (read-trace-stream
                                      in :predicate predicate :max max))
                                #+sbcl (sb-ext:process-close proc)
                                #+ccl (ccl:signal-external-process proc 15
                                                          :error-if-exited nil)
                                #-(or sbcl ccl)
                                (error "Needs SBCL or CCL."))))))
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
