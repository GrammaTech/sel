;;; test-suite.lisp --- an abstraction around test suites
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defclass test-suite ()
  ((test-cases
    :initarg :test-cases :initform nil :accessor test-cases :type list
    :documentation "List of `test-case' objects that make up the test suite."))
  (:documentation "A suite of unit tests."))

(defclass test-case ()
  ((program-name
    :initarg :program-name :initform nil :accessor program-name
    :type (or string symbol)
    :documentation "The name of an executable file which runs the test case.
The special symbol :bin may be used instead of a string and will be replaced by
the name of the compiled phenome.")
   (program-args
    :initarg :program-args :initform nil :accessor program-args
    :type list
    :documentation "A list of arguments which will be passed to the executable
PROGRAM-NAME. Occurrences of the symbol :bin will be replaced by the name of the
compiled phenome.")
   (fitness
    :initarg :fitness :initform nil :accessor fitness
    :documentation "Fitness result last time test-case was run."))
  (:documentation "Test case object."))

(defmethod print-object ((obj test-suite) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (test-cases obj))))

(defmethod print-object ((obj test-case) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a ~{~a ~}" (program-name obj) (program-args obj))))

(defclass process ()
  ((os-process
    :initarg :os-process :initform nil :reader os-process
    :documentation "The underlying process object (compiler-specific)."))
  (:documentation "Object representing an external process."))

;;; TODO: Promote `process-id', `process-input-stream',
;;; `process-output-stream', and `process-exit-code' to sel/utility,
;;; also not clear why process need by a class when the notion already
;;; exists in most lisps where it seems to be accessed by functions.
(defgeneric process-id (process)
  (:documentation "Return the process id for PROCESS"))

(defmethod process-id ((process process))
  #+sbcl
  (sb-ext:process-pid (os-process process))
  #+ccl
  (ccl:external-process-id (os-process process))
  #+ecl
  (ext:external-process-pid (os-process process))
  #-(or sbcl ccl ecl)
  (error "`PROCESS' only implemented for SBCL, CCL, or ECL."))

(defgeneric process-input-stream (process)
  (:documentation "Return the input stream for PROCESS."))

(defmethod process-input-stream ((process process))
  #+sbcl
  (sb-ext:process-input (os-process process))
  #+ccl
  (ccl:external-process-input-stream (os-process process))
  #+ecl
  (ext:external-process-input (os-process process))
  #-(or sbcl ccl ecl)
  (error "`PROCESS' only implemented for SBCL, CCL, or ECL."))

(defgeneric process-output-stream (process)
  (:documentation "Return the output stream for PROCESS."))

(defmethod process-output-stream ((process process))
  #+sbcl
  (sb-ext:process-output (os-process process))
  #+ccl
  (ccl:external-process-output-stream (os-process process))
  #+ecl
  (ext:external-process-output (os-process process))
  #-(or sbcl ccl ecl)
  (error "`PROCESS' only implemented for SBCL, CCL, or ECL."))

(defgeneric process-error-stream (process)
  (:documentation "Return the error stream for PROCESS."))

(defmethod process-error-stream ((process process))
  #+sbcl
  (sb-ext:process-error (os-process process))
  #+ccl
  (ccl:external-process-error-stream (os-process process))
  #+ecl
  (ext:external-process-error-stream (os-process process))
  #-(or sbcl ccl ecl)
  (error "`PROCESS' only implemented for SBCL, CCL, or ECL."))

(defgeneric process-exit-code (process)
  (:documentation "Return the exit code for PROCESS or nil if PROCESS has not
exited."))

(defmethod process-exit-code ((process process))
  #+sbcl
  (sb-ext:process-exit-code (os-process process))
  #+(or ccl ecl)
  (multiple-value-bind (status code)
      #+ccl (ccl:external-process-status (os-process process))
      #+ecl (ext:external-process-status (os-process process))
    (declare (ignorable status))
    code)
  #-(or sbcl ccl ecl)
  (error "`PROCESS' only implemented for SBCL, CCL, or ECL."))

(defgeneric process-status (process)
  (:documentation "Return the status of PROCESS: one of :running, :stopped,
:signaled, or :exited."))

(defmethod process-status ((process process))
  #+sbcl
  (sb-ext:process-status (os-process process))
  #+ (or ccl ecl)
  (multiple-value-bind (status code)
      #+ccl (ccl:external-process-status (os-process process))
      #+ecl (ext:external-process-status (os-process process))
    (declare (ignorable code))
    status)
  #-(or sbcl ccl ecl)
  (error "`PROCESS' only implemented for SBCL, CCL, or ECL."))

(defgeneric signal-process (process signal-number)
  (:documentation "Send the signal SIGNAL-NUMBER to PROCESS."))

(defmethod signal-process ((process process) (signal-number integer))
  (multiple-value-bind (stdout stderr errno)
      (shell "kill -~d -$(ps -o pgid= ~d | ~
                          grep -o '[0-9]*' | ~
                          head -n 1 | ~
                          tr -d ' ')"
             signal-number
             (process-id process))
    (declare (ignorable stdout stderr))
    (zerop errno)))

(defgeneric start-test (phenome test-case &rest extra-keys
                        &key &allow-other-keys)
  (:documentation "Begin running TEST-CASE on PHENOME and return the process."))

(defgeneric finish-test (test-process &key kill-signal timeout)
  (:documentation "Complete TEST-PROCESS and return the test results. Optionally,
KILL-SIGNAL specifies a signal to use to kill the process if it is still
running, and TIMEOUT specifies an amount of time to wait between sending the kill
signal and sending the SIGKILL signal to ensure the process is killed."))

(defgeneric run-test (phenome test-case &rest extra-keys &key &allow-other-keys)
  (:documentation "Run TEST-CASE on PHENOME and return the results."))

(defmethod start-test (phenome (test-case test-case) &rest extra-keys
                       &key &allow-other-keys)
  (flet ((bin-sub (bin it)
           (if (eq :bin it) bin it)))
    (let ((real-name (bin-sub (namestring phenome) (program-name test-case)))
          (real-args (mapcar {bin-sub (namestring phenome)}
                             (program-args test-case))))
      (when *shell-debug*
        (format t "  cmd: ~a ~{~a ~}~%" real-name real-args))
      (make-instance 'process
        :os-process
        #+sbcl
        (apply #'sb-ext:run-program real-name real-args
               ;; Ensure environment values are specified as a list of
               ;; KEY=VALUE strings expected by SBCL's :environment
               ;; keyword argument to `sb-ext:run-program'.
               (iter (for arg in extra-keys)
                     (for prev previous arg)
                     (cond ((eq arg :env)
                            (collect :environment))
                           ((eq prev :env)
                            (collect (append
                                      (mapcar (lambda-bind ((key . value))
                                                (concatenate 'string
                                                  key "=" value))
                                              arg)
                                      (sb-ext:posix-environ))))
                           (t (collect arg)))))
        #+ccl
        (apply #'ccl:run-program real-name real-args extra-keys)
        #+ecl
        (nth-value 2 (apply #'ext:run-program real-name real-args
                            (mapcar (lambda (it) (if (eq it :env) :environ it))
                                    extra-keys)))
        #-(or sbcl ccl ecl)
        (error "`START-TEST' only supported for SBCL, CCL, or ECL.")))))

(defmethod finish-test ((test-process process) &key kill-signal timeout)
  (flet ((running-p (process)
           (eq :running (process-status process))))
    (when (and kill-signal (running-p test-process))
      ;; If process is running and there's a kill signal, send it.
      (signal-process test-process kill-signal))

    ;; If still running and there's a timeout, sleep.
    (when (and (running-p test-process)
               (and timeout (>= timeout 0)))
      (sleep timeout))

    ;; If still running, send SIGKILL.
    (when (running-p test-process)
      (signal-process test-process 9)

      ;; If still running and there's a timeout, sleep.
      (when (and (running-p test-process)
                 (and timeout (>= timeout 0)))
        (sleep timeout))

      ;; If it's *still* running, warn someone.
      (when (running-p test-process)
        (note 0 "WARNING: Unable to kill process ~a" test-process)))

    ;; Now that we've made every effort to kill it, read the output.
    (let* ((stdout (stream-to-string (process-output-stream test-process)))
           ;; Can't read from error stream if it's the same as stdout.
           (stderr (unless (eq (process-output-stream test-process)
                               (process-error-stream test-process))
                     (stream-to-string (process-error-stream test-process))))
           (exit-code (process-exit-code test-process)))
      (when *shell-debug*
        (format t "~&stdout:~a~%stderr:~a~%errno:~a" stdout stderr exit-code))
      (values stdout stderr exit-code))))

(defmethod run-test (phenome (test-case test-case) &rest extra-keys
                     &key &allow-other-keys)
  (finish-test (apply #'start-test phenome test-case extra-keys)))

(defmethod evaluate (phenome (obj test-case) &rest extra-keys
                     &key &allow-other-keys)
  (multiple-value-bind (stdout stderr exit-code)
      (apply #'run-test phenome obj extra-keys)
    (declare (ignorable stdout stderr))
    (if (zerop exit-code) 1 0)))
