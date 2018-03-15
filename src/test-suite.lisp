
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
    :documentation "The executable that runs the test case.  
Contains either a string containing the name of an executable file or
special symbol :bin, which will be dynamically replaced by the name of
the compiled phenome for the software object under test.")
   (program-args
    :initarg :program-args :initform nil :accessor program-args
    :type list
    :documentation "A list of arguments which will be passed to the executable
PROGRAM-NAME. 
Each argument must be either a string or symbol :bin, which will be
replaced by the name of the compiled phenome.")
   (fitness
    :initarg :fitness :initform nil :accessor fitness
    :documentation "May be used to store the fitness result from a prior run of the test case. 
This field is not updated automatically since some components may need
to perform comparisons between new and old values (e.g., condition
synthesis)."))
  (:documentation "Test case object."))

(defmethod print-object ((obj test-suite) stream)
  "Print `test-suite' OBJ to STREAM"
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (test-cases obj))))

(defmethod print-object ((obj test-case) stream)
  "Print `test-case' OBJ to STREAM"
  (print-unreadable-object (obj stream :type t)
    (format stream "~a ~{~a ~}" (program-name obj) (program-args obj))))

(defgeneric start-test (phenome test-case &rest extra-keys
                        &key &allow-other-keys)
  (:documentation "Start an external process to run TEST-CASE on PHENOME and return the `process'.

This is essentially a wrapper around the SBCL or CCL `run-program' methods and any EXTRA-KEYS will be passed through to that method.

* PHENOME the phenome of the `software' object under test.
* TEST-CASE the `test-case' to run.
* EXTRA-KEYS additional keyword arguments to pass to the SBCL or CCL `run-program' method. 

Some EXTRA-KEYS that may be useful are:
* :output and :error - to specify how output and error streams are
  handled. In some cases, these are sent to /dev/null by default,
  making output inaccessible after the process completes, so it's
  often useful to set one or both of these to `:stream' to capture the
  output.

* :wait - whether to wait for the process to complete before
  continuing. The default is to wait; however, some
  components (such as `traceable') may elect not to wait and
  instead to stream results through a named pipe.

* :env - to set environment variables  
"))


(defgeneric finish-test (test-process &key kill-signal timeout)
  (:documentation "Ensure that TEST-PROCESS either runs to completion or is killed; return the standard output, error output, and process exit code as strings.

* TEST-PROCESS the `process' associated with the test case.

* KILL-SIGNAL if TEST-PROCESS is still running when `finish-test' is called, this signal will be sent to TEST-PROCESS in an effort to kill it.

* TIMEOUT if TEST-PROCESS is still running when `finish-test' is called, the Lisp process will sleep for this many seconds, then check if TEST-PROCESS is still running and send a SIGKILL signal if so.
"))


(defgeneric run-test (phenome test-case &rest extra-keys &key &allow-other-keys)
  (:documentation "Run TEST-CASE on PHENOME and return the results.

This is a convenience method whose default behavior is simply to run
`start-test' and `finish-test'.
"))


(defmethod start-test (phenome (test-case test-case) &rest extra-keys
		       &key &allow-other-keys)
  "DOCFIXME

* PHENOME DOCFIXME
* TEST-CASE DOCFIXME
* EXTRA-KEYS DOCFIXME
"
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
  "DOCFIXME

* TEST-PROCESS DOCFIXME
* KILL-SIGNAL DOCFIXME
* TIMEOUT DOCFIXME
"
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
  "DOCFIXME

* PHENOME DOCFIXME
* TEST-CASE DOCFIXME
* EXTRA-KEYS DOCFIXME
"
  (finish-test (apply #'start-test phenome test-case extra-keys)))

(defmethod evaluate (phenome (obj test-case) &rest extra-keys
			     &key &allow-other-keys)
   "Run `test-case' OBJ on PHENOME and return a fitness score (as
opposed to the output and exit code returned by `finish-test`).
Default behavior is to return 1 if the exit code is 0 and 0 otherwise.

Override this method if you want to use other criteria, such as
test output, to determine the fitness score.

- PHENOME the phenome of the `software' object under test.

- OBJ the `test-case' to run.

- EXTRA-KEYS additional keyword arguments to pass to the SBCL or CCL `run-program' method. See the `start-test' documentation for more information.
"  
  (multiple-value-bind (stdout stderr exit-code)
      (apply #'run-test phenome obj extra-keys)
    (declare (ignorable stdout stderr))
    (if (zerop exit-code) 1 0)))

(defmethod evaluate (phenome (test-suite test-suite) &rest extra-keys
                     &key &allow-other-keys)
  "Evaluate all test-cases in TEST-SUITE collecting their output.
By default, sum results of applying `evaluate' to each test-case using `reduce'.
Keyword arguments :function and :initial-value may be used as in `reduce' to
specify an aggregation function and starting value."
  (let ((keys (plist-drop :function (plist-drop :initial-value extra-keys))))
    (apply #'reduce (or (plist-get :function extra-keys) #'+)
           (mapcar (lambda (test) (apply #'evaluate phenome test keys))
                   (test-cases test-suite))
           (when-let ((val (plist-get :initial-value extra-keys)))
             (list :initial-value val)))))
