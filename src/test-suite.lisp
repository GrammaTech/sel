;;; test-suite.lisp --- an abstraction around test suites

#|
*************************************************************************************
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* UNLIMITED RIGHTS
*
* The Government's rights to use, modify, reproduce, release, perform, display, or
* disclose this software are governed by DFARS 252.227-7013, RIGHTS IN TECHNICAL DATA
* --NONCOMMERCIAL ITEMS, and DFARS 252.227-7014 RIGHTS IN NONCOMMERCIAL SOFTWARE AND
* NONCOMMERCIAL COMPUTER SOFTWARE DOCUMENTATION.
*
*************************************************************************************
*
* All GrammaTech IP (sole or co-developed) needs to include the GrammaTech copyright.
*
* (c) 2016 GrammaTech, Inc.  All rights reserved.
*
* Such IP is also subject to the terms of the Prioprietary Information Agreement (PIA)
* executed between BAE Systems Information and Electronics Systems Integration Inc.
* and GrammaTech, Inc. dated April 21, 2015
*
*************************************************************************************
|#

(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)

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

(defclass process ()
  ((os-process
    :initarg :os-process :initform nil :reader os-process
    :documentation "The underlying process object (compiler-specific)."))
  (:documentation "Object representing an external process."))

(defgeneric process-input-stream (process)
  (:documentation "Return the input stream for PROCESS."))

(defmethod process-input-stream ((process process))
  #+sbcl
  (sb-ext:process-input (os-process process))
  #+ccl
  (ccl:external-process-input-stream (os-process process))
  #-(or sbcl ccl)
  (error "`PROCESS' only implemented for SBCL or CCL."))

(defgeneric process-output-stream (process)
  (:documentation "Return the output stream for PROCESS."))

(defmethod process-output-stream ((process process))
  #+sbcl
  (sb-ext:process-output (os-process process))
  #+ccl
  (ccl:external-process-output-stream (os-process process))
  #-(or sbcl ccl)
  (error "`PROCESS' only implemented for SBCL or CCL."))

(defgeneric process-error-stream (process)
  (:documentation "Return the error stream for PROCESS."))

(defmethod process-error-stream ((process process))
  #+sbcl
  (sb-ext:process-error (os-process process))
  #+ccl
  (ccl:external-process-error-stream (os-process process))
  #-(or sbcl ccl)
  (error "`PROCESS' only implemented for SBCL or CCL."))

(defgeneric process-exit-code (process)
  (:documentation "Return the exit code for PROCESS or nil if PROCESS has not
exited."))

(defmethod process-exit-code ((process process))
  #+sbcl
  (sb-ext:process-exit-code (os-process process))
  #+ccl
  (multiple-value-bind (status code)
      (ccl:external-process-status (os-process process))
    (declare (ignorable status))
    code)
  #-(or sbcl ccl)
  (error "`PROCESS' only implemented for SBCL or CCL."))

(defgeneric process-status (process)
  (:documentation "Return the status of PROCESS: one of :running, :stopped,
:signaled, or :exited."))

(defmethod process-status ((process process))
  #+sbcl
  (sb-ext:process-status (os-process process))
  #+ccl
  (multiple-value-bind (status code)
      (ccl:external-process-status (os-process process))
    (declare (ignorable code))
    status)
  #-(or sbcl ccl)
  (error "`PROCESS' only implemented for SBCL or CCL."))

(defgeneric signal-process (process signal-number)
  (:documentation "Send the signal SIGNAL-NUMBER to PROCESS."))

(defmethod signal-process ((process process) (signal-number integer))
  #+sbcl
  (sb-ext:process-kill (os-process process) signal-number)
  #+ccl
  (ccl:signal-external-process (os-process process) signal-number))

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
  (bind (((:flet bin-sub (bin it))
          (if (eq :bin it) bin it))
         (real-name (bin-sub (namestring phenome) (program-name test-case)))
         (real-args (mapcar {bin-sub (namestring phenome)}
                            (program-args test-case))))
    (when *shell-debug*
      (format t "  cmd: ~a ~{~a ~}~%" real-name real-args))
    (let ((proc
           #+sbcl
            (apply #'sb-ext:run-program real-name real-args extra-keys)
            #+ccl
            (apply #'ccl:run-program real-name real-args extra-keys)
            #-(or sbcl ccl)
            (error "`START-TEST' only supported for SBCL or CCL.")))
      (make-instance 'process :os-process proc))))

(defmethod finish-test ((test-process process) &key kill-signal timeout)
  (flet ((running-p (process)
           (eq :runing (process-status process))))
    (when (and kill-signal (running-p test-process))
      ;; if process is running and there's a kill signal, send it
      (signal-process test-process kill-signal))

    ;; if still running and there's a timeout, sleep
    (when (and (running-p test-process)
               (and timeout (>= timeout 0)))
      (sleep timeout))

    ;; if still running, SIGKILL
    (when (running-p test-process)
      (signal-process test-process 9)
      ;; if it's *still* running warn someone
      (when (running-p test-process)
        (note 0 "WARNING: Unable to kill process ~a" test-process)))

    ;; Now that we've made every effort to kill it, read the output
    (let* ((stdout (stream-to-string (process-output-stream test-process)))
           ;; can't read from error stream if it's the same as stdout
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


(defmethod evaluate (phenome (test-case test-case) &rest extra-keys
                     &key &allow-other-keys)
  (multiple-value-bind (stdout stderr exit-code)
      (apply #'run-test phenome test-case extra-keys)
    (declare (ignorable stdout stderr))
    (if (zerop exit-code) 1 0)))
