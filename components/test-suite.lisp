;;; test-suite.lisp --- Object to hold test suites.
(defpackage :software-evolution-library/components/test-suite
  (:nicknames :sel/components/test-suite :sel/cp/test-suite)
  (:use :gt/full
   :software-evolution-library
        :software-evolution-library/utility/debug)
  #-windows (:shadowing-import-from :uiop :wait-process)
  (:export :*process-sleep-interval*
   :*process-kill-timeout*
   :test-suite
   :test-cases
   :test-case
   :program-name
   :program-args
   :time-limit
   :limit-path
   :start-test
   :finish-test
   :run-test
   :all-tests-suite
   :all-tests-case
   :random-test-suite
   :analyze-fitness
   :create-all-tests-suite
   :create-random-test-suite
   :update-random-test-suite))
;; dummy definition for Windows
#+windows (defun uiop::wait-process (x) 0)
(in-package :software-evolution-library/components/test-suite)
(in-readtable :curry-compose-reader-macros)

(defvar *process-sleep-interval* 0.1
  "Frequency (in seconds) at which to check if a process has completed.")

(defvar *process-kill-timeout* nil
  "Optional timeout (in seconds) before killing a process")

(defclass test-suite (oid-object)
  ((test-cases
    :initarg :test-cases :initform nil :accessor test-cases :type list
    :documentation "List of `test-case' objects that makeup the test suite."))
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
    :documentation "May be used to store the fitness result from a prior run of
the test case. This field is not updated automatically since some components
may need to perform comparisons between new and old values (e.g., condition
synthesis).")
   (time-limit
    :initarg :time-limit
    :initform nil
    :accessor time-limit
    :documentation "If non-nil, specifies the maximum number of seconds the test
is allowed to run before terminating it.")
   (limit-path
    :initarg :limit-path
    :initform "limit"
    :accessor limit-path
    :documentation "Specifies the path to the 'limit' executable."))
  (:documentation "Test case object, to be used in test cases of a test suite."))

(defmethod print-object ((obj test-suite) stream)
  "Print `test-suite' OBJ to STREAM"
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (length (test-cases obj)))))

(defmethod print-object ((obj test-case) stream)
  "Print `test-case' OBJ to STREAM"
  (print-unreadable-object (obj stream :type t)
    (format stream "~a~a ~{~a ~}"
            (if (numberp (time-limit obj))
                (format nil "~a -s ~d " (limit-path obj) (time-limit obj))
                "")
            (program-name obj)
            (program-args obj))))

(defgeneric start-test (phenome test-case &rest extra-keys
                        &key &allow-other-keys)
  (:documentation "Start an external process to run TEST-CASE on PHENOME.
Return the `process'.

* PHENOME the phenome of the `software' object under test.
* TEST-CASE the `test-case' to run.
* EXTRA-KEYS additional keyword arguments."))


(defgeneric finish-test (test-process)
  (:documentation
   "Ensure that TEST-PROCESS either runs to completion or is killed.
Return the standard output, error output, and process exit code as strings.

* TEST-PROCESS the `process' associated with the test case.
"))


(defgeneric run-test (phenome test-case &rest extra-keys &key &allow-other-keys)
  (:documentation "Run TEST-CASE on PHENOME and return the results.
Return the standard output, error output, and process exit code as strings."))


(defmethod start-test (phenome (test-case test-case) &rest extra-keys
                       &key &allow-other-keys)
  "Start an external process to run TEST-CASE on PHENOME.
Return the `process'. This is a wrapper around `uiop:launch-program', and any
EXTRA-KEYS will be passed through to that method.

* PHENOME the phenome of the `software' object under test.
* TEST-CASE the `test-case' to run.
* EXTRA-KEYS additional keyword arguments to pass to `uiop:launch-program'.

Some EXTRA-KEYS that may be useful are:
* :output and :error-output - to specify how output and error streams are
  handled. By default, these are captured so they can be returned as strings.

* :wait - whether to wait for the process to complete before continuing.
  The default is to wait; however, some components may elect not to wait
  and instead to stream results through a named pipe.

* :env - to set environment variables
"
  (flet ((bin-sub (bin it)
           (if (eq :bin it) bin it)))
    (let* ((real-cmd (mapcar {bin-sub (namestring phenome)}
                             (cons (program-name test-case)
                                   (program-args test-case))))
           (output (cond (*shell-debug* :stream)
                         ((member :output (plist-keys extra-keys))
                          (plist-get :output extra-keys))
                         (t :stream)))
           ;; Backwards compatible: we used to use :error to refer to the error
           ;; output stream, but uiop:run-program standardizes to :error-output
           (error-output (cond (*shell-debug* :stream)
                               ((member :error-output (plist-keys extra-keys))
                                (plist-get :error-output extra-keys))
                               ((member :error (plist-keys extra-keys))
                                (plist-get :error extra-keys))
                               (t :stream))))

      (plist-drop :output extra-keys)
      (plist-drop :error-output extra-keys)
      (plist-drop :error extra-keys)

      ;; if time limit is specified, modify real-cmd with limit call
      (when (numberp (time-limit test-case))
        (setf real-cmd
              (append (list (limit-path test-case) "-s"
                            (format nil "~A" (time-limit test-case)))
                      real-cmd)))

      (when *shell-debug*
        (format t "~&  cmd: ~{~a ~}~%" real-cmd))

      #-windows
      (apply #'uiop:launch-program real-cmd
             :output output
             :error-output error-output
             #+sbcl
             ;; Ensure environment values are specified as a list of
             ;; KEY=VALUE strings expected by SBCL's :environment
             ;; keyword argument to `sb-ext:run-program'.
             (iter (for arg in extra-keys)
                   (for prev previous arg)
                   (cond ((eq arg :env)
                          (collect :environment))
                         ((eq prev :env)
                          (collect (append
                                    (mapcar
                                     (lambda (pair)
                                       (destructuring-bind (key . value) pair
                                         (concatenate 'string
                                           key "=" value)))
                                     arg)
                                    (sb-ext:posix-environ))))
                         (t (collect arg))))
             #+ccl
             extra-keys
             #+ecl
             (mapcar (lambda (it) (if (eq it :env) :environ it))
                     extra-keys)))))

(defmethod finish-test (test-process)
  "Ensure that TEST-PROCESS either runs to completion or is killed.
Return the standard output, error output, and process exit code as strings.

* TEST-PROCESS the `process-info' object returned by `launch-program'
associated with the test case.

See also `*process-sleep-interval*' and `*process-kill-timeout*'. If
TEST-PROCESS is still running when `finish-test' is called, and if
`*process-kill-timeout*' is bound to a non-nil value greater than 0,
alternate between sleeping for `*process-sleep-interval*' seconds and
checking again until either `*process-kill-timeout*' seconds have
elapsed or the process completes. After the timeout, send a SIGTERM
signal if TEST-PROCESS is still running, sleep for
`*process-sleep-interval*' seconds once more and then if the process
is still running, send a SIGKILL signal."
  (labels ((read-and-close (stream)
             (when stream
               (prog1
                   (read-stream-content-into-string stream)
                 (close stream)))))
    ;; If still running and there are timeout and sleep intervals, sleep up to
    ;; timeout, checking if process is still running every sleep-interval seconds.
    (when (and *process-kill-timeout* (> *process-kill-timeout* 0)
               *process-sleep-interval* (> *process-sleep-interval* 0)
               (process-alive-p test-process))
      (iter (for i below (floor (/ *process-kill-timeout*
                                   *process-sleep-interval*)))
	    (if (process-alive-p test-process)
		(sleep *process-sleep-interval*)
		(leave t)))

      ;; Send a non-urgent kill signal (SIGTERM) to the process and all its children
      (when (process-alive-p test-process)
        (kill-process test-process))

      ;; If still running, sleep short interval, then send an urgent kill signal
      ;; (SIGKILL), to the process and all its children
      (when (process-alive-p test-process)
        (sleep *process-sleep-interval*)
        (when (process-alive-p test-process)
          (kill-process test-process :urgent t))

        ;; If it's *still* running, warn someone.
        (when (process-alive-p test-process)
          (note 0 "WARNING: Unable to kill process ~a" test-process))))

    ;; Now that it's finished, or we've made every effort to kill it,
    ;; read the output.
    (let* ((stdout (read-and-close (process-info-output test-process)))
           ;; Can't read from error stream if it's the same as stdout.
           (stderr (unless (eq (process-info-output test-process)
                               (process-info-error-output test-process))
                     (read-and-close (process-info-error-output test-process))))
           (exit-code (wait-process test-process)))
      (when *shell-debug*
        (format t "~&stdout:~a~%stderr:~a~%errno:~a~%" stdout stderr exit-code))
      (values stdout stderr exit-code))))

(defmethod run-test (phenome (test-case test-case) &rest extra-keys
                     &key &allow-other-keys)
  "Run TEST-CASE on PHENOME and return the results.
Return three values: output printed to standard out, error output, and exit
status. Th default behavior is simply to run `start-test' and `finish-test'."
  (finish-test (apply #'start-test phenome test-case extra-keys)))

;;; TODO: The following two `evaluate' methods really must be changed
;;;       to use a different name.  They conflict with the defgeneric
;;;       for evaluate defined in software-evolution-library.lisp.
(defmethod evaluate (phenome (obj test-case) &rest extra-keys
			     &key &allow-other-keys)
   "Run `test-case' OBJ on PHENOME and return a fitness score (as
opposed to the output and exit code returned by `finish-test`).
Default behavior is to return 1 if the exit code is 0 and 0 otherwise.

Override this method if you want to use other criteria, such as
test output, to determine the fitness score.

- PHENOME the phenome of the `software' object under test.

- OBJ the `test-case' to run.

- EXTRA-KEYS additional keyword arguments to pass to `run-test'. See the
`start-test' documentation for more information.
"
  (multiple-value-bind (stdout stderr exit-code)
      (apply #'run-test phenome obj extra-keys)
    (declare (ignorable stdout stderr))
    (if (zerop exit-code) 1 0)))

(defmethod evaluate (phenome (test-suite test-suite) &rest extra-keys
                     &key &allow-other-keys)
  "Evaluate all test-cases in TEST-SUITE aggregating their output.
By default, sum the results of applying `evaluate' to each test-case using
`reduce'. Keyword arguments `:function' and `:initial-value' may be used as in
`reduce' to specify an aggregation function and starting value."
  (let ((keys (plist-drop :function (plist-drop :initial-value extra-keys))))
    (apply #'reduce (or (plist-get :function extra-keys) #'+)
           (mapcar (lambda (test) (apply #'evaluate phenome test keys))
                   (test-cases test-suite))
           (when-let ((val (plist-get :initial-value extra-keys)))
             (list :initial-value val)))))

(defclass all-tests-suite (test-suite) ()
  (:documentation "Like test-suite, but a single test-case runs all tests."))
(defclass all-tests-case (test-case) ()
  (:documentation "A test case to run all tests in test script. A single one of
 these should be included in ALL-TESTS-SUITE"))

(defclass random-test-suite (test-suite)
  ((failed-tests :initarg :failed-tests :accessor failed-tests :initform nil)
   (full-tests :initarg :full-tests :accessor full-tests :initform nil)
   (num-random-tests :initarg :num-random-tests :accessor num-random-tests)))

(defmethod test-cases :around ((suite random-test-suite))
  "Return both the randomly-selected cases and the previously failed tests."
  (append (call-next-method)
          (failed-tests suite)))

(defgeneric analyze-fitness (obj exit-code stdout stderr)
  (:documentation
   "Determine fitness based on output of exit-code, stdout and stderr")
  (:method ((obj all-tests-case) exit-code stdout stderr)
                                        ; default to fitness in (0, 1)
    (if (zerop exit-code)
        1 ; success, return 1
        0)))

(defmethod evaluate (phenome (obj all-tests-case) &rest extra-keys
		     &key &allow-other-keys)
  "Run `test-case' OBJ on PHENOME and return a fitness score (as
 opposed to the output and exit code returned by `finish-test`).
 Return total number of test cases passing, if all pass, or 0 if an error
 occurs. The ANALYZE-FITNESS method can be overridden to provide the correct
 count of passing tests based on exit-code, stdout and stderr.

- PHENOME the phenome of the `software' object under test.

- OBJ the `test-case' to run.

- EXTRA-KEYS additional keyword arguments to pass to `run-test'. See the
`start-test' documentation for more information.
"
  (multiple-value-bind (stdout stderr exit-code)
      (apply #'run-test phenome obj extra-keys)
    (declare (ignorable stdout stderr))
    (analyze-fitness obj exit-code stdout stderr)))

(defun create-all-tests-suite (script
                               &key time-limit limit-path all-tests-case-class)
  "Create a test script with a single test-case, which runs all tests."
  (make-instance
   'all-tests-suite
   :test-cases
   (list (make-instance (or all-tests-case-class 'all-tests-case)
                        :program-name (car (split-sequence #\space script))
                        :program-args
                        (append (cdr (split-sequence #\space script)) '(:bin))
                        :time-limit time-limit
                        :limit-path limit-path))))

(defparameter *max-failed-tests* 100
  "This is the maximum number of unique failed tests we maintain during
 evolution. When the number exceeds this we remove the oldest from the queue.")

(defun randomly-select-n (list n)
  "Return randomly selected subset list of length n"
  (let ((len (length list)))
    (iter (for i from 0 below (min n len))
      (let* ((r (random len))
             (rando (elt list r)))
        (setf list (remove rando list :test 'eq))
        (collect rando)
        (decf len)))))

(defun create-random-test-suite (test-suite num-random-tests)
  "Create an instance of random-test-suite containing random subset of
 scripted tests."
  (let ((random-suite
          (make-instance 'random-test-suite
                         :full-tests test-suite
                         :num-random-tests num-random-tests
                         :failed-tests nil
                         :test-cases
                         (randomly-select-n
                          (test-cases test-suite) num-random-tests))))
    random-suite))

(defun update-random-test-suite (random-test-suite)
  "Update the set of random test cases and return modified suite."
  (setf (test-cases random-test-suite)
        (randomly-select-n
         (test-cases (full-tests random-test-suite))
         (num-random-tests random-test-suite)))
  random-test-suite)

(defmethod evaluate (phenome (test-suite random-test-suite) &rest extra-keys
                     &key &allow-other-keys)
  "Evaluate all test-cases in TEST-SUITE aggregating their output.
By default, sum the results of applying `evaluate' to each test-case using
`reduce'. Keyword arguments `:function' and `:initial-value' may be used as in
`reduce' to specify an aggregation function and starting value."
  (let ((keys (plist-drop :function (plist-drop :initial-value extra-keys))))
    (note 2 "Evaluating random-test-suite with ~D tests."
          (length (test-cases test-suite)))
    (apply #'reduce (or (plist-get :function extra-keys) #'+)
           (mapcar
            (lambda (test)
              #+debug (note 2 "Evaluating test-case.")
              (let ((result (apply #'evaluate phenome test keys)))
                (when (zerop result)
                  (note 2 "Failed test ~A~%" test)
                  (pushnew test (failed-tests test-suite)
                           :test 'equal :key 'program-args) ;track failed tests
                  (if (length> (failed-tests test-suite) *max-failed-tests*)
                      (setf (failed-tests test-suite)
                            (subseq (failed-tests test-suite) 0
                                    *max-failed-tests*)))
                  (return-from evaluate result))
                result))
            (test-cases test-suite))
           (when-let ((val (plist-get :initial-value extra-keys)))
             (list :initial-value val)))))
