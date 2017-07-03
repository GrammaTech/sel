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

(defclass test-suite () ())

(defgeneric test-cases (suite)
  (:documentation
   "List of test cases. The type of the test cases themselves is
    implementation-dependent."))

(defgeneric run-test (suite phenome case &key environment)
  (:documentation
   "Execute a test case, returning a fitness score.

  SUITE --------- The test-suite object.
  CASE ---------- A test case, as returned by the test-cases method.
  PHENOME ------- The phenome to test.
  ENVIRONMENT --- The environment in which to run the test. The type and
                  interpretation of this argument is implementation-dependent."))

(defgeneric case-fitness (suite case)
  (:documentation "The saved fitness score for an individual test case."))

(defgeneric (setf case-fitness) (value suite case))


(defclass scripted-test-suite (test-suite)
  ((command-gen
    :initarg :command-gen :accessor command-gen
    :documentation "Function which generates a shell command to run tests. It
should take two parameters: the executable and a list of command-line arguments.
Additionally, the script should accept as a flag '--which N' to run test case
N.")
   (test-count :initarg :test-count :initform nil :accessor test-count
               :documentation "Number of test cases in suite.")
   (fitness :initform nil :accessor fitness
            :documentation "Vector containing fitness for each test case.")))

(defgeneric run-test-script (suite phenome env &rest args)
  (:documentation "Run a test script in SUITE with shell environment ENV, passing
PHENOME and ARGS as parameters to the script."))

(defmethod run-test-script ((suite scripted-test-suite) phenome env &rest args)
  (multiple-value-bind (stdout stderr exit)
      (shell-with-env env (funcall (command-gen suite) phenome args))
    (if (zerop exit)
        (let ((val (read-from-string stdout)))
          (if (listp val) (car val) val))
        (error "test command failed: ~a" stderr))))

(defun make-scripted-test-suite (command-gen test-count)
  (let* ((suite (make-instance 'scripted-test-suite
                               :command-gen command-gen
                               :test-count test-count)))
    (setf (fitness suite) (make-array test-count :initial-element nil))
    suite))

(defmethod run-test ((suite scripted-test-suite) phenome case &key environment)
  (run-test-script suite phenome environment case))

(defmethod test-cases ((suite scripted-test-suite))
  (iota (test-count suite)))

(defmethod case-fitness ((suite scripted-test-suite) case)
  (elt (fitness suite) case))

(defmethod (setf case-fitness) (value (suite scripted-test-suite) (case integer))
  (setf (elt (fitness suite) case)
        value))
