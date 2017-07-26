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

(defclass test-case ()
  ((test-runner
    :initarg :test-runner :initform nil :accessor test-runner
    :type function
    :documentation "Function to run the test case. Takes as input a phenome and
optional environment.")
   (fitness
    :initarg nil :initform nil :accessor fitness
    :documentation "Result from the last time this test case was run."))
  (:documentation "Test case object."))

(defun make-scripted-test-suite (test-runner-gen test-count)
  "Create a list of test-case objects. TEST-RUNNER-GEN is a function of two
arguments (a test ID from 0 to TEST-COUNT and the binary name) which returns a
command string that will be invoked by `shell-with-env'. The test-case's
`test-runner' function will run the command and return a result written to
stdout."
  (iter (for i below test-count)
        (let ((test i))
          (collecting
           (make-instance
            'test-case
            :test-runner
            (lambda (bin &optional env)
              (multiple-value-bind (stdout stderr exit)
                  (shell-with-env env (funcall test-runner-gen test bin))
                (if (zerop exit)
                    (let ((val (read-from-string stdout)))
                      (if (listp val) (car val) val))
                    (error "test command failed: ~a" stderr)))))))))

(defun make-test-suite (test-runner-gen test-count)
  (iter (for i below test-count)
        (let ((runner-fun (funcall test-runner-gen i)))
          (collecting (make-instance
                       'test-case
                       :test-runner
                       runner-fun)))))
