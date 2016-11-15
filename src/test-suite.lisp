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
