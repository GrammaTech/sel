#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
;;; instrument --- generic interface for instrumentation
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation

(defvar *instrument-log-env-name* "__SEL_TRACE_FILE"
  "Default environment variable in which to store log file.")

(defvar *instrument-handshake-env-name* "__SEL_HANDSHAKE_FILE"
  "Default environment variable in which to store log file.")

(define-constant +instrument-log-variable-name+ "__sel_trace_file"
  :test #'string=
  :documentation "Variable used for instrumentation.")

(define-constant +instrument-log-lock-variable-name+ "__sel_trace_file_lock"
  :test #'string=
  :documentation "File lock variable used for instrumentation")

(defgeneric instrumented-p (obj)
  (:documentation "Return true if OBJ is instrumented"))

(defgeneric instrument (obj &key points functions functions-after
                                 trace-file trace-env instrument-exit filter)
  (:documentation
   "Instrument OBJ to print AST index before each full statement.

The indices printed here are not clang-mutate counters, but rather the
position of the ast in (asts obj).

Keyword arguments are as follows:
  POINTS --------------- alist of additional values to print at specific points
  FUNCTIONS ------------ functions to calculate instrumentation at each point
  FUNCTIONS-AFTER ------ functions to calculate instrumentation after each point
  TRACE-FILE ----------- file for trace output
  TRACE-ENV ------------ trace output to file specified by ENV variable
  INSTRUMENT-EXIT ------ print counter of function body before exit
  FILTER --------------- function to select a subset of ASTs for instrumentation
"))

(defgeneric uninstrument (obj)
  (:documentation "Remove instrumentation from OBJ"))

(defgeneric instrumentation-files (project)
  (:documentation
   "Return files in PROJECT in the order which they would be instrumented"))

(defclass instrumenter ()
  ((software :accessor software :initarg :software :initform nil))
  (:documentation "Base class for objects which handle instrumentation.
Stores instrumentation state and provides methods for instrumentation
operations."))

#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
