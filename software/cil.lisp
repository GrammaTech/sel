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
;;; cil.lisp --- cil software representation
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; cil software objects
(define-software cil (ast)
  ((compiler :initarg :compiler :accessor compiler :initform "gcc"))
  (:documentation "C abstract syntax trees using C Intermediate Language (CIL).
See http://kerneis.github.io/cil/."))

(defmethod apply-mutation ((cil cil) op)
  "DOCFIXME"
  (with-temp-file-of (src (ext cil)) (genome cil)
    (multiple-value-bind (stdout stderr exit)
        (shell "cil-mutate ~a ~a ~a"
               (ecase (car op)
                 (:cut    "-cut")
                 (:insert "-insert")
                 (:swap   "-swap")
                 (:ids    "-ids")
                 (:trace  "-trace"))
               (if (eq (car op) :trace)
                   (if (second op)
                       (format nil "-trace-file ~a" (second op))
                       "")
                   (mapconcat (lambda (pair)
                                (format nil "-stmt~d ~d" (car pair) (cdr pair)))
                              (loop :for id :in (cdr op) :as i :from 1
                                 :collect (cons i id)) " "))
               src)
      (unless (zerop exit)
        (error (make-condition 'mutate
                 :text (format nil "cil-mutate:~a" stderr)
                 :obj cil
                 :op op)))
      (setf (genome cil) stdout)))
  cil)

(defmethod instrument ((cil cil) &key points functions functions-after
                                      trace-file trace-env
                                      instrument-exit filter)
  "Instrument CIL for traced execution.
Optionally specify the name of the file in which to save trace data."
  (unless (null trace-env)
    (warn "Tracing to env variable is not support for CIL software objects."))
  (unless (null points)
    (warn
     "Program point instrumentation not supported for CIL software objects."))
  (unless (and (null functions) (null functions-after))
    (warn
     "Custom function instrumentation not supported for CIL software objects."))
  (unless (null instrument-exit)
    (warn
     "Custom instrument-exit not supported for CIL software objects."))
  (unless (null filter)
    (warn
     "Custom filter not supported for CIL software objects."))
  (apply-mutation cil (list :trace trace-file))
  cil)

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
