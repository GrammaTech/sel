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

;;; Software Object with an executable
(defclass software-exe (software)
  ((exe :initarg :exe :accessor raw-exe :initform nil))
  (:documentation "DOCFIXME"))

(defgeneric exe (software &optional place)
  (:documentation
   "Return the path to an executable of the software. (caching)"))

(defmethod (setf exe) (new (software software-exe))
  "DOCFIXME"
  (setf (raw-exe software) new))

(defmethod exe :around ((software software-exe) &optional place)
  "DOCFIXME

* SOFTWARE DOCFIXME
* PLACE DOCFIXME
"
  (declare (ignorable place))
  (or (raw-exe software) (setf (exe software) (or (call-next-method) :failed))))

(defgeneric delete-exe (software)
  (:documentation
   "Delete any external executables associated with the software."))

(defmethod delete-exe ((software software-exe))
  "DOCFIXME"
  (when (raw-exe software)
    (when (and (not (eq :failed (raw-exe software)))
               (probe-file (exe software)))
      (delete-file (exe software)))
    (setf (exe software) nil)))

(defun evaluate-with-script (software script pos-num neg-num)
  "Evaluate SOFTWARE with SCRIPT.
POS-NUM is the number of positive tests defined in SCRIPT NEG-NUM is
the number of negative tests.  SCRIPT will be called with the
following arguments.

  $ SCRIPT SOFTWARE-EXECUTABLE pN for all N upto POS-NUM
  $ SCRIPT SOFTWARE-EXECUTABLE nN for all N upto NEG-NUM

SCRIPT should return 0 on success and 1 on failure."
  (let ((pos 0) (neg 0))
    (if (eq (exe software) :failed)
        0
        (progn
          (loop :for i :from 1 :to pos-num
             :do (multiple-value-bind (output err-output exit)
                     (shell "~a ~a p~d" script (exe software) i)
                   (declare (ignorable output err-output))
                   (when (= exit 0) (incf pos))))
          (loop :for i :from 1 :to neg-num
             :do (multiple-value-bind (output err-output exit)
                     (shell "~a ~a n~d" script (exe software) i)
                   (declare (ignorable output err-output))
                   (when (= exit 0) (incf neg))))
          (incf *fitness-evals*)
          (delete-exe software)
          (+ pos neg)))))

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
