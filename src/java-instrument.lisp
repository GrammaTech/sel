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
;;; java-instrument --- Instrument java-language source files
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation

(defclass java-instrumenter (instrumenter)
  ((file-id :accessor file-id :initform 0))
  (:documentation "Handles instrumentation for JAVA software objects."))

(defmethod instrumented-p ((obj java))
  (search *instrument-log-env-name* (genome obj)))

(defmethod instrumented-p ((obj java-project))
  (some #'instrumented-p (mapcar #'cdr (evolve-files obj))))

(defmethod instrument ((obj java) &rest args)
  (apply #'instrument (make-instance 'java-instrumenter :software obj)
         args))

(defmethod instrument
  ((instrumenter java-instrumenter)
   &key points functions functions-after trace-file trace-env instrument-exit
     (filter #'identity)
   &aux (obj (software instrumenter)))
  (declare (ignorable points functions functions-after
                      trace-file trace-env
                      instrument-exit filter))
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (java-jar-exec (format nil "-instrument ~a -out=~a -file=~a"
                           src-file
                           (directory-namestring src-file)
                           (file-id instrumenter)))
    (setf (genome obj) (file-to-string src-file)))

  obj)

(defmethod uninstrument ((obj java))
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (java-jar-exec (format nil "-uninstrument ~a -out=~a"
                           src-file
                           (directory-namestring src-file)))
    (setf (genome obj) (file-to-string src-file)))
  obj)

(defmethod instrument ((java-project java-project) &rest args
                       &aux (instrumenter (make-instance 'java-instrumenter)))
  (declare (ignorable args))
  (iterate (for (f . obj) in (instrumentation-files java-project))
           (for i upfrom 1)
           (note 3 "Instrumenting ~a" f)
           (note 4 "Instrument progress: ~a/~a"
                 i (length (instrumentation-files java-project)))
           (setf (software instrumenter) obj)
           (setf (file-id instrumenter) (1- i))
           (instrument instrumenter))
  java-project)

(defmethod uninstrument ((java-project java-project))
  (iter (for (f . obj) in (instrumentation-files java-project))
        (for i upfrom 1)
        (note 3 "Uninstrumenting ~a" f)
        (note 4 "Uninstrument progress: ~a/~a"
              i (length (instrumentation-files java-project)))
        (uninstrument obj))
  java-project)

(defmethod instrumentation-files ((java-project java-project))
  (evolve-files java-project))

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
