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
;;;; software object with dynamically adapting mutation probabilities
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(define-constant +initial-mutation-results-queue+
  (make-array 1024
              :element-type '(cons symbol symbol)
              :initial-element (cons :nothing :nothing))
  :test #'equalp
  :documentation
  "Initial value of the *mutation-results-queue*")

(defvar *mutation-results-queue* (copy-seq +initial-mutation-results-queue+)
  "Queue containing pairs (MUTATION-TYPE . MUTATION-RESULT) for
the last *max-mutation-results-queue-length* mutations")

(defvar *mutation-results-queue-next* (the fixnum 0)
  "DOCFIXME")

(defvar *mutation-results-queue-lock*
  (make-lock "mutation-results-queue")
  "DOCFIXME")

(defvar *bias-toward-dynamic-mutation* 1/2
  "Degree to which dynamic weights are emphasized over default weights.")
(defvar *better-bias* 5/4
  "DOCFIXME")
(defvar *same-bias* 1
  "DOCFIXME")
(defvar *worse-bias* 1/10
  "DOCFIXME")
(defvar *dead-bias* 0
  "DOCFIXME")

(define-software adaptive-mutation (software)
  ()
  (:documentation "DOCFIXME"))

(defun queue-mutation (type classification)
  "DOCFIXME

* TYPE DOCFIXME
* CLASSIFICATION DOCFIXME
"
  (declare (optimize speed))
  (with-lock-held (*mutation-results-queue-lock*)
    (setf (the (cons symbol symbol)
               (aref *mutation-results-queue* *mutation-results-queue-next*))
          (the (cons symbol symbol) (cons type classification)))
    (incf (the fixnum *mutation-results-queue-next*))
    (when (= (the fixnum (length (the vector *mutation-results-queue*)))
             (the fixnum *mutation-results-queue-next*))
      (setf *mutation-results-queue-next* 0))))

(defun adaptive-analyze-mutation (obj mutation-info test)
  "Adaptively update mutation probabilities based on the result of the mutation"
  (when (not (zerop *bias-toward-dynamic-mutation*))
    (destructuring-bind (mutation software-a cross-point-a
                                  crossed software-b cross-point-b)
        mutation-info
      (declare (ignorable software-a cross-point-a
                          software-b cross-point-b))
      (evaluate test crossed) ; Evaluate for fitness
      (evaluate test obj)     ; Safety - should have fitness
      (queue-mutation (type-of mutation) (classify obj crossed)))))

(defun update-mutation-types (mutation-types &aux by-type)
  "DOCFIXME

* MUTATION-TYPES DOCFIXME
* BY-TYPE DOCFIXME
"
  (flet ((dynamic-weight (mutation-results)
           ;; Return a new dynamic mutation probability weight
           ;; for MUTATION-TYPE by examining the results of previous
           ;; mutations of MUTATION-TYPE in MUTATION-RESULTS
           (mean (mapcar (lambda-bind ((type . bias))
                           (* (/ (count-if {equal _ type} mutation-results)
                                 (length mutation-results))
                              bias))
                         `((:better . ,*better-bias*)
                           (:same   . ,*same-bias*)
                           (:worse  . ,*worse-bias*)
                           (:dead   . ,*dead-bias*))))))
    (if (equal :nothing  ; Array is too small to update mutations.
               (car (aref *mutation-results-queue*
                          *mutation-results-queue-next*)))
      mutation-types
      (progn
        ;; Collect our accumulated mutations into association list.
        (map nil (lambda-bind ((type . result)) (push result (aget type by-type)))
             *mutation-results-queue*)
        (->> mutation-types
             (mapcar
              (lambda-bind ((type . prior-probability))
                (cons type
                      (if (aget type by-type)
                          ;; Take the weighted average of the static
                          ;; mutation probability and the dynamically
                          ;; calculated mutation probability.
                          (+ (* (- 1 *bias-toward-dynamic-mutation*)
                                prior-probability)
                             (* *bias-toward-dynamic-mutation*
                                (dynamic-weight (aget type by-type))
                                prior-probability))
                          prior-probability))))
             (normalize-probabilities)
             (cumulative-distribution))))))

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
