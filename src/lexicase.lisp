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
;;; lexicase selection

;; Fitness values should be a vector of numeric scores.
;; (A key option may be supplied for non-numeric scores.)
;;
;; Where each entry represents a single test case or objective.  All
;; fitness values in the same population must be the same length and
;; have their scores in the same order.

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defvar *lexicase-key* nil
  "Optional key function for components of test vector.")

(defun lexicase-select (population max-size)
  "Choose max-size individuals from the population by lexicase selection.
The same individual may be selected multiple times."
  (assert
   (= 1 (length (remove-duplicates population :key [#'length #'fitness])))
   (population)
   "All fitness vectors must be the same length.")
  (iter (for n below max-size)
        (collect (funcall *tournament-tie-breaker*
                          (lexicase-select-best population)))))

(defun lexicase-select-best (group &key (predicate *fitness-predicate*))
  "Choose best individuals by lexicase selection.

If there is a tie after all tests, return all remaining individuals.

Set the value of `*tournament-selector*' to `lexicase-select-best' to
use lexicase-style selection in tournament selection."
  (iter (for which in (shuffle (iota (1- (length (fitness (first group)))))))
        (setf group
              ;; Keep individuals with the highest score on the current test.
              (remove-if-not
               [{equal (extremum (mapcar [{elt _ which} #'fitness] group)
                                 predicate :key *lexicase-key*)}
                {elt _ which} #'fitness]
               group :key *lexicase-key*))
        ;; Stop when we get down to one individual
        (until (not (cdr group))))
  group)

(defun lexicase-better-p (order fitness-a fitness-b)
  "Compare fitness vectors with a fixed order."

  (iter (for which in order)
        (let ((a (elt fitness-a which))
              (b (elt fitness-b which)))

          (cond
            ((funcall *fitness-predicate* a b) (return t))
            ((funcall *fitness-predicate* b a) (return nil))))))

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
