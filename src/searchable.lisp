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
;;; Interface for all external assets searchable for snippets
;;; similar to a given target.

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defclass searchable ()
  ()
  (:documentation "DOCFIXME"))

(defgeneric weighted-pick
    (searchable target weight
     &key predicate metric key limit ast-class filter limit-considered)
  (:documentation
   "Perform a random pick weighted by weight from `similar-snippets'.
All other arguments are passed through to sorted snippets."))

(defmethod weighted-pick ((obj searchable) target weight
                          &key key limit ast-class
                               (predicate #'<)
                               (metric #'diff-scalar)
                               (filter #'null)
                               (limit-considered infinity))
  "DOCFIXME"
  (random-elt-with-decay
    (similar-snippets obj target
                      :predicate predicate :metric metric
                      :key key :limit limit :ast-class ast-class
                      :filter filter :limit-considered limit-considered)
    weight))

(defgeneric similar-snippets
    (searchable target
     &key predicate metric key limit ast-class limit-considered filter)
  (:documentation
   "Return snippets from SEARCHABLE similar to TARGET
* SEARCHABLE DOCFIXME
* TARGET DOCFIXME
* :PREDICATE predicate for similarity metric
* :METRIC a function to generate a similarity metric
* :KEY a function called on each snippet before metric
* :LIMIT only return the MANY most similar snippets
* :AST-CLASS only consider snippets matching this AST class
* :LIMIT-CONSIDERED  limit search to MANY-CONSIDERED random snippets
* :FILTER limit search to snippets for which FILTER returns false"))

(defmethod similar-snippets ((db searchable) target
                            &key key ast-class limit
                                 (predicate #'<)
                                 (metric #'diff-scalar)
                                 (filter #'null)
                                 (limit-considered infinity))
  "DOCFIXME"
  (declare (ignorable target))
  (let ((base (sort (remove-if filter
                               (find-snippets db
                                 :ast-class ast-class :full-stmt (not ast-class)
                                 :limit limit-considered))
                    predicate :key [{funcall metric target}
                                    {funcall key}])))
    (if limit (take limit base) base)))

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
