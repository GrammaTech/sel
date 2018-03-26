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
;;; Concrete implementation of the database interface
;;; for an external JSON fodder database parsed and stored
;;; entirely within the current LISP image
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defclass json-database (in-memory-database)
  ((json-stream
    :initarg :json-stream :accessor json-stream
    :initform (error "JSON-STREAM field is required for DATABASE.")
    :documentation "Stream of incoming JSON."))
  (:documentation "DOCFIXME"))

(defmethod print-object ((db json-database) stream)
  "DOCFIXME

* DB DOCFIXME
* STREAM DOCFIXME
"
  (print-unreadable-object (db stream :type t)
    (when (subtypep (type-of (json-stream db)) 'file-stream)
      (format stream "~a:" (pathname (json-stream db))))
    (prin1 (length (ast-database-list db)) stream)))

(defmethod initialize-instance :after ((db json-database) &key)
  "DOCFIXME"
  ;; Initialize (load) a new json database.
  (dolist (snippet (load-json-with-caching db))
    (cond ((and (assoc :hash snippet)
                (assoc :reqs snippet)
                (assoc :type snippet))
           ;; Types
           (setf (gethash (aget :hash snippet)
                          (type-database-ht db))
                 snippet))
          ((and (assoc :hash snippet)
                (assoc :name snippet)
                (assoc :body snippet))
           ;; Macros
           (setf (gethash (aget :hash snippet)
                          (macro-database-ht db))
                 snippet))
          (t ;; ASTs
             (when-let ((ast-class (aget :ast-class snippet)))
               (setf (ast-database-list db)
                     (cons snippet (ast-database-list db)))
               (setf (ast-database-full-stmt-list db)
                     (if (aget :full-stmt snippet)
                         (cons snippet (ast-database-full-stmt-list db))
                         (ast-database-full-stmt-list db)))
               (setf (gethash ast-class (ast-database-ht db))
                     (cons snippet
                           (gethash ast-class (ast-database-ht db)))))))))

(defmethod load-json-with-caching ((db json-database))
  "DOCFIXME"
  (let ((json:*identifier-name-to-key* 'se-json-identifier-name-to-key))
    (if (subtypep (type-of (json-stream db)) 'file-stream)
        (let* ((json-db-path (pathname (json-stream db)))
               (json-stored-db-path (make-pathname
                                     :directory (pathname-directory json-db-path)
                                     :name (pathname-name json-db-path)
                                     :type "dbcache")))
          (if (and (probe-file json-stored-db-path)
                   (> (file-write-date json-stored-db-path)
                      (file-write-date json-db-path)))
              ;; Cache exists and is newer than the original
              ;; JSON database; use the cache.
              (cl-store:restore json-stored-db-path)
              ;; Cache does not yet exist or has been invalidated;
              ;; load from JSON and write back to the cache.
              (cl-store:store (json:decode-json-from-source (json-stream db))
                              json-stored-db-path)))
        (json:decode-json-from-source (json-stream db)))))

(defun se-json-identifier-name-to-key (json-identifier)
  "DOCFIXME"
  (make-keyword (string-upcase (regex-replace-all "--" json-identifier "-"))))

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
