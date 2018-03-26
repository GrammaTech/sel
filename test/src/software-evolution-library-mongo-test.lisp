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
(in-package :software-evolution-library/mongo-test)
(in-readtable :curry-compose-reader-macros)

#-gt (load (make-pathname :name "testbot"
                          :type "lisp"
                          :directory (pathname-directory
                                      #.(or *compile-file-truename*
                                            *load-truename*
                                            *default-pathname-defaults*))))


;;;; Mongo Database tests.
(sel-suite* mongo-database-tests "Mongo database tests."
            (and (fboundp 'mongo-database)
                 (let ((host "dog")
                       (port 27017))
                   (handler-case (make-instance 'mongo-database
                                                :db "euler_test_clang_O0_no_pic"
                                                :host host
                                                :port port)
                     (usocket:ns-host-not-found-error (e)
                       (declare (ignorable e))
                       nil)))))

(defixture mongo-database
  (:setup
   (setf *database*
         (let ((host "dog")
               (port 27017))
           (make-instance 'mongo-database
                          :db "euler_test_clang_O0_no_pic"
                          :host host
                          :port port))))
  (:teardown
   (setf *database* nil)))

(deftest mongo-database-find-snippet-respects-class ()
  (with-fixture mongo-database
    (when *database*
      (is (null (-<>> (find-snippets *database* :ast-class "CompoundStmt")
                      (remove "CompoundStmt" <> :test #'string=
                              :key {aget :ast-class})))))))

(deftest mongo-database-find-snippet-respects-decl ()
  (with-fixture mongo-database
    (when *database*
      (is (null (->> (find-snippets *database* :decls nil)
                     (remove-if-not {aget :is-decl})))))))

(deftest mongo-database-find-snippet-respects-full-stmt ()
  (with-fixture mongo-database
    (when *database*
      (is (null (->> (find-snippets *database* :full-stmt t)
                     (remove-if {aget :full-stmt})))))))

(deftest mongo-database-find-snippet-is-random ()
  (with-fixture mongo-database
    (when *database*
      (let ((picks (loop :for i :from 0 :to 5
                         :collect (aget :hash (find-snippets *database*
                                                             :limit 1)))))
        (equal picks (remove-duplicates picks))))))

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
