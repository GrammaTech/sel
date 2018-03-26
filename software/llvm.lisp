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
;;; llvm.lisp --- llvm software representation

;; This software object uses the llvm-mutate [1] compiler pass run by
;; llvm opt [2] to manipulate LLVM intermediate representation (IR)
;; code (see [3] for more information).

;; [1] https://github.com/eschulte/llvm-mutate
;; [2] http://llvm.org/releases/3.2/docs/CommandGuide/opt.html
;; [3] http://llvm.org/docs/LangRef.html#introduction

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; llvm software objects
(define-software llvm (ast)
  ((ext      :initarg :ext      :accessor ext      :initform "ll")
   (compiler :initarg :compiler :accessor compiler :initform "llc")
   (linker   :initarg :linker   :accessor linker   :initform "gcc"))
  (:documentation
   "Low Level Virtual Machine (LLVM) intermediate representation (IR).
See http://llvm.org)."))

(defmethod from-file ((llvm llvm) path)
  "DOCFIXME

* LLVM DOCFIXME
* PATH DOCFIXME
"
  (setf (genome llvm) (file-to-string path))
  (setf (ext llvm)  (pathname-type (pathname path)))
  llvm)

(defmethod mutate ((llvm llvm))
  "DOCFIXME"
  (unless (> (size llvm) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj llvm)))
  (let ((op (case (random-elt '(cut replace insert swap))
              (cut     `(:cut     ,(pick-bad llvm)))
              (replace `(:replace ,(pick-bad llvm) ,(pick-good llvm)))
              (insert  `(:insert  ,(pick-bad llvm) ,(pick-good llvm)))
              (swap    `(:swap    ,(pick-bad llvm) ,(pick-good llvm))))))
    (apply-mutation llvm op)
    (values llvm op)))

(defmethod apply-mutation ((llvm llvm) op)
  "DOCFIXME

* LLVM DOCFIXME
* OP DOCFIXME
"
  (with-temp-file-of (src (ext llvm)) (genome llvm)
    (multiple-value-bind (stdout stderr exit)
        (shell "cat ~a|llvm-mutate --~a ~a"
               src
               (string-downcase (symbol-name (car op)))
               (mapconcat [{format nil "~a"} #'1+] (cdr op) ","))
      (declare (ignorable stdout stderr))
      (unless (zerop exit)
        (error (make-condition 'mutate
                 :text "llvm-mutate" :obj llvm :op op)))
      llvm)))

(defmethod phenome ((llvm llvm) &key (bin (temp-file-name)))
  "DOCFIXME

* LLVM DOCFIXME
* BIN DOCFIXME
"
  #-ccl (declare (values t fixnum string string string))
  (setf bin (ensure-path-is-string bin))
  (with-temp-file-of (src (ext llvm)) (genome llvm)
    (multiple-value-bind (stdout stderr errno)
        (shell "cat ~a|~a|~a ~{~a~^ ~} -x assembler - -o ~a"
               src (compiler llvm) (linker llvm) (flags llvm) bin)
      (declare (ignorable stdout stderr))
      (values bin errno stderr stdout src))))

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
