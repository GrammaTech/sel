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
;;; forth.lisp --- software representation of Forth code
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; asm software objects
(define-software forth (simple)
  ((shebang :initarg :shebang :accessor shebang :initform nil))
  (:documentation "Forth program represented as a list of commands."))

(defmethod from-file ((forth forth) path &aux strings)
  "Read forth script from PATH setting `genome' and `shebang'."
  (setf (genome forth)
        (flet ((rm-eol-comments (line)
                 (subseq line 0 (search "\\ " line)))
               (rm-inline-comments (line)
                 (cl-ppcre:regex-replace-all "[\\s]\\([\\s][^\\)]*\\)" line ""))
               (rm-strings (line)
                 (let ((start 0) out)
                   (loop :while
                      (multiple-value-bind (match-start match-end)
                          (scan "[\.sS]\"[^\"]+\"" line :start start)
                        (when match-start
                          (push (subseq line start match-start) out)
                          (push :string out)
                          (push (subseq line match-start match-end) strings)
                          (setq start match-end))))
                   (push (subseq line start) out)
                   (nreverse out))))
          (with-open-file (in path)
            (loop :for line = (read-line in nil) :while line :append
               (if (and (> (length line) 2) (string= "#! " (subseq line 0 3)))
                   (prog1 nil (setf (shebang forth) line))
                   (mapcar [#'list {cons :code}]
                           (mapcan (lambda (el)
                                     (if (equal :string el)
                                         (list (pop strings))
                                         (remove-if #'emptyp
                                                    (split "[\\s]+"
                                                           (rm-inline-comments
                                                            el)))))
                                   (rm-strings (rm-eol-comments line)))))))))
  forth)

(defmethod phenome ((forth forth) &key (bin (temp-file-name)))
  "Write FORTH to an executable script suitable for evaluation."
  #-ccl (declare (values t fixnum string string string))
  (setf bin (ensure-path-is-string bin))
  (string-to-file (format nil "~a~%~a" (shebang forth) (genome-string forth))
                  bin)
  (multiple-value-bind (stdout stderr errno) (shell "chmod +x ~s" bin)
    (values bin errno stderr stdout bin)))

(declaim (inline genome-string))
(defmethod genome-string ((forth forth) &optional stream)
  "Return genome of FORTH as a string.
Like `(genome-string simple)' but lines are delimited by spaces
instead of Newlines."
  (format stream "~{~a ~}~%" (lines forth)))

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
