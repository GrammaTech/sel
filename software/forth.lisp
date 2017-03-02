;;; forth.lisp --- software representation of Forth code

;; Copyright (C) 2014  Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; asm software objects
(define-software forth (simple)
  ((shebang :initarg :shebang :accessor shebang :initform nil)))

(defmethod from-file ((forth forth) path &aux strings)
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
  #-ccl (declare (values string fixnum string string string))
  (setf bin (ensure-path-is-string bin))
  (string-to-file (format nil "~a~%~a" (shebang forth) (genome-string forth))
                  bin)
  (multiple-value-bind (stdout stderr errno) (shell "chmod +x ~s" bin)
    (values bin errno stderr stdout bin)))

(declaim (inline genome-string))
(defmethod genome-string ((forth forth) &optional stream)
  (format stream "~{~a ~}~%" (lines forth)))
