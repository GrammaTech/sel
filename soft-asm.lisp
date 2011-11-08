;;; soft-asm.lisp --- software representation of Assembly files

;; Copyright (C) 2011  Eric Schulte

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
(in-package :soft-ev)

(defclass soft-asm (soft) ())

(defmethod from ((soft soft-asm) (in stream) &aux genome)
  (loop for line = (read-line in nil)
     while line do (push `((:line . ,line)) genome))
  (setf (genome soft) (reverse genome))
  soft)

(defmethod to ((soft soft-asm) (to stream))
  (dolist (line (genome soft))
    (format to "~a~%" (cdr (assoc :line line)))))

(defun asm-from-file (path)
  (let ((new (make-instance 'soft-asm)))
    (with-open-file (in path) (from new in))))

(defun asm-to-file (soft path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (to soft out)))

(defun link (asm exe)
  (multiple-value-bind (output error-output exit)
      (shell-command (format nil "gcc -o ~a ~a" exe asm))
    (values output error-output exit)))

(defmethod executable ((soft soft-asm) (exe string))
  (let ((tmp (temp-file-name "s")))
    (asm-to-file soft tmp)
    (multiple-value-bind (output err-output exit)
        (link tmp exe)
      (unless *keep-source* (when (probe-file tmp) (delete-file tmp)))
      (values exit output err-output))))
