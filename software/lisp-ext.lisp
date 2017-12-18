;;; lisp-exe.lisp --- software rep of Lisp code (external eval)

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
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; the class of lisp software objects
(defclass lisp-exe (software-exe) ())

(defvar *test-script*  nil "Script capable of running tests.")
(defvar *pos-test-num* nil "Number of positive tests")
(defvar *neg-test-num* nil "Number of negative tests")

(defmethod from-file ((lisp-exe lisp-exe) file)
  (with-open-file (in path)
    (setf (genome lisp-exe)
          (loop :for form = (read in nil :eof)
             :until (eq form :eof)
             :collect form)))
  lisp-exe)

(defun lisp-exe-from-file (path)
  (from-file (make-instance 'lisp-exe) path))

(defun lisp-exe-to-file (software path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (dolist (form (genome software))
      (format out "~&~S" form))))

(defmethod exe ((lisp-exe lisp-exe) &optional place)
  (let ((exe (or place (temp-file-name))))
    (lisp-exe-to-file lisp-exe exe)
    exe))

(defmethod evaluate ((lisp-exe lisp-exe))
  (evaluate-with-script lisp-exe *test-script* *pos-test-num* *neg-test-num*))
