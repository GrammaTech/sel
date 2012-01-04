;;; lisp.lisp --- software representation of Lisp source code

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
(in-package :software-evolution)


;;; the class of lisp software objects
(defclass lisp (software-exe) ())

(defvar *test-script*  nil "Script capable of running tests.")
(defvar *pos-test-num* nil "Number of positive tests")
(defvar *neg-test-num* nil "Number of negative tests")

(defun lisp-from-file (path)
  (let ((new (make-instance 'lisp)))
    (with-open-file (in path)
      (setf (genome new)
            (loop :for form = (read in nil :eof)
               :until (eq form :eof)
               :collect form)))
    new))

(defun lisp-to-file (software path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (dolist (form (genome software))
      (format out "~&~S" form))))

(defmethod exe ((lisp lisp) &optional place)
  (let ((exe (or place (temp-file-name))))
    (lisp-to-file lisp exe)
    exe))

(defmethod evaluate ((lisp lisp))
  (evaluate-with-script lisp *test-script* *pos-test-num* *neg-test-num*))
