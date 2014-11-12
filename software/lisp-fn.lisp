;;; lisp-fn.lisp --- software representation of individual Lisp functions

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
(defclass lisp-fn (lisp) ())

(defvar *test-script*  nil "Script capable of executing external code.")

(defmacro externally (&body body)
  "Evaluate body in a protective external process."
  `(multiple-value-bind (output err-output exit)
       (shell "~a ~a" *test-script* (format nil "~S" `(format nil "~S" ,@body)))
     (if (= exit 0)
         (read-from-string output)
         (error "external execution failed"))))

(defmethod evaluate ((lisp-fn lisp-fn))
  (externally
   (mapcar #'test-flopped
           (funcall #'collect-test-results `(lambda () ,(get lisp-fn 'tests))))))
