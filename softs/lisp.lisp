;;; soft-lisp.lisp --- software representation of Lisp source code

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


;;; the class of lisp software objects
(defclass soft-lisp (soft) ())

(defmethod from ((soft soft-lisp) (in stream))
  (setf (genome soft)
        (loop :for form = (read in nil :eof)
           :until (eq form :eof)
           :collect form)))

(defmethod to ((soft soft-lisp) (to stream))
  (dolist (form (genome soft))
    (format to "~&~S" form)))

(defun lisp-from-file (path)
  (let ((new (make-instance 'soft-lisp)))
    (with-open-file (in path) (from new in))
    new))

(defun lisp-to-file (soft path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (to soft out)))

(defmethod exe ((lisp soft-lisp) &optional place)
  (declare (ignorable lisp place))
  (error "Lisp software objects are interpreted not compiled."))


;;; execution tracing in lisp source code


;;; weighted genome access
(defun good-key (el)
  (if (or (assoc :pos el) (assoc :neg el)) 1 0.25))

(defun bad-key (el)
  (if (assoc :neg el) (if (assoc :pos el) 0.5 1) 0.25))

(defmethod good-ind ((lisp soft-lisp))
  (weighted-ind (genome lisp) #'good-key))

(defmethod bad-ind ((lisp soft-lisp))
  (weighted-ind (genome lisp) #'bad-key))

(defmethod good-place ((lisp soft-lisp))
  (weighted-place (genome lisp) #'good-key))

(defmethod bad-place ((lisp soft-lisp))
  (weighted-place (genome lisp) #'bad-key))
