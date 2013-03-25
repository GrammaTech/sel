;;; lisp.lisp --- software rep of Lisp code (internal eval)

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
(require :trivial-timeout)
(use-package :trivial-timeout)


;;; the class of lisp software objects
(defclass lisp (software)
  ((genome :initarg :genome :accessor genome :initform nil)))

(defmethod func ((lisp lisp)) (eval (genome lisp)))

(defvar *test-forms* nil "Forms used to `evaluate' a `lisp' instance.")
(defvar *test-timeout* 0.05 "Time limit used to `evaluate' a `lisp' instance.")

(defmethod from-file ((lisp lisp) file)
  (with-open-file (in file)
    (setf (genome lisp)
          (loop :for form = (read in nil :eof)
             :until (eq form :eof)
             :collect form)))
  lisp)

(defmethod to-file ((software lisp) path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (dolist (form (genome software))
      (format out "~&~S~%" form))))

#+nil ;; NOTE: don't want to include trivial-timeout just now
(defmacro with-harness (&rest body)
  ;; TODO: protect against stack overflow
  `(locally
       (declare
        #+sbcl (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind
         (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning))
       (handler-case (with-timeout (*test-timeout*) ,@body)
         (timeout-error (c) (declare (ignorable c)) :timeout)
         (error (e) (declare (ignorable e)) :error)))))

(defmethod evaluate ((lisp lisp))
  (count-if (lambda (result)
              (case result ((:timeout :error) nil) (t result)))
            (mapcar (lambda (test)
                      (with-harness (funcall test (func lisp))))
                    *test-forms*)))
