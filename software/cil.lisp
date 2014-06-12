;;; cil.lisp --- cil software representation

;; Copyright (C) 2012  Eric Schulte

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

;;; Code:
(in-package :software-evolution)


;;; cil software objects
(defclass cil (ast)
  ((compiler :initarg :compiler :accessor compiler :initform "gcc")))

(defmethod apply-mutation ((cil cil) op)
  (with-temp-file-of (src (ext cil)) (genome cil)
    (multiple-value-bind (stdout stderr exit)
        (shell "cil-mutate ~a ~a ~a"
               (ecase (car op)
                 (:cut    "-cut")
                 (:insert "-insert")
                 (:swap   "-swap")
                 (:ids    "-ids"))
               (mapconcat (lambda (pair)
                            (format nil "-stmt~d ~d" (car pair) (cdr pair)))
                          (loop :for id :in (cdr op) :as i :from 1
                             :collect (cons i id)) " ")
               src)
      (unless (zerop exit)
        (error 'mutate
               :text (format nil "cil-mutate:~a" stderr) :obj cil))
      stdout)))

(defmethod phenome ((cil cil) &key bin)
  (with-temp-file-of (src (ext cil)) (genome cil)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell "~a ~a -o ~a ~{~a~^ ~}" (compiler cil) src bin (flags cil))
        (declare (ignorable stdout))
        (values (if (zerop exit) bin stderr) exit)))))
