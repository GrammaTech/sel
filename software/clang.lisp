;;; clang.lisp --- clang software representation

;; Copyright (C) 2012 Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(in-package :software-evolution)

(defclass clang (ast)
  ((compiler :initarg :compiler :accessor compiler :initform "clang")))

(defmethod apply-mutation ((clang clang) op)
  (clang-mutate clang op))

(defmethod clang-mutate ((clang clang) op)
  (with-temp-file-of (src (ext clang)) (genome clang)
    (multiple-value-bind (stdout stderr exit)
      (shell "clang-mutate ~a ~a ~a -- ~{~a~^ ~}|tail -n +2"
             (ecase (car op)
               (:cut          "-cut")
               (:insert       "-insert")
               (:swap         "-swap")
               (:set-value    "-set")
               (:insert-value "-insert-value")
               (:ids          "-ids")
               (:list         "-list")
               (:list-json    "-list -json"))
             (mapconcat (lambda (pair)
                          (if (stringp (cdr pair))
                              (format nil "-value='~a'" (cdr pair))
                              (format nil "-stmt~d=~d" (car pair) (cdr pair))))
                        (loop :for id :in (cdr op) :as i :from 1
                           :collect (cons i id)) " ")
             src (flags clang))
      stdout)))

(defmethod phenome ((clang clang) &key bin)
  (with-temp-file-of (src (ext clang)) (genome clang)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell "~a ~a -o ~a ~a"
                 (compiler clang)
                 src bin (mapconcat #'identity (flags clang) " "))
        (declare (ignorable stdout stderr))
        (values (if (zerop exit) bin stderr) exit)))))
