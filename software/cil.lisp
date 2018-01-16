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
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; cil software objects
(define-software cil (ast)
  ((compiler :initarg :compiler :accessor compiler :initform "gcc"))
  (:documentation "C abstract syntax trees using C Intermediate Language (CIL).
See http://kerneis.github.io/cil/."))

(defmethod apply-mutation ((cil cil) op)
  (with-temp-file-of (src (ext cil)) (genome cil)
    (multiple-value-bind (stdout stderr exit)
        (shell "cil-mutate ~a ~a ~a"
               (ecase (car op)
                 (:cut    "-cut")
                 (:insert "-insert")
                 (:swap   "-swap")
                 (:ids    "-ids")
                 (:trace  "-trace"))
               (if (eq (car op) :trace)
                   (if (second op)
                       (format nil "-trace-file ~a" (second op))
                       "")
                   (mapconcat (lambda (pair)
                                (format nil "-stmt~d ~d" (car pair) (cdr pair)))
                              (loop :for id :in (cdr op) :as i :from 1
                                 :collect (cons i id)) " "))
               src)
      (unless (zerop exit)
        (error (make-condition 'mutate
                 :text (format nil "cil-mutate:~a" stderr)
                 :obj cil
                 :op op)))
      (setf (genome cil) stdout)))
  cil)

(defmethod instrument ((cil cil) &key points functions functions-after
                                      trace-file trace-env
                                      instrument-exit filter
                                      postprocess-functions)
  "Instrument CIL for traced execution.
Optionally specify the name of the file in which to save trace data."
  (declare (ignorable postprocess-functions))

  (unless (null trace-env)
    (warn "Tracing to env variable is not support for CIL software objects."))
  (unless (null points)
    (warn
     "Program point instrumentation not supported for CIL software objects."))
  (unless (and (null functions) (null functions-after))
    (warn
     "Custom function instrumentation not supported for CIL software objects."))
  (unless (null instrument-exit)
    (warn
     "Custom instrument-exit not supported for CIL software objects."))
  (unless (null filter)
    (warn
     "Custom filter not supported for CIL software objects."))
  (apply-mutation cil (list :trace trace-file))
  cil)
