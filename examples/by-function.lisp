;;; by-function.lisp --- program repair of individual CL functions

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

;;; Commentary:

;; The goal of this code is to provide for automated evolution of
;; individual CL functions.  A new `defadaptive' form is provided for
;; the specification of functions with associated meta-data for use by
;; evolution.  The two main pieces of meta-data (at least initially)
;; are `tests' and `alternatives'.
;; 
;;        tests -- CL forms used to evaluate the fitness of variants
;; alternatives -- used to hold a list of alternative
;;                 implementations

;;; Code:
(in-package :software-evolution)

(defmacro defadapt (name args conditions &body body)
  "Define a function with associated meta-data used for evolution."
  (let ((func-name (gensym "function"))
        (condition (gensym "condition")))
    `(let ((,func-name (defun ,name ,args ,@body)))
       (dolist (,condition ,conditions)
         (setf (get ,func-name (car ,condition)) (cdr ,condition)))
       ,func-name)))
