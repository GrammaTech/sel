;;; by-function.lisp --- program repair of individual CL functions

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
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


(defmacro defadapt (name args conditions &body body)
  "Define a function with associated meta-data used for evolution."
  (let ((func-name (gensym "function"))
        (condition (gensym "condition")))
    `(let ((,func-name (defun ,name ,args ,@body)))
       (dolist (,condition ,conditions)
         (setf (get ,func-name (car ,condition)) (cdr ,condition)))
       ,func-name)))
