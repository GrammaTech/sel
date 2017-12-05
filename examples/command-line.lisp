;;; command-line.lisp --- Command line helpers for software evolution

;; Copyright (C) 2011-2013  Eric Schulte

;;; License: GNU General Public License, Version 3 or later

;;; Commentary:

;;; Code:
(defpackage :software-evolution-library-command-line
  (:use :common-lisp :metabang-bind)
  (:export :quit :throw-error :getopts))
(in-package :software-evolution-library-command-line)
(enable-curry-compose-reader-macros :include-utf8)


(defun quit (&optional (errno 0))
  #+sbcl (sb-ext:exit :code errno)
  #+ccl  (ccl:quit errno))

(defun throw-error (&rest args)
  (apply #'format *error-output* args)
  (quit))

(defmacro getopts (args &rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop ,args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (and ,short (string= ,arg ,short))
                            (and ,long  (string= ,arg ,long)))
                        ,@body))
                    forms)))))
