#!/usr/local/bin/clisp
;; -*- mode: lisp -*-
(unless (= (length ext:*args*) 2)
  (error "usage: mem-mapping asm-file.s bin-file"))

(let ((*standard-output* nil)
      (*error-output* nil))
  (load (merge-pathnames ".clisprc.lisp" (user-homedir-pathname)))
  (require :cl-ppcre)
  (require :split-sequence))
(defpackage :mm (:use :cl :cl-ppcre :split-sequence))
(in-package :mm)

(defun shell (&rest rst)
  (let ((in (ext:run-shell-command
             (apply #'format (cons nil rst)) :output :stream)))
    (loop for line = (read-line in nil :eof)
       until (eq line :eof) collect line)))

(defvar asm-path (first ext:*args*))
(defvar bin-path (second ext:*args*))
(defvar asm-lines
  (with-open-file (in asm-path)
    (loop for line = (read-line in nil :eof)
       until (eq line :eof) collect line)))

(defun gdb-disassemble (function)
  (shell "gdb --batch --eval-command=\"disassemble ~s\" ~s 2>/dev/null"
         function bin-path))

(defun addrs (function)
  "Return the numerical addresses of the lines (in order) of FUNCTION."
  (remove nil
    (mapcar
     (lambda (line)
       (register-groups-bind
           (addr offset) ("[\\s]*0x([\\S]+)[\\s]*<([\\S]+)>:.*" line)
         (parse-integer addr :radix 16)))
     (gdb-disassemble function))))

(defun function-lines (&aux function)
  "Return the line numbers of the lines (in order) of FUNCTION."
  (loop for line in asm-lines as counter from 0
     do (setf function (register-groups-bind
                        (line-function) ("^([^\\.][\\S]+):" line)
                        line-function))
     if function
     collect function
     else collect counter))

(defun lines (flines)
  (cdr
   (mapcar (lambda (lines)
             (remove-if (lambda (n) (scan "^[\\s]*\\." (nth n asm-lines)))
                        lines))
           (split-sequence-if #'stringp flines))))

(defun mapping ()
  (let ((flines (function-lines)))
    (mapcar (lambda (addrs lines)
              (mapcar #'cons addrs lines))
            (mapcar #'addrs (remove-if-not #'stringp flines))
            (lines flines))))

(loop for (addr . line) in (sort (apply #'append (mapping)) #'< :key #'car)
   do (format t "~&~x ~d" addr line))
