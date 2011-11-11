;;; soft-asm.lisp --- software representation of Assembly files

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

(defclass soft-asm (soft)
  ((addr-map :initarg :addr-map :accessor raw-addr-map :initform nil)))

(defmethod from ((soft soft-asm) (in stream) &aux genome)
  (loop for line = (read-line in nil)
     while line do (push `((:line . ,line)) genome))
  (setf (genome soft) (reverse genome))
  soft)

(defmethod to ((soft soft-asm) (to stream))
  (dolist (line (genome soft))
    (format to "~a~%" (cdr (assoc :line line)))))

(defun asm-from-file (path)
  (let ((new (make-instance 'soft-asm)))
    (with-open-file (in path) (from new in))))

(defun asm-to-file (soft path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (to soft out)))

(defun link (asm exe)
  (multiple-value-bind (output error-output exit)
      (shell "gcc -o ~a ~a" exe asm)
    (values output error-output exit)))

(defmethod exe ((asm soft-asm) &optional place)
  (let ((exe (or place (temp-file-name)))
        (tmp (temp-file-name "s")))
    (asm-to-file asm tmp)
    (multiple-value-bind (output err-output exit) (link tmp exe)
      (declare (ignorable output err-output))
      (unless *keep-source* (when (probe-file tmp) (delete-file tmp)))
      (unless (= exit 0) (error "ASM compilation failed (~a->~a)" tmp exe)))
    exe))

(defmethod lines ((asm soft-asm))
  (mapcar (lambda (line) (cdr (assoc :line line))) (genome asm)))


;;; memory mapping, address -> LOC
(defun gdb-disassemble (asm function)
  (shell "gdb --batch --eval-command=\"disassemble ~s\" ~s 2>/dev/null"
         function (exe asm)))

(defun addrs (asm function)
  "Return the numerical addresses of the lines (in order) of FUNCTION."
  (remove nil
    (mapcar
     (lambda (line)
       (register-groups-bind  (addr offset)
           ("[\\s]*0x([\\S]+)[\\s]*<([\\S]+)>:.*" line)
         (declare (ignorable offset) (string addr))
         (parse-integer addr :radix 16)))
     (split-sequence #\Newline (gdb-disassemble asm function)))))

(defun function-lines (asm &aux function)
  "Return the line numbers of the lines (in order) of FUNCTION."
  (loop for line in (mapcar (lambda (line) (cdr (assoc :line line)))
                            (genome asm)) as counter from 0
     do (setq function (register-groups-bind
                           (line-function) ("^([^\\.][\\S]+):" line)
                         line-function))
     if function
     collect function
     else collect counter))

(defmethod (setf addr-map) (new (asm soft-asm))
  (setf (raw-addr-map asm) new))

(defmethod addr-map ((asm soft-asm))
  (or (raw-addr-map asm)
      (setf (addr-map asm)
            (let ((flines (function-lines asm)))
              (mapcar (lambda (addrs lines)
                        (mapcar #'cons addrs lines))
                      (mapcar (lambda (func) (addrs asm func))
                              (remove-if-not #'stringp flines))
                      (cdr
                       (mapcar (lambda (lines)
                                 (remove-if (lambda (n)
                                              (scan "^[\\s]*\\."
                                                    (nth n (lines asm))))
                                            lines))
                               (split-sequence-if #'stringp flines))))))))
