;;; asm.lisp --- software representation of Assembly files

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


;;; the class of assembly software objects
(defclass asm (software)
  ((addr-map :initarg :addr-map :accessor addr-map :initform nil)
   (genome   :initarg :genome   :accessor genome   :initform nil)))

(defvar asm-linker "gcc")

(defmethod from-file ((asm asm) path)
  (with-open-file (in path)
    (setf (genome asm)
          (coerce (loop :for line = (read-line in nil)
                     :while line :collect (list (cons :line line))) 'vector))
    asm))

(defun asm-from-file (path)
  (from-file (make-instance 'asm) path))

(defun genome-string (asm)
  (mapconcat (comp (curry #'format nil "~a~%") (curry #'aget :line))
             (coerce (genome asm) 'list) ""))

(defmethod phenome ((asm asm) &key bin)
  (with-temp-file-of (src "s") (genome-string asm)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell "~a -o ~a ~a" asm-linker bin src)
        (declare (ignorable stdout ))
        (values (if (zerop exit) bin stderr) exit)))))


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
                            (coerce (genome asm) 'list)) as counter from 0
     do (setq function (register-groups-bind
                           (line-function) ("^([^\\.][\\S]+):" line)
                         line-function))
     if function
     collect function
     else collect counter))

(defmethod calculate-addr-map ((asm asm))
  (apply
   #'append
   (let ((flines (function-lines asm)))
     (mapcar (lambda (addrs lines)
               (mapcar #'cons addrs lines))
             (mapcar (lambda (func) (addrs asm func))
                     (remove-if-not #'stringp flines))
             (cdr
              (mapcar (lambda (lines)
                        (remove-if
                         (lambda (n)
                           (scan "^[\\s]*\\."
                                 (cdr (assoc :line
                                             (aref (genome asm) n)))))
                         lines))
                      (split-sequence-if #'stringp flines)))))))


;;; incorporation of oprofile samples
(defun apply-path (asm key addresses &aux applied)
  "Apply a list of sampled ADDRESSES to the ASM's genome behind KEY.
If each element of ADDRESSES is a cons cell then assume the car is the
address and the cdr is the value."
  (let ((map (addr-map asm)))
    (loop :for el :in addresses :as i :from 0 :do
       (let* ((addr (if (consp el) (car el) el))
              (val (if (consp el) (cdr el) t))
              (loc (cdr (assoc addr map))))
         (when loc
           (push (cons key val) (aref (genome asm) loc))
           (push (list i key val) applied)))))
  (reverse applied))

(defun samples-from-oprofile-file (path)
  (with-open-file (in path)
    (remove nil
      (loop :for line = (read-line in nil :eof)
         :until (eq line :eof)
         :collect
         (register-groups-bind (c a) ("^ *(\\d+).+: +([\\dabcdef]+):" line)
           (declare (string c) (string a))
           (cons (parse-integer a :radix 16) (parse-integer c)))))))

(defun samples-from-tracer-file (path &aux samples)
  (with-open-file (in path)
    (loop :for line = (read-line in nil :eof)
       :until (eq line :eof)
       :do
       (let ((addr (parse-integer line)))
         (if (assoc addr samples)
             (setf (cdr (assoc addr samples))
                   (1+ (cdr (assoc addr samples))))
             (setf samples (cons (cons addr 0) samples)))))
    samples))
