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
(defclass asm (software-exe)
  ((addr-map :initarg :addr-map :accessor raw-addr-map :initform nil)))

(defvar *test-script*  nil "Script capable of running tests.")
(defvar *pos-test-num* nil "Number of positive tests")
(defvar *neg-test-num* nil "Number of negative tests")

(defun asm-from-file (path &aux genome)
  (let ((new (make-instance 'asm)))
    (with-open-file (in path)
      (loop for line = (read-line in nil)
         while line do (push `((:line . ,line)) genome))
      (setf (genome new) (reverse (coerce genome 'vector)))
      new)))

(defun asm-to-file (software path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (dotimes (n (length (genome software)))
      (format out "~a~%" (cdr (assoc :line (aref (genome software) n)))))))

(defun link (asm exe)
  (multiple-value-bind (output error-output exit)
      (shell "gcc -o ~a ~a" exe asm)
    (values output error-output exit)))

(defmethod exe ((asm asm) &optional place)
  (let ((exe (or place (temp-file-name)))
        (tmp (temp-file-name "s")))
    (asm-to-file asm tmp)
    (multiple-value-bind (output err-output exit) (link tmp exe)
      (declare (ignorable output err-output))
      (when (probe-file tmp) (delete-file tmp))
      (when (= exit 0) exe))))

(defmethod evaluate ((asm asm))
  (evaluate-with-script asm *test-script* *pos-test-num* *neg-test-num*))


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

(defmethod (setf addr-map) (new (asm asm))
  (setf (raw-addr-map asm) new))

(defmethod addr-map ((asm asm))
  (or (raw-addr-map asm)
      (setf (addr-map asm)
            (apply
             #'append
             (let ((flines (function-lines asm)))
               (mapcar (lambda (addrs lines)
                         (mapcar #'cons addrs lines))
                       (mapcar (lambda (func) (addrs asm func))
                               (remove-if-not #'stringp flines))
                       (cdr
                        (mapcar (lambda (lines)
                                  (remove-if (lambda (n)
                                               (scan "^[\\s]*\\."
                                                     (assoc :line
                                                            (aref n (genome asm)))))
                                             lines))
                                (split-sequence-if #'stringp flines)))))))))

(defmethod lines ((asm asm))
  (mapcar (lambda (line) (cdr (assoc :line line))) (genome asm)))


;;; incorporation of oprofile samples
(defun apply-path (asm key addresses &aux applied)
  "Apply a list of sampled ADDRESSES to the ASM's genome behind KEY.
If each element of ADDRESSES is a cons cell then assume the car is the
address and the cdr is the value."
  (let ((map (addr-map asm)))
    (loop for el in addresses as i from 0
       do
         (let* ((addr (if (consp el) (car el) el))
                (val (if (consp el) (cdr el) t))
                (loc (cdr (assoc addr map))))
           (when loc
             (push (cons key val) (nth loc (genome asm)))
             (push (list i key val) applied)))))
  (reverse applied))

(defun samples-from-oprofile-file (path)
  (with-open-file (in path)
    (remove nil
      (loop for line = (read-line in nil :eof)
         until (eq line :eof)
         collect
           (register-groups-bind (c a) ("^ *(\\d+).+: +([\\dabcdef]+):" line)
             (declare (string c) (string a))
             (cons (parse-integer a :radix 16) (parse-integer c)))))))
