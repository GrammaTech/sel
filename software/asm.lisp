;;; asm.lisp --- software representation of Assembly code

;; Copyright (C) 2011-2013  Eric Schulte

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


;;; asm software objects
(define-software asm (simple)
  ((addr-map :initarg :addr-map :accessor addr-map :initform nil
             :copier copy-tree)
   (linker   :initarg :linker   :accessor linker :initform nil)
   (flags    :initarg :flags    :accessor flags :initform nil)))

(defvar *asm-linker* "gcc")

(defmethod phenome ((asm asm) &key bin)
  (declare (values string fixnum string string string))
  (with-temp-file-of (src "s") (genome-string asm)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr errno)
          (shell "~a -o ~a ~a ~{~a~^ ~}"
                 (or (linker asm) *asm-linker*) bin src (flags asm))
        (declare (ignorable stdout ))
        (values bin errno stderr stdout src)))))


;;; incorporation of oprofile samples
(defmethod apply-path ((asm asm) key addresses &aux applied)
  "Apply a list of sampled ADDRESSES to the ASM's genome behind KEY.
If each element of ADDRESSES is a cons cell then assume the car is the
address and the cdr is the value."
  (let ((map (addr-map asm)))
    (loop :for el :in addresses :as i :from 0 :do
       (let* ((addr (if (consp el) (car el) el))
              (val (if (consp el) (cdr el) t))
              (loc (gethash addr map)))
         (when loc
           (push (cons key val) (aref (genome asm) loc))
           (push (list i key val) applied)))))
  (reverse applied))
