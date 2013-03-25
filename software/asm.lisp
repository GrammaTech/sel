;;; asm.lisp --- software representation of Assembly files

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
(defclass asm (software)
  ((addr-map :initarg :addr-map :accessor addr-map :initform nil)
   (genome   :initarg :genome   :accessor genome   :initform nil)
   (linker   :initarg :linker   :accessor linker   :initform nil)
   (flags    :initarg :flags    :accessor flags    :initform nil)))

(defvar asm-linker "gcc")

(defmethod copy ((asm asm)
                 &key (edits (copy-tree (edits asm))) (fitness (fitness asm)))
  (make-instance (type-of asm)
    :edits edits
    :fitness fitness
    :addr-map (copy-tree (addr-map asm))
    :genome (copy-tree (genome asm))
    :linker (linker asm)
    :flags (flags asm)))

(defmethod from-file ((asm asm) path)
  (with-open-file (in path)
    (setf (genome asm)
          (loop :for line = (read-line in nil)
             :while line :collect (list (cons :line line))))
    asm))

(defun lines (asm)
  (mapcar (curry #'aget :line) (genome asm)))

(defmethod genome-string (asm)
  (mapconcat (curry #'format nil "~a~%") (lines asm) ""))

(defmethod to-file ((asm asm) file)
  (string-to-file (genome-string asm) file))

(defmethod phenome ((asm asm) &key bin)
  (with-temp-file-of (src "s") (genome-string asm)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell "~a ~a -o ~a ~a"
                 (or (linker asm) asm-linker)
                 (mapconcat #'identity (flags asm) " ")
                 bin src)
        (declare (ignorable stdout ))
        (values (if (zerop exit) bin stderr) exit)))))

(defmethod size ((asm asm))
  "Return the length of the genome of ASM."
  (length (genome asm)))

(defmethod mutate ((asm asm))
  "Randomly mutate ASM."
  (unless (> (size asm) 0) (error 'mutate :text "No valid IDs" :obj asm))
  (setf (fitness asm) nil)
  (flet ((place () (random (size asm))))
    (let ((mut (case (random-elt '(cut insert swap))
                 (cut    `(:cut    ,(place)))
                 (insert `(:insert ,(place) ,(place)))
                 (swap   `(:swap   ,(place) ,(place))))))
      (push mut (edits asm))
      (apply-mutation asm mut)))
  asm)

(defmethod apply-mutation ((asm asm) mutation)
  (let ((op (first mutation))
        (s1 (second mutation))
        (s2 (third mutation)))
    (with-slots (genome) asm
      (setf genome (case op
                     (:cut (append (subseq genome 0 s1)
                                   (subseq genome (1+ s1))))
                     (:insert (append (subseq genome 0 s1)
                                      (list (nth s2 genome))
                                      (subseq genome s1)))
                     (:swap (let ((tmp (nth s1 genome)))
                              (setf (nth s1 genome) (nth s2 genome))
                              (setf (nth s2 genome) tmp))
                            genome))))))

(defmethod crossover ((a asm) (b asm))
  "Two point crossover."
  (let* ((range (min (size a) (size b)))
         (points (sort (loop :for i :below 2 :collect (random range)) #'<))
         (new (copy a)))
    (setf (genome new)
          (copy-tree (append (subseq (genome a) 0 (first points))
                             (subseq (genome a) (first points) (second points))
                             (subseq (genome b) (second points)))))
    new))


;;; memory mapping, address -> LOC
(defun gdb-disassemble (phenome function)
  "Return the raw gdb disassembled code of FUNCTION in PHENOME."
  (shell "gdb --batch --eval-command=\"disassemble ~s\" ~s 2>/dev/null"
         function phenome))

(defun addrs (phenome function)
  "Return the numerical addresses of the lines (in order) of FUNCTION."
  (remove nil
    (mapcar
     (lambda (line)
       (declare (string line))
       (register-groups-bind (addr offset)
           ("[\\s]*0x([\\S]+)[\\s]*<([\\S]+)>:.*" line)
         (declare (ignorable offset) (string addr))
         (parse-integer addr :radix 16)))
     (split-sequence #\Newline (gdb-disassemble phenome function)))))

(defun function-lines (asm)
  "Return the line numbers of the lines (in order) of FUNCTION."
  (loop :for line :in (lines asm) :as counter :from 0
     :for function = (register-groups-bind
                         (line-function) ("^([^\\.][\\S]+):" line)
                       line-function)
     :collect (or function counter)))

(defun calculate-addr-map (asm)
  (let ((flines (function-lines asm))
        (phenome (phenome asm))
        (genome (coerce (genome asm) 'vector))
        (map (make-hash-table)))
    (loop
       :for addrs :in (mapcar (lambda (func) (addrs phenome func))
                              (remove-if-not #'stringp flines))
       :for lines :in (cdr (mapcar
                            {remove-if
                             [{scan "^[\\s]*\\."} {aget :line} {aref genome}]}
                             (split-sequence-if #'stringp flines)))
       :do (mapc (lambda (addr line) (setf (gethash addr map) line))
                 addrs lines))
    map))


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
