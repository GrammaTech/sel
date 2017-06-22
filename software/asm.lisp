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
(enable-curry-compose-reader-macros :include-utf8)


;;; asm software objects
(define-software asm (simple)
  ((addr-map :initarg :addr-map :accessor addr-map :initform nil
             :copier copy-tree)
   (linker   :initarg :linker   :accessor linker :initform nil)
   (flags    :initarg :flags    :accessor flags :initform nil)))

(defvar *asm-linker* "gcc")

(defvar *asm-new-mutation-types* '(asm-replace-operand)
  "New mutations specific to asm software objects.")

(defvar *asm-mutation-types*
  (let ((orig (un-cumulative-distribution *simple-mutation-types*)))
    (cumulative-distribution
     (normalize-probabilities
      (append orig
              (mapcar {cons _ (/ (length *simple-mutation-types*))}
                      *asm-new-mutation-types*)))))
  "Add asm mutations with probability 1/N, where N is the number of possible
mutations (simple plus asm mutations).")

(defmethod pick-mutation-type ((asm asm))
  (random-pick *asm-mutation-types*))

(defmethod phenome ((asm asm) &key (bin (temp-file-name)))
  #-ccl (declare (values string fixnum string string string))
  (defmethod phenome)
  (with-temp-file-of (src "s") (genome-string asm)
    (multiple-value-bind (stdout stderr errno)
        (shell "~a -o ~a ~a ~{~a~^ ~}"
               (or (linker asm) *asm-linker*) bin src (flags asm))
      (declare (ignorable stdout ))
      (values bin errno stderr stdout src))))


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

(define-mutation asm-replace-operand (simple-mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "Select two instructions, and replace an operand in the first
with an operand in the second."))


(defun asm-split-instruction (instruction)
  "Split INSTRUCTION string on white space or commas. Return a list of strings."
  (->> instruction
       (string-trim '(#\Space #\Tab #\Newline))
       (split "(\\s*,\\s*)|(\\s+)")))

(defun asm-nth-instruction (asm n)
  "Extract the string portion of the Nth instruction from the genome of ASM."
  (assoc-value (elt (genome asm) n) :code))

(defmethod apply-mutation ((asm asm) (mutation asm-replace-operand))
  (let ((bad-good (targets mutation)))
    (assert (listp bad-good) (mutation)
            "Requires mutations targets to be a list of two elements.")
    ;; NOTE: assumes instructions start with :code symbol
    (let* ((genome (genome asm))
           (bad (first bad-good))
           (bad-instr-string (asm-nth-instruction asm bad))
           (bad-operands (cdr (asm-split-instruction bad-instr-string)))
           (good-operands (->> (asm-nth-instruction asm (second bad-good))
                               (asm-split-instruction)
                               (cdr))))
      (when (or (null bad-operands) (null good-operands))
        (error (make-condition 'no-mutation-targets
                               :text "No operands in instruction(s)"
                               :obj asm)))
      (let ((bad-operand (random-elt bad-operands))
            (good-operand (random-elt good-operands)))
        (setf (genome asm)
              (concatenate (class-of genome)
                           (subseq genome 0 bad)
                           (list (acons :code
                                        (regex-replace-all
                                         (quote-meta-chars bad-operand)
                                         bad-instr-string
                                         good-operand)
                                        nil))
                           (subseq genome (1+ bad))))
        asm))))
