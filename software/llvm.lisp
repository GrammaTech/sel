;;; llvm.lisp --- llvm software representation

;; Copyright (C) 2013 Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This software object uses the llvm-mutate [1] compiler pass run by
;; llvm opt [2] to manipulate LLVM intermediate representation (IR)
;; code (see [3] for more information).

;; [1] https://github.com/eschulte/llvm-mutate
;; [2] http://llvm.org/releases/3.2/docs/CommandGuide/opt.html
;; [3] http://llvm.org/docs/LangRef.html#introduction

;;; Code:
(in-package :software-evolution)


;;; llvm software objects
(defclass llvm (ast)
  ((ext      :initarg :ext      :accessor ext      :initform "ll")
   (compiler :initarg :compiler :accessor compiler :initform "llc")
   (linker   :initarg :linker   :accessor linker   :initform "gcc")))

(defmethod from-file ((llvm llvm) path)
  (setf (genome llvm) (file-to-string path))
  (setf (ext llvm)  (pathname-type (pathname path)))
  llvm)

(defmethod mutate ((llvm llvm))
  (unless (> (num-ids llvm) 0)
    (error 'mutate :text "No valid IDs" :obj llvm))
  (setf (fitness llvm) nil)
  (let ((mut (case (random-elt '(cut replace insert swap))
               (cut     `(:cut     ,(pick-bad llvm)))
               (replace `(:replace ,(pick-bad llvm) ,(pick-good llvm)))
               (insert  `(:insert  ,(pick-bad llvm) ,(pick-good llvm)))
               (swap    `(:swap    ,(pick-bad llvm) ,(pick-good llvm))))))
    (push mut (edits llvm))
    (apply-mutation llvm mut))
  llvm)

(defmethod apply-mutation ((llvm llvm) op)
  (with-temp-file-of (src (ext llvm)) (genome llvm)
    (multiple-value-bind (stdout stderr exit)
        (shell "cat ~a|llvm-mutate --~a ~a"
               src
               (string-downcase (symbol-name (car op)))
               (mapconcat [{format nil "~a"} #'1+] (cdr op) ","))
      (unless (zerop exit)
        (error 'mutate :text "llvm-mutate" :obj llvm))
      (if (equal (car op) :ids) stderr stdout))))

(defmethod phenome ((llvm llvm) &key bin)
  (with-temp-file-of (src (ext llvm)) (genome llvm)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell "cat ~a|~a|~a -x assembler - -o ~a"
                 src (compiler llvm) (linker llvm) bin)
        (declare (ignorable stdout stderr))
        (values (if (zerop exit) bin stderr) exit)))))
