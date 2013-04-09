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

;; There will likely ultimately be two different LLVM representations,
;; one which will perform single-line mutations on a serial vector
;; array of LLVM assembly instructions (similar to ASM), and another
;; which will use the llvm opt tool [1] to manipulate the in-memory
;; LLVM representation (see [2] for more information).

;; [1] http://llvm.org/releases/3.2/docs/CommandGuide/opt.html
;; [2] http://llvm.org/docs/LangRef.html#introduction

;;; Code:
(in-package :software-evolution)


;;; asm software objects
(defclass llvm (simple)
  ((compiler :initarg :compiler :accessor compiler :initform "llc")
   (linker   :initarg :linker   :accessor linker   :initform "gcc")))

(defmethod copy ((llvm llvm)
                 &key (edits (copy-tree (edits llvm))) (fitness (fitness llvm)))
  (make-instance (type-of llvm) :edits edits :fitness fitness
                 :genome (copy-tree (genome llvm))
                 :compiler (compiler llvm)
                 :linker (linker llvm)))

(defmethod phenome ((llvm llvm) &key bin)
  (with-temp-file-of (src "ll") (genome-string llvm)
    (let ((bin (or bin (temp-file-name))))
      ;; echo 'main(){puts("hello");}'|clang -x c - -S -emit-llvm -o hello.ll
      ;; cat hello.ll|llc|gcc -x assembler - -o hello
      (multiple-value-bind (stdout stderr exit)
          (shell "cat ~a|~a|~a -x assembler - -o ~a"
                 src (compiler llvm) (linker llvm) bin)
        (declare (ignorable stdout))
        (values (if (zerop exit) bin stderr) exit)))))
