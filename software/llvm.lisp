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

;; This software object uses the llvm-mutate [1] pass run by the llvm
;; opt tool [2] to manipulate the in-memory LLVM representation (see
;; [3] for more information).

;; [1] https://github.com/eschulte/llvm-mutate
;; [2] http://llvm.org/releases/3.2/docs/CommandGuide/opt.html
;; [3] http://llvm.org/docs/LangRef.html#introduction

;;; Code:
(in-package :software-evolution)


;;; llvm software objects
(defclass llvm (ast)
  ((ext      :initarg :ext      :accessor ext      :initform "lb")
   (compiler :initarg :compiler :accessor compiler :initform "llc")
   (linker   :initarg :linker   :accessor linker   :initform "gcc")))

(defmethod from-file ((llvm llvm) path)
  (setf (genome llvm) (file-to-bytes path))
  (setf (ext llvm)  (pathname-type (pathname path)))
  llvm)

(defmethod ast-mutate ((llvm llvm) &optional op)
  (flet ((stmt (num arg) (format nil "-stmt~d=~d" num arg)))
    (if op
        (with-temp-file (output)
          (with-temp-file-of (src (ext llvm)) (genome llvm)
            (multiple-value-bind (stdout stderr exit)
                (shell "opt ~a "
                       (mapconcat #'identity
                                  (append
                                   (case (car op)
                                     (:ids     (list "-count"))
                                     (:cut     (list "-cut"
                                                     (stmt 1 (second op))))
                                     (:insert  (list "-insert"
                                                     (stmt 1 (second op))
                                                     (stmt 2 (third op))))
                                     (:swap    (list "-swap"
                                                     (stmt 1 (second op))
                                                     (stmt 2 (third op)))))
                                   `(,src "-o" ,output))
                                  " "))
              (declare (ignorable stderr))
              (unless (zerop exit) (throw 'ast-mutate nil))
              (if (equal :ids (car op))
                  stdout
                  (file-to-bytes output)))))
        (values (genome llvm) 0))))

(defmethod phenome ((llvm llvm) &key bin)
  (with-temp-file-of (src (ext llvm)) (genome llvm)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell "cat ~a|~a|~a -x assembler - -o ~a"
                 src (compiler llvm) (linker llvm) bin)
        (declare (ignorable stdout stderr))
        (values (if (zerop exit) bin stderr) exit)))))
