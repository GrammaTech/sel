;;; ast.lisp --- clang software representation

;; Copyright (C) 2012 Eric Schulte

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

;;; Code:
(in-package :software-evolution)

(defclass clang (ast)
  ((compiler  :initarg :compiler :accessor compiler :initform "clang")
   (ext       :initarg :ext      :accessor ext      :initform "c")))

(defmethod copy ((clang clang)
                 &key
                   (edits (copy-tree (edits clang)))
                   (fitness (fitness clang)))
  (make-instance (type-of clang)
    :c-flags  (copy-tree (c-flags clang))
    :base     (base clang)
    :edits    edits
    :fitness  fitness
    :compiler (compiler clang)
    :ext      (ext clang)))

(defmethod from-file ((clang clang) path)
  (setf (base clang) (file-to-string path))
  (setf (ext clang)  (pathname-type (pathname path)))
  clang)

(defmethod ast-mutate ((clang clang) &optional op)
  (flet ((stmt (num arg) (format nil "-stmt~d=~d" num arg)))
    (if op
        (with-temp-file-of (src (ext clang)) (genome clang)
          (multiple-value-bind (stdout stderr exit)
              (shell "clang-mutate ~a"
                     (mapconcat #'identity
                                (append
                                 (case (car op)
                                   (:ids     (list "-ids"))
                                   (:cut     (list "-delete"
                                                   (stmt 1 (second op))))
                                   (:insert  (list "-insert"
                                                   (stmt 1 (second op))
                                                   (stmt 2 (third op))))
                                   (:swap    (list "-swap"
                                                   (stmt 1 (second op))
                                                   (stmt 2 (third op))))
                                   (t (list (string-downcase
                                             (format nil "-~a" (car op))))))
                                 `(,src "--" ,@(c-flags clang) "|tail -n +3"))
                                " "))
            (unless (zerop exit) (throw 'ast-mutate nil))
            stdout))
        (values (genome clang) 0))))

(defmethod phenome ((clang clang) &key bin)
  (with-temp-file-of (src (ext clang)) (genome clang)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell "~a ~a -o ~a ~a"
                 (compiler clang)
                 src bin (mapconcat #'identity (c-flags clang) " "))
        (declare (ignorable stdout stderr))
        (values (if (zerop exit) bin stderr) exit)))))
