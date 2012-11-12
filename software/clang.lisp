;;; clang.lisp --- clang software representation

;; Copyright (C) 2012  Eric Schulte

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


;;; clang software objects
(defclass clang (software-exe)
  ((c-flags :initarg :c-flags :accessor c-flags :initform nil)))

(defun clang-from-file (path)
  (let ((new (make-instance 'clang))
        (orig (file-to-string path)))
    (setf (edits new) (list (reverse orig)))
    new))

(defun asm-to-file (software path)
  (string-to-file (genome software) path :if-exists :supersede))

(defun clang-mutate (clang &rest ops)
  (flet ((stmt (num arg) (format nil "-stmt~d=~d" num arg)))
    (let ((src (temp-file-name "c")))
      (string-to-file (genome clang) src)
      (multiple-value-bind (output err-output exit)
          (shell "clang-mutate"
                 (append
                  (case (car ops)
                    (:ids     (list "-ids"))
                    (:cut     (list "-delete"
                                    (stmt 1 (second ops))))
                    (:insert  (list "-insert"
                                    (stmt 1 (second ops))
                                    (stmt 2 (third ops))))
                    (:swap    (list "-swap"
                                    (stmt 1 (second ops))
                                    (stmt 2 (third ops))))
                    (:default (list (format nil "-~a" (car ops)))))
                  `(,src "--" ,@(c-flags clang) "|tail -n +3")))
        (declare (ignorable err-output))
        (when (zerop exit) output)))))

(defun num-ids (clang) ;; TODO: memoize
  (parse-integer (clang-mutate clang :ids)))

(defmethod mutate ((clang clang))
  "Randomly mutate VARIANT with chance MUT-P."
  (let ((num-ids (num-ids clang)))
    (assert (and num-ids (> num-ids 0)) (num-ids) "No valid IDs")
    (flet ((place () (random num-ids)))
      (push (case (random-elt '(cut insert swap))
              (cut    `(cut    ,(place)))
              (insert `(insert ,(place) ,(place)))
              (swap   `(swap   ,(place) ,(place))))
            (edits clang)))))

(defmethod patch-subset ((a clang) (b clang))
  "Return a new clang composed of subsets of the edits from A and B."
  (flet ((some-of (edits)
           (remove-if (lambda (_) (declare (ignorable _)) (zerop (random 2)))
                      (butlast edits 1))))
    (setf (edits a)
          (append (some-of (edits a)) (some-of (edits b))
                  (last (edits a))))))

(defmethod crossover ((a clang) (b clang)) (patch-subset a b))

(defmethod phenome ((clang clang))
  (let ((src (temp-file-name "c"))
        (bin (temp-file-name)))
    (string-to-file (genome clang) src)
    (multiple-value-bind (output err-output exit)
        (shell "clang" src "-o" bin (c-flags clang))
      (declare (ignorable output err-output))
      (when (zerop exit) bin))))
