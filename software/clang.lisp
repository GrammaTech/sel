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
(defclass clang (software)
  ((c-flags :initarg :c-flags :accessor c-flags :initform nil)))

(defmethod copy ((clang clang))
  (make-instance 'clang
    :c-flags (c-flags clang)
    :fitness (fitness clang)
    :edits   (edits clang)))

(defun clang-from-file (path &key c-flags)
  (assert (listp c-flags) (c-flags) "c-flags must be a list")
  (let ((new (make-instance 'clang :c-flags c-flags))
        (orig (file-to-string path)))
    (setf (edits new) (list orig))
    new))

(defun asm-to-file (software path)
  (string-to-file (genome software) path :if-exists :supersede))

(def-memoized-function genome-helper (edits c-flags)
  (if (> (length edits) 1)
      (clang-mutate (genome-helper (cdr edits) c-flags) (car edits)
                    :c-flags c-flags)
      (car edits)))

(defmethod genome ((clang clang))
  (genome-helper (edits clang) (c-flags clang)))

(defun clang-mutate (genome op &key c-flags)
  "Returns the results of mutating GENOME with OP.
Mutations are performed using the clang-mutate executable."
  (flet ((stmt (num arg) (format nil "-stmt~d=~d" num arg)))
    (let ((src (temp-file-name "c")))
      (string-to-file genome src)
      (multiple-value-bind (output err-output exit)
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
                               (:default (list (format nil "-~a" (car op)))))
                             `(,src "--" ,@c-flags "|tail -n +3"))
                            " "))
        (declare (ignorable err-output))
        (values output exit)))))
(memoize-function 'clang-mutate)

(defun num-ids (clang) ;; TODO: memoize
  (parse-integer (clang-mutate (genome clang) (list :ids)
                               :c-flags (c-flags clang))))

(defmethod mutate ((clang clang))
  "Randomly mutate VARIANT with chance MUT-P."
  (let ((num-ids (num-ids clang)))
    (assert (and num-ids (> num-ids 0)) (num-ids) "No valid IDs")
    (setf (fitness clang) nil)
    (flet ((place () (random num-ids)))
      (push (case (random-elt '(cut insert swap))
              (cut    `(:cut    ,(place)))
              (insert `(:insert ,(place) ,(place)))
              (swap   `(:swap   ,(place) ,(place))))
            (edits clang)))
    clang))

(defmethod patch-subset ((a clang) (b clang))
  "Return a new clang composed of subsets of the edits from A and B."
  (let ((new (copy a)))
    (flet ((some-of (edits)
             (remove-if (lambda (_) (declare (ignorable _)) (zerop (random 2)))
                        (butlast edits 1))))
      (setf (edits new)
            (append (some-of (edits a)) (some-of (edits b))
                    (last (edits a))))
      (setf (fitness new) nil)
      new)))

(defmethod crossover ((a clang) (b clang)) (patch-subset a b))

(defmethod phenome ((clang clang))
  (let ((src (temp-file-name "c"))
        (bin (temp-file-name)))
    (string-to-file (genome clang) src)
    (multiple-value-bind (output err-output exit)
        (shell "clang ~a -o ~a ~a"
               src bin (mapconcat #'identity (c-flags clang) " "))
      (declare (ignorable output err-output))
      (values (if (zerop exit) bin src) exit))))
