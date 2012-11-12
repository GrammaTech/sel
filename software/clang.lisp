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

;; TODO: clean up temporary files

;;; Code:
(in-package :software-evolution)


;;; clang software objects
(defclass clang (software)
  ((base    :initarg :base    :accessor base    :initform nil)
   (c-flags :initarg :c-flags :accessor c-flags :initform nil)))

(defmethod copy ((clang clang))
  (make-instance 'clang
    :c-flags (c-flags clang)
    :base    (base clang)
    :fitness (fitness clang)
    :edits   (edits clang)))

(defun clang-from-file (path &key c-flags)
  (assert (listp c-flags) (c-flags) "c-flags must be a list")
  (make-instance 'clang
    :base    (file-to-string path)
    :c-flags c-flags))

(defun asm-to-file (software path &key if-exists)
  (string-to-file (genome software) path :if-exists if-exists))

(defun genome-helper (edits base c-flags)
  (if edits
      (clang-mutate (genome-helper (cdr edits) base c-flags) (car edits)
                    :c-flags c-flags)
      base))
(memoize-function 'genome-helper :key #'identity)
;; (unmemoize-function 'genome-helper)

(defmethod genome ((clang clang))
  (genome-helper (edits clang) (base clang) (c-flags clang)))

(defun clang-mutate (genome op &key c-flags)
  "Returns the results of mutating GENOME with OP.
Mutations are performed using the clang-mutate executable."
  (flet ((stmt (num arg) (format nil "-stmt~d=~d" num arg)))
    (with-temp-file-of (src "c") genome
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
                               (t (list (string-downcase
                                         (format nil "-~a" (car op))))))
                             `(,src "--" ,@c-flags "|tail -n +3"))
                            " "))
        (unless (zerop exit) (throw 'clang-mutate err-output))
        output))))
(memoize-function 'clang-mutate :key #'identity)
;; (unmemoize-function 'clang-mutate)

(defun num-ids (clang)
  (handler-case
      (read-from-string
       (clang-mutate (genome clang) (list :ids) :c-flags (c-flags clang)))
    (clang-mutate (err) (declare (ignorable err)) (format t "caught") 0)))

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

(defmethod phenome ((clang clang) &key bin)
  (with-temp-file-of (src "c") (genome clang)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (output err-output exit)
          (shell "clang ~a -o ~a ~a"
                 src bin (mapconcat #'identity (c-flags clang) " "))
        (declare (ignorable output err-output))
        (values (if (zerop exit) bin src) exit)))))
