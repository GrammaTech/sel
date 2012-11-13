;;; cil.lisp --- cil software representation

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


;;; cil software objects
(defclass cil (software)
  ((base :initarg :base :accessor base :initform nil)))

(defmethod copy ((cil cil))
  (make-instance 'cil
    :base    (base cil)
    :fitness (fitness cil)
    :edits   (copy-tree (edits cil))))

(defun cil-from-file (path)
  (make-instance 'cil :base (file-to-string path)))

(defun cil-to-file (software path &key if-exists)
  (string-to-file (genome software) path :if-exists if-exists))

(defun genome-helper (edits base)
  (if edits
      (cil-mutate (genome-helper (cdr edits) base) (car edits))
      (cil-mutate base)))
;; (memoize-function 'genome-helper :key #'identity)
;; (unmemoize-function 'genome-helper)

(defmethod genome ((cil cil))
  (genome-helper (edits cil) (base cil)))

(defun cil-mutate (genome &optional op)
  "Returns the results of mutating GENOME with OP.
Mutations are performed using the cil-mutate executable."
  (flet ((stmt (num arg) (format nil "-stmt~d ~d" num arg)))
    (with-temp-file-of (src "c") genome
      (multiple-value-bind (output err-output exit)
          (shell "cil-mutate ~a"
                 (mapconcat #'identity
                            (append
                             (if op
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
                                 nil)
                             `(,src))
                            " "))
        (unless (zerop exit) (throw 'cil-mutate err-output))
        output))))
;; (memoize-function 'cil-mutate :key #'identity)
;; (unmemoize-function 'cil-mutate)

(defun num-ids (cil)
  (handler-case
      (read-from-string
       (cil-mutate (genome cil) (list :ids)))
    (cil-mutate (err) (declare (ignorable err)) (format t "caught") 0)))

(defmethod mutate ((cil cil))
  "Randomly mutate VARIANT with chance MUT-P."
  (let ((num-ids (num-ids cil)))
    (unless (and num-ids (> num-ids 0)) (error 'mutate "No valid IDs"))
    (setf (fitness cil) nil)
    (flet ((place () (random num-ids)))
      (push (case (random-elt '(cut insert swap))
              (cut    `(:cut    ,(place)))
              (insert `(:insert ,(place) ,(place)))
              (swap   `(:swap   ,(place) ,(place))))
            (edits cil)))
    cil))

(defmethod patch-subset ((a cil) (b cil))
  "Return a new cil composed of subsets of the edits from A and B."
  (let ((new (copy a)))
    (flet ((some-of (edits)
             (remove-if (lambda (_) (declare (ignorable _)) (zerop (random 2)))
                        (butlast edits 1))))
      (setf (edits new)
            (append (some-of (edits a)) (some-of (edits b))
                    (last (edits a))))
      (setf (fitness new) nil)
      new)))

(defmethod crossover ((a cil) (b cil)) (patch-subset a b))

(defmethod phenome ((cil cil) &key bin)
  (with-temp-file-of (src "c") (genome cil)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (output err-output exit)
          (shell "gcc ~a -o ~a" src bin)
        (declare (ignorable output err-output))
        (values (if (zerop exit) bin src) exit)))))
