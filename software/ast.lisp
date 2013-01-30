;;; ast.lisp --- ast software representation

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

;; TODO: get memoization working

;;; Code:
(in-package :software-evolution)


;;; ast software objects
(defclass ast (software)
  ((base    :initarg :base    :accessor base    :initform nil)
   (c-flags :initarg :c-flags :accessor c-flags :initform nil)))

(defmethod copy ((ast ast))
  (make-instance (type-of ast)
    :c-flags (copy-tree (c-flags ast))
    :base    (base ast)
    :fitness (fitness ast)
    :edits   (copy-tree (edits ast))))

(defmethod from-file ((ast ast) path)
  (setf (base ast) (file-to-string path))
  ast)

(defun ast-from-file (path &key c-flags)
  (assert (listp c-flags) (c-flags) "c-flags must be a list")
  (from-file (make-instance 'ast :c-flags c-flags) path))

(defun ast-to-file (software path &key if-exists)
  (string-to-file (genome software) path :if-exists if-exists))

(defun genome-helper (ast)
  (catch 'ast-mutate
    (let ((base (copy-seq (base ast))))
      (loop :for edit :in (reverse (copy-seq (edits ast))) :do
         (setf base (ast-mutate (make-instance (type-of ast) :base base) edit)))
      base)))

(defmethod genome ((ast ast)) (genome-helper ast))
(defmethod genome-string ((ast ast)) (genome ast))

;; TODO: memoize by base and edit op
(defgeneric ast-mutate (ast &optional op)
  (:documentation "Mutate AST with either clang-mutate or cil-mutate."))

(defun num-ids (ast)
  (catch 'ast-mutate (read-from-string (ast-mutate ast (list :ids)))))

(defmethod mutate ((ast ast))
  "Randomly mutate AST."
  (let ((num-ids (num-ids ast)))
    (unless (and num-ids (> num-ids 0))
      (error 'mutate :text "No valid IDs" :obj ast))
    (setf (fitness ast) nil)
    (flet ((place () (random num-ids)))
      (push (case (random-elt '(cut insert swap))
              (cut    `(:cut    ,(place)))
              (insert `(:insert ,(place) ,(place)))
              (swap   `(:swap   ,(place) ,(place))))
            (edits ast)))
    ast))

(defmethod patch-subset ((a ast) (b ast))
  "Return a new ast composed of subsets of the edits from A and B."
  (let ((new (copy a)))
    (flet ((some-of (edits)
             (remove-if (lambda (n) (declare (ignorable n)) (zerop (random 2)))
                        (butlast edits 1))))
      (setf (edits new)
            (append (some-of (edits a)) (some-of (edits b))
                    (last (edits a))))
      (setf (fitness new) nil)
      new)))

(defmethod crossover ((a ast) (b ast)) (patch-subset a b))
