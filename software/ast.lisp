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
(define-software ast (software)
  ((genome   :initarg :genome   :accessor genome   :initform nil
             :copier copy-seq)
   (flags    :initarg :flags    :accessor flags    :initform nil
             :copier copy-tree)
   (compiler :initarg :compiler :accessor compiler :initform "clang"
             :copier copy-seq)
   (ext      :initarg :ext      :accessor ext      :initform "c"
             :copier copy-tree)
   (raw-size :initarg :size     :accessor raw-size :initform nil
             :copier :none)))

(defmethod genome-string ((ast ast) &optional stream)
  (write-string (or (genome ast) "") stream))

(defmethod from-file ((ast ast) path)
  (setf (genome ast) (file-to-string path))
  (setf (ext ast)  (pathname-type (pathname path)))
  ast)

(defun ast-from-file (path &key flags)
  (assert (listp flags) (flags) "flags must be a list")
  (from-file (make-instance 'ast :flags flags) path))

(defun ast-to-file (software path &key if-exists)
  (string-to-file (genome software) path :if-exists if-exists))

(defmethod size ((ast ast))
  (or (raw-size ast)
      (setf (raw-size ast)
            (or (ignore-errors
                  (parse-number (apply-mutation ast (list :ids))))
                0))))

(defmethod mutate ((ast ast))
  (unless (> (size ast) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj ast)))
  (let ((op (case (random-elt '(cut insert swap))
              (cut    `(:cut    (:stmt1 . ,(pick-bad ast))))
              (insert `(:insert (:stmt1 . ,(pick-bad ast)) 
                                (:stmt2 . ,(pick-good ast))))
              (swap   `(:swap   (:stmt1 . ,(pick-bad ast)) 
                                (:stmt2 . ,(pick-good ast)))))))
    (apply-mutation ast op)
    (values ast op)))

(defmethod apply-mutation :around ((ast ast) mut)
  ;; Apply MUT to AST, and then update `SIZE' for AST.
  (if (member (car mut) '(:ids :list :json))
      (call-next-method)
      (let ((new-genome (call-next-method)))
        (when new-genome
          (setf (genome ast) new-genome)))))

(defmethod crossover ((a ast) (b ast))
  (let ((a-point (random-elt (line-breaks (genome a))))
        (b-point (random-elt (line-breaks (genome b))))
        (new (copy a)))
    (setf (genome new)
          (copy-seq (concatenate 'string
                      (subseq (genome a) 0 a-point)
                      (subseq (genome b) b-point))))
    (values new (list a-point b-point))))

(defmethod lines ((ast ast))
  (split-sequence #\Newline (genome ast)))

(defmethod line-breaks ((ast ast))
  (cons 0 (loop :for char :in (coerce (genome ast) 'list) :as index 
                :from 0
                :when (equal char #\Newline) :collect index)))

(defmethod (setf lines) (new (ast ast))
  (setf (genome ast) (format nil "~{~a~^~%~}" new)))
