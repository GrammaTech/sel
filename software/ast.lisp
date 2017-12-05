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
(in-package :software-evolution-library)
(enable-curry-compose-reader-macros :include-utf8)


(defvar *compilation-timeout* 600
  "Timeout compilation after this number of seconds has elapsed")

;;; ast software objects
(define-software ast (software)
  ((genome   :initarg :genome   :accessor genome   :initform ""
             :copier copy-seq)
   (flags    :initarg :flags    :accessor flags    :initform nil
             :copier copy-tree)
   (compiler :initarg :compiler :accessor compiler :initform "clang"
             :copier copy-seq)
   (ext      :initarg :ext      :accessor ext      :initform "c"
             :copier copy-tree)
   (raw-size :initarg :size     :accessor raw-size :initform nil
             :copier :none)))

(defmethod phenome ((obj ast) &key (bin (temp-file-name)))
  #-ccl (declare (values t fixnum string string string))
  (setf bin (ensure-path-is-string bin))
  (with-temp-file-of (src (ext obj)) (genome-string obj)
    (multiple-value-bind (stdout stderr errno)
      (handler-case
          (with-timeout (*compilation-timeout*)
            (shell "~a ~a -o ~a ~{~a~^ ~}" (compiler obj) src bin (flags obj)))
        (timeout (e)
          (declare (ignorable e))
          (values "" "" 124)))
      (values (when (zerop errno) bin) errno stderr stdout src))))

(defmethod compile-p ((obj ast))
  (with-temp-file (bin)
    (zerop (second (multiple-value-list (phenome obj :bin bin))))))

(defmethod genome-string ((ast ast) &optional stream)
  (let ((genome (or (genome ast) "")))
    (if stream (write-string genome stream) genome)))

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
            (or (progn
                  (error "TODO: alternate interface for :list and :ids.")
                  (ignore-errors
                    (parse-number (apply-mutation ast (list :ids)))))
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

(defmethod crossover ((a ast) (b ast))
  (let ((a-point (random-elt (line-breaks (genome a))))
        (b-point (random-elt (line-breaks (genome b))))
        (new (copy a)))
    (setf (genome new)
          (copy-seq (concatenate 'string
                      (subseq (genome a) 0 a-point)
                      (subseq (genome b) b-point))))
    (values new (list a-point b-point))))

(defgeneric select-crossover-points (a b)
  (:documentation "Select suitable crossover points in A and B.
If no suitable points are found the returned points may be nil."))

(defmethod (setf genome-string) (text (obj ast))
  (setf (genome obj) text))

(defmethod lines ((ast ast))
  (split-sequence #\Newline (genome ast)))

(defmethod line-breaks ((ast ast))
  (cons 0 (loop :for char :in (coerce (genome ast) 'list) :as index 
                :from 0
                :when (equal char #\Newline) :collect index)))

(defmethod (setf lines) (new (ast ast))
  (setf (genome ast) (format nil "~{~a~^~%~}" new)))
