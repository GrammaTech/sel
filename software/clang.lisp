;;; clang.lisp --- clang software representation

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
  ((compiler :initarg :compiler :accessor compiler :initform "clang")
   (clang-asts :initarg :clang-asts :accessor clang-asts  :initform nil)))

(defmethod apply-mutation ((clang clang) op)
  (clang-mutate clang op))

(defmethod clang-mutate ((clang clang) op)
  (with-temp-file-of (src (ext clang)) (genome clang)
    (multiple-value-bind (stdout stderr exit)
      (shell "clang-mutate ~a ~a ~a -- ~{~a~^ ~}|tail -n +2"
             (ecase (car op)
               (:cut              "-cut")
               (:cut-full-stmt    "-cut")
               (:insert           "-insert")
               (:swap             "-swap")
               (:swap-full-stmt   "-swap")
               (:set-value        "-set")
               (:insert-value     "-insert-value")
               (:insert-full-stmt "-insert-value")
               (:ids              "-ids")
               (:list             "-list")
               (:list-json        "-list -json"))
             (mapconcat (lambda (pair)
                          (if (stringp (cdr pair))
                              (format nil "-value='~a'" (cdr pair))
                              (format nil "-stmt~d=~d" (car pair) (cdr pair))))
                        (loop :for id :in (cdr op) :as i :from 1
                           :collect (cons i id)) " ")
             src (flags clang))
      (declare (ignorable stderr exit))
      stdout)))

(defmethod asts-of ((clang clang))
  (or (clang-asts clang)
      (setf (clang-asts clang) (to-ast-hash-table clang))))

(defmethod to-ast-hash-table ((clang clang))
  (let ((ast-hash-table (make-hash-table :test 'equal)))
    (dolist (ast-entry 
              (let ((list-string (clang-mutate clang '(:list-json))))
                (unless (zerop (length list-string))
                  (json:decode-json-from-source list-string))))
      (let* ((ast-class (aget :AST--CLASS ast-entry))
             (cur (gethash ast-class ast-hash-table)))
        (setf (gethash ast-class ast-hash-table) (cons ast-entry cur))))
    ast-hash-table))

(defmethod crossover ((a clang) (b clang))
  (let* ((a-asts (asts-of a))
         (b-asts (asts-of b))
         (random-ast-class (random-hash-table-key a-asts))
         (a-crossover-ast (when-let ((it (gethash random-ast-class a-asts)))
                            (random-elt it)))
         (b-crossover-ast (when-let ((it (gethash random-ast-class b-asts)))
                            (random-elt it)))
         (variant (copy a)))
    (if (and a-crossover-ast b-crossover-ast)
        (let* ((a-crossover-src-ln (aget :END--SRC--LINE a-crossover-ast))
               (a-crossover-src-col (aget :END--SRC--COL a-crossover-ast))
               (a-line-breaks (line-breaks a))
               (a-crossover-pt (+ (nth (1- a-crossover-src-ln) a-line-breaks)
                                  a-crossover-src-col))
               (b-crossover-src-ln (aget :BEGIN--SRC--LINE b-crossover-ast))
               (b-crossover-src-col (aget :BEGIN--SRC--COL b-crossover-ast))
               (b-line-breaks (line-breaks b))
               (b-crossover-pt (+ (nth (1- b-crossover-src-ln) b-line-breaks)
                                   b-crossover-src-col)))
          (setf (genome variant)
                (copy-seq (concatenate 'string
                            (subseq (genome a) 0 a-crossover-pt)
                            (subseq (genome b) b-crossover-pt))))))
    variant))
       


(defmethod phenome ((clang clang) &key bin)
  (with-temp-file-of (src (ext clang)) (genome clang)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell "~a ~a -o ~a ~a"
                 (compiler clang)
                 src bin (mapconcat #'identity (flags clang) " "))
        (declare (ignorable stdout stderr))
        (values (if (zerop exit) bin stderr) exit)))))
