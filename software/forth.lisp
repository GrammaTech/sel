;;; forth.lisp --- software representation of Forth code

;; Copyright (C) 2014  Eric Schulte

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


;;; asm software objects
(defclass forth (simple)
  ((compiler :initarg :compiler :accessor compiler :initform nil)))

(defvar *forth-linker* "gforth")

(defmethod from-file ((forth forth) path)
  (flet ((rm-eol-comments (line)
           (subseq line 0 (search "\\ " line)))
         (rm-inline-comments (line)
           (cl-ppcre:regex-replace-all "[\\s]\\([\\s][^\\)]*\\)" line "")))
    (with-open-file (in path)
      (loop :for line = (read-line in nil) :while line :append
         (mapcar {cons :symbol}
                 (remove-if #'emptyp
                            (split "[\\s]+" (rm-inline-comments
                                             ;; TODO: handle
                                             ;;       strings
                                             ;;       before
                                             ;;       inline
                                             ;;       comments
                                             (rm-eol-comments line)))))))))

(defmethod copy ((forth forth))
  (make-instance (type-of forth)
    :fitness (fitness forth)
    :genome (copy-tree (genome forth))
    :compiler (compiler forth)))

(defmethod phenome ((forth forth) &key bin)
  (with-temp-file-of (src "fs") (genome-string forth)
    (let ((bin (or bin (temp-file-name))))
      (multiple-value-bind (stdout stderr exit)
          (shell #| TODO: |# (or (linker forth) *forth-linker*) bin src)
        (declare (ignorable stdout ))
        (values (if (zerop exit) bin stderr) exit)))))
