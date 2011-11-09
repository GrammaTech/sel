;;; main.lisp --- command line interface to soft-ev

;; Copyright (C) 2011  Eric Schulte

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
(require :soft-ev)
(require :cl-ppcre)
(use-package :cl-ppcre)

(defvar *options*
  `(*max-population-size*
    *tournament-size*
    *test-script*
    *pos-test-num*
    *neg-test-num*
    *pos-test-mult*
    *neg-test-mult*
    *cross-chance*
    ,@(remove nil
              (mapcar
               (lambda (line)
                 (cl-ppcre:register-groups-bind (name doc)
                     ("^ *(.*)? -* (.*) *$" line)
                   (let ((sym )))
                   (cons name doc)
                   (eval `(defvar ,(intern name) nil ,doc))
                   (intern name)))
               (cl-ppcre:split
                "\\n" (documentation 'soft-ev:evolve 'function))))))

(defun main (argv)
  "Command line driver of `soft-ev' software evolution."
  (when (member (second argv) '("-help" "--help" "help") :test #'equal)
    (format t "~&USAGE: ./soft-ev [options]~%")
    (format t "~&options:~%")
    (format t "~:{~& ~22a ~77a ~a~}"
            (mapcar (lambda (opt)
                      (let* ((val (if (boundp opt)
                                      opt
                                      (intern (symbol-name opt) :soft-ev)))
                             (doc (documentation val 'variable))
                             (default (eval val)))
                        (list opt doc default)))
                    *options*)))
  (format t "~&Software Evolution~%"))
