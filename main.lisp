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
(in-package :soft-ev)

(defvar *seed-soft* nil
  "File holding a seed individual")

(defvar *incoming-population* nil
  "File holding a cl-store serialized incoming population.")

(defvar *save-soft* nil
  "File to hold any potential individual returned by `evolve'.")

(defvar *save-population* nil
  "Save the final population here.")

(defvar *paths* nil
  "List of pairs of the form '(keyword . \"sample-file\").")

(defvar *options*
  `(*max-population-size*
    *tournament-size*
    *test-script*
    *pos-test-num*
    *neg-test-num*
    *pos-test-mult*
    *neg-test-mult*
    *cross-chance*
    *paths*
    *seed-soft*
    *incoming-population*
    *save-soft*
    *save-population*
    ,@(remove nil
              (mapcar
               (lambda (line)
                 (cl-ppcre:register-groups-bind (name doc)
                     ("^ *(.*)? -* (.*) *$" line)
                   (cons name doc)
                   (eval `(defvar ,(intern name) nil ,doc))
                   (intern name)))
               (cl-ppcre:split
                "\\n" (documentation 'evolve 'function))))))

(defun show-usage ()
  (format t "~&USAGE: ./soft-ev options-file.lisp~%~%")
  (format t "~&(in-package :soft-ev)~%")
  (format t "~&(setq~%")
  (format t "~:{~& ~21a ~11S ;; ~a~}~%"
          (mapcar (lambda (opt)
                    (let* ((val (if (boundp opt)
                                    opt
                                    (intern (symbol-name opt) :soft-ev)))
                           (doc (documentation val 'variable))
                           (default (eval val)))
                      (list opt default doc)))
                  *options*))
  (format t "~&)~%"))

(defun print-options ()
  (print (mapcar (lambda (option) `(,option . ,(eval option)))
                 (remove-if-not (lambda (option) (eval option)) *options*))))

(defun main (argv &aux res)
  "Command line driver of `soft-ev' software evolution."
  ;; set options of load usage information
  (if (not (and (second argv) (probe-file (second argv))))
      (show-usage)
      (progn
        (load (second argv))
        (print-options)
        ;; build initial population
        (when *incoming-population*
          (setq *population* (cl-store:restore *incoming-population*)))
        (when *seed-soft*
          (dotimes (_ 12)
            (let ((asm (asm-from-file *seed-soft*)))
              (dolist (spec *paths*)
                (apply-path asm (car spec)
                            (samples-from-oprofile-file (cdr spec))))
              (incorporate asm))))
        (when *paths*
          (setq *genome-averaging-keys* (mapcar #'car *paths*)))
        ;; evolve
        (if *population*
            (progn
              (format t "~&;; evolving...~%")
              (setq res (evolve :max-evals max-evals
                                :max-time max-time
                                :max-inds max-inds
                                :max-fit max-fit
                                :min-fit min-fit
                                :pop-fn pop-fn
                                :ind-fn ind-fn))
              ;; save results
              (when (and res *save-soft*)
                (cl-store:store res *save-soft*))
              (when *save-population*
                (cl-store:store *population* *save-population*)))
            (format t "~&Can't evolve without some initial population.~%")))))
