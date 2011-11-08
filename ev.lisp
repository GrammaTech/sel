;;; ev.lisp --- evolutionary operations over software objects

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


;;; settings
(defvar *tournament-size* 2
  "Number of individuals to participate in tournament selection.")

(defvar *test-script* nil
  "The script to use to evaluate individuals.")

(defvar *pos-test-num* 0
  "Number of positive tests.")

(defvar *neg-test-num* 0
  "Number of negative tests.")

(defvar *pos-test-mult* 1
  "Multiplier for positive test cases")

(defvar *neg-test-mult* 1
  "Multiplier for negative test cases")

(defvar *share-freq* )
(defvar *mut-freq* )
(defvar *cross-freq* )


;;; functions
(defun evaluate (soft &aux (pos 0) (neg 0))
  "Evaluate SOFT setting the fitness."
  (let ((exe (temp-file-name)))
    (executable soft exe)
    (loop for i from 0 to *pos-test-num*
       do (multiple-value-bind (_ _ exit)
              (shell-command (format nil "~a ~a p~d" *test-script* exe i))
            (when (= exit 0) incf pos)))
    (loop for i from 0 to *neg-test-num*
       do (multiple-value-bind (_ _ exit)
              (shell-command (format nil "~a ~a n~d" *test-script* exe i))
            (when (= exit 0) incf neg)))
    (+ (* pos *pos-test-mult*)
       (* neg *neg-test-mult*))))

(defun incorporate (population soft)
  "Incorporate SOFT into POPULATION, keeping the size of POPULATION constant."
  (evict population)
  (push soft population))

(defun evict ()
  (let ((loser (tournament #'<)))
    (setf *population* (remove loser *population* :count 1))
    loser))

(defun tournament (predicate &aux competitors)
  "Select an individual from *POPULATION* with a tournament of size NUMBER."
  (sort (dotimes (_ *tournament-size* competitors)
          (push (random-elt *population*) competitors))
        predicate :key #'fitness))

(defun mutant ()
  "Generate a new mutant from a population."
  (apply (case (random 3)
           (0 #'insert)
           (1 #'cut)
           (2 #'swap))
         (tournament #'<)))

(defun crossed ()
  "Generate a new individual from a population using crossover."
  (crossover (tournament #'<) (tournament #'<)))
