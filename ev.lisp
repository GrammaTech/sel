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

(defvar *share-freq* )
(defvar *mut-freq* )
(defvar *cross-freq* )


;;; functions
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

(defun variant (population)
  "Generate a new variant from a population."
  )

(defun evaluate (soft)
  "Evaluate SOFT setting the fitness.")
