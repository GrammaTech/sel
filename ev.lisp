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

(defun tournament (population number)
  "Select an individual from POPULATION with a tournament of size NUMBER.")

(defun evaluate (soft)
  "Evaluate SOFT setting the fitness.")

(defun insert (soft)
  "Duplicate and insert an element of the genome of SOFT")

(defun delete (soft)
  "Delete an element of the genome of SOFT.")

(defun swap (soft)
  "Swap two elements of the genome of SOFT.")

(defun crossover (soft-a soft-b)
  "Crossover between the genomes of SOFT-A and SOFT-B.")
