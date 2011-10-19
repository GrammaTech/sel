;;; soft.lisp --- general representation of an instance of software

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

(defclass soft ()
  ((genome  :initarg :genome  :accessor genome)
   (path    :initarg :path    :accessor path)
   (fitness :initarg :fitness :accessor fitness)
   (edits   :initarg :edits   :accessor edits)))

(defmethod copy ((soft soft))
  "Return a copy of a software artifact.")

(defun from-file (path)
  "Read a file on disk into a software object in memory.")

(defmethod to-file ((soft soft) path)
  "Write a software object in memory to a file on disk.")

(defmethod executable ((soft soft) path)
  "Write (and possibly compile) a software object to an executable.")
