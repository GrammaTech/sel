;;; neutral.lisp --- Exploration of neutral portions of the fitness landscape

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
;;; Code:
(in-package :software-evolution)

(defun neutral (software &key (many 100))
  (let (variants)
    (loop :for variant = (mutate (copy software))
       :until (= (length variants) many)
       :do (unless (member variant variants :key #'edits :test #'tree-equal)
             (setf (fitness variant) (test-suite variant))
             (push variant variants)))
    variants))

;; TODO: multiple step neutral walks
