;;; repair.lisp --- program repair using software-evolution

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
(in-package :software-evolution)

(defun repair (&key max-evals max-time max-inds max-fit min-fit pop-fn ind-fn)
  "Evolves population until an optional stopping criterion is met.

Optional keys are as follows.
  MAX-EVALS ------- quit after this many fitness evaluations
  MAX-INDS -------- quit after this many new individuals have been tried
  MAX-TIME -------- quit after this many seconds
  MAX-FIT --------- quit when an individual achieves this fitness or higher
  MIN-FIT --------- quit when an individual achieves this fitness or lower
  POP-FN ---------- quit when the population satisfies this function
  IND-FN ---------- quit when an individual satisfies this function"
  (let ((start-time (get-internal-real-time))
        (inds 0))
    (setq *fitness-evals* 0)
    (setq *running* t)
    (loop :until (or (not *running*)
                     (and max-evals (> *fitness-evals* max-evals))
                     (and max-inds (> inds max-inds))
                     (and max-time (> (/ (- (get-internal-real-time) start-time)
                                         internal-time-units-per-second)
                                      max-time)))
       :do (let ((new (new-individual)))
             (incf inds)
             (setf (fitness new) (evaluate new))
             (assert (numberp (fitness new)))
             (incorporate new)
             (when (or (and max-fit (>= (fitness new) max-fit))
                       (and min-fit (<= (fitness new) min-fit))
                       (and ind-fn (funcall ind-fn new)))
               (return new))
             (when (and pop-fn (funcall pop-fn *population*))
               (return))))))
