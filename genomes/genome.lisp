;;; genome.lisp --- generic support for types of genomes

;; Copyright (C) 2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :software-evolution)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *genome-averaging-keys* nil
  "List of keys to keep averaged during genome manipulation.")

(defgeneric inds (genome)
  (:documentation "Return a list of all indices into GENOME."))

(defgeneric ind (genome index)
  (:documentation "Return the value located at INDEX in GENOME."))

(defgeneric (setf ind) (new genome index)
  (:documentation "Set the value located at INDEX in GENOME."))

(defgeneric del-ind (genome index)
  (:documentation "Delete the value located at INDEX from GENOME."))

(defgeneric size (genome)
  (:documentation "Return the size of GENOME."))

(defgeneric average-keys (genome place)
  (:documentation "Average the keys around PLACE in GENOME."))

(defgeneric cut (genome at)
  (:documentation "Cut the value at AT from GENOME."))

(defgeneric insert (genome from to)
  (:documentation "Insert value from FROM in GENOME to TO in GENOME."))

(defgeneric swap (genome left right)
  (:documentation "Swap the values located at LEFT and RIGHT in GENOME."))
