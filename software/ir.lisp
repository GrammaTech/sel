;;; ir.lisp --- ir software representation
(defpackage :software-evolution-library/software/ir
  (:nicknames :sel/software/ir :sel/sw/ir)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/components/file)
  (:export :ir
           :raw-size))
(in-package :software-evolution-library/software/ir)
(in-readtable :curry-compose-reader-macros)


;;; IR software objects
(define-software ir (software file)
  ((genome   :initarg :genome   :accessor genome   :initform ""
             :copier :direct)
   (raw-size :initarg :size     :accessor raw-size :initform nil
             :copier :none))
  (:documentation "Raw IR code software representation."))

;;; NOTE: the following code assumes genome is a string
;;;  In the Clang json from end, offsets are in bytes, not
;;;  characters, so the offset computation code needs to be
;;;  made aware of how many bytes are in each character.
;;;  This may depend on the external coding of the IR file.

(defmethod genome-string ((obj ir) &optional stream)
  "Return the IR code of OBJ, optionally writing to STREAM"
  (let ((genome (or (genome obj) "")))
    (if stream (write-string genome stream) genome)))

(defmethod from-file ((obj ir) path)
  "Initialize OBJ with the contents of PATH."
  (setf (genome obj) (file-to-string path))
  obj)

(defmethod from-string ((obj ir) string)
  "Initialize OBJ with the contents of STRING."
  (setf (genome obj) string)
  obj)

(defmethod size ((obj ir))
  "Return the size of OBJ"
  (or (raw-size obj)
      (setf (raw-size obj)
            (or (progn
                  (error "TODO: alternate interface for :list and :ids.")
                  (ignore-errors
                    (parse-number (apply-mutation obj (list :ids)))))
                0))))

(defmethod mutate ((obj ir))
  "Apply a mutation to OBJ"
  (unless (> (size obj) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj obj)))
  (let ((op (case (random-elt '(cut insert swap))
              (cut    `(:cut    (:stmt1 . ,(pick-bad obj))))
              (insert `(:insert (:stmt1 . ,(pick-bad obj))
                                (:stmt2 . ,(pick-good obj))))
              (swap   `(:swap   (:stmt1 . ,(pick-bad obj))
                                (:stmt2 . ,(pick-good obj)))))))
    (apply-mutation obj op)
    (values obj op)))

(defmethod crossover ((a ir) (b ir))
  "Crossover two software objects, A and B."
  (let ((a-point (random-elt (line-breaks (genome a))))
        (b-point (random-elt (line-breaks (genome b))))
        (new (copy a)))
    (setf (genome new)
          (copy-seq (concatenate 'string
                      (subseq (genome a) 0 a-point)
                      (subseq (genome b) b-point))))
    (values new (list a-point b-point))))

(defmethod lines ((obj ir) &rest args &key)
  "Return a list of lines of IR in OBJ"
  (apply #'lines (genome obj) args))

(defgeneric line-breaks (ir)
  (:documentation "Return a list with the index of line breaks in IR.")
  (:method ((ir string))
    (cons 0 (loop :for char :in (coerce ir 'list) :as index
               :from 0
               :when (equal char #\Newline) :collect index)))
  (:method ((obj ir))
    (line-breaks (genome obj))))

(defmethod (setf lines) (new (obj ir))
  "Set the lines of IR in OBJ
* NEW list of IR code lines
* OBJ software object to modify
"
  (setf (genome obj) (format nil "~{~a~^~%~}" new)))
