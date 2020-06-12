;;; source.lisp --- source software representation
(defpackage :software-evolution-library/software/source
  (:nicknames :sel/software/source :sel/sw/source)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/components/file)
  (:export :source
           :raw-size))
(in-package :software-evolution-library/software/source)
(in-readtable :curry-compose-reader-macros)


;;; source software objects
(define-software source (software file)
  ((genome   :initarg :genome   :accessor genome   :initform ""
             :copier :direct)
   (raw-size :initarg :size     :accessor raw-size :initform nil
             :copier :none))
  (:documentation "Raw source code software representation."))

;;; NOTE: the following code assumes genome is a string
;;;  In the Clang json from end, offsets are in bytes, not
;;;  characters, so the offset computation code needs to be
;;;  made aware of how many bytes are in each character.
;;;  This may depend on the external coding of the source file.

(defmethod genome-string ((obj source) &optional stream)
  "Return the source code of OBJ, optionally writing to STREAM"
  (let ((genome (or (genome obj) "")))
    (if stream (write-string genome stream) genome)))

(defmethod from-file ((obj source) path)
  "Initialize OBJ with the contents of PATH."
  (setf (genome obj) (file-to-string path))
  obj)

(defmethod from-string ((obj source) string)
  "Initialize OBJ with the contents of STRING."
  (setf (genome obj) string)
  obj)

(defmethod size ((obj source))
  "Return the size of OBJ"
  (or (raw-size obj)
      (setf (raw-size obj)
            (or (progn
                  (error "TODO: alternate interface for :list and :ids.")
                  (ignore-errors
                    (parse-number (apply-mutation obj (list :ids)))))
                0))))

(defmethod mutate ((obj source))
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

(defmethod crossover ((a source) (b source))
  "Crossover two software objects, A and B."
  (let ((a-point (random-elt (line-breaks (genome a))))
        (b-point (random-elt (line-breaks (genome b))))
        (new (copy a)))
    (setf (genome new)
          (copy-seq (concatenate 'string
                      (subseq (genome a) 0 a-point)
                      (subseq (genome b) b-point))))
    (values new (list a-point b-point))))

(defmethod lines ((obj source))
  "Return a list of lines of source in OBJ"
  (split-sequence #\Newline (genome obj)))

(defgeneric line-breaks (source)
  (:documentation "Return a list with the index of line breaks in SOURCE.")
  (:method ((source string))
    (cons 0 (loop :for char :in (coerce source 'list) :as index
               :from 0
               :when (equal char #\Newline) :collect index)))
  (:method ((obj source))
    (line-breaks (genome obj))))

(defmethod (setf lines) (new (obj source))
  "Set the lines of source in OBJ
* NEW list of source code lines
* OBJ software object to modify
"
  (setf (genome obj) (format nil "~{~a~^~%~}" new)))
