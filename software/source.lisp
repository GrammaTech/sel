;;; source.lisp --- source software representation
(defpackage :software-evolution-library/software/source
  (:nicknames :sel/software/source :sel/sw/source)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :split-sequence
        :software-evolution-library
        :software-evolution-library/utility)
  (:export :source
           :compiler
           :ext
           :flags
           :raw-size))
(in-package :software-evolution-library/software/source)
(in-readtable :curry-compose-reader-macros)


;;; source software objects
(define-software source (software)
  ((genome   :initarg :genome   :accessor genome   :initform ""
             :copier :direct)
   (flags    :initarg :flags    :accessor flags    :initform nil
             :copier copy-tree)
   (compiler :initarg :compiler :accessor compiler :initform "clang"
             :copier copy-seq)
   (ext      :initarg :ext      :accessor ext      :initform "c"
             :copier copy-tree)
   (raw-size :initarg :size     :accessor raw-size :initform nil
             :copier :none))
  (:documentation "Raw source code software representation."))

(defmethod phenome ((obj source) &key (bin (temp-file-name)))
  "Compile OBJ to create an executable version of the software
on the filesystem at BIN."
  #-ccl (declare (values t fixnum string string string))
  (setf bin (namestring bin))
  (with-temp-file-of (src (ext obj)) (genome-string obj)
    (multiple-value-bind (stdout stderr errno)
        (shell "~a ~a -o ~a ~{~a~^ ~}" (compiler obj) src bin (flags obj))
      (restart-case
          (unless (zerop errno)
            (error (make-condition 'phenome :text stderr :obj obj :loc src)))
        (retry-project-build ()
          :report "Retry `phenome' on OBJ."
          (phenome obj :bin bin))
        (return-nil-for-bin ()
          :report "Allow failure returning NIL for bin."
          (setf bin nil)))
      (values bin errno stderr stdout src))))

(defmethod genome-string ((obj source) &optional stream)
  "Return the source code of OBJ, optionally writing to STREAM"
  (let ((genome (or (genome obj) "")))
    (if stream (write-string genome stream) genome)))

(defmethod from-file ((obj source) path)
  "Initialize OBJ with the contents of PATH."
  (setf (genome obj) (file-to-string path))
  (setf (ext obj)  (pathname-type (pathname path)))
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

(defmethod (setf genome-string) (text (obj source))
  "Update the source code of OBJ to TEXT."
  (setf (genome obj) text))

(defmethod lines ((obj source))
  "Return a list of lines of source in OBJ"
  (split-sequence #\Newline (genome obj)))

(defmethod line-breaks ((obj source))
  "Return a list with the index of line breaks in OBJ"
  (cons 0 (loop :for char :in (coerce (genome obj) 'list) :as index
                :from 0
                :when (equal char #\Newline) :collect index)))

(defmethod (setf lines) (new (obj source))
  "Set the lines of source in OBJ
* NEW list of source code lines
* OBJ software object to modify
"
  (setf (genome obj) (format nil "~{~a~^~%~}" new)))
