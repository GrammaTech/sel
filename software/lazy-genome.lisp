(defpackage :software-evolution-library/software/lazy-genome
  (:documentation "Software with a lazily-parsed genome")
  (:use :gt/full
        :software-evolution-library)
  (:export
    :parse-lazy-genome
    :serialize-lazy-genome
    :lazy-genome))
(in-package :software-evolution-library/software/lazy-genome)

(define-software lazy-genome (software)
  ((genome  :initarg :genome :accessor genome :initform ""
            :documentation "Lazily parsed AST representation of the code."
            ;; We don't want to force parsing the genome when copying.
            :copier :direct)))

(defgeneric parse-lazy-genome (software genome)
  (:documentation "Parse GENOME, the genome of SOFTWARE."))

(defgeneric serialize-lazy-genome (software genome stream)
  (:documentation "Seriailize GENOME, the genome of SOFTWARE, to STREAM."))

(defmethod genome-string ((obj lazy-genome) &optional stream)
  "Return the source code of OBJ, optionally writing to STREAM"
  (with-slots (genome) obj
    (typecase genome
      (string
       (if (null stream) genome
           (with-string (s stream)
             (write-string genome s))))
      (pathname
       (setf (genome-string obj) (read-file-into-string genome))
       (genome-string obj stream))
      (t
       (with-string (s stream)
         (serialize-lazy-genome obj genome s))))))

(defmethod (setf genome-string) ((new string) (obj lazy-genome))
  ;; We will lazily parse the ASTs from the genome when it is next accessed.
  (setf (genome obj) new))

(defmethod genome :before ((obj lazy-genome))
  "Lazily parse the genome upon first access."
  (with-slots (genome) obj
    (typecase genome
      ((or string pathname)
       (setf genome (parse-lazy-genome obj genome))))))

(defmethod from-file ((obj lazy-genome) path)
  "Initialize LAZY-GENOME with PATH."
  ;; `truename' signals an error if it doesn't exist.
  (let ((path (truename path)))
    (setf (genome obj) path))
  obj)

(defmethod from-string ((obj lazy-genome) string)
  "Initialize OBJ with the contents of STRING."
  (setf (genome obj) string)
  obj)
