(defpackage :software-evolution-library/software/cl-yaml
  (:nicknames :sel/software/cl-yaml :sel/sw/cl-yaml)
  (:use
   :gt/full
   :cl-json
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/components/file)
  (:shadowing-import-from :cl-yaml)
  (:shadowing-import-from
   :software-evolution-library/software/tree-sitter
   :create-tree-sitter-language
   :define-language-alias-mappings)
  (:export :yaml))

(in-package :software-evolution-library/software/cl-yaml)
(in-readtable :curry-compose-reader-macros)

(define-software yaml (parseable)
  ()
  (:documentation "Parsed YAML representation"))

(defmethod from-file ((obj yaml) (file string))
  (with-open-file (s file :direction :input)
    (from-file obj s)))

(defmethod from-file ((obj yaml) (file pathname))
  (setf (genome obj) (cl-yaml:parse file))
  obj)

(defmethod parse-asts ((obj yaml) &optional source-text)
  (if source-text
      (setf (genome obj) (cl-yaml:parse source-text))
      (let ((g (slot-value obj 'genome)))
        (when (and g (not (equal "" g)))
          (setf (genome obj) (cl-yaml:parse g))))))




