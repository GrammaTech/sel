;;; rust-project.lisp --- Projects composed of Rust objects
(defpackage :software-evolution-library/software/rust-project
  (:nicknames :sel/software/rust-project
              :sel/sw/rust-project)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/directory)
  (:export :rust-project))
(in-package :software-evolution-library/software/rust-project)
(in-readtable :curry-compose-reader-macros)

(define-software rust-project (directory-project parseable-project) ()
  (:documentation "Project specialization for rust software objects."))

(defmethod initialize-instance :after ((rust-project rust-project) &key)
  (setf (component-class rust-project)
        (or (component-class rust-project) 'rust)))

(defmethod collect-evolve-files ((project rust-project))
  (collect-evolve-files* project :extensions '("rs")))
