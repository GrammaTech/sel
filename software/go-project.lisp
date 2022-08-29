(defpackage :software-evolution-library/software/go-project
  (:nicknames :sel/software/go-project :sel/sw/go-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/go
        :software-evolution-library/software/parseable
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/software/compilable
        :software-evolution-library/software/directory
        :software-evolution-library/components/file)
  (:export :go-project))

(in-package :software-evolution-library/software/go-project)
(in-readtable :curry-compose-reader-macros)

(defclass go-project
    (directory-project parseable-project compilable normal-scope)
  ()
  (:documentation "Project specialization for Go software objects"))

(defmethod initialize-instance :after ((project go-project) &key)
  (unless (component-class project)
    (setf (component-class project) 'go))
  (unless (compiler project)
    (setf (compiler project) "golang")))

(defmethod collect-evolve-files ((project go-project))
  (collect-evolve-files-with-extensions project :extensions '("go")))
