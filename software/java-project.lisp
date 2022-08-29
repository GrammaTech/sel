;;; java-project.lisp --- Projects composed of Java objects
(defpackage :software-evolution-library/software/java-project
  (:nicknames :sel/software/java-project
              :sel/sw/java-project)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/directory)
  (:export :java-project))
(in-package :software-evolution-library/software/java-project)
(in-readtable :curry-compose-reader-macros)

(define-software java-project (directory-project parseable-project) ()
  (:documentation "Project specialization for java software objects."))

(defmethod initialize-instance :after ((java-project java-project) &key)
  (setf (component-class java-project)
        (or (component-class java-project) 'java)))

(defmethod collect-evolve-files ((project java-project))
  (collect-evolve-files-with-extensions project :extensions '("java")))
