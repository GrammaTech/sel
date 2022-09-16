(defpackage :software-evolution-library/software/cl-yaml-project
  (:nicknames :sel/sw/cl-yaml-project)
  (:use
   :gt/full
   :software-evolution-library
   :software-evolution-library/software/project
   :software-evolution-library/software/cl-yaml
   :software-evolution-library/components/file)
  (:shadowing-import-from :software-evolution-library/software/directory
                          :collect-evolve-files*)
  (:export :yaml-project)
  (:documentation "Package for projects consisting of yaml files.
Currently, the individual yaml files are not represented as ASTs, but
as trees of hash tables produced by CL-YAML."))

(in-package :software-evolution-library/software/cl-yaml-project)
(in-readtable :curry-compose-reader-macros)

(defclass yaml-project (project)
  ()
  (:documentation "A set of yaml files, possibly refering each other"))

(defmethod initialize-instance :after ((project yaml-project) &key)
  (unless (component-class project)
    (setf (component-class project) 'yaml)))

(defmethod collect-evolve-files ((project yaml-project))
  (collect-evolve-files* project :extensions '("yaml")))

