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

(defmethod collect-evolve-files ((project java-project) &aux result)
  (with-current-directory ((project-dir project))
    (walk-directory
     (project-dir project)
     (lambda (file)
       (push (cons (pathname-relativize (project-dir project) file)
                   (from-file (make-instance (component-class project))
                              file))
             result))
     :test (lambda (file)
             ;; Heuristics for identifying files in the project:
             ;; 1) The file is not in an ignored directory.
             ;;    and the file has a "java" extension.
             (let ((rel-path (pathname-relativize (project-dir project)
                                                  file)))
               (and (not (ignored-evolve-path-p project rel-path))
                    (equal "java" (pathname-type rel-path)))))))
  result)
