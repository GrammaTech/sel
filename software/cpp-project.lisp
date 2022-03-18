;;; cpp-project.lisp --- C++ Projects
;;;
(defpackage :software-evolution-library/software/cpp-project
  (:nicknames :sel/software/cpp-project :sel/sw/cpp-project)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/simple
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/compilable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c-cpp-project
        :software-evolution-library/software/directory)
  (:export :cpp-project))
(in-package :software-evolution-library/software/cpp-project)
(in-readtable :curry-compose-reader-macros)

(defparameter *cpp-extensions*
  '("h" "cpp" "cp" "cc")
  "List of extensions we will consider for evolving.")

(define-software cpp-project
    (directory-project parseable-project compilable include-paths-mixin)
  ()
  (:documentation "Project specialization for c++ software objects."))

(defmethod initialize-instance :after ((project cpp-project) &key)
  (unless (component-class project)
    (setf (component-class project) 'cpp))
  (unless (compiler project)
    (setf (compiler project) "c++")))

(defmethod collect-evolve-files ((project cpp-project) &aux result)
  (with-current-directory ((project-dir project))
    (walk-directory
     (project-dir project)
     (lambda (file)
       (push (cons (pathname-relativize (project-dir project) file)
                   (from-file (make-instance (component-class project)
                                             :compiler (compiler project)
                                             :flags (flags project))
                              file))
             result))
     :test (lambda (file)
             ;; Heuristics for identifying files in the project:
             ;; 1) The file is not in an ignored directory.
             ;; 2) The file has an extension from the list *cpp-extensions*.
             (let ((rel-path (pathname-relativize (project-dir project) file)))
               (and (not (ignored-evolve-path-p project rel-path))
                    (member (pathname-type file) *cpp-extensions*
                            :test 'equal))))))
  result)

