;;; c-project.lisp --- C projects.
;;;
(defpackage :software-evolution-library/software/c-project
  (:nicknames :sel/software/c-project :sel/sw/c-project)
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
  (:export :c-project))
(in-package :software-evolution-library/software/c-project)
(in-readtable :curry-compose-reader-macros)

(defparameter *c-extensions*
  '("c" "h")
  "List of extensions we will consider for evolving.")

(define-software c-project
    (directory-project parseable-project compilable include-paths-mixin)
  ()
  (:documentation "Project specialization for c software objects."))

(defmethod initialize-instance :after ((project c-project) &key)
  (unless (component-class project)
    (setf (component-class project) 'c))
  (unless (compiler project)
    (setf (compiler project) "cc")))

(defmethod collect-evolve-files ((project c-project) &aux result)
  (with-current-directory ((project-dir project))
    (walk-directory
     (project-dir project)
     (lambda (file)
       (handler-case
           (push (cons (pathname-relativize (project-dir project) file)
                       (from-file (make-instance (component-class project)
                                                 :compiler (compiler project)
                                                 :flags (flags project))
                                  file))
                 result)
         ;; A file error can occur if the file is unreadable, or if
         ;; it's a symlink to a nonexistent target.  Do not include the
         ;; file in that case.
         (file-error () nil)))
     :test (lambda (file)
             ;; Heuristics for identifying files in the project:
             ;; 1) The file is not in an ignored directory.
             ;; 2) The file has an extension specified in *c-extensions*.
             (let ((rel-path (pathname-relativize (project-dir project) file)))
               (and (not (ignored-evolve-path-p project rel-path))
                    (member (pathname-type file) *c-extensions*
                            :test 'equal)))))
    result))

(defmethod find-include-files ((proj project) (file t) (include c/cpp-preproc-include))
  (let ((path (c-path include)))
    (match (source-text path)
      ((ppcre "\"(.*)\"" s) (find-include-files proj file s))
      ((ppcre "<(.*)>" s)
       (warn "Processing of system includes not yet implemented: ~a" include)
       (let ((s (ensure-suffix s ".h")))
         (find-include-files proj file s)))
      (_
       (warn "Could not understand include AST: ~a" include)
       nil))))
