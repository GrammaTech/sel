;;; rust-project.lisp --- Projects composed of Rust objects
(defpackage :software-evolution-library/software/rust-project
  (:nicknames :sel/software/rust-project
              :sel/sw/rust-project)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/software/tree-sitter)
  (:export :rust-project))
(in-package :software-evolution-library/software/rust-project)
(in-readtable :curry-compose-reader-macros)

(define-software rust-project (parseable-project) ()
  (:documentation "Project specialization for rust software objects."))

(defmethod initialize-instance :after ((rust-project rust-project) &key)
  (setf (component-class rust-project)
        (or (component-class rust-project) 'rust)))

(defmethod collect-evolve-files ((project rust-project) &aux result)
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
             ;;    and the file has a "rust" extension.
             (let ((rel-path (pathname-relativize (project-dir project)
                                                  file)))
               (and (not (ignored-evolve-path-p project rel-path))
                    (equal "rs" (pathname-type rel-path)))))))
  result)
