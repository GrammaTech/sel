;;; clang-project.lisp --- Projects with a clang compilation database
(defpackage :software-evolution-library/software/clang-project
  (:nicknames :sel/software/clang-project :sel/sw/clang-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/components/compilation-database
        :software-evolution-library/software/simple
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/compilation-database-project)
  (:export :clang-project))
(in-package :software-evolution-library/software/clang-project)
(in-readtable :curry-compose-reader-macros)

(define-software clang-project (compilation-database-project parseable-project)
  ()
  (:documentation "Project specialization for clang software objects."))

(defmethod initialize-instance :after ((clang-project clang-project)
                                       &key (compilation-database
                                             nil compilation-database-supplied?))
  (setf (component-class clang-project)
        (or (component-class clang-project) 'clang))
  (when compilation-database-supplied?
    (setf (compilation-database clang-project)
          (parse-compilation-database compilation-database
                                      :path (project-dir clang-project)))))

(defmethod mutation-key ((obj project) op)
  "Return key used to organize mutations in *mutation-stats* hashtable.
* OBJ object mutation is to be applied to
* OP operation to be performed
"
  (list (nth 1 op))) ; default to mutation type, as per default mutation-key impl

(defmethod ensure-compilation-database ((obj clang-project))
  "Ensure CLANG-PROJECT has a compilation-database populated."
  ;; (assert (or (compilation-database obj)
  ;;             (and (build-command obj) (which "bear")))
  ;;         (obj)
  ;;         "Clang-project requires a compilation-database ~
  ;;          or a build-command and 'bear' in your PATH")
  (let ((comp-db-path (project-relative-pathname obj "compile_commands.json")))
    (multiple-value-bind (stdout stderr errno)
        (shell "cd ~a && bear ~a" (project-dir obj) (build-command obj))
      (declare (ignorable errno))
      (unless (probe-file comp-db-path)
        (error "Failed to create compilation database for project.~%~
                   build command: ~a~%~
                   stdout: ~a~%~
                   stderr: ~a~%"
               (build-command obj)
               stdout stderr)))))

(defmethod collect-evolve-files ((clang-project clang-project))
  (labels ((get-file-path (entry)
             (with-accessors ((directory command-object.directory)
                              (file command-object.file))
                 entry
               (merge-pathnames-as-file (nest (ensure-directory-pathname)
                                              directory)
                                        file))))
    (mapcar (lambda (entry)
              (cons (pathname-relativize (project-dir clang-project)
                                         (get-file-path entry))
                    (from-file
                     (make-instance (component-class clang-project)
                                    :compiler (command-object.compiler entry)
                                    :flags (command-object.flags entry))
                     (get-file-path entry))))
            (remove-if
             (lambda (obj)
               (or (ignored-evolve-path-p clang-project
                                          (command-object.file obj))
                   (not (file-exists-p (get-file-path obj)))))
             (compilation-database.command-objects
              (compilation-database clang-project))))))
