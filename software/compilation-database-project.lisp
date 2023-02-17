(defpackage :software-evolution-library/software/compilation-database-project
  (:nicknames :sel/software/compilation-database-project
   :sel/sw/compilation-database-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/components/compilation-database
        :software-evolution-library/software/simple
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project)
  (:local-nicknames (:json :cl-json))
  (:export
    :compilation-database-project
    :compilation-database
    :ensure-compilation-database))
(in-package :software-evolution-library/software/compilation-database-project)

(define-software compilation-database-project (project)
  ((compilation-database :initarg :compilation-database
                         :accessor compilation-database
                         :initform nil
                         :documentation "Compilation database for the project.
See https://clang.llvm.org/docs/JSONCompilationDatabase.html for
information on the format of compilation databases.")))

(defmethod collect-evolve-files :before ((obj compilation-database-project))
  "Ensure OBJ has a compilation-database populated."
  (unless (compilation-database obj)
    (let* ((comp-db-paths
            (mapcar (op (project-relative-pathname obj _))
                    '("compile_commands.json"
                      "build/compile_commands.json")))
           (compilation-database
            (progn
              (unless (find-if #'file-exists-p comp-db-paths)
                (ensure-compilation-database obj))
              (when-let (comp-db-path (find-if #'file-exists-p comp-db-paths))
                (with-open-file (in comp-db-path)
                  (parse-compilation-database in))))))
      (when compilation-database
        (setf (compilation-database obj) compilation-database)))))

(defgeneric ensure-compilation-database (obj)
  (:method ((obj compilation-database-project))
    nil))
